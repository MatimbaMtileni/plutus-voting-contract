{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Voting.OffChain where

import Plutus.V2.Ledger.Api
import Plutus.V2.Ledger.Contexts
import Plutus.Contract
import Plutus.Trace
import Ledger (PaymentPubKeyHash(..))
import Ledger.Constraints as Constraints
import Ledger.Tx (getCardanoTxId)
import qualified Ledger.Typed.Scripts as Scripts
import qualified Plutus.Script.Utils.V2.Typed.Scripts as Scripts
import Control.Monad (void)
import qualified Data.Map as Map

import Voting.Types
import Voting.Validator
import Voting.Helpers

-- Script instance for the voting validator
data Voting
instance Scripts.ValidatorTypes Voting where
    type instance DatumType Voting = VoteDatum
    type instance RedeemerType Voting = VoteAction

votingScript :: VotingParams -> Scripts.TypedValidator Voting
votingScript params = Scripts.mkTypedValidator @Voting
    ($$(PlutusTx.compile [|| mkVotingValidator ||])
        `PlutusTx.applyCode` PlutusTx.liftCode params)
    $$(PlutusTx.compile [|| wrap ||])
  where
    wrap = Scripts.mkUntypedValidator

-- Initialize voting contract
initializeVoting 
    :: VotingParams
    -> [PubKeyHash]
    -> [Candidate]
    -> BuiltinString
    -> Integer
    -> Contract w s Text ()
initializeVoting params voters candidates title minAda = do
    let datum = createInitialDatum voters candidates title
        tx = Constraints.mustPayToTheScript datum (lovelaceValueOf minAda)
    ledgerTx <- submitTxConstraints (votingScript params) tx
    void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
    logInfo @String $ "Voting contract initialized: " ++ show title

-- Get voting results
getVotingResults :: VotingParams -> Contract w s Text VotingResult
getVotingResults params = do
    utxos <- utxosAt (validatorAddress params)
    case findVotingUTXO utxos of
        Nothing -> throwError "No voting UTXO found"
        Just (_, _, datum) -> do
            let counts = calculateVoteCounts (vdCandidates datum) (vdVotes datum)
                totalVotes = toInteger $ length (vdVotes datum)
                totalVoters = toInteger $ length (vdVoters datum)
                turnout = if totalVoters > 0 
                         then (fromIntegral totalVotes / fromIntegral totalVoters) * 100
                         else 0
                winner = findWinner counts
            pure $ VotingResult
                { vrWinner = winner
                , vrVoteCounts = counts
                , vrTotalVotes = totalVotes
                , vrVoterTurnout = turnout
                }

-- Helper to find voting UTXO
findVotingUTXO 
    :: Map.Map TxOutRef ChainIndexTxOut
    -> Maybe (TxOutRef, ChainIndexTxOut, VoteDatum)
findVotingUTXO utxos = do
    let votingUTXOs = Map.filter isVotingUTXO utxos
    case Map.toList votingUTXOs of
        [(oref, o)] -> do
            datum <- getDatum o
            pure (oref, o, datum)
        _ -> Nothing
  where
    isVotingUTXO :: ChainIndexTxOut -> Bool
    isVotingUTXO = \case
        ScriptChainIndexTxOut{} -> True
        _ -> False
    
    getDatum :: ChainIndexTxOut -> Maybe VoteDatum
    getDatum ScriptChainIndexTxOut{_ciTxOutDatum} = do
        Datum d <- either (const Nothing) Just _ciTxOutDatum
        PlutusTx.unsafeFromBuiltinData d
    getDatum _ = Nothing