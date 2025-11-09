{-# LANGUAGE BangPatterns #-}

module Voting.Validator where

import Plutus.V2.Ledger.Api
import Plutus.V2.Ledger.Contexts
import PlutusTx
import PlutusTx.Prelude hiding (Semigroup(..), unless)
import Voting.Types
import Voting.Helpers

{-# INLINABLE mkVotingValidator #-}
mkVotingValidator :: VotingParams -> VoteDatum -> VoteAction -> ScriptContext -> Bool
mkVotingValidator params datum redeemer ctx =
    case redeemer of
        Initialize ->
            validateInitialization params datum ctx
            
        Vote candidate ->
            validateVote params datum candidate ctx
            
        Close ->
            validateClose params datum ctx
            
        Tally ->
            validateTally params ctx
            
        RegisterVoter newVoter ->
            validateRegisterVoter params datum newVoter ctx
  where
    info = scriptContextTxInfo ctx
    
    {-# INLINABLE validateInitialization #-}
    validateInitialization :: VotingParams -> VoteDatum -> ScriptContext -> Bool
    validateInitialization params' datum' ctx' =
        traceIfFalse "Not authorized: only admin can initialize" 
            (txSignedBy (scriptContextTxInfo ctx') (vpAdmin params')) &&
        traceIfFalse "Initialization requires correct datum state" 
            (vdVotingOpen datum' && null (vdVotes datum')) &&
        traceIfFalse "Insufficient ADA locked" 
            (validateMinAdaLocked params' ctx')
    
    {-# INLINABLE validateVote #-}
    validateVote :: VotingParams -> VoteDatum -> Candidate -> ScriptContext -> Bool
    validateVote params' datum' candidate ctx' =
        traceIfFalse "Voting is closed" (vdVotingOpen datum') &&
        traceIfFalse "Invalid candidate" 
            (isValidCandidate candidate (vdCandidates datum')) &&
        case findVoterSignature (scriptContextTxInfo ctx') (vdVoters datum') of
            Nothing -> traceError "No registered voter signed this transaction"
            Just voter ->
                traceIfFalse "Voter already voted" 
                    (not $ hasVoted voter (vdVotes datum')) &&
                traceIfFalse "Invalid vote state transition" 
                    (validateVoteTransition voter candidate datum' ctx')
    
    {-# INLINABLE validateClose #-}
    validateClose :: VotingParams -> VoteDatum -> ScriptContext -> Bool
    validateClose params' datum' ctx' =
        traceIfFalse "Not authorized: only admin can close voting" 
            (txSignedBy (scriptContextTxInfo ctx') (vpAdmin params')) &&
        traceIfFalse "Voting already closed" (vdVotingOpen datum') &&
        traceIfFalse "Invalid close state transition" 
            (validateCloseTransition datum' ctx')
    
    {-# INLINABLE validateTally #-}
    validateTally :: VotingParams -> ScriptContext -> Bool
    validateTally params' ctx' =
        traceIfFalse "Not authorized: only admin can tally votes" 
            (txSignedBy (scriptContextTxInfo ctx') (vpAdmin params'))
    
    {-# INLINABLE validateRegisterVoter #-}
    validateRegisterVoter :: VotingParams -> VoteDatum -> PubKeyHash -> ScriptContext -> Bool
    validateRegisterVoter params' datum' newVoter ctx' =
        traceIfFalse "Not authorized: only admin can register voters" 
            (txSignedBy (scriptContextTxInfo ctx') (vpAdmin params')) &&
        traceIfFalse "Voting must be open to register voters" (vdVotingOpen datum') &&
        traceIfFalse "Invalid voter registration transition" 
            (validateRegistrationTransition datum' newVoter ctx')
    
    {-# INLINABLE validateMinAdaLocked #-}
    validateMinAdaLocked :: VotingParams -> ScriptContext -> Bool
    validateMinAdaLocked params' ctx' =
        case getContinuingOutputs ctx' of
            [output] -> checkMinAda (vpMinAda params') output
            _ -> False
    
    {-# INLINABLE validateVoteTransition #-}
    validateVoteTransition :: PubKeyHash -> Candidate -> VoteDatum -> ScriptContext -> Bool
    validateVoteTransition voter candidate oldDatum ctx' =
        case getContinuingOutputs ctx' of
            [output] -> 
                let newVotes = addVote voter candidate (vdVotes oldDatum)
                    newDatum = oldDatum { vdVotes = newVotes }
                in txOutDatum output == Datum (toBuiltinData newDatum)
            _ -> traceError "Expected exactly one continuing output"
    
    {-# INLINABLE validateCloseTransition #-}
    validateCloseTransition :: VoteDatum -> ScriptContext -> Bool
    validateCloseTransition oldDatum ctx' =
        case getContinuingOutputs ctx' of
            [output] -> 
                let newDatum = oldDatum { vdVotingOpen = False }
                in txOutDatum output == Datum (toBuiltinData newDatum)
            _ -> traceError "Expected exactly one continuing output"
    
    {-# INLINABLE validateRegistrationTransition #-}
    validateRegistrationTransition :: VoteDatum -> PubKeyHash -> ScriptContext -> Bool
    validateRegistrationTransition oldDatum newVoter ctx' =
        case getContinuingOutputs ctx' of
            [output] -> 
                let newVoters = addVoter newVoter (vdVoters oldDatum)
                    newDatum = oldDatum { vdVoters = newVoters }
                in txOutDatum output == Datum (toBuiltinData newDatum)
            _ -> traceError "Expected exactly one continuing output"

-- Compile the validator
votingValidator :: VotingParams -> Validator
votingValidator params = mkValidatorScript $
    $$(PlutusTx.compile [|| 
        \p d r ctx -> 
            check (mkVotingValidator 
                (unsafeFromBuiltinData p)
                (unsafeFromBuiltinData d) 
                (unsafeFromBuiltinData r)
                (unsafeFromBuiltinData ctx))
    ||])

validatorHash :: VotingParams -> ValidatorHash
validatorHash params = Ledger.validatorHash (votingValidator params)

validatorAddress :: VotingParams -> Address
validatorAddress params = scriptHashAddress (validatorHash params)