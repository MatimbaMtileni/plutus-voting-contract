{-# LANGUAGE BangPatterns #-}

module Voting.Helpers where

import Plutus.V2.Ledger.Api
import Plutus.V2.Ledger.Contexts
import PlutusTx
import PlutusTx.Prelude hiding (Semigroup(..), unless)
import Voting.Types

{-# INLINABLE hasVoted #-}
hasVoted :: PubKeyHash -> [(PubKeyHash, Candidate)] -> Bool
hasVoted voter votesList = any (\(v, _) -> v == voter) votesList

{-# INLINABLE isRegisteredVoter #-}
isRegisteredVoter :: PubKeyHash -> [PubKeyHash] -> Bool
isRegisteredVoter voter registeredVoters = voter `elem` registeredVoters

{-# INLINABLE isValidCandidate #-}
isValidCandidate :: Candidate -> [Candidate] -> Bool
isValidCandidate candidate validCandidates = candidate `elem` validCandidates

{-# INLINABLE findVoterSignature #-}
findVoterSignature :: TxInfo -> [PubKeyHash] -> Maybe PubKeyHash
findVoterSignature info voters = 
    case filter (\voter -> txSignedBy info voter) voters of
        [] -> Nothing
        (voter:_) -> Just voter

{-# INLINABLE calculateVoteCounts #-}
calculateVoteCounts :: [Candidate] -> [(PubKeyHash, Candidate)] -> [(Candidate, Integer)]
calculateVoteCounts allCandidates votesList =
    map (\candidate -> (candidate, countVotes candidate)) allCandidates
  where
    countVotes c = toInteger $ length $ filter (\(_, cand) -> cand == c) votesList

{-# INLINABLE findWinner #-}
findWinner :: [(Candidate, Integer)] -> Maybe Candidate
findWinner counts =
    case sortBy (\(_, c1) (_, c2) -> compare c2 c1) counts of
        [] -> Nothing
        [(winner, _)] -> Just winner
        ((winner1, count1):(winner2, count2):_) -> 
            if count1 > count2 then Just winner1 else Nothing

{-# INLINABLE addVote #-}
addVote :: PubKeyHash -> Candidate -> [(PubKeyHash, Candidate)] -> [(PubKeyHash, Candidate)]
addVote voter candidate existingVotes = 
    (voter, candidate) : existingVotes

{-# INLINABLE addVoter #-}
addVoter :: PubKeyHash -> [PubKeyHash] -> [PubKeyHash]
addVoter newVoter existingVoters =
    if newVoter `elem` existingVoters 
    then existingVoters
    else newVoter : existingVoters

{-# INLINABLE checkMinAda #-}
checkMinAda :: Integer -> TxOut -> Bool
checkMinAda minAda txOut = 
    let value = txOutValue txOut
    in value `geq` lovelaceValueOf minAda

-- Helper to create initial datum
{-# INLINABLE createInitialDatum #-}
createInitialDatum :: [PubKeyHash] -> [Candidate] -> BuiltinString -> VoteDatum
createInitialDatum initialVoters validCandidates title =
    VoteDatum
        { vdVoters = initialVoters
        , vdVotes = []
        , vdVotingOpen = True
        , vdCandidates = validCandidates
        , vdTitle = title
    }