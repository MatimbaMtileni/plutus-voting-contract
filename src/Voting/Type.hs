{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

module Voting.Types where

import Plutus.V2.Ledger.Api
import PlutusTx
import PlutusTx.Prelude hiding (Semigroup(..), unless)
import GHC.Generics (Generic)
import Data.Aeson (FromJSON, ToJSON)
import Prelude (Show)

-- Candidate type
data Candidate = Alice | Bob | Charlie | David | Emily
    deriving (Show, Eq, Generic, FromJSON, ToJSON)

PlutusTx.unstableMakeIsData ''Candidate
PlutusTx.makeLift ''Candidate

-- Vote Datum - tracks the state of the voting
data VoteDatum = VoteDatum
    { vdVoters :: [PubKeyHash]          
    , vdVotes :: [(PubKeyHash, Candidate)] 
    , vdVotingOpen :: Bool              
    , vdCandidates :: [Candidate]       
    , vdTitle :: BuiltinString         
    }
    deriving (Show, Generic)

PlutusTx.unstableMakeIsData ''VoteDatum
PlutusTx.makeLift ''VoteDatum

-- Redeemer actions
data VoteAction 
    = Vote Candidate   
    | Close            
    | Tally            
    | RegisterVoter PubKeyHash  
    | Initialize        
    deriving (Show, Generic)

PlutusTx.unstableMakeIsData ''VoteAction
PlutusTx.makeLift ''VoteAction

-- Voting parameters
data VotingParams = VotingParams
    { vpAdmin :: PubKeyHash
    , vpVoteToken :: Maybe AssetClass  
    , vpMinAda :: Integer              
    }
    deriving (Show, Generic)

PlutusTx.unstableMakeIsData ''VotingParams
PlutusTx.makeLift ''VotingParams

-- Voting result type for off-chain
data VotingResult = VotingResult
    { vrWinner :: Maybe Candidate
    , vrVoteCounts :: [(Candidate, Integer)]
    , vrTotalVotes :: Integer
    , vrVoterTurnout :: Double
    }
    deriving (Show, Generic)