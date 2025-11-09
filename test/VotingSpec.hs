{-# LANGUAGE OverloadedStrings #-}

module VotingSpec where

import Test.Hspec
import Plutus.V2.Ledger.Api
import PlutusTx
import Voting.Types
import Voting.Validator
import Voting.Helpers

spec :: Spec
spec = describe "Voting Contract" $ do
    describe "Helper Functions" $ do
        it "correctly calculates vote counts" $ do
            let votes = [("voter1", Alice), ("voter2", Bob), ("voter3", Alice)]
                counts = calculateVoteCounts [Alice, Bob, Charlie] votes
            counts `shouldBe` [(Alice, 2), (Bob, 1), (Charlie, 0)]
        
        it "finds winner correctly" $ do
            let counts = [(Alice, 5), (Bob, 3), (Charlie, 5)]
            findWinner counts `shouldBe` Nothing  
            
            let counts2 = [(Alice, 5), (Bob, 3), (Charlie, 2)]
            findWinner counts2 `shouldBe` Just Alice
        
        it "detects if voter has voted" $ do
            let votes = [("voter1", Alice), ("voter2", Bob)]
            hasVoted "voter1" votes `shouldBe` True
            hasVoted "voter3" votes `shouldBe` False

    describe "Validator Logic" $ do
        it "valid parameters creation" $ do
            let params = VotingParams "admin" Nothing 2000000
            vpAdmin params `shouldBe` "admin"
            vpMinAda params `shouldBe` 2000000