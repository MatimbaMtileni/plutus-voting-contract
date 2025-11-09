module Main where

import Voting.Types
import Voting.Validator
import Voting.Helpers

main :: IO ()
main = do
    putStrLn "Plutus Voting Contract"
    putStrLn "======================"
    
    
    let params = VotingParams 
            { vpAdmin = "example_admin"
            , vpVoteToken = Nothing
            , vpMinAda = 2000000
            }
    
    let initialDatum = createInitialDatum 
            ["voter1", "voter2", "voter3"]
            [Alice, Bob, Charlie]
            "Community Election"
    
    putStrLn $ "Admin: " ++ show (vpAdmin params)
    putStrLn $ "Voters: " ++ show (vdVoters initialDatum)
    putStrLn $ "Candidates: " ++ show (vdCandidates initialDatum)
    putStrLn $ "Voting Open: " ++ show (vdVotingOpen initialDatum)