module Main where

import Criterion.Main
import qualified Data.ByteString as B
import qualified Brute (run,findSolution)
import qualified Backtrack (run,findSolution)

main :: IO ()
main = do
  raw <- B.readFile "../../input"
  defaultMain [
    bgroup "Bruteforce" [ bench "with-io"  $ nfIO Brute.run
                        , bench "no-io"    $ whnf Brute.findSolution raw
                        ],
    bgroup "Backtrack" [ bench "with-io" $ nfIO Backtrack.run
                        , bench "no-io"   $ whnf Backtrack.findSolution raw
                        ] 
    ]