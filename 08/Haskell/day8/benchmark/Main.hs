module Main where

import Criterion.Main
import qualified Data.ByteString as B
import Lib (run,findSolution)

main :: IO ()
main = do
  raw <- B.readFile "../../input"
  defaultMain [
    bgroup "Solver" [ bench "with-io"  $ nfIO run
                    , bench "no-io"    $ whnf findSolution raw
                    ]
            ]