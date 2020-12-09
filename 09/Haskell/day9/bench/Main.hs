module Main where

import Criterion.Main
import Lib

main :: IO ()
main = do
  raw <- readInpt "../../input"
  defaultMain [
    bgroup "StackSets" [ bench "with-io"  $ nfIO $ run "../../input"
                        , bench "no-io"    $ whnf runSolver raw
                        ]
    ]