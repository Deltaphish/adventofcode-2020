module Main where

import Criterion.Main
import Lib

main :: IO ()
main = do
  raw <- readFile "../../input"
  let rawParsed = map read $ lines raw
  defaultMain [
    bgroup "QueueBased" [ bench "with-io"  $ nfIO (run "../../input")
                        , bench "no-io"    $ whnf runSolver rawParsed
                        ]
    ]