module Main where

import Backtrack (run)

main :: IO ()
main = run >>= print
