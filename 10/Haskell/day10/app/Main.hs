module Main where

import qualified Lib

main :: IO ()
main = print =<< Lib.run "../../input"
