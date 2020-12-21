module Main where

import Data.Functor
import Lib

main :: IO ()
main = readInpt "../../input" >>= print.(uncurry solvep1).runParser
