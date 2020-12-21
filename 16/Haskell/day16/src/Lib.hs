{-# LANGUAGE OverloadedStrings #-}
module Lib where

import Data.Attoparsec.ByteString.Char8
import Control.Applicative
import Data.Functor
import qualified Data.ByteString as B

data Range = Range {begin :: Int, end :: Int} deriving Show

validateInt :: [Range] -> Int -> Bool
validateInt rs i =  any (\x -> (i >= begin x) && (i <= end x)) rs

solvep1 :: [Range] -> [Int] -> Int
solvep1 rs xs = sum $ filter (not . validateInt rs) xs

runParser :: B.ByteString -> ([Range],[Int])
runParser raw = case parseOnly pProgram raw of
                    Right p -> p
                    Left _  -> error "could not parse file"

readInpt :: FilePath -> IO B.ByteString 
readInpt = B.readFile

pProgram :: Parser ([Range],[Int])
pProgram = do
    ranges <- many1 pField <&> concat
    skipSpace
    string "your ticket:\n"
    yticket <- pTicket
    skipSpace 
    string "nearby tickets:\n"
    ntickets <- many1 pTicket <&> concat
    return (ranges,yticket ++ ntickets)

 
pField :: Parser [Range]
pField = do
    skipWhile (/= ':')
    char ':'
    skipSpace
    l1 <- decimal
    char '-'
    h1 <- decimal
    string " or "
    l2 <- decimal
    char '-'
    h2 <- decimal
    endOfLine
    return [Range l1 h1, Range l2 h2]

pTicket :: Parser [Int]
pTicket = do 
    xs <- decimal `sepBy1'` char ','
    endOfLine 
    return xs
