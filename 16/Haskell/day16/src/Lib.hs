{-# LANGUAGE OverloadedStrings #-}
module Lib where

import Data.Attoparsec.ByteString.Char8
import Control.Applicative
import Data.Functor
import Data.List
import qualified Data.ByteString as B

data Range = Range {name :: B.ByteString, begin1 :: Int, end1 :: Int,begin2 :: Int, end2 :: Int} deriving (Show,Eq)

validateInt :: Range -> Int -> Bool
validateInt (Range _ b1 e1 b2 e2) i = (i >= b1 && i <= e1) || (i >= b2 && i <= e2)

validateTicket :: [Range] -> [Int] -> Bool
validateTicket rs = all (\x -> or [validateInt r x | r <- rs])

validate :: [Range] -> [[Int]] -> [[Int]]
validate rs = filter (validateTicket rs)

possibleFields :: [Range] -> [[Int]] -> [[Range]]
possibleFields rs xs = map (isValid rs) xs' 
    where
        xs' = transpose $ validate rs xs

        isValid :: [Range] -> [Int] -> [Range]
        isValid rs t = filter (isFieldPossible t) rs

        isFieldPossible :: [Int] -> Range -> Bool
        isFieldPossible t r = all (r `validateInt`) t

narrowDown :: [[Range]] -> [[Range]]
narrowDown rs | any (\x -> length x /= 1) rs = narrowDown $ clear rs findMatches
              | otherwise                    = rs
    where
        findMatches :: [Range]
        findMatches = concat [[r] | [r] <- rs]

        clear :: [[Range]] -> [Range] -> [[Range]]
        clear [] matches       = []
        clear ([r]:rs) matches = [r] : clear rs matches 
        clear (r:rs) matches   = (r \\ matches) : clear rs matches


getIndexes :: [Range] -> [Int]
getIndexes rs = map fst $ filter (B.isPrefixOf "departure". snd) $ zip [0..] $ map name rs

solver :: B.ByteString -> Int
solver s = product [myTicket !! i | i <- indexes]
    where
        (rs,xs) = runParser s
        myTicket = head xs
        indexes = getIndexes $ concat $ narrowDown $ possibleFields rs xs

runParser :: B.ByteString -> ([Range],[[Int]])
runParser raw = case parseOnly pProgram raw of
                    Right p -> p
                    Left _  -> error "could not parse file"

readInpt :: FilePath -> IO B.ByteString 
readInpt = B.readFile

pProgram :: Parser ([Range],[[Int]])
pProgram = do
    ranges <- many1 pField
    skipSpace
    string "your ticket:\n"
    yticket <- pTicket
    skipSpace 
    string "nearby tickets:\n"
    ntickets <- many1 pTicket
    return (ranges,yticket : ntickets)

 
pField :: Parser Range
pField = do
    name <- takeTill (== ':')
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
    return $ Range name l1 h1 l2 h2

pTicket :: Parser [Int]
pTicket = do 
    xs <- decimal `sepBy1'` char ','
    endOfLine 
    return xs
