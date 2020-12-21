{-# LANGUAGE OverloadedStrings #-}

module Lib where

import Data.Attoparsec.ByteString.Char8
import Control.Applicative
import Data.Functor
import Data.Int
import Data.Maybe
import Data.Bits
import qualified Data.IntMap.Lazy as M
import qualified Data.ByteString as B

data Computer = Computer {mask :: [Mask],mem :: Memory} deriving Show
type Memory = M.IntMap Int64

newComputer :: Computer
newComputer = Computer [] M.empty

type Addr = Int
type Val  = Int64
data Mask = OverWrite | Unchanged | Split deriving Show
data Stmt = SetMem Addr Val | SetMask [Mask] deriving Show

solve :: [Stmt] -> Int64
solve c = sum $ mem $ foldl runStmt newComputer c

runStmt :: Computer -> Stmt -> Computer
runStmt (Computer _ m) (SetMask xs)  = Computer xs m
runStmt (Computer msk m) (SetMem addr x)  = Computer msk m' 
    where
        m' = foldl (\mp a -> M.insert a x mp) m addrs
        addrs = evalAddr addr msk


evalAddr :: Int -> [Mask] -> [Int]
evalAddr i xs = evalAddr' i (length xs - 1) xs
    where
        evalAddr' :: Int -> Int -> [Mask] -> [Int]
        evalAddr' a _ [] = [a]
        evalAddr' a j (Unchanged:xs) = evalAddr' a (j-1) xs
        evalAddr' a j (OverWrite:xs) = let a' = a `setBit` j in evalAddr' a' (j-1) xs
        evalAddr' a j (Split:xs) = concat [evalAddr' x (j-1) xs | x <- [a1,a0]]
            where
                a1 = a `setBit` j
                a0 = a `clearBit` j

findSolution :: B.ByteString -> Int64
findSolution raw = solve $ runParser raw

run :: FilePath -> IO Int64
run s = B.readFile s <&> findSolution

runParser :: B.ByteString -> [Stmt]
runParser raw = case parseOnly pProgram raw of
                    Right p -> p
                    Left _  -> error "could not parse file"

{- Parsers -}

pProgram :: Parser [Stmt]
pProgram = choice [pMask,pAssign] `manyTill'` endOfInput 

pAssign :: Parser Stmt
pAssign = do
    string "mem["
    addr <- decimal
    string "] = "
    val <- decimal
    endOfLine
    return $ SetMem addr val

pMask :: Parser Stmt
pMask = (string "mask = " >> pRawMask) <&> SetMask

pRawMask :: Parser [Mask]
pRawMask = choice [pX,p1,p0] `manyTill'` endOfLine  
 

pX :: Parser Mask
pX = char 'X' >> return Split

p1 :: Parser Mask
p1 = char '1' >> return OverWrite

p0 :: Parser Mask
p0 = char '0' >> return Unchanged
