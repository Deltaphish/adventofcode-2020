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

data Computer = Computer {mask :: Mask,mem :: Memory} deriving Show
type Memory = M.IntMap Int64

newComputer :: Computer
newComputer = Computer [] M.empty

type Addr = Int
type Val  = Int64
type Mask = [(Int,Int)]
data Stmt = SetMem Addr Val | SetMask Mask deriving Show

solve :: [Stmt] -> Int64
solve c = sum $ mem $ foldl runStmt newComputer c

runStmt :: Computer -> Stmt -> Computer
runStmt (Computer _ m) (SetMask xs)  = Computer xs m
runStmt (Computer msk m) (SetMem addr x)  = Computer msk m' 
    where
        m' = M.insert addr val m
        val = foldr setBits x msk

setBits :: (Int,Int) -> Int64 -> Int64
setBits (off,0) i = i `clearBit` off
setBits (off,1) i = i `setBit` off

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
pMask = do 
    string "mask = "
    mask <- pRawMask 
    return $ SetMask $ reverse [(i,fromJust x) | (i,x) <- zip [0..] (reverse mask), isJust x] 

pRawMask :: Parser [Maybe Int]
pRawMask = choice [pX,p1,p0] `manyTill'` endOfLine  
 

pX :: Parser (Maybe Int)
pX = char 'X' >> return Nothing

p1 :: Parser (Maybe Int)
p1 = char '1' >> return (Just 1)

p0 :: Parser (Maybe Int)
p0 = char '0' >> return (Just 0)
