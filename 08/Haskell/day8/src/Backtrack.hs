{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Backtrack where

import Control.Lens
import Data.Attoparsec.ByteString.Char8
import Control.Applicative
import Data.List
import Data.Maybe

import qualified Data.ByteString as B
import qualified Data.IntSet as I
import qualified Data.Sequence as S

data Computer = Computer {
        _ireg :: Int,
        _acc  :: Int,
        _lastJmp :: Int
    }

makeLenses ''Computer

data Instruction = ACC Int | JMP Int | NOP Int
type Visited = I.IntSet
type Program = S.Seq Instruction

newComputer :: Computer
newComputer = Computer 0 0 0

runOP :: Computer -> Instruction -> Computer
runOP c (ACC i) = (ireg +~ 1).(acc +~ i) $ c
runOP c (JMP i) = (ireg +~ i).(lastJmp .~ (c ^. ireg)) $ c
runOP c (NOP _) = ireg +~ 1 $ c

solve :: (Computer, Program) ->  Visited -> Maybe Int
solve (c,p) v | nextAddr `I.member` v  = Nothing
              | nextAddr == S.length p = Just (c ^. acc)
              | otherwise              = solve (c', p) v'
    where
        nextAddr = c ^. ireg
        nextOP   = fromJust $ S.lookup nextAddr p
        c'  = runOP c nextOP
        v'  = I.insert (c ^. ireg) v

solveWithBacktrack :: (Computer, Program) ->  Visited -> Maybe Int
solveWithBacktrack (c,p) v | nextAddr `I.member` v  = Nothing
                           | nextAddr == S.length p = Just (c ^. acc)
                           | otherwise              = case solveWithBacktrack (c',p) v' of
                                                        Just v -> Just v
                                                        Nothing -> solve (c'',p) v'
    where
        nextAddr = c ^. ireg
        nextOP   = fromJust $ S.lookup nextAddr p
        c'  = runOP c nextOP
        c'' = runOP c $ swapOP nextOP
        v'  = I.insert (c ^. ireg) v

        swapOP :: Instruction -> Instruction
        swapOP (JMP i) = NOP i
        swapOP (NOP i) = JMP i
        swapOP a       = a


findSolution :: B.ByteString -> Int
findSolution raw = case parseOnly pProgram raw of
                    Right p -> fromJust $ solveWithBacktrack (newComputer,p) I.empty
                    Left _  -> error "could not parse file"

run :: IO Int
run = B.readFile "../../input" >>= return.findSolution

{- Parsers -}

pProgram :: Parser Program
pProgram = pInst `sepBy` endOfLine >>= return . S.fromList

pInst :: Parser Instruction
pInst = pACC <|> pJMP <|> pNOP

pNOP :: Parser Instruction
pNOP = do
    string "nop"
    space
    off <- signed decimal
    return $ NOP off

pJMP :: Parser Instruction
pJMP = do
    string "jmp"
    space
    off <- signed decimal
    return $ JMP off

pACC :: Parser Instruction
pACC = do
    string "acc"
    space
    off <- signed decimal
    return $ ACC off




