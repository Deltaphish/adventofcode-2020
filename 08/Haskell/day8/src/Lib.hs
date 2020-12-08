{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Lib where

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
solve (c,p) v | nextOP `I.member` v  = Nothing
              | nextOP == S.length p = Just (c ^. acc)
              | otherwise            = solve (c',p) v'
    where
        nextOP = c ^. ireg
        c' = runOP c $ fromJust $ S.lookup nextOP p
        v' = I.insert (c ^. ireg) v

createOptions :: Program -> [Program]
createOptions prog = splitFlowAt (length prog - 1)
    where
        splitFlowAt :: Int -> [Program]
        splitFlowAt 0 = case S.lookup 0 prog of
                            Just (JMP off) -> [S.update 0 (NOP off) prog]
                            Just (NOP off) -> [S.update 0 (JMP off) prog]
                            _            -> []
        
        splitFlowAt i = case S.lookup i prog of
                            Just (JMP off) -> (S.update i (NOP off) prog) : (splitFlowAt (i-1))
                            Just (NOP off) -> (S.update i (JMP off) prog) : (splitFlowAt (i-1))
                            _              -> splitFlowAt (i-1)


-- Create a list of all possible solutions, and search for the solution within them 

findSolution :: B.ByteString -> Int
findSolution raw = case parseOnly pProgram raw of
                    Right p ->  head $ mapMaybe (\x -> solve (newComputer,x) I.empty) $ createOptions p
                    Left _  -> error "could not parse file"

run :: IO ()
run = B.readFile "../../input" >>= print.findSolution

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




