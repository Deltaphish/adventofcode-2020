{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Lib
    ( run
    ) where

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
        _acc  :: Int
    }

makeLenses ''Computer

data Instruction = ACC Int | JMP Int | NOP

newComputer :: Computer
newComputer = Computer 0 0

runOP :: Computer -> Instruction -> Computer
runOP c (ACC i) = (ireg +~ 1).(acc +~ i) $ c
runOP c (JMP i) = ireg +~ i $ c
runOP c NOP     = ireg +~ 1 $ c

type Visited = I.IntSet

run :: IO ()
run = do
    inpt <- B.readFile "../../input"
    let parsed = case parseOnly pProgram inpt of
                    Right p -> p
                    Left _ -> error "could not parse file"
    print $ solve (newComputer,parsed) I.empty


solve :: (Computer, S.Seq Instruction) ->  Visited -> Int
solve (c,p) v | (c ^. ireg) `I.member` v = c ^. acc
              | otherwise                = solve (c',p) v'
    where
        c' = runOP c $ fromJust $ S.lookup (c ^. ireg) p
        v' = I.insert (c ^. ireg) v



--dotted cyan bags contain 3 wavy aqua bags, 4 shiny brown bags, 4 faded tan bags.
-- name "contain" [nr name, nr name,...] | no other bags

pProgram :: Parser (S.Seq Instruction)
pProgram = pInst `sepBy` endOfLine >>= return . S.fromList

pInst :: Parser Instruction
pInst = pACC <|> pJMP <|> pNOP

pNOP :: Parser Instruction
pNOP = do
    string "nop"
    space
    signed decimal
    return NOP

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




