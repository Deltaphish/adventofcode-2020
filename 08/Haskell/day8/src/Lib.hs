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
        _acc  :: Int,
        _lastJmp :: Int
    }

makeLenses ''Computer

data Instruction = ACC Int | JMP Int | NOP Int

newComputer :: Computer
newComputer = Computer 0 0 0

runOP :: Computer -> Instruction -> Computer
runOP c (ACC i) = (ireg +~ 1).(acc +~ i) $ c
runOP c (JMP i) = (ireg +~ i).(lastJmp .~ (c ^. ireg)) $ c
runOP c (NOP _) = ireg +~ 1 $ c

type Visited = I.IntSet
type Program = S.Seq Instruction

run :: IO ()
run = do
    inpt <- B.readFile "../../input"
    let parsed = case parseOnly pProgram inpt of
                    Right p -> p
                    Left _ -> error "could not parse file"
    let options = createOptions parsed
    print $ head $ mapMaybe (\x -> solve (newComputer,x) I.empty) options


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


--dotted cyan bags contain 3 wavy aqua bags, 4 shiny brown bags, 4 faded tan bags.
-- name "contain" [nr name, nr name,...] | no other bags

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




