{-# LANGUAGE OverloadedStrings #-}
module Lib
    ( run
    ) where

import qualified Data.ByteString as B
import qualified Data.HashMap as H
import  Data.HashMap ((!))

import Data.Attoparsec.ByteString.Char8
import Control.Applicative
import Data.List

type KV = H.Map B.ByteString Contents

containsG :: Contents -> KV -> Bool
containsG EmptyBag _ = False
containsG (Bags bs) kv | any (\x -> snd x == "shinygold") bs = True
                       | otherwise                           = any (\x -> containsG (kv ! x) kv) $ map snd bs

run :: IO ()
run = do
    inpt <- B.readFile "../../input"
    let parsed = case parseOnly pMain inpt of
                    Right p -> p
                    Left _ -> []
    let mp = H.fromList parsed
    print $ length $ filter (\c -> containsG (snd c) mp) parsed



--dotted cyan bags contain 3 wavy aqua bags, 4 shiny brown bags, 4 faded tan bags.
-- name "contain" [nr name, nr name,...] | no other bags
data Contents = EmptyBag | Bags [(Int,B.ByteString)] | GoldenBag deriving Show

pMain :: Parser [(B.ByteString,Contents)]
pMain = many1 pRow

pRow :: Parser (B.ByteString,Contents)
pRow = do
    name <- pBagName
    string "contain "
    cont <- pNoBag <|> pContains 
    char '.'
    endOfLine
    return $ (name,cont)

    
pContains :: Parser Contents
pContains = do
    xs <- pListEl `sepBy` (char ',')
    return $ Bags xs
    where
        pListEl = do
            skipSpace
            c <- decimal
            skipSpace
            name <- pBagName
            return (c,name)

pNoBag :: Parser Contents
pNoBag = string "no other bags" >> return EmptyBag



pBagName :: Parser B.ByteString
pBagName = do
    skipSpace
    first <- takeTill isSpace
    skipSpace
    second <- takeTill isSpace
    skipSpace
    string "bags" <|> string "bag"
    skipSpace
    return $ B.append first second




