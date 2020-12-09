module Lib where 

import Data.Functor
import Data.Maybe

import qualified Data.IntSet as IS
import qualified Data.Sequence as S
import Data.Sequence ((<|),(|>),Seq(..))

type SumTables = S.Seq (Int,IS.IntSet)


--(a -> b -> a) -> a -> [b] -> a
--[Int,IS.IntSet] -> Int -> [Int, IS.IntSet]

calcSums :: Seq (Int,IS.IntSet) -> Int -> Seq (Int,IS.IntSet)
calcSums xs i = (i,set) <| xs
    where
        terms = fmap fst $ xs
        set = foldl (\a b -> IS.insert b a) IS.empty $ fmap (i+) terms

genInitPreable :: [Int] -> SumTables
genInitPreable xs = foldl calcSums S.empty xs

checkInt :: SumTables -> Int -> Bool
checkInt xs x = IS.member x tables 
    where
        tables = IS.unions $ fmap snd xs

addInt :: SumTables -> Int -> SumTables
addInt (_ :<| xs) i = fmap (addInt' i) xs |> (i,IS.empty)
    where
        addInt' :: Int -> (Int,IS.IntSet) -> (Int,IS.IntSet)
        addInt' y (x,t) = (x,IS.insert (x+y) t)

solver :: [Int] -> SumTables -> Maybe Int
solver []     st  = Nothing 
solver (x:xs) st | checkInt st x = solver xs (addInt st x)
                 | otherwise     = Just x

runSolver :: [Int] -> Int
runSolver xs = fromJust $ solver xs' st
    where xs' = drop 25 xs
          st = genInitPreable $ take 25 xs

readInpt :: String -> IO [Int]
readInpt f = readFile f <&> (map read).lines

run :: String -> IO Int
run f = readInpt f <&> runSolver