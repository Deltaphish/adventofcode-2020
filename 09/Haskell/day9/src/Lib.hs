module Lib where 

import Control.Applicative
import Data.Functor
import Data.Maybe
import Data.List

import qualified Data.IntSet as IS
import qualified Data.Sequence as S
import Data.Sequence ((<|),(|>),Seq(..))

type SumTables = S.Seq (Int,IS.IntSet)


--(a -> b -> a) -> a -> [b] -> a
--[Int,IS.IntSet] -> Int -> [Int, IS.IntSet]

{- Find the outlier -}

calcSums :: Seq (Int,IS.IntSet) -> Int -> Seq (Int,IS.IntSet)
calcSums xs i = (i,set) <| xs
    where
        terms = fst <$> xs
        set = foldl (flip IS.insert) IS.empty $ (i+) <$> terms

genInitPreable :: [Int] -> SumTables
genInitPreable = foldl calcSums S.empty

checkInt :: SumTables -> Int -> Bool
checkInt xs x = IS.member x tables 
    where
        tables = IS.unions $ fmap snd xs

addInt :: SumTables -> Int -> SumTables
addInt (_ :<| xs) i = fmap (addInt' i) xs |> (i,IS.empty)
    where
        addInt' :: Int -> (Int,IS.IntSet) -> (Int,IS.IntSet)
        addInt' y (x,t) = (x,IS.insert (x+y) t)

findOutlier :: [Int] -> Int
findOutlier xs = fromJust $ solver xs' st
    where xs' = drop 25 xs
          st  = genInitPreable $ take 25 xs
          
          solver :: [Int] -> SumTables -> Maybe Int
          solver []     st                 = Nothing 
          solver (x:xs) st | checkInt st x = solver xs (addInt st x)
                           | otherwise     = Just x

{- Find the sequence -}

{-
findseq:
    starting with x,sum subsequent xs:
        == -> FoundEM, return x and latest xs
        >  -> move to x[+1], run again
-}

findSeq :: Int -> [Int] -> Maybe (Int,Int)
findSeq target []          = Nothing
findSeq target (x:xs) | target == x = Nothing
                      | otherwise =  maybe (findSeq target xs) return (findSeq' xs x)
    where
        findSeq' :: [Int] -> Int -> Maybe (Int,Int)
        findSeq' [] acc                         = Nothing
        findSeq' (t:ts) acc | t + acc == target = Just (x,t)
                            | t + acc > target  = Nothing
                            | otherwise         = findSeq' ts (t + acc)  


findMinMax :: [Int] -> (Int,Int) -> Int
findMinMax xs (low,high) = maximum xs' + minimum xs'
    where
        -- Order does not matter, we only need max/min
        xs' = high : takeWhile (/= high) (dropWhile (/= low) xs)

{-Glue-}

runSolver :: [Int] -> Int
runSolver xs = findMinMax xs $ fromJust $ findSeq (findOutlier xs) xs

readInpt :: String -> IO [Int]
readInpt f = readFile f <&> map read . lines

run :: String -> IO Int
run f = readInpt f <&> runSolver