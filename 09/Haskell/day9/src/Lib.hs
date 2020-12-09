module Lib where 

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
        terms = fmap fst $ xs
        set = foldl (\a b -> IS.insert b a) IS.empty $ fmap (i+) terms

genInitPreable :: [Int] -> SumTables
genInitPreable = foldl calcSums S.empty

checkInt :: SumTables -> Int -> Bool
checkInt xs x = any (\s -> IS.member x s) sets
    where
        sets = fmap snd xs

addInt :: SumTables -> Int -> SumTables
addInt (_ :<| xs) i = fmap (addInt' i) xs |> (i,IS.empty)
    where
        addInt' :: Int -> (Int,IS.IntSet) -> (Int,IS.IntSet)
        addInt' y (x,t) = (x,IS.insert (x+y) t)

solver :: [Int] -> SumTables -> Maybe Int
solver []     st  = Nothing 
solver (x:xs) st | checkInt st x = solver xs (addInt st x)
                 | otherwise     = Just x

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
                      | otherwise = case findSeq' xs x of
                                        Nothing -> (findSeq target xs) 
                                        Just a -> return a
    where
        findSeq' :: [Int] -> Int -> Maybe (Int,Int)
        findSeq' [] acc                         = Nothing
        findSeq' (t:ts) acc | t + acc == target = Just (x,t)
                            | t + acc > target  = Nothing
                            | otherwise         = findSeq' ts (t + acc)  


findMinMax :: [Int] -> (Int,Int) -> Int
findMinMax xs (low,high) = (maximum xs') + (minimum xs')
    where
        xs' = high : (takeWhile (/= high) $ dropWhile (/= low) xs)

{-Glue-}

runSolver :: [Int] -> Int
runSolver xs = findMinMax xs $ fromJust $ findSeq (findOutlier xs) xs

readInpt :: String -> IO [Int]
readInpt f = readFile f <&> (map read).lines

run :: String -> IO Int
run f = readInpt f <&> runSolver