{-# LANGUAGE Strict #-}
module Lib where 

import Data.Functor
import Data.Maybe
import Data.List

import qualified Data.IntSet as IS
import qualified Data.Sequence as S
import Data.Sequence ((<|),(|>),Seq(..))

import qualified Data.ByteString.Char8 as B

type SumTables = S.Seq (Int,IS.IntSet)


{- Find the outlier -}

calcSums :: Seq (Int,IS.IntSet) -> Int -> Seq (Int,IS.IntSet)
calcSums xs i = (i,set) <| xs
    where
        terms = fmap fst $ xs
        set = foldl' (flip IS.insert) IS.empty $ fmap (i+) terms

genInitPreable :: [Int] -> SumTables
genInitPreable = foldl' calcSums S.empty

checkInt :: SumTables -> Int -> Bool
checkInt Empty _ = False
checkInt ((x,set) :<| xs) i =
    if IS.member i set
        then True
        else checkInt xs i

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

seqTail :: Seq a -> Seq a
seqTail (_ :<| xs) = xs

seqHead :: Seq a -> a
seqHead (x :<| _) = x 

findSeq :: Int -> [Int] -> Seq Int
findSeq _ []          = error "No seq given"
findSeq target (t:ts) = findSeq' target t (S.singleton t) ts 
    where
        findSeq' :: Int -> Int -> Seq Int -> [Int] -> Seq Int
        findSeq' _ _ _ [] = error "could not find seq"
        findSeq' tgt acc seq (x:xs) | acc < tgt = findSeq' tgt acc' seq' xs
                                    | acc > tgt = findSeq' tgt (acc - h) (seqTail seq) (x:xs)
                                    | otherwise = seq
            where 
                seq' = seq |> x
                acc' = acc + x
                h = seqHead seq

minMax :: Seq Int -> Int
minMax (x :<| xs) = high + low
    where
        (low,high) = minMax' xs (x,x)
        minMax' Empty a = a
        minMax' (x :<| xs) (low,high) | x < low = minMax' xs (x,high)
                                     | x > high = minMax' xs (low,x)
                                     | otherwise = minMax' xs (low,high)

{-Glue-}

runSolver :: [Int] -> Int
runSolver xs = minMax $! findSeq (findOutlier xs) xs

readInpt :: String -> IO [Int]
readInpt f = B.readFile f <&> parse

parse :: B.ByteString -> [Int]
parse s = map parse' $ B.lines s
    where parse' s = let Just (n, _) = B.readInt s in n

run :: String -> IO Int
run f = readInpt f <&> runSolver