{-# LANGUAGE OverloadedStrings #-}
module Lib where

import Data.Maybe
import Data.Functor
import qualified Data.ByteString.Char8 as CH

minimumBy :: Ord b => (a ->  b) -> [a] -> a
minimumBy f xs = fst $ minimumBy' f xs
    where
        minimumBy' _ [] = error "minimumBy:: No List"
        minimumBy' fn [x] = (x,fn x) 
        minimumBy' fn (x:xs) | ordX < acc = (x, ordX)
                            | otherwise  = (x', acc)
            where
                ordX = fn x
                (x',acc) = minimumBy' fn xs 

solver (tgt,xs) = uncurry (*) $ minimumBy snd [(x,x - (tgt `mod` x)) | x <- xs]

parseHead :: CH.ByteString -> Int
parseHead s = let Just (n,_) = CH.readInt s in n

parseBody :: CH.ByteString -> [Int]
parseBody s = catMaybes $ map (parseNr) $ CH.split ',' s
    where 
        parseNr "x" = Nothing
        parseNr x   = CH.readInt x <&> fst

readInput :: FilePath -> IO (Int,[Int])
readInput fn = do
    f <- CH.readFile fn
    let (tgt:tm:[]) = CH.lines f
    return $ (parseHead tgt, parseBody tm)


run :: FilePath -> IO Int
run f = readInput f <&> solver