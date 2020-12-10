module Lib where

import Data.Functor
import Data.Sequence as SEQ
import Data.Sequence (Seq(..))
import qualified Data.ByteString.Char8 as B

{- Solver -}

-- (a -> b -> a) -> a -> [b] -> a

countSteps :: (Int,Int,Int) -> Int -> (Int,Int,Int)
countSteps (prev,s1,s3) x | x - prev == 1 = (x,s1+1,s3)
                          | x - prev == 3 = (x,s1,s3+1)
                          | otherwise     = (x,s1,s3)

solver :: Seq Int -> Int
solver xs = s1 * s3
    where
        xs' = SEQ.unstableSort xs
        (_,s1,s3) = foldl (countSteps) (0,0,1) xs'

{- Setup -}

readInpt :: String -> IO (Seq Int)
readInpt f = B.readFile f <&> parse

parse :: B.ByteString -> Seq Int
parse s = SEQ.fromList $ map parse' $ B.lines s
    where parse' s = let Just (n, _) = B.readInt s in n

run :: String -> IO Int
run f = readInpt f <&> solver