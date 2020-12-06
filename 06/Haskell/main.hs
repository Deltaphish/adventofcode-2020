import Data.List
import Data.Functor
splitOnBlank :: String -> [String]
splitOnBlank xs = let (x,y) = splitOnBlank' xs in x:y
    where
        splitOnBlank' :: String -> (String,[String])
        splitOnBlank' [] = ("",[])
        splitOnBlank' (x:xs) 
            | s /= [] && head s == x && x == '\n' = ([], tail s : acc)
            | otherwise                           = ( x : s, acc)
                where
                    (s,acc) = splitOnBlank' xs

countOC :: [String] -> Int
countOC [] = 0
countOC xs = length $ nub $ foldl intersect (head xs) xs

solve :: [[String]] -> Int
solve xs = sum $ map countOC xs

readInput :: String -> IO [[String]]
readInput fn = readFile fn <&> map lines . splitOnBlank

main :: IO()
main = readInput "../input" >>= print . solve
