import Data.List
splitOnBlank :: String -> [String]
splitOnBlank xs = splitOnBlank' xs ("",[""])

splitOnBlank' :: String -> (String,[String]) -> [String]
splitOnBlank' [] (rest,acc)     = reverse rest : acc
splitOnBlank' ['\n'] (rest,acc) = reverse rest : acc
splitOnBlank' [x]    (rest,acc) = reverse (x:rest) : acc 
splitOnBlank' (s:x:xs) (rest,acc) | [s,x] == "\n\n" = splitOnBlank' xs ("",(reverse rest):acc)
		                  | otherwise       = splitOnBlank' (x:xs) (s:rest,acc)

countOC :: [String] -> Int
countOC [] = 0
countOC xs = length $ nub $ foldl intersect (head xs) xs

solve :: [[String]] -> Int
solve xs = foldl (+) 0 $ map countOC xs

readInput :: String -> IO [[String]]
readInput fn = readFile fn >>= return.(map lines). splitOnBlank

main :: IO()
main = readInput "../input" >>= print.solve
