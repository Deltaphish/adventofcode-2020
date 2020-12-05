findRow :: String -> (Int,Int) -> Int
findRow ('B':[]) (low,high) = high
findRow ('F':[]) (low,high) = low
findRow ('F':xs) (low,high) = findRow xs (low,high-(1+high-low) `div` 2)
findRow ('B':xs) (low,high) = findRow xs (1+low+(high-low) `div` 2,high)


findCol :: String -> (Int,Int) -> Int
findCol ('R':[]) (low,high) = high
findCol ('L':[]) (low,high) = low
findCol ('L':xs) (low,high) = findCol xs (low,high-(1+high-low) `div` 2)
findCol ('R':xs) (low,high) = findCol xs (1+low+(high-low) `div` 2,high)

binaryIndex :: Char -> Char -> (String -> (Int,Int) -> Int)
binaryIndex l h = runIndex
    where
       runIndex :: String -> (Int,Int) -> Int
       runIndex (h:[]) (low,high) = high
       runIndex (l:[]) (low,high) = low
       runIndex (l:xs) (low,high) = runIndex xs (low,high-(1+high-low) `div` 2)
       runIndex (h:xs) (low,high) = runIndex xs (1+low+(high-low) `div` 2,high)
 

parseRowCol :: String -> (Int,Int)
parseRowCol s = (findRow (take 7 s) (0,127), findCol (drop 7 s) (0,7))

genID :: (Int,Int) -> Int
genID (row,col) = row*8 + col

solution :: [String] -> Int
solution xs = maximum $ map (genID.parseRowCol) xs

main :: IO ()
main = readFile "../input" >>= print.solution.lines