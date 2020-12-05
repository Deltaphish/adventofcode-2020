import Data.List
binaryIndex :: Char -> Char -> (String -> (Int,Int) -> Int)
binaryIndex l h = runIndex
    where
       runIndex :: String -> (Int,Int) -> Int
       runIndex (c:[]) (low,high) | c == l    = low
                                  | otherwise = high
       runIndex (c:xs) (low,high) | c == l    = runIndex xs (low,high - diff - 1)
                                  | otherwise = runIndex xs (low + diff + 1,high)
            where diff = div (high-low) 2

findRow = binaryIndex 'F' 'B'
findCol = binaryIndex 'L' 'R'

parseRowCol :: String -> (Int,Int)
parseRowCol s = (findRow (take 7 s) (0,127), findCol (drop 7 s) (0,7))

genID :: (Int,Int) -> Int
genID (row,col) = row*8 + col

findMissing :: [String] -> Int
findMissing xs = head $ [68..970] \\ (map (genID.parseRowCol) xs)

main :: IO ()
main = readFile "../input" >>= print.findMissing.lines