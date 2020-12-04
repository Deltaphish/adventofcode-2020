import Data.Functor
import Data.List
import Data.Maybe
import Data.Char

newtype Passport = Passport [PassportField] deriving Show

data PassportField = Byr | Iyr | Eyr | Hgt | Hcl | Ecl | Pid | Cid deriving (Eq,Show)



validatePassport :: Passport -> Bool
validatePassport (Passport xs) = all (`elem` xs) [Byr,Iyr,Eyr,Hgt,Hcl,Ecl,Pid]

parseField :: (String,String) -> Maybe PassportField
parseField ("byr",year) | length year == 4 = 
    let r = (read year) :: Int in 
        if r >= 1920 && r <= 2002 then Just Byr 
                                  else Nothing
                        | otherwise        = Nothing

parseField ("iyr",year) | length year == 4 = 
    let r = (read year) :: Int in 
        if r >= 2010 && r <= 2020 then Just Iyr 
                                  else Nothing

parseField ("eyr",year) | length year == 4 = 
    let r = (read year) :: Int in 
        if r >= 2020 && r <= 2030 then Just Eyr 
                                  else Nothing

parseField ("hgt",height) | "cm" `isSuffixOf` height = let h = (read $ takeWhile isDigit height) :: Int in
                                                            if h >= 150 && h <= 193 then Just Hgt else Nothing
                          | "in" `isSuffixOf` height = let h = (read $ takeWhile isDigit height) :: Int in
                                                            if h >= 59 && h <= 76 then Just Hgt else Nothing
                          | otherwise = Nothing

parseField ("hcl",color)  | '#' == head color && length color == 7 && all (`elem` "0123456789abcdef") (tail color) = Just Hcl
                          | otherwise = Nothing

parseField ("ecl", color) | color `elem` ["amb","blu","brn","gry","grn","hzl","oth"] = Just Ecl
                          | otherwise = Nothing

parseField ("pid", id) | length id == 9 && all isDigit id = Just Pid
                       | otherwise = Nothing
                        
parseField _     = Nothing

parseFieldHead :: String -> (String,String)
parseFieldHead xs = (takeWhile (':' /=) xs,dropWhile(' ' ==) $ drop 1 $ dropWhile(':'/=) xs)

parsePassport :: [String] -> Passport
parsePassport xs = Passport $ mapMaybe (parseField.parseFieldHead) xs

parsePassports :: [[String]] -> [Passport]
parsePassports = map parsePassport

readInput :: String -> IO [[String]]
readInput filename = readFile filename 
    <&> lines 
    <&> (\x -> x ++ [""])
    <&> groupBy (\a b -> "" `notElem` [a,b])
    <&> filter ([""] /=)
    <&> map unwords
    <&> map words

main :: IO()
main = readInput "../input" >>= print . length .filter validatePassport . parsePassports