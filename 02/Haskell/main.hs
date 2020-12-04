import Data.Char

data Header = Header Int Int Char deriving Show

parseHeader :: String -> (Header,String)
parseHeader (l:'-':h:' ':c:xs) = (Header (digitToInt l) (digitToInt h) c, drop 2 xs) 
parseHeader (lh:ll:'-':h:' ':c:xs) = (Header (10 * digitToInt lh + digitToInt ll) (digitToInt h) c, drop 2 xs) 
parseHeader (l:'-':hh:hl:' ':c:xs) = (Header (digitToInt l) (10 * digitToInt hh + digitToInt hl) c, drop 2 xs) 
parseHeader (lh:ll:'-':hh:hl:' ':c:xs) = (Header (10 * digitToInt lh + digitToInt ll) (10 * digitToInt hh + digitToInt hl) c, drop 2 xs) 


xor :: Bool -> Bool -> Bool 
xor True False = True
xor False True = True
xor _ _        = False

validatePassword :: (Header,String) -> Bool
validatePassword (Header l h c,xs) = (c == xs !! (l-1)) `xor` (c == xs !! (h-1))

validate :: String -> Bool
validate  =  validatePassword . parseHeader

getCount :: String -> Int
getCount xs = length $ filter validate $ lines xs

main = readFile "../input" >>= print . getCount 

