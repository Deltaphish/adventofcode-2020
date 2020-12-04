import Data.Functor

newtype Map = Map [[Char]]
type Vel = (Int,Int)
type Pos = (Int,Int)
type CollisionCount = Int

unwrap (Map m) = m

readInput :: String -> IO Map
readInput path = readFile path <&> lines <&> Map

mapWidth :: Map -> Int
mapWidth (Map xs) = length $ head xs

mapHeight :: Map -> Int
mapHeight (Map xs) = length xs

collision :: Pos -> Map -> Maybe Bool
collision p m = m `index` p <&> ('#' ==)

index :: Map -> (Int,Int) -> Maybe Char
index m (x,y) | y >= mapHeight m = Nothing
              | otherwise        = Just $ unwrap m !! y !! (x `mod` mapWidth m)

bti False = 0
bti True  = 1

tp :: (Int,Int) -> (Int,Int) -> (Int,Int)
(x,y) `tp` (z,u) = (x+z,y+u)

solver :: Map -> Pos -> Vel -> CollisionCount
solver m p v = case collision p m <&> bti of
                Just n  -> n + solver m (p `tp` v) v
                Nothing -> 0

angles :: [Vel]
angles = [(1,1),(3,1),(5,1),(7,1),(1,2)]

main :: IO ()
main = readInput "../input" >>= \m -> print $ product $ map (solver m (0,0)) angles