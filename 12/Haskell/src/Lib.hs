module Lib where

import qualified Data.ByteString.Char8 as B
import Data.Functor

data Direction = North Int | East Int | South Int | West Int | Forward Int deriving Show
data Orientation = Port Int | Starboard Int deriving Show

type Command = Either Direction Orientation

type X = Int
type Y = Int
type Postition = (X,Y)
type Bearing = Int

bearingToCartesian :: Int -> (X,Y)
bearingToCartesian 0 = (1,0)
bearingToCartesian 90 = (0,1)
bearingToCartesian 180 = (-1,0)
bearingToCartesian 270 = (0,-1)
bearingToCartesian 360 = (1,0)
bearingToCartesian x | x < 0 = bearingToCartesian (360 + x)
                     | x > 360 = bearingToCartesian (x - 360)

data Boat = Boat Postition Bearing deriving Show

newBoat :: Boat
newBoat = Boat (0,0) 0

runCommand ::  Command -> Boat -> Boat
runCommand c b = either (moveBoat b) (turnBoat b) c

moveBoat :: Boat -> Direction -> Boat
moveBoat (Boat (x,y) b) (North v) = Boat (x,y+v) b 
moveBoat (Boat (x,y) b) (South v) = Boat (x,y-v) b 
moveBoat (Boat (x,y) b) (West v) = Boat (x-v,y) b 
moveBoat (Boat (x,y) b) (East v) = Boat (x+v,y) b
moveBoat (Boat (x,y) b) (Forward v) = Boat (x+dx*v,y+dy*v) b
    where (dx,dy) = bearingToCartesian b

turnBoat :: Boat -> Orientation -> Boat
turnBoat (Boat pos b) (Port deg)      = Boat pos (b+deg)
turnBoat (Boat pos b) (Starboard deg) = Boat pos (b-deg)

runSolver :: [Command] -> Boat
runSolver = foldr runCommand newBoat

findDistance :: Boat -> Int
findDistance (Boat (x,y) _ ) = abs x + abs y 

readInpt :: String -> IO [Command]
readInpt f = B.readFile f >>= \x -> return $ parse x

parse :: B.ByteString -> [Command]
parse s = map parse' $ B.lines s
    where 
        parse' xs = let Just (n, _) = B.readInt $ B.tail xs in parseDir n (B.head xs)


parseDir :: Int -> Char -> Either Direction Orientation
parseDir i 'N' = Left $ North i
parseDir i 'E' = Left $ East i
parseDir i 'S' = Left $ South i
parseDir i 'W' = Left $ West i
parseDir i 'L' = Right $ Port i
parseDir i 'R' = Right $ Starboard i
parseDir i 'F' = Left $ Forward i

run :: String -> IO Int
run f = readInpt f >>= \x -> return $ findDistance $ runSolver x