module Lib where

import qualified Data.ByteString.Char8 as B
import Data.Functor

data Direction = North Int | East Int | South Int | West Int | Forward Int deriving Show
data Orientation = Port Int | Starboard Int deriving Show

type Command = Either Direction Orientation

type X = Int
type Y = Int
type Postition = (X,Y)
type Waypoint = (X,Y)

data Boat = Boat Postition Waypoint deriving Show

newBoat :: Boat
newBoat = Boat (0,0) (10,1)

runCommand ::  Command -> Boat -> Boat
runCommand c b = either (moveBoat b) (turnBoat b) c

moveBoat :: Boat -> Direction -> Boat
moveBoat (Boat p (wx,wy)) (North v) = Boat p (wx,wy+v) 
moveBoat (Boat p (wx,wy)) (South v) = Boat p (wx,wy-v)  
moveBoat (Boat p (wx,wy)) (West v) = Boat p (wx-v,wy) 
moveBoat (Boat p (wx,wy)) (East v) = Boat p (wx+v,wy) 
moveBoat (Boat (x,y) (wx,wy)) (Forward v) = Boat (x+wx*v,y+wy*v) (wx,wy)

rotate (x,y) 90 = (-y,x)
rotate (x,y) 180 = (-x,-y)
rotate (x,y) 270 = (y,-x)
rotate (x,y) 360 = (x,y)
rotate (x,y) deg | deg < 0 = rotate (x,y) (360 + deg)
                    | deg > 360 = rotate (x,y) (deg - 360)

turnBoat :: Boat -> Orientation -> Boat
turnBoat (Boat pos wp) (Port deg) = 
    let wp' = rotate wp deg in
        Boat pos wp'
     
turnBoat (Boat pos wp) (Starboard deg) = 
    let wp' = rotate wp (-deg) in
        Boat pos wp'


runSolver :: [Command] -> Boat
runSolver = foldl (flip runCommand) newBoat

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