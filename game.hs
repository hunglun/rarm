{-# LANGUAGE DuplicateRecordFields #-}
-- DuplicateRecordFields is added in ghc v8.0

---------------------------------------------------------------------------------------------------------
-- OBJECTIVE --------------------------------------------------------------------------------------------
-- This program aims to meet the Engineering Week 2017 robotic arm challenge through modelling in Haskell.
---------------------------------------------------------------------------------------------------------
import Control.Monad
import Data.List
---------------------------------------------------------------------------------------------------------
-- Data Type --------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------------
data Object = BallOject Ball | BinObject Bin | RobotObject Robot
data Color = Yellow | Green | Red | Blue | Invalid deriving (Show,Eq)
data Ball = Ball {x::Int, y::Int, color::Color} deriving (Show,Eq)
type Balls = [Ball]
data Bin = Bin {ballCount :: Int, colorCount :: Int, color :: Color} deriving (Show)
type Bins = [Bin]
data Robot = Robot {x::Int,y::Int, mem :: [(Int,Int)]} deriving (Show)
type Time = Int
data Game =  Game { balls :: Balls, bins :: Bins, robot:: Robot, time :: Time } deriving (Show)
---------------------------------------------------------------------------------------------------------
-- Functions  -------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------------
updateBalls :: Game -> Balls
updateBalls g = (balls g) \\ caughtBalls -- stationary
    where caughtBalls = catch (robot g) (balls g)

updateRobot :: Game -> Robot
updateRobot g 
    | length (balls g) == 0 = robot g
    | bx > rx = Robot {x = rx + 1, y = y (robot g:: Robot),mem = []}
    | bx < rx = Robot {x = rx - 1, y = y (robot g:: Robot),mem = []}
    | by > ry = Robot {y = ry + 1, x = x (robot g:: Robot),mem = []}
    | by < ry = Robot {y = ry - 1, x = x (robot g:: Robot),mem = []}
    | otherwise = robot g
    where rx = x (robot g:: Robot)
          ry = y (robot g:: Robot)
          bx = x (ball :: Ball)
          by = y (ball :: Ball)
          ball = head $balls g 

catch :: Robot -> Balls -> Balls
catch r bs = filter (\b -> x (r :: Robot) == x (b:: Ball)
                           && y (r :: Robot) == y (b:: Ball) ) bs

_updateBins :: Ball -> Bins -> Bins
_updateBins ball bins = map (\bin -> if (color (bin::Bin) == color (ball::Ball)) 
                                     then Bin {ballCount = ballCount bin + 1, colorCount = 1 , color = color (bin :: Bin)}
                                     else bin) bins
updateBins :: Game -> Bins
updateBins g = if length caughtBalls == 0 then bins g 
               else _updateBins (head caughtBalls) (bins g)
               where caughtBalls = catch (robot g) (balls g)
               
play :: Game -> [Game]
play g = if (isComplete g) then [g] else g : play g'
    where g' = Game {balls = updateBalls g, bins = updateBins g, robot = updateRobot g, time = (time g) + 1}

isComplete :: Game -> Bool
isComplete g = all ((== 1) . colorCount) (bins g) -- in each bin, all the balls are of the same color
               && 0 == length (balls g) -- all the balls are in the bins
---------------------------------------------------------------------------------------------------------
-- Set Up Game  -----------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------------
_balls = [Ball {x=0,y=0,color=Yellow},Ball {x=0,y=1,color=Green}]
_bins = [Bin {ballCount = 0, colorCount = 0, color = Green},Bin {ballCount = 0, colorCount = 0, color = Yellow}]
_robot = Robot {x=0,y=0,mem = []}
_time = 0
-----------------------------------------------------------------------------------------------------------
-- Play! ---------------------------------------------------------------------------------------------------
-----------------------------------------------------------------------------------------------------------
main =  do
  let g = play (Game {balls = _balls, bins = _bins, robot = _robot, time = _time})
  mapM print g
-----------------------------------------------------------------------------------------------------------
-- END OF PROGRAM ---------------------------------------------------------------------------------------------------
-----------------------------------------------------------------------------------------------------------
