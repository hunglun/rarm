{-# LANGUAGE DuplicateRecordFields #-}  -- DuplicateRecordFields is added in ghc v8.0
---------------------------------------------------------------------------------------------------------
-- OBJECTIVE
-- This program aims to meet the Engineering Week 2017 robotic arm challenge through modelling in Haskell.
---------------------------------------------------------------------------------------------------------
import Control.Monad
import Data.List
---------------------------------------------------------------------------------------------------------
-- Data Type
---------------------------------------------------------------------------------------------------------
class Object a where 
    pos :: a -> (Int,Int)
instance Object Ball where
    pos (Ball x y _ _) = (x,y)
instance Object Robot where
    pos (Robot x y _) = (x,y)
instance Object Bin where
    pos (Bin x y _) = (x,y)
data Color = Yellow | Green | Red | Blue | Invalid deriving (Show,Eq,Ord)
data Status = Free | Caught | Collected deriving (Show,Eq)
data Ball = Ball {x::Int, y::Int, color::Color, status :: Status} deriving (Show,Eq)
data Bin = Bin {x::Int,y::Int,color::Color} deriving (Show)
data Robot = Robot {x::Int,y::Int, mem :: [(Int,Int)]} deriving (Show)
data Game =  Game { balls :: [Ball], bins :: [Bin], robot:: Robot, time :: Int } deriving (Show)
---------------------------------------------------------------------------------------------------------
-- Functions
---------------------------------------------------------------------------------------------------------
-- TODO: balls are not moving at all.
-- TODO: add a case for collected
updateBalls :: Game -> [Ball]
updateBalls g = map (\b-> if (pos b) == (pos _robot) then b {status=Caught} else b) (balls g)  -- mark ball as collected if applicable
    where caughtBalls = catch (robot g) (balls g)
          _robot = robot g
-- TODO: make use of the memory to make smarter moves.
updateRobot :: Game -> Robot
updateRobot g 
    | bx > rx = Robot {x = rx + 1, y = y (robot g:: Robot),mem = []}
    | bx < rx = Robot {x = rx - 1, y = y (robot g:: Robot),mem = []}
    | by > ry = Robot {y = ry + 1, x = x (robot g:: Robot),mem = []}
    | by < ry = Robot {y = ry - 1, x = x (robot g:: Robot),mem = []}
    | otherwise = robot g
    where rx = x (robot g:: Robot)
          ry = y (robot g:: Robot)
          bx = x (ball :: Ball)
          by = y (ball :: Ball)
          ball = head $filter ((/= Caught) . status) $balls g 
catch :: (Object a, Object  b) => a -> [b] -> [b]
catch catcher targets = filter (\t -> (pos catcher) == (pos t)) targets               
play :: Game -> [Game]
play g = if (isComplete g) then [g] else g : play g'
    where g' = Game {balls = updateBalls g, bins = bins g, robot = updateRobot g, time = (time g) + 1}
-- Game Completion Criteria:
-- all the balls are CAUGHT  -- TODO : change to COLLECTED
isComplete :: Game -> Bool
isComplete g = all ((== Caught) . status) (balls g) 
-- Game Winning Criteria:
-- 1. all the balls must be inside bins
-- 2. all the object in the same position share the same color
result :: Game -> Bool
result g = length ((nub ballData) \\ (nub binData)) == 0 
    where binData  = map (\b -> ( (x (b::Bin),  y (b::Bin)),  color (b::Bin)))  (bins  g) -- convert object to (position, color)
          ballData = map (\b -> ( (x (b::Ball), y (b::Ball)), color (b::Ball))) (balls g) 
---------------------------------------------------------------------------------------------------------
-- Set Up Game
---------------------------------------------------------------------------------------------------------
_balls = [Ball {x=0,y=0,color=Yellow, status = Free},Ball {x=0,y=1,color=Green,status=Free}] 
_bins = [Bin {x=0,y=0,color = Yellow},Bin {x=0,y=1,color = Green}] -- TODO: currently bins are placed at where the balls are.
_robot = Robot {x=0,y=0,mem = []}
_time = 0
-----------------------------------------------------------------------------------------------------------
-- Play! 
-----------------------------------------------------------------------------------------------------------
main =  do
  let g = play (Game {balls = _balls, bins = _bins, robot = _robot, time = _time})
  mapM print g
  putStrLn ("Game Result:" ++ (if result $last g then "You win!" else "You Lose!"))
-----------------------------------------------------------------------------------------------------------
-- END OF PROGRAM 
-----------------------------------------------------------------------------------------------------------
