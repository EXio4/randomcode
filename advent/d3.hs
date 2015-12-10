{-# LANGUAGE BangPatterns #-}

module Main (main) where

import Data.List
import Data.Maybe
import qualified Data.Map.Strict as M

data Position = Position !Int !Int 
   deriving (Eq,Ord)
data Universe = Universe !Position !Position !(M.Map Position Int)
data C = UP | DOWN | LEFT | RIGHT

start = Universe (Position 0 0) (Position 0 0) M.empty

santaGift :: Universe -> Universe
santaGift (Universe santa robosanta world) = Universe santa robosanta (M.insertWith (+) santa 1 world)
roboGift :: Universe -> Universe
roboGift (Universe santa robosanta world) = Universe santa robosanta (M.insertWith (+) robosanta 1 world)

apply :: C -> Position -> Position
apply UP    (Position x y) = Position (x+1) (y  )
apply DOWN  (Position x y) = Position (x-1) (y  )
apply LEFT  (Position x y) = Position (x  ) (y+1)
apply RIGHT (Position x y) = Position (x  ) (y-1)


stepSanta,stepRobo :: C -> Universe -> Universe
stepSanta santa (Universe s rs m) = santaGift (Universe (apply santa s) rs m)
stepRobo  robo  (Universe s rs m) = roboGift  (Universe s (apply robo rs) m)


parse :: Char -> Maybe C
parse '>' = Just RIGHT
parse '<' = Just LEFT
parse '^' = Just UP
parse 'v' = Just DOWN
parse  _  = Nothing

solution (Universe _ _ m) = M.size m

main = readFile "d3.input.txt" >>= print . solution . f start . mapMaybe parse 
  where f !z [] = z
        f !z (x1:x2:xs) = f (stepSanta x1 (stepRobo x2 z)) xs
        f !z [x1] = stepSanta x1 z

