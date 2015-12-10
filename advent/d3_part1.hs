{-# LANGUAGE BangPatterns #-}

module Main (main) where

import Data.List
import qualified Data.Map.Strict as M

data Universe = Universe !(Int,Int) !(M.Map (Int,Int) Int)
data C = UP | DOWN | LEFT | RIGHT

start = Universe (0,0) M.empty

giveGift :: Universe -> Universe
giveGift (Universe p m) = Universe p (M.insertWith (+) p 1 m)

go :: C -> Universe -> Universe
go x (Universe (a,b) m) = Universe p' m
  where p' = case x of
                UP -> (a+1,b)
                DOWN -> (a-1,b)
                LEFT -> (a, b+1)
                RIGHT -> (a, b-1)

step :: C -> Universe -> Universe
step x = giveGift . go x

parse :: Char -> Maybe C
parse '>' = Just RIGHT
parse '<' = Just LEFT
parse '^' = Just UP
parse 'v' = Just DOWN
parse  _  = Nothing

solution (Universe _ m) = M.size m

main = readFile "d3.input.txt" >>= print . solution . foldl' (flip w) start 
  where w x  = case parse x of Nothing -> id
                               Just o  -> step o 
