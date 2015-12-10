module Main (main) where
import Data.List

pps (x:y:ws) = [x,y] `isInfixOf` ws || pps (y:ws)
pps _ = False

p_2 (x:y:z:ws) | x == z = True
p_2 (x:xs) = p_2 xs
p_2 _ = False

l :: String -> Bool
l x = pps x && p_2 x

main = print . length . filter l . lines =<< readFile "d5.input.txt"
