module Main (main) where

import Data.List
import Data.Function

count :: Eq a => a -> [a] -> Int
count x qs = length $ filter (==x) qs

match :: [String] -> [Char] -> [String]
match dict [] = dict
match dict q@(x:_) = let l = length q
                     in filter (\str -> count x str >= l) dict

unscramble :: [String] -> [Char] -> [String]
unscramble dict w = (foldl' match (pre dict (length w)) . group . sort) w where
        pre dict w = filter (\str -> length str == w) dict

main :: IO ()
main = do
   dict <- lines <$> readFile "dict.txt"
   fix $ \loop -> do
      putStr "thing to unscramble: "
      x <- getLine
      let d' = unscramble dict x
      mapM_ putStrLn d'
      putStrLn "-----"
      loop
