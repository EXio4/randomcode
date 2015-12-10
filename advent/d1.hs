{-# LANGUAGE BangPatterns #-}
module Main (main) where


part1 = sum . map f 
  where f '(' = 1
        f ')' = -1
        f  _  = 0

part2 = go 0 0
   where go !pos !x _ | x == (-1) = pos
         go !pos !x ('(':rs) = go (pos+1) (x+1) rs
         go !pos !x (')':rs) = go (pos+1) (x-1) rs
         go pos   x (_:rs)   = go pos x rs
         go _ _ _ = error "wot"

main :: IO ()
main = print =<< fmap part2 getLine
