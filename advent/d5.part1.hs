module Main (main) where


threeVowels = (>= 3) . length . filter (`elem` "aeiou")

twice (x:q@(y:ws)) = (x == y) || twice q
twice _            = False

bad (x:y:ws) | [x,y] `elem` ["xy", "ab", "cd", "pq"] = True
             | otherwise = bad (y:ws)
bad _ = False

l :: String -> Bool
l x = threeVowels x && twice x && not (bad x)

main = print . length . filter l . lines =<< readFile "d5.input.txt"
