module Main (main) where
import Data.List
import Data.Maybe
import Text.Parsec
import Control.Applicative

data Present = Present !Int !Int !Int
  deriving (Show,Eq,Ord)

wrap :: Present -> Int
wrap (Present l w h) = (2 * l_w) + (2 * w_h) + (2 * l_h) + minimum qs
     where qs@[l_w, w_h, l_h] = [l * w, w * h, l * h]

ribbon :: Present -> Int
ribbon (Present l w h) = (s1 + s1 + s2 + s2) + (l * w * h)
   where [s1,s2,_] = sort [l,w,h]

parseP :: String -> Maybe Present
parseP = either (const Nothing) Just . runParser p () "present_parser" 
    where p = Present <$> (int <* char 'x') <*> (int <* char 'x') <*> int
          int = (read :: String -> Int) <$> many1 digit    

    

main = print =<< fmap (sum . mapMaybe (fmap ribbon . parseP) . lines) (readFile "d2.input.txt")

