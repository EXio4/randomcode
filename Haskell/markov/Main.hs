{-# LANGUAGE GeneralizedNewtypeDeriving, TupleSections #-}

module Main (main) where

import           Data.Functor
import           Control.Applicative
import           Control.Monad
import           Control.Monad.Trans
import           Control.Monad.State.Strict
import           Control.Monad.Reader

import           System.Random

import           Data.Maybe
import           Data.List
import qualified Data.Map.Strict as M
import qualified Data.Set        as S

data T a b c = T !a !b !c
   deriving (Show)

data Chain = MkChain !(M.Map [String] [String])
     deriving (Show)

newtype Markov m a = MkMarkov (StateT (T Int StdGen Chain) m a)
     deriving (MonadIO, Monad, Applicative, Functor)

idx :: Int -> [a] -> Maybe a
idx 0 (x : xs) = Just x
idx n (_ : xs) = idx (n-1) xs
idx _  []      = Nothing


rand :: Monad m => (StdGen -> (a, StdGen)) -> Markov m a
rand f = MkMarkov $ do
               (T a b c) <- get
               let (x, b') = f b
               put (T a b' c)
               return x

randList :: [a] -> StdGen -> (Maybe a, StdGen)
randList ls gen = (r, gen') 
     where (z, gen') = randomR (0, length ls - 1) gen
           r | null ls   = Nothing
             | otherwise = Just (ls !! z) -- ugly stuff

randMap :: Ord k => M.Map k [a] -> StdGen -> (Maybe (k,a), StdGen)
randMap map gen = let (k, gen' ) = randList (M.keys map) gen
                  in case k of
                       Nothing -> (Nothing , gen') -- empty map, nothing we can do there
                       Just k' -> let (v, gen'') = randList (map M.! k') gen'
                                  in case v of
                                        Nothing -> (Nothing     , gen'')
                                        Just a  -> (Just (k',a) , gen'')
                                        

train :: Monad m => String -> Markov m ()
train str = MkMarkov $ 
                modify $ \(T n_words gen (MkChain ch)) ->
                    let w = (M.fromList . mapMaybe (\ws -> (take n_words ws,) <$> (fmap (:[]) (idx n_words ws))) . tails . words) str
                        ch' = M.unionWith (++) w ch
                    in T n_words gen (MkChain ch')
               

gen :: (Functor m, Monad m) => Markov m String
gen = do
          T _ _ (MkChain ch) <- MkMarkov get        
          s <- rand (randMap ch)
          let go ((k:ks),v) = do
                            let nx = ks ++ [v]  
                            ((k:) .) <$> (case M.lookup nx ch of
                                 Nothing    -> return ((ks++))
                                 Just nextL -> do
                                      z <- rand (randList nextL)
                                      case z of
                                         Nothing -> return ((ks++))
                                         Just z' -> go (nx, z'))
          maybe (return "") (fmap (unwords . ($ [])) . go) s
          

run :: Monad m => Int -> StdGen -> Markov m a -> m a
run wl std (MkMarkov x) = evalStateT x (T wl std (MkChain M.empty))


trainFromFile :: FilePath -> Markov IO ()
trainFromFile fp = do
    xs <- lines <$> liftIO (readFile fp)
    mapM_ train xs

indigoTest :: Int -> Markov IO [String]
indigoTest n = do
    train (unwords
       ["Indigo children according to a pseudoscientific New Age concept,"
       ,"are children who are believed to possess special, unusual, and sometimes"
       ,"supernatural traits or abilities The idea is based on concepts developed"
       ,"in the 1970s by Nancy Ann Tappe and further developed by Jan Tober and Lee Carroll."
       ,"The concept of indigo children gained popular interest with the publication of a series"
       ,"of books in the late 1990s and the release of several films in the following decade"
       ,"A variety of books conferences and related materials have been created surrounding belief"
       ,"in the idea of indigo children and their nature and abilities The interpretations of these"
       ,"beliefs range from their being the next stage in human evolution in some cases possessing paranormal"
       ,"abilities such as telepathy to the belief that they are more empathetic and creative than their"
       ,"peers"])
    mapM (\_ -> gen) [1..n]

runW :: Int -> Markov IO a -> IO a
runW wl mk = newStdGen >>= \g -> run wl g mk

test go = do
    trainFromFile "mark-input"
    go

main = putStrLn "hi"
