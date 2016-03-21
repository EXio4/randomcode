{-# LANGUAGE ScopedTypeVariables #-}

import Data.Functor
import Control.Concurrent
import Control.Exception
import System.IO.Unsafe

liftB :: forall a b. (a -> a -> b) -> a -> a -> IO b
liftB f x y = do
    mv <- newEmptyMVar
    let work e1 e2 = forkIO $ do
              (r :: Either SomeException b) <- try (evaluate (f e1 e2))
              putMVar mv r
    t2 <- work x y
    t1 <- work y x
    a <- readMVar mv
    mapM_ killThread [t1,t2]
    case a of
       Left e  -> throwIO e
       Right v -> return v   
              

unsafeLiftB :: (a -> a -> b) -> a -> a -> b
unsafeLiftB f x y = unsafePerformIO (liftB f x y)
