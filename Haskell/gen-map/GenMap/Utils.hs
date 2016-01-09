{-# LANGUAGE BangPatterns, ViewPatterns, MagicHash, OverloadedStrings #-}
module GenMap.Utils (stPrint, loop, forMV, blmapL_, bmapL_) where

import           Data.Word

import           Control.Monad.ST

import qualified Data.ByteString.Lazy  as BSL
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString       as BS

import qualified Data.Vector.Mutable as MV

import           GHC.Types
import           GHC.Prim

loop :: Monad m => Int -> Int -> (Int -> m a) -> m ()
loop (I# start) (I# stop) = \fun ->
        let go !x = case x ># stop of
                        1# -> return ()
                        _  -> let !al_i = I# x in fun al_i >> go (x +# 1#)
        in go start

forMV :: MV.STVector s a -> (Int -> a -> ST s a) -> ST s ()
forMV !v !f = 
    loop 0 (MV.length v-1) $ \ !i -> do
        e <- MV.unsafeRead v i
        !x <- f i e
        MV.write v i x

stPrint :: Show a => a -> ST s ()
stPrint s = unsafeCoerce# (print s)



blmapL_ :: Monad m => (Word8 -> m a) -> BSL.ByteString -> m ()
blmapL_ f = BSL.foldr (\x y -> f x >> y) (return ()) 

bmapL_ :: Monad m => (Word8 -> m a) -> BS8.ByteString -> m ()
bmapL_ f = BS.foldr (\x y -> f x >> y) (return ())
