{-# LANGUAGE BangPatterns, ViewPatterns, MagicHash, OverloadedStrings #-}
module Main (main) where

import           Control.DeepSeq
import           Control.Applicative ((<$>), (<*>), (*>), (<*))

import           Codec.Picture.Types
import           Codec.Picture

import           Data.Function
import           Data.List
import           Data.Word
import           Data.Monoid
import qualified Data.Foldable    as F
import qualified Data.Traversable as T
import           Control.Monad
import           Control.Monad.ST
import           Data.STRef

import qualified Data.ByteString.Lazy  as BSL
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString       as BS
import           Data.Binary.Put

import           Data.Vector (Vector)
import qualified Data.Vector as V

import qualified Data.Vector.Unboxed as UV
import qualified Data.Vector.Unboxed.Mutable as MUV

import qualified Data.Vector.Mutable as MV

import qualified Codec.Compression.Zlib as ZL

import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as M

import           Data.Set (Set)
import qualified Data.Set as S



import           GenMap.Types
import           GenMap.MutMapNode
import           GenMap.Utils
import           GenMap.ColorNodeMP
import           GenMap.ImageProcess

s :: Int -> Node
s = Node stone

level :: CityMap
level = CityMap . V.fromList . map V.fromList $
      [map s [01, 01, 01, 01, 01]
      ,map s [01, 02, 02, 02, 01]
      ,map s [01, 02, 03, 02, 01]
      ,map s [01, 02, 02, 02, 01]
      ,map s [01, 01, 01, 01, 01]
      ]


i2w :: Int -> Word16
i2w = fromIntegral
w2i :: Word16 -> Int
w2i = fromIntegral


forMV :: MV.STVector s a -> (Int -> a -> ST s a) -> ST s ()
forMV !v !f = 
    F.forM_ [0..MV.length v-1] $ \ !i -> do
        e <- MV.unsafeRead v i
        !x <- f i e
        MV.write v i x

-- why the fuck
putLo :: Integral n => (xs -> n) -> ((Word8 -> Put) -> xs -> Put) -> [xs] -> Put
putLo len rec xs = do
        putWord16be (i2w (length xs))
        F.forM_ xs $ \x -> do
            putWord16be (fromIntegral (len x))
            rec putWord8 x

putS :: [BS8.ByteString] -> Put
putS = putLo BS8.length bmapL_
putLS :: [BSL.ByteString] -> Put
putLS = putLo BSL.length blmapL_


            
for' :: Monad m => MapNode -> (Word16 -> m a) -> m ()
for' im f = case dims im of
              Dims x_s y_s z_s ->
                loop 0 (z_s-1) $ \z -> 
                 let !z_v = vec im `V.unsafeIndex` z 
                 in loop 0 (y_s-1) $ \y ->
                     let !y_v = z_v `V.unsafeIndex` y
                     in loop 0 (x_s-1) $ \x -> do
                         f (y_v `UV.unsafeIndex` x)


zlib :: Put -> Put
zlib = blmapL_ putWord8 . ZL.compress . runPut

mapNode :: MapNode -> BSL.ByteString
mapNode (force -> !im) = runPut $ do
                blmapL_ putWord8 "MTSM"
                putWord16be 4
                let (Dims x y z) = dims im
                putWord16be (i2w x)
                putWord16be (i2w y)
                putWord16be (i2w z)
                F.forM_ [0..y-1] $ \ !_ ->
                    putWord8 255
                putS (nodeMaps im)
                zlib $ do
                    for' im $ \ !b ->
                        putWord16be b
                    for' im $ \ !_ ->
                        putWord8 255
                    for' im $ \ !_ ->
                        putWord8 0


iforM_ :: Monad m => V.Vector a -> (Int -> a -> m a) -> m ()
iforM_ !vc !f = V.ifoldr (\i e r -> f i e >> r) (return ()) vc

foldV :: V.Vector (V.Vector a) -> (Int ->Int -> a -> ST s ()) -> ST s ()
foldV !vc !f = iforM_ vc  $ \ !p_x !xv -> do
                iforM_ xv $ \ !p_y !n  -> do
                    f p_x p_y n
                    return n
                return xv

cityDims :: CityMap -> Dims
cityDims (CityMap xs) =
    let (!p_x,!p_z) = (V.length xs, V.length (V.head xs))
        !p_y = V.maximum (V.map (V.maximum . V.map (\(Node _ x) -> fromIntegral x)) xs)
    in Dims p_x p_y p_z

mt :: CityMap -> MapNode
mt city@(CityMap !xs) = force $ runST $ do
    let !(Dims p_x p_y p_z) = cityDims city
    mapNode <- newMutMapNode p_x p_y p_z
    foldV xs $ \ !x !z (Node n top) -> do
        case n of
             block | block == grass -> do
                            writeNode mapNode x top z grass
                            dirt'id <- getNodeID mapNode dirt
                            forM_ [0..top-1] $ \ !y ->
                                writeNode' mapNode x y z dirt'id
                   | block == water -> do
                            writeNode mapNode x 0 z dirt
                            water'id <- getNodeID mapNode water
                            forM_ [1..top] $ \ !y ->
                                writeNode' mapNode  x y z water'id
                   | otherwise -> do
                            block'id <- getNodeID mapNode block
                            forM_ [0..top] $ \ !y ->
                                writeNode' mapNode  x y z block'id

    frz mapNode
    

writeSch :: FilePath -> MapNode -> IO ()
writeSch fp = BSL.writeFile fp . mapNode


ct :: FilePath -> IO ()
ct basePath = do
        !m <- loadCityMap (basePath <> "_points.png")  (basePath <> "_height.png")
        !nodepic <- parseMP <$> BS8.readFile (basePath <> "_nodes.txt")
        force m `seq` putStrLn ""
        writeSch "/home/exio4/.minetest/worlds/wow/schems/test_01.mts" (mt m)

        
main :: IO ()
main = ct "gm"
