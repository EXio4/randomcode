{-# LANGUAGE BangPatterns #-}
module GenMap.ImageProcess 
    (toCityMap
    ,loadCityMap) where

import           Control.DeepSeq

import           Control.Applicative ((<$>), (<*>), (*>), (<*))

import           Codec.Picture.Types
import           Codec.Picture

import           Data.Word
import           Control.Monad
import           Control.Monad.ST

import           Data.Vector (Vector)
import qualified Data.Vector as V

import qualified Data.Vector.Mutable as MV

import           Data.STRef

import           GenMap.Types
import           GenMap.Utils

imageRGB8 (ImageRGB8 x) = Right x
imageRGB8 _ = Left "Invalid format (Expected RGB8)"
imageY8 (ImageY8 x) = Right x
imageY8 _ = Left "Invalid format (Expected Y8)"




loadPic :: (DynamicImage -> Either String (Image p)) -> FilePath -> IO (Image p)
loadPic f p = do x <- fmap (>>= f) (readImage p)
                 case x of
                      Left err -> error err
                      Right x  -> return x

foldPic :: (Pixel p, Monad m) => (Int -> Int -> p -> m a) -> Image p -> m ()
foldPic f im = pixelFold (\acc x y p-> f x y p >> acc) (return ()) im

toCityMap :: Image PixelRGB8 -> Image Pixel8 -> CityMap
toCityMap pVec hVec = force $ runST $ do
            let (h,w) = (imageHeight pVec, imageWidth pVec)
            !v <- MV.new h
            loop 0 (h-1) $ \i -> MV.write v i =<< MV.replicate w (Node air 0)
            foldPic (\x y p -> go v x y (pixelAt hVec x y) p) pVec
            CityMap <$> (V.mapM V.unsafeFreeze <=< V.unsafeFreeze) v
    where writ :: MV.STVector s (MV.STVector s Node) -> (Int, Int) -> Node -> ST s ()
          writ !outer (!x,!y) !n = do
              inner <- MV.read outer x
              MV.write inner y n
          go :: MV.STVector s (MV.STVector s Node) -> Int -> Int -> Word8 -> PixelRGB8 -> ST s ()
          go !vc  !x !y !height (PixelRGB8 r g b) = do
                let nod | b >= 200 && g <= 150 && r <= 150  = water
                        | g >  150 && b <= 150 && r >  150  = sand
                        | g >  150 && b <= 150 && r <= 150  = grass
                        | otherwise = stone
                    ibrig = 1 + floor (fromIntegral height / 10)
                writ vc (x,y) (Node nod ibrig)

loadCityMap :: FilePath -> FilePath -> IO CityMap
loadCityMap nmap hmap = do
        !points <- loadPic imageRGB8 nmap
        !height <- loadPic imageY8   hmap
        return (toCityMap points height)