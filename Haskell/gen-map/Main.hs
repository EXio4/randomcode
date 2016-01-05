{-# LANGUAGE OverloadedStrings, ViewPatterns, BangPatterns, MagicHash #-}

module Main (main) where

import           Control.DeepSeq
import           Control.Applicative ((<$>), (<*>))

import           Codec.Picture.Types
import           Codec.Picture

import           Data.Function
import           Data.List
import           Data.Word
import           Data.Monoid
import qualified Data.Foldable as F
import           Control.Monad
import           Control.Monad.ST
import           Data.STRef

import           Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as BSL
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

import qualified Data.Attoparsec.ByteString as AP

import           GHC.Types
import           GHC.Prim

data Node = Node !BNode {-# UNPACK #-} !Int
        deriving (Show,Eq)

instance NFData Node where
    rnf (Node{}) = ()

data CityMap = CityMap {-# UNPACK #-} !(V.Vector (V.Vector Node))

instance NFData CityMap where
    rnf (CityMap v) = rnf v `seq` ()

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


newtype BNode = BNode ByteString
    deriving (Eq,Ord,Show)
newtype WNode = WNode Word16
    deriving (Eq,Ord,Show)

instance NFData BNode where
    rnf !_ = ()
instance NFData WNode where
    rnf !_ = ()

[air, water, dirt, grass, stone] =
    map BNode [ "air"
              , "default:water_source"
              , "default:dirt"
              , "default:dirt_with_grass"
              , "default:stone"]

data Dims = Dims {-# UNPACK #-} !Int {-# UNPACK #-} !Int {-# UNPACK #-} !Int
    deriving (Show,Eq,Ord)

{- (X,Y,Z) -> BNode -}
data MapNode = MapNode {
     nodeMaps :: ![ByteString]
    ,dims     :: {-# UNPACK #-} !Dims
    ,vec      :: {-# UNPACK #-} !(V.Vector (V.Vector (UV.Vector Word16)))
} deriving (Show,Eq,Ord)

data Mapping = Mapping {-# UNPACK #-} !Word16 !(Map BNode Word16)

data MutMapNode s = MutMapNode {
      mut_nodeMaps :: {-# UNPACK #-} !(STRef s Mapping)
     ,mut_Vec      :: {-# UNPACK #-} !(MV.STVector s (MV.STVector s (MUV.STVector s Word16)))
}

frz :: MutMapNode s -> ST s MapNode
frz (MutMapNode mps vecs) = do
        Mapping _ m <- readSTRef mps
        let m' = map (\(BNode x, _) -> x) . sortBy (compare `on` snd) . M.toList $ m
        lv <- (V.mapM (V.mapM UV.unsafeFreeze) <=<
                       V.mapM  V.unsafeFreeze  <=<
                               V.unsafeFreeze      ) vecs
        let map_node = MapNode m' (dims'vc lv) lv
        map_node `deepseq` return map_node

newMutMapNode :: Int -> Int -> Int -> ST s (MutMapNode s)
newMutMapNode p_x p_y p_z = do
    !v_z <- MV.new p_z
    forMV v_z $ \ !i_z _ -> do
                    !v_y <- MV.new (p_y+1)
                    forMV v_y $ \ !_ _ -> do
                        MUV.replicate p_x 0
                    return v_y
    mpng <- newSTRef (Mapping 1 (M.singleton (BNode "air") 0))
    return (MutMapNode mpng v_z)

writeNode' :: MutMapNode s -> Int -> Int -> Int -> Word16 -> ST s ()
writeNode' (MutMapNode _ vec) x y z node_id = do
                !i1 <- MV.unsafeRead vec z
                !i2  <- MV.unsafeRead i1 y
                MUV.unsafeWrite i2 x node_id    
getNodeID :: MutMapNode s -> BNode -> ST s Word16
getNodeID (MutMapNode mapsRef _) node = do
                Mapping mx maps <- readSTRef mapsRef
                case M.lookup node maps of
                     Nothing -> let !mp = Mapping (mx+1) (M.insert node mx maps)
                                in do writeSTRef mapsRef mp
                                      return mx
                     Just v -> return v

writeNode :: MutMapNode s -> Int -> Int -> Int -> BNode -> ST s ()
writeNode q x y z node = do
                !node_id <- getNodeID q node
                writeNode' q x y z node_id

instance NFData MapNode where
    rnf (MapNode x y z) = rnf x `seq` rnf z `seq` ()

bmapM_ :: Monad m => (Word8 -> m a) -> ByteString -> m ()
bmapM_ f = BSL.foldr (\x y -> f x >> y) (return ()) 

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

dims'vc :: Vector (Vector (UV.Vector Word16)) -> Dims
dims'vc im = Dims x y z
    where x = UV.length . V.head . V.head $ im
          y =  V.length . V.head          $ im
          z =  V.length                   $ im

putS :: [ByteString] -> Put
putS xs = do
        putWord16be (i2w (length xs))
        F.forM_ xs $ \x -> do
            putWord16be (fromIntegral (BSL.length x))
            bmapM_ putWord8 x

loop :: Monad m => Int -> Int -> (Int -> m a) -> m ()
loop (I# start) (I# stop) = \fun ->
        let go !x = case x ># stop of
                        1# -> return ()
                        _  -> let !al_i = I# x in fun al_i >> go (x +# 1#)
        in go start
            
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
zlib = bmapM_ putWord8 . ZL.compress . runPut

mapNode :: MapNode -> ByteString
mapNode (force -> !im) = runPut $ do
                bmapM_ putWord8 "MTSM"
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

toMap :: Image PixelRGB8 -> Image Pixel8 -> CityMap
toMap pVec hVec = force $ runST $ do
            let (h,w) = (imageHeight pVec, imageWidth pVec)
            !v <- MV.new h
            F.forM_ [0..h-1] $ \i -> MV.write v i =<< MV.replicate w (Node air 0)
            foldPic (\x y p -> go v x y (pixelAt hVec x y) p) pVec
            CityMap <$> (V.mapM V.unsafeFreeze <=< V.unsafeFreeze) v
    where writ :: MV.STVector s (MV.STVector s Node) -> (Int, Int) -> Node -> ST s ()
          writ !outer (!x,!y) !n = do
              inner <- MV.read outer x
              MV.write inner y n
             
          go :: MV.STVector s (MV.STVector s Node) -> Int -> Int -> Word8 -> PixelRGB8 -> ST s ()
          go !vc  !x !y !height (PixelRGB8 r g b) = do
                let nod | b >= 200 && g <= 150 && r <= 150  = water
                        | g >  150 && b <= 150 && r <= 150  = grass
                        | otherwise = stone
                    ibrig = 1 + ceiling (fromIntegral height)
                writ vc (x,y) (Node nod ibrig)
                
ct :: FilePath -> IO ()
ct basePath = do
        !points <- loadPic imageRGB8 (basePath <> "_points.png")
        !height <- loadPic imageY8   (basePath <> "_height.png")
        let !m = mt (toMap points height)
        force m `seq` putStrLn ""
        writeSch "/home/exio4/.minetest/worlds/wow/schems/test_01.mts" m


main :: IO ()
main = ct "gm"