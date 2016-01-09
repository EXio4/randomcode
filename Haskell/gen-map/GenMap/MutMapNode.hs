{-# LANGUAGE BangPatterns, ViewPatterns, MagicHash, OverloadedStrings #-}
module GenMap.MutMapNode
    (newMutMapNode
    ,frz
    ,writeNode'
    ,writeNode
    ,getNodeID
    ) where


import           Control.DeepSeq

import           Control.Monad
import           Data.List
import           Data.Function
import           Data.Word
import           Control.Monad.ST
import           Data.STRef

import           Data.Vector (Vector)
import qualified Data.Vector as V

import qualified Data.Vector.Unboxed as UV
import qualified Data.Vector.Unboxed.Mutable as MUV

import qualified Data.Vector.Mutable as MV

import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as M

import           GenMap.Types
import           GenMap.Utils


dims'vc :: Vector (Vector (UV.Vector Word16)) -> Dims
dims'vc im = Dims x y z
    where x = UV.length . V.head . V.head $ im
          y =  V.length . V.head          $ im
          z =  V.length                   $ im

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