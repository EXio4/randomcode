{-# LANGUAGE BangPatterns, OverloadedStrings #-}
module GenMap.Types where

import           Control.DeepSeq
import           Codec.Picture.Types

import qualified Data.ByteString.Lazy  as BSL
import qualified Data.ByteString.Char8 as BS8

import           Data.Word
import           Control.Monad.ST
import           Data.STRef

import           Data.Vector (Vector)
import qualified Data.Vector as V

import qualified Data.Vector.Unboxed as UV
import qualified Data.Vector.Unboxed.Mutable as MUV

import qualified Data.Vector.Mutable as MV

import           Data.Map.Strict (Map)

data Node = Node !BNode {-# UNPACK #-} !Int
    deriving (Show,Eq)

data Dims = Dims {-# UNPACK #-} !Int {-# UNPACK #-} !Int {-# UNPACK #-} !Int
    deriving (Show,Eq,Ord)

data CityMap = CityMap {-# UNPACK #-} !(V.Vector (V.Vector Node))

newtype BNode = BNode BS8.ByteString
    deriving (Eq,Ord,Show)
newtype WNode = WNode Word16
    deriving (Eq,Ord,Show)


data MapNode = MapNode {
     nodeMaps :: ![BS8.ByteString]
    ,dims     :: {-# UNPACK #-} !Dims
    ,vec      :: {-# UNPACK #-} !(V.Vector (V.Vector (UV.Vector Word16)))
} deriving (Show,Eq,Ord)

data Mapping = Mapping {-# UNPACK #-} !Word16 !(Map BNode Word16)

data MutMapNode s = MutMapNode {
      mut_nodeMaps :: {-# UNPACK #-} !(STRef s Mapping)
     ,mut_Vec      :: {-# UNPACK #-} !(MV.STVector s (MV.STVector s (MUV.STVector s Word16)))
}

data MP = MP !(Map PixelRGB8 MPEntry)
    deriving (Show)
data MPEntry = MPEntry !BNode ![Mod]
    deriving (Show)
data Mod = TopBlock !BNode
         | BottomBlock !BNode
    deriving (Show)


instance NFData CityMap where
    rnf (CityMap v) = rnf v `seq` ()

instance NFData Node where
    rnf (Node{}) = ()

instance NFData BNode where
    rnf !_ = ()
instance NFData WNode where
    rnf !_ = ()

instance NFData MapNode where
    rnf (MapNode x y z) = rnf x `seq` rnf z `seq` ()




[air, water, dirt, grass, stone, sand] =
    map BNode [ "air"
              , "default:water_source"
              , "default:dirt"
              , "default:dirt_with_grass"
              , "default:stone"
              , "default:sand"]