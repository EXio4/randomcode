{-# LANGUAGE OverloadedStrings #-}
module GenMap.ColorNodeMP
        (parseMP
        ,loadMP) where

import           Control.Applicative ((<$>), (<*>), (*>), (<*))

import           Codec.Picture.Types

import qualified Data.Traversable as T

import qualified Data.ByteString.Lazy  as BSL
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString       as BS

import qualified Data.Attoparsec.ByteString.Char8 as AP

import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as M

import           GenMap.Types

loadMP :: FilePath -> IO MP
loadMP = fmap parseMP . BS8.readFile

parseMP :: BS8.ByteString -> MP
parseMP d = 
    case  T.traverse (AP.parseOnly entry) (BS8.lines d) of
         Left l  -> error ("Error Parsing " ++ l)
         Right v -> MP (M.fromList v)
    where entry = (,) <$> (PixelRGB8 <$> ("<" *> (floor <$> AP.scientific))
                                     <*> ("," *> (floor <$> AP.scientific))
                                     <*> ("," *> (floor <$> AP.scientific))
                                    <*  ">")
                      <*>  ((flip MPEntry [] . BNode) <$> (AP.space *> AP.takeTill (=='\n')))
