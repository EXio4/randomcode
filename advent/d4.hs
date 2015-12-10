{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Crypto.Classes
import Data.Digest.Pure.MD5
import qualified Data.ByteString.Lazy.Char8 as BSL
import Data.ByteString.Lazy (ByteString)
import Data.Monoid
import Control.Applicative
import System.Environment

h_ :: ByteString -> Int
h_ = length . takeWhile (=='0') . show . md5

nice :: Int -> ByteString -> Bool
nice s = (>= s) . h_

nice' :: Int -> ByteString -> Int -> Bool
nice' s xs n = nice s (xs <> BSL.pack (show n))

m :: Int -> ByteString -> Int
m s secret = head [ n | n <- [1..], nice' s secret n ]


main = do [d, x] <- getArgs
          print (m (read d) (BSL.pack x))
