{-# LANGUAGE OverloadedStrings #-}
module Main where


import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C8
import qualified Data.ByteString.Lazy as BSL

import           Data.Binary
import           Data.Binary.Get
import           Data.Binary.Put
import           Data.Int

import           Control.Applicative
import           Control.Monad

import           System.Environment
import           Network.Simple.TCP

{- helper, written shittly for demo purposes -}
getZeroTerminatedString :: Get BS.ByteString
getZeroTerminatedString = do
        x <- getWord8
        if x == 0
        then return ""
        else BS.cons x <$> getZeroTerminatedString

putZeroTermString :: BS.ByteString -> Put
putZeroTermString xs = do
    putByteString xs
    put (0 :: Word8)

data DuktoHeader = DuktoHeader {
        dukto_header_elements_to_receive :: !Int64
       ,dukto_header_total_size          :: !Int64
} deriving (Show,Eq)

instance Binary DuktoHeader where
    get = DuktoHeader <$> getInt64host
                      <*> getInt64host
    put (DuktoHeader i1 i2) = putInt64host i1 >> putInt64host i2

data DuktoFileHeader = DuktoFileHeader {
       dukto_fileheader_filename :: !BS.ByteString {- raw data, duktoprotocol implies it's in UTF8
                                                       .. even though that seems quite fucked -}
      ,dukto_fileheader_size     :: !Int64
} deriving (Show,Eq)


instance Binary DuktoFileHeader where
    get = DuktoFileHeader <$> getZeroTerminatedString
                          <*> getInt64host
    put (DuktoFileHeader str i64) = putZeroTermString str >> putInt64host i64

data DuktoFile = DuktoFile { duktofile_header  :: !DuktoFileHeader
                           , duktofile_content :: !BS.ByteString
                           } deriving (Show,Eq)

instance Binary DuktoFile where
    put (DuktoFile h d) = put h >> putByteString d
    get = do h <- get
             f <- getByteString (fromIntegral (dukto_fileheader_size h))
             return (DuktoFile h f)

data FilePackage = FilePackage { filepackage_header :: !DuktoHeader
                               , filepackage_files  :: [DuktoFile]
                               } deriving (Show,Eq)

instance Binary FilePackage where
    put (FilePackage header files) = put header >> mapM_ put files
    get = do h <- get
             f <- replicateM (fromIntegral (dukto_header_elements_to_receive h)) get
             return (FilePackage h f)

{- package creation stuff -}

data DK = DFile !BS.ByteString !BS.ByteString
        | DText !BS.ByteString


example :: FilePackage
example = create [DFile "lol.txt" "12345abcdef TEST ABCDEF"
                 ,DText "moar text here"
                 ]

create :: [DK] -> FilePackage
create xs = FilePackage header files
    where header = DuktoHeader (fromIntegral (length xs))
                               (sum (map (dukto_fileheader_size . duktofile_header) files))
          files = map files_h xs
          files_h (DFile filename content) = DuktoFile (DuktoFileHeader filename (fromIntegral (BS.length content))) content
          files_h (DText content) = files_h (DFile "___DUKTO___TEXT___" content)




main :: IO ()
main = do
    x <- getArgs
    case x of
         (host:port:[])    -> go_f host port example
         (host:port:files) -> go_f host port =<< fromFiles files
         _ -> putStrLn "usage: program <host> <port> <files>" >>
              putStrLn "[if files is empty, example files are sent"

go_f host port filepackage = connect host port $ \(sock, _) -> do
                                    send sock (BSL.toStrict (encode filepackage))

fromFiles xs = create <$> forM xs (\file -> DFile (C8.pack file) <$> C8.readFile file)

