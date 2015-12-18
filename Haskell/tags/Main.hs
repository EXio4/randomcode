{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module Main (main) where

import qualified Data.Foldable as F
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Text.IO as T.IO
import qualified TagDB as DB
import           TagDB (Implication(..))
import qualified Data.Binary as B
import           Control.Exception
import           Control.Monad
import           Data.Functor
import           Data.Monoid
import           Control.Concurrent.MVar
import           System.Environment

file_db, tags_db :: String
file_db = ".tags_nodes"
tags_db = ".tags_edges"

data Ex = IForm

withDB :: (B.Binary a, Monoid a) => FilePath -> (a -> IO (Maybe a)) -> IO ()
withDB str cb = do
    y <- (      fmap join                                                 .
                fmap (either (\(_ :: SomeException) -> Left IForm) Right) .
          try . fmap (either (\_                    -> Left IForm) Right) .
          B.decodeFileOrFail) str
    w <- cb (case y of
       Left _  -> mempty
       Right m -> m)
    maybe (return ()) (B.encodeFile str) w
    return ()

imply :: T.Text -> T.Text -> IO ()
imply t1 t2 = withDB tags_db $
                        return . Just . DB.addImplication (DB.tag t1 :-> DB.tag t2)

tagFile :: T.Text -> T.Text -> IO ()
tagFile file tag = withDB file_db $
                        return . Just . DB.insert (DB.tag tag) (DB.file file)

ls :: T.Text -> IO ()
ls tag = withDB file_db (\f_db ->
         Nothing <$ withDB tags_db (\t_db -> do
            Nothing <$ case DB.ls (DB.tag tag) True (f_db, t_db) of
                [] -> putStrLn "No files found (matching the tag)"
                s  -> F.forM_ s $ \(finf,w) -> do
                         T.IO.putStrLn ("\t*\t" <> DB.eFile w)))

listAll :: IO ()
listAll = withDB file_db $ \db -> do
     T.IO.putStrLn "Tags: "
     forM_ (DB.listAll db) $ \(t, s) -> do
        T.IO.putStrLn ("\t*\t" <> DB.eTag t <> "\t(" <> T.pack (show (S.size s)) <> ")")
     return Nothing

help = mapM_ putStrLn
          ["add FILE TAG"
          ,"\tAdd TAG to FILE"
          ,"imply TAG1 TAG2"
          ,"\tAdd implication from TAG1 to TAG2"
          ,"ls TAG"
          ,"\tList all files that match TAG"
          ,"ls"
          ,"\tList all tags"
          ]
main :: IO ()
main = do
   x <- getArgs
   case fmap T.pack x of
     ["add", file, tag] -> tagFile file tag
     ["ls", tag]        -> ls tag
     ["imply", t1, t2]  -> imply t1 t2
     ["ls"]             -> listAll
     _ -> help
   return ()
