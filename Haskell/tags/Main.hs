{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module Main (main) where

import qualified Data.Foldable as F
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Text.IO as T.IO
import qualified Data.Binary as B
import Control.Exception
import Control.Monad
import Data.Monoid
import Control.Concurrent.MVar
import System.Environment

type Tag = T.Text
type File = T.Text
type DB = M.Map Tag (S.Set File)

database :: String
database = ".tag.fs.v0"

data Ex = IForm

withDB :: (DB -> IO (Maybe DB)) -> IO ()
withDB cb = do
    y <- (      fmap join                                                 .
                fmap (either (\(_ :: SomeException) -> Left IForm) Right) .
          try . fmap (either (\_                    -> Left IForm) Right) .
          B.decodeFileOrFail) database
    w <- cb (case y of
       Left _  -> M.empty
       Right m -> m)
    maybe (return ()) (B.encodeFile database) w
    return ()


addTag :: T.Text -> T.Text -> IO ()
addTag file tag = withDB $ \db -> do
          return (Just (M.insertWith S.union tag (S.singleton (T.toLower file)) db))

ls :: T.Text -> IO ()
ls tag = withDB $ \db -> do
          case M.lookup (T.toLower tag) db of
              Nothing -> putStrLn "No files found (matching the tag)"
              Just s  -> F.forM_ s $ \w -> do
                        T.IO.putStrLn ("\t*\t" <> w)
          return Nothing

help = mapM_ putStrLn
          ["add FILE TAG"
          ,"ls TAG"
          ]
main :: IO ()
main = do
   x <- getArgs
   case fmap T.pack x of
     ["add", file, tag] -> addTag file tag
     ["ls", tag]        -> ls tag
     _ -> help
   return ()
