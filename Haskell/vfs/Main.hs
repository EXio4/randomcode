{-# LANGUAGE GADTs, TypeFamilies, DataKinds, ConstraintKinds, PolyKinds, RankNTypes, BangPatterns, OverloadedStrings #-}
module Main (main) where

import           GHC.Exts (Constraint)
import qualified Data.ByteString as BS
import           Data.ByteString (ByteString)
import           Control.Monad.Free
import           Control.Applicative -- still using GHC7.8 on desktop
{- for sample IO impl -}
import           System.IO
import           System.Directory
{- for sample VFS impl -}
import qualified Data.IntMap.Strict as IM
import           Data.IntMap.Strict (IntMap)
import qualified Data.Map.Strict    as M
import           Data.Map.Strict    (Map)
import           Control.Monad.Except
import           Control.Monad.ST
import           Control.Monad.State.Strict
import           Data.STRef

data FS_Perm = RW
             | R

type family F_Read (k :: FS_Perm) :: Constraint where
    F_Read RW = ()
    F_Read R  = ()

type family F_Write (k :: FS_Perm) :: Constraint where
    F_Write RW = ()
    F_Write R  = "Write operation used in read-only context" ~ ""

class FileManagement (i :: k) where
        data family FileHandle i

data FS_Action (i :: k) (t :: FS_Perm) a where
    FS_ListDir ::              FilePath     -> ([FilePath] -> a)         -> FS_Action i p a
    FS_IsFile  ::              FilePath     -> (Bool -> a)               -> FS_Action i p a
    FS_GetFile ::              FilePath     -> (FileHandle i -> a)       -> FS_Action i p a
    FS_Read    :: F_Read  p => FileHandle i -> Int -> (ByteString -> a)  -> FS_Action i p a
    FS_Write   :: F_Write p => FileHandle i -> ByteString -> a           -> FS_Action i p a

instance Functor (FS_Action i p) where
    fmap f (FS_ListDir p g  ) = FS_ListDir p (fmap f g)
    fmap f (FS_IsFile  p g  ) = FS_IsFile  p (fmap f g)
    fmap f (FS_GetFile p g  ) = FS_GetFile p (fmap f g)
    fmap f (FS_Read    h n g) = FS_Read    h n (fmap f g)
    fmap f (FS_Write   h b x) = FS_Write   h b (f x)

type FS (i :: k) (t :: FS_Perm) a = Free (FS_Action i t) a

ls :: FilePath -> FS i p [FilePath]
ls dir = liftF (FS_ListDir dir id)

isFile :: FilePath -> FS i p Bool
isFile dir = liftF (FS_IsFile dir id)

isDirectory :: FilePath -> FS i p Bool
isDirectory = fmap not . isFile

getFileH :: FilePath -> FS i p (FileHandle i)
getFileH fl = liftF (FS_GetFile fl id)

fs_write :: F_Write p => FileHandle i -> ByteString -> FS i p ()
fs_write h bs = liftF (FS_Write h bs ())

fs_read :: F_Read p => FileHandle i -> Int -> FS i p ByteString
fs_read h l = liftF (FS_Read h l id)

---------------------------------------------------------------------

instance FileManagement IO where
    data FileHandle IO = IOHandle !Handle

run_io :: FS IO RW a -> IO a
run_io = go where
    go (Pure x) = return x
    go (Free x) = case x of
                    FS_ListDir dir r  -> go . r =<< map ((dir ++) . ("/" ++)) . filter (`notElem` [".", ".."]) <$> getDirectoryContents dir -- nice? hack just for demo
                    FS_IsFile  fl r   -> go . r =<< doesFileExist fl
                    FS_GetFile file r -> go . r . IOHandle =<< openFile file ReadWriteMode
                    FS_Read (IOHandle h) n r  -> go . r =<< BS.hGet h n
                    FS_Write (IOHandle h) b r -> BS.hPut h b >> go r


---------------------------------------------------------------------

data FSNode = File !ByteString
            | Dir !(Map String Int)

data FileSystemSpec = FSSpec !(IntMap FSNode)

data VFS (s :: *)

instance FileManagement (VFS s) where
    data FileHandle (VFS s) = VFSHandle !(STRef s Int) !Int

data VFS_Error = DirFileNotFound !FilePath !String
               | NotADirectory !FilePath !(Maybe String)
               | NotAFile      !FilePath
               | NotAFile_ID   !Int
               | MissingNode   !Int
    deriving (Show)
rootNode :: Int
rootNode = 0

split'c :: Char -> String -> [String]
split'c c = go [] where
    go [] [] = []
    go []  (x : xs) | x == c = go [] xs -- 'a//b' = 'a/b'
    go acc [] = [reverse acc]
    go acc (x : xs) | x == c = reverse acc : go [] xs
                    | otherwise = go (x : acc) xs

getNodeIDFromPath :: Monad m => FileSystemSpec -> FilePath -> ExceptT VFS_Error m Int
getNodeIDFromPath (FSSpec im) dir = go rootNode (split'c '/' dir) where
    go curr []       = return curr
    go curr (x : xs) =
         case IM.lookup curr im of
              Nothing -> throwError $ DirFileNotFound dir x
              Just (File{}) -> throwError $ NotADirectory dir (Just x)
              Just (Dir dr) -> case M.lookup x dr of
                                    Nothing -> throwError (DirFileNotFound dir x)
                                    Just n  -> go n xs

getNode :: Monad m => FileSystemSpec -> Int -> ExceptT VFS_Error m FSNode
getNode (FSSpec im) n = case IM.lookup n im of
                             Nothing -> throwError $ MissingNode n
                             Just  v -> return v


dirCnts :: (Functor m, Monad m) => FileSystemSpec -> FilePath -> ExceptT VFS_Error m [FilePath]
dirCnts spc dir = map ((dir ++) . ("/" ++)) <$> do
        x <- getNode spc =<< getNodeIDFromPath spc dir
        case x of
             File{} -> throwError $ NotADirectory dir Nothing
             Dir xs -> return (M.keys xs)

getFile :: (Functor m, Monad m) => FileSystemSpec -> Int -> ExceptT VFS_Error m ByteString
getFile spc fl = do
    x <- getNode spc fl
    case x of
         File x -> return x
         Dir{}  -> throwError $ NotAFile_ID fl

vfs_isFile :: (Functor m, Monad m) => FileSystemSpec -> FilePath -> ExceptT VFS_Error m Bool
vfs_isFile spc x = fmap f . getNode spc =<< getNodeIDFromPath spc x
    where f File{} = True
          f Dir{}  = False


run_vfs :: FileSystemSpec -> (forall s. FS (VFS s) R a) -> Either VFS_Error a
run_vfs spc e = runST (runExceptT (go e))
    where
        go :: FS (VFS s) R a -> ExceptT VFS_Error (ST s) a
        go (Pure a) = return a
        go (Free x) = case x of
                           FS_ListDir dir r -> go . r =<< dirCnts spc dir
                           FS_IsFile  dir r -> go . r =<< vfs_isFile spc dir
                           FS_GetFile dir r -> do
                               ref <- lift (newSTRef 0)
                               go . r . VFSHandle ref =<< getNodeIDFromPath spc dir
                           FS_Read (VFSHandle ref node) n r -> do
                               fl <- getFile spc node
                               start <- lift (readSTRef ref)
                               lift (writeSTRef ref (start+n+1))
                               let !v = BS.take n (BS.drop start fl)
                               go (r v)

example_fs :: FileSystemSpec
example_fs = FSSpec . IM.fromList $
    [(0, Dir (M.fromList [("boot", 1) , ("init", 2)]))
    ,(1, Dir (M.fromList [("kernel",3), ("drivers", 4)]))
    ,(2, File "init-system")
    ,(3, File "kernel")
    ,(4, Dir (M.fromList [("vga", 5)  , ("keyboard", 6)]))
    ,(5, Dir (M.fromList [("magic", 7)]))
    ,(6, Dir (M.fromList [("magic", 8)]))
    ,(7, File "driver-vga")
    ,(8, File "driver-keyboard")]

tra :: FilePath -> FS i p String
tra = fmap unlines . go where
    go path = do
        b <- isDirectory path
        if b
        then do xs <- ls path
                r <- mapM go xs
                return $ path : concat r
        else do return $ [path]

example_write :: F_Write p => FS i p ()
example_write = do
    x <- getFileH "/test"
    fs_write x "dog"

{-
*Main> run_vfs example_fs example_write 

<interactive>:6:20:
    Couldn't match type ‘"Write operation used in read-only context"’
                  with ‘""’
    In the second argument of ‘run_vfs’, namely ‘example_write’

-}

main :: IO ()
main = return ()