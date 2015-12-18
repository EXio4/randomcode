{-# LANGUAGE TypeOperators, TupleSections, DeriveGeneric #-}

module TagDB (
          Tag
        , File
        , TDB, FDB
        , Implication(..)

        , file
        , eFile
        , tag
        , eTag

        , addImplication
        , removeImplication

        , insert
        , rm
        , ls

        , listAll
) where

import           Data.Binary
import           GHC.Generics (Generic)


import qualified Data.Map.Strict  as M
import           Data.Map.Strict  (Map)
import qualified Data.Set         as S
import           Data.Set         (Set)
import qualified Data.Text        as T
import           Data.Text        (Text)
import           Data.Monoid

newtype Tag  = Tag  { eTag  :: Text } deriving (Show,Eq,Ord,Generic)
newtype File = File { eFile :: Text } deriving (Show,Eq,Ord,Generic)

tag :: Text -> Tag
tag = Tag . T.toLower
file :: Text -> File
file = File

data FileInfo = FileInfo !Tag deriving (Show,Eq,Ord)

data TagInfo = TagInfo { 
                  impliedBy :: !(Set Tag)
                 ,implies   :: !(Set Tag)
             } deriving (Show,Eq,Ord,Generic)

instance Monoid TagInfo where
    mempty = TagInfo S.empty S.empty
    (TagInfo x0 y0) `mappend` (TagInfo x1 y1) = TagInfo (x0 `S.union` x1) (y0 `S.union` y1)

instance Monoid TDB where
    mempty = TDB M.empty
    (TDB x) `mappend` (TDB y) = TDB (M.unionWith (<>) x y)

instance Monoid FDB where
    mempty = FDB M.empty M.empty
    (FDB x0 y0) `mappend` (FDB x1 y1) = FDB (M.unionWith S.union x0 x1) (M.unionWith S.union y0 y1)

data TDB = TDB !(Map Tag TagInfo)    deriving (Generic)
data FDB = FDB !(Map Tag  (Set File))
               !(Map File (Set Tag ))
               deriving (Generic)

instance Binary TDB
instance Binary FDB
instance Binary TagInfo
instance Binary Tag
instance Binary File

data Implication = Tag :-> Tag

{- I should look at this and stop confusing the xs and ys) -}
addImplication :: Implication -> TDB -> TDB
addImplication (x :-> y) (TDB m) = TDB (f m)
    where f = M.insertWith (<>) y (TagInfo S.empty (S.singleton x)) .
              M.insertWith (<>) x (TagInfo (S.singleton y) S.empty)


removeImplication :: Implication -> TDB -> TDB
removeImplication _ _ = undefined

insert :: Tag -> File -> FDB -> FDB
insert tag file (FDB t_f f_t) = FDB (M.insertWith S.union tag  (S.singleton file) t_f)
                                    (M.insertWith S.union file (S.singleton tag ) f_t)

rm :: File -> FDB -> FDB
rm file db = undefined

ls :: Tag -> Bool -> (FDB, TDB) -> [(FileInfo, File)]
ls tag recur (FDB fdb _, TDB tdb) = go tag
    where go t = map (FileInfo t,) (S.toList (maybe S.empty id (M.lookup t fdb))) ++ re t
          re t | recur, Just s <- M.lookup t tdb = S.toList (implies s) >>= go
               | otherwise = []

listAll :: FDB -> [(Tag, Set File)]
listAll (FDB x _) = M.toList x




