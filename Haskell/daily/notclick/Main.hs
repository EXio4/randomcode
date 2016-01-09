{-# LANGUAGE OverloadedStrings, ViewPatterns, PatternSynonyms, BangPatterns #-}

module Main (main) where

import qualified Data.Foldable as F

import           Data.Fixed

fI :: (Integral a, Num b) => a -> b
fI = fromIntegral

type Number = Fixed E3

data BuildingName = Cursor
                  | Grandma
                  | Farm
                  | Mine
                  | Factory
                  | Bank
                  | Temple
                  | City
    deriving (Show,Eq,Ord,Enum)

data BuildingTmpl = MkBuildingTmpl {
         building_name                      :: !BuildingName
        ,building_initial_cost              :: !Number
        ,building_resources_generated       :: !Number
        ,building_extra_cookies_1th_upgrade :: !Number
        ,building_cost_first_upgrade        :: !Number
} deriving (Show,Eq,Ord)


data SList a = SCons !Integer !a !(SList a) | SNil
    deriving (Show,Eq,Ord)
instance F.Foldable SList where
    foldl = F.foldl'
    foldl' f z = go z where
        go !acc (SCons _ x xs) = go (f acc x) xs
        go !acc  SNil          = acc
    foldr f z = go where
        go (SCons _ x xs) = f x (go xs)
        go  SNil          = z

instance Functor SList where
    fmap f = go where
        go (SCons n x xs) = SCons n (f x) (go xs)
        go  SNil          = SNil

toSList :: F.Foldable f => f a -> SList a
toSList = F.foldr cons nil

slength :: SList a -> Integer
slength (SCons n _ _) = n
slength  SNil         = 0

cons :: a -> SList a -> SList a
cons x xs = SCons (slength xs + 1) x xs

nil :: SList a
nil = SNil

matchCons :: SList a -> Maybe (a, SList a)
matchCons (SCons _ x xs) = Just (x,xs)
matchCons  SNil          = Nothing

pattern x :< xs <- (matchCons -> Just (x,xs))
pattern NIL     <- (matchNil  -> Just ()    )

matchNil  :: SList a -> Maybe ()
matchNil  SNil     = Just ()
matchNil (SCons{}) = Nothing


data BuildingCurr = MkBuildingCurr {
        b_curr_tmpl  :: !BuildingTmpl
       ,b_buildings  :: !Integer
       ,b_level      :: !Integer
} deriving (Show,Eq,Ord)

mod's :: SList BuildingCurr -> BuildingName -> (BuildingCurr -> BuildingCurr) -> SList BuildingCurr
mod's list name cb = fmap f list where
        f (q@MkBuildingCurr{b_curr_tmpl= MkBuildingTmpl { building_name = name_l }}) | name_l == name = cb q
        f x = x
look's :: BuildingName -> SList BuildingCurr-> BuildingCurr
look's name = F.foldr go (error "Building not found")
    where go (q@MkBuildingCurr{b_curr_tmpl = MkBuildingTmpl { building_name = name_l }}) _ | name_l == name = q
          go _ r = r

upgrade_cost :: BuildingCurr -> Number
upgrade_cost (MkBuildingCurr{b_curr_tmpl=MkBuildingTmpl{building_cost_first_upgrade=start_cost},b_level=lvl})
    = 3^lvl * start_cost

upgrade :: BuildingCurr -> BuildingCurr
upgrade q@(MkBuildingCurr{b_level=lvl}) = q { b_level = lvl+1 }

cost_next_building :: BuildingCurr -> Number
cost_next_building (MkBuildingCurr { b_curr_tmpl = MkBuildingTmpl { building_initial_cost = init_cost }
                                   , b_buildings = blds })
    = (1.2)^blds * init_cost

build :: BuildingCurr -> BuildingCurr
build q@(MkBuildingCurr{b_buildings=blds}) = q { b_buildings = blds+1 }

resources_generated :: BuildingCurr -> Number
resources_generated (MkBuildingCurr { b_curr_tmpl = MkBuildingTmpl { building_resources_generated       = res_gen
                                                                   , building_extra_cookies_1th_upgrade = fst_upgrade }
                                    , b_level     = lvl
                                    , b_buildings = blds })
    = fI blds * res where
        res | lvl == 0  = res_gen
            | otherwise = 2^(lvl-1) * (res_gen + fst_upgrade)

setup :: SList BuildingTmpl
setup = toSList $ map (\(a0,a1,a2,a3,a4) -> MkBuildingTmpl a0 a1 a2 a3 a4)
        [(Cursor   , 12       , 0.1  , 0.1  , 100       )
        ,(Grandma  , 100      , 0.8  , 0.3  , 1000      )
        ,(Farm     , 500      , 4    , 1    , 10000     )
        ,(Factory  , 5000     , 40   , 10   , 200000    )
        ,(Bank     , 100000   , 100  , 40   , 5000000   )
        ,(Temple   , 1000000  , 400  , 100  , 100000000 )
        ,(City     , 300000000, 5000 , 2000 , 1000000000)]

data Actions = Build   !BuildingName
             | Upgrade !BuildingName
    deriving (Show)

data GS = MkGS {
         gs_build_queue :: !(SList Actions)
        ,gs_resources   :: !Number
        ,gs_buildings   :: !(SList BuildingCurr) -- basically Map BuildingTmpl BuildInfo
    }

showGS :: GS -> [String]
showGS (MkGS { gs_build_queue    = queue
             , gs_resources      = cash
             , gs_buildings      = blds })
    = ["CASH " ++ show cash
      ,"Remaining elements in queue:"
      ] ++ map (('\t':) . show) (F.foldr (:) [] queue) ++
      ["Buildings:"] ++ map f  (F.foldr (:) [] blds)
    where f (MkBuildingCurr{b_curr_tmpl=MkBuildingTmpl{building_name = name}
                           ,b_level = upgrds
                           ,b_buildings = num_builds})
            = "\t" ++ show name ++ "\t|Level " ++ show upgrds ++ "\t|Builds =" ++ show num_builds

gen :: [Actions] -> GS
gen xs = MkGS (toSList xs) 0 (fmap (\tmpl -> MkBuildingCurr tmpl 0 0) setup)

iter :: GS -> GS
iter = step3 . step2 . step1
    where step1 q@(MkGS {gs_resources = n, gs_buildings = blds}) = q { gs_resources = n + apply blds}
          step2 q@(MkGS {gs_resources = n, gs_buildings = blds}) = q { gs_resources = n + 1 + cursors_bld blds }
          step3 q@(MkGS {gs_resources = n, gs_buildings = blds, gs_build_queue = queue})
                = case queue of
                       (act :< acts) | Just (n', blds') <- adv n act blds
                           -> q { gs_resources = n' , gs_buildings = blds' , gs_build_queue = acts }
                       _   -> q

          adv cash (Build name) bld | let bld' = look's name bld
                                    , let cost = cost_next_building bld'
                                    , cash >= cost
                                    = Just (cash - cost , mod's bld name build)
          adv cash (Upgrade name) bld | let bld' = look's name bld
                                      , let cost = upgrade_cost bld'
                                      , cash >= cost
                                      = Just (cash - cost, mod's bld name upgrade)
          adv _ _ _ = Nothing
          apply = F.foldl' (\acc x -> acc + resources_generated x) 0
          cursors_bld = resources_generated . look's Cursor

solve :: Int -> [Actions] -> [GS]
solve n act = take (n+1) (iterate iter (gen act))

main :: IO ()
main = mapM_ putStrLn . showGS . last $ solve 20 [Build Cursor,  Build Cursor, Build Grandma]