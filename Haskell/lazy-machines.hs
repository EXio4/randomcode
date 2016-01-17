{-# LANGUAGE GeneralizedNewtypeDeriving, TupleSections #-}

module Main (main) where

import           Data.Function
import           Data.List
import           Data.Monoid
import           Control.Applicative
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import           Control.Monad.State.Strict

type Rate = Integer

data MachineID = MachineID !Integer
     deriving (Show,Eq,Ord)

data ParType = Oxygen
             | Hydrogen
             | Water
    deriving (Show,Eq,Ord)

data Particle = Particle !ParType !Integer -- n particles of type partype
    deriving (Show)
{- particles aren't repeated, so if we match one, we have matched enough -}
data ContainerDT = ContainerDT !Integer [Particle] {- cached length for insanity purposes -}
    deriving (Show)
normalizeContainer :: [Particle] -> [Particle]
normalizeContainer = combine where
        part (Particle p _) = p
        combine = map f . groupBy ((==) `on` part) . sortBy (compare `on` part)
        f xs = Particle (part (head xs)) (sum (map (\(Particle _ n) -> n) xs))

instance Monoid ContainerDT where
    mempty = container []
    ContainerDT n1 p1 `mappend` ContainerDT n2 p2 = ContainerDT (n1 + n2) (normalizeContainer (p1 ++ p2)) where


data Machine = Producer !Rate !Particle !MachineID
             | Combine  [(Particle, MachineID)] !Particle !MachineID
             | Container [MachineID] !ContainerDT
    deriving (Show)
{- packs a machine specification next to "the last time frame when it was used/accessed/modified/whatever" -}

data WorldItem = WorldItem !TimeFrame !Machine
    deriving (Show)

newtype TimeFrame = TimeFrame Integer
       deriving (Show,Eq,Ord,Num)

data World = World !TimeFrame !(Map MachineID WorldItem)
    deriving (Show)

emptyContainer :: [MachineID] -> Machine
emptyContainer m_ids = Container m_ids $ ContainerDT 0 []

container :: [Particle] -> ContainerDT
container xs = ContainerDT l xs
    where l = sum (map (\(Particle _ n) -> n) xs)

zeroTime :: TimeFrame
zeroTime = 0

getCurrTime :: State World TimeFrame
getCurrTime = fmap (\(World tf _) -> tf) get

look :: MachineID -> State World WorldItem
look m_id = do
       World _ m <- get
       case M.lookup m_id m of
            Nothing -> fail $ "broken invariant -- M_ID (" ++ show m_id ++ ") should exist in the graph"
            Just i  -> return i

mody :: MachineID -> (WorldItem -> WorldItem) -> State World ()
mody m_id f = do
        World tf m <- get
        put (World tf (M.adjust f m_id m))

addToContainer :: MachineID -> [Particle] -> State World ()
addToContainer m parts = mody m $ \(WorldItem n (Container z w)) -> WorldItem n (Container z (w <> container parts))

updateTimeFrame :: MachineID -> TimeFrame -> State World ()
updateTimeFrame m tf = mody m $ \(WorldItem _ z) -> WorldItem tf z

world1 :: World
world1 = World zeroTime . M.fromList . map (\(x,y) -> (x, WorldItem zeroTime y)) $
    [ (core_01  , Producer 10 (Particle Hydrogen 2) main_cnt) {- produces two units of Hydrogen every 10 units of time -}
    , (core_02  , Producer 5  (Particle Oxygen   1) main_cnt) {- produces one unit  of Oxygen   every  5 units of time -}
    , (main_cnt , emptyContainer [core_01, core_02])
    , (water_gn , Combine   [(Particle Hydrogen 2, main_cnt)
                            ,(Particle Oxygen   1, main_cnt)] (Particle Water 1) river_cnt)
    , (river_cnt , emptyContainer [water_gn])
    ]
  where ( core_01 : core_02 : main_cnt : water_gn : river_cnt : _ ) = map MachineID [1..]

produceF :: TimeFrame -> TimeFrame -> Integer -> Integer
produceF (TimeFrame old_time) (TimeFrame new_time) rate = new_time `div` rate - old_time `div` rate

affordFromC :: MachineID -> Particle -> State World Integer
affordFromC container_id (Particle wanted_type num_items) = do
        (WorldItem _ (Container _ (ContainerDT _ conts))) <- look container_id
        return $ foldr f 0 conts
    where f (Particle typ stock) r | typ == wanted_type
                                   = stock `div` num_items
                                   | otherwise
                                   = r

subFromC :: MachineID -> Particle -> Integer -> State World ()
subFromC container_id (Particle wanted_type num_items) num = do
        mody container_id $ \(WorldItem x (Container ids (ContainerDT _ qs))) -> WorldItem x (Container ids (container (foldr f [] qs)))
    where f q@(Particle t s) r
                | t == wanted_type
                , let z = s - num_items * num
                = if z == 0
                  then r
                  else Particle t z : r
                | otherwise = q : r


{- assumes pointers/machineids aren't point toward invalid nodes -}
{- no cyclic references are assumed even though it should halt -}

simulate :: Integer -> State World ()
simulate diff = modify $ \(World tf graph) -> World (tf + fromInteger diff) graph

update :: MachineID -> State World ()
update curr_machine_id = do
        current_time <- getCurrTime
        (WorldItem old curr_machine) <- look curr_machine_id        
        if old == current_time
        then return () -- the machine is already updated to the current time frame
        else do updateTimeFrame curr_machine_id current_time -- we update the time eagerly in case a recursive update call leads to the same node
                case curr_machine of
                    Producer rate (Particle part_type atoms) machine_id -> do
                        let parts_produced = produceF old current_time rate
                        when (parts_produced > 0) $ do
                            addToContainer machine_id [Particle part_type (atoms * parts_produced)]
                    Container ids c_dt -> do
                        forM_ ids $ \xid -> do
                            update xid
                    Combine input (Particle endType endNum) end_id -> do 
                        forM input $ \(_,i) -> update i
                        is <- minimum <$> (forM input $ \(p,i) -> affordFromC i p)
                        if is <= 0
                        then return () -- we can't afford anything
                        else do
                            forM_ input $ \(p,i) -> do
                                    subFromC i p is
                            addToContainer end_id [Particle endType (endNum * is)]

main = putStrLn "Hello world"
