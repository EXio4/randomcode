{-# LANGUAGE TypeFamilies, DataKinds, ScopedTypeVariables, UndecidableInstances, BangPatterns #-}
module Main (main) where

import GHC.TypeLits
import Data.Monoid
import Data.Proxy
import Criterion.Main
import Test.QuickCheck

main = do
    putStrLn "QuickCheck"
    quickCheck (\n -> (n > 1) ==> q_test n)
    putStrLn "Criterion"
    defaultMain
        [bgroup "big_int"
                (c_test p1 nr)
        ,bgroup "naive_list"
                (c_test p2 nr)
        ]

c_test :: (Stack s, e ~ StackElem s, Eq e, Monoid e) => Proxy s -> (Integer -> e) -> [Benchmark]
c_test p lift = map (\n -> bench (show n) (whnf (test p lift) n)) [1000, 10000, 20000, 25000, 50000]

q_test :: Integer -> Bool
q_test n = test p1 nr n == test p2 nr n

class Stack f where
    type StackElem f :: *
    empty :: f
    push :: StackElem f -> f -> f
    pop  :: f -> Maybe (StackElem f, f)

instance Stack [a] where
    type StackElem [a] = a
    empty = []
    push = (:)
    pop [] = Nothing
    pop (x:xs) = Just (x, xs)


newtype Sk (n :: Nat) = Sk Integer
    deriving (Show,Eq)
newtype Nr (n :: Nat) = Nr Integer
    deriving (Show,Eq)

nr :: forall v. KnownNat v => Integer -> Nr v
nr n = Nr (n `mod` natVal (Proxy :: Proxy v))

instance KnownNat v => Stack (Sk v) where
    type StackElem (Sk v) = Nr v
    empty = Sk 0
    push (Nr n) (Sk m) = Sk (n + m * natVal (Proxy :: Proxy v))
    pop  (Sk 0) = Nothing -- ??
    pop  (Sk m) = Just (Nr x , Sk xs) where
        (xs, x) = m `divMod` natVal (Proxy :: Proxy v)

p1 :: Proxy (Sk 1000)
p2 :: Proxy [Nr 1000]
(p1, p2) = (Proxy, Proxy)

instance KnownNat n => Monoid (Nr n) where
    Nr x `mappend` Nr y = Nr ((x + y) `mod` natVal (Proxy :: Proxy n))
    mempty = Nr 0

test :: forall s e. (Stack s, e ~ StackElem s, Eq e, Monoid e) => Proxy s -> (Integer -> e) -> Integer -> e
test _ lift = go empty where
    go :: s -> Integer -> e
    go !s 0 = recur mempty s
    go !s n = go (push (lift n) s) (n - 1)
    recur :: e -> s -> e
    recur !acc !s = case pop s of
                    Nothing -> acc
                    Just (x, xs) -> recur (x <> acc) xs
