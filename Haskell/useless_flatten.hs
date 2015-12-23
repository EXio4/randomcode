{-# LANGUAGE TypeFamilies, MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances, UndecidableInstances, OverlappingInstances #-}

module Main (main) where

import Control.Monad

type family Flt m where
    Flt (m (m a)) = Flt (m a)
    Flt (m a)     = m a

class Flatten f a where
    flatten :: f a -> Flt (f a)

instance (Monad f, Flatten f a) => Flatten f (f a) where
    flatten = flatten . join

instance (Flt (f a) ~ f a) => Flatten f a where
   flatten = id

-- type inference works
--go :: Maybe String
go = flatten (Just (Just (Just "abc")))

main = print go
