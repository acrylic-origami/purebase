{-# LANGUAGE NoImplicitPrelude #-}

module C.Data.Semigroup.Internal where

import  GHC.Real (Integral)
import {-# SOURCE #-} C.GHC.Base (Semigroup,Monoid,Maybe)
import GHC.Integer () -- See Note [Depend on GHC.Integer] in GHC.Base

stimesIdempotentMonoid :: (Integral b, Monoid a) => b -> a -> a

stimesDefault :: (Integral b, Semigroup a) => b -> a -> a
stimesMaybe :: (Integral b, Semigroup a) => b -> Maybe a -> Maybe a
stimesList :: Integral b => b -> [a] -> [a]
