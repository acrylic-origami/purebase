{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Auxilary definitions for 'Semigroup'
--
-- This module provides some @newtype@ wrappers and helpers which are
-- reexported from the "Data.Semigroup" module or imported directly
-- by some other modules.
--
-- This module also provides internal definitions related to the
-- 'Semigroup' class some.
--
-- This module exists mostly to simplify or workaround import-graph
-- issues; there is also a .hs-boot file to allow "GHC.Base" and other
-- modules to import method default implementations for 'stimes'
--
-- @since 4.11.0.0
module C.Data.Semigroup.Internal where

import GHC.Base hiding (Any)
import GHC.Enum
import GHC.Num
import GHC.Read
import GHC.Show
import GHC.Generics
import GHC.Real

-- | This is a valid definition of 'stimes' for an idempotent 'Semigroup'.
--
-- When @x <> x = x@, this definition should be preferred, because it
-- works in \(\mathcal{O}(1)\) rather than \(\mathcal{O}(\log n)\).
import Data.Semigroup.Internal ( Endo(..), Sum(..), Dual(..), Alt(..), Any(..), All(..), Product(..) )

stimesIdempotent :: Integral b => b -> a -> a
stimesIdempotent n x
  | n <= 0 = errorWithoutStackTrace "stimesIdempotent: positive multiplier expected"
  | otherwise = x

-- | This is a valid definition of 'stimes' for an idempotent 'Monoid'.
--
-- When @mappend x x = x@, this definition should be preferred, because it
-- works in \(\mathcal{O}(1)\) rather than \(\mathcal{O}(\log n)\)
stimesIdempotentMonoid :: (Integral b, Monoid a) => b -> a -> a
stimesIdempotentMonoid n x = case compare n 0 of
  LT -> errorWithoutStackTrace "stimesIdempotentMonoid: negative multiplier"
  EQ -> mempty
  GT -> x

-- | This is a valid definition of 'stimes' for a 'Monoid'.
--
-- Unlike the default definition of 'stimes', it is defined for 0
-- and so it should be preferred where possible.
stimesMonoid :: (Integral b, Monoid a) => b -> a -> a
stimesMonoid n x0 = case compare n 0 of
  LT -> errorWithoutStackTrace "stimesMonoid: negative multiplier"
  EQ -> mempty
  GT -> f x0 n
    where
      f x y
        | even y = f (x `mappend` x) (y `quot` 2)
        | y == 1 = x
        | otherwise = g (x `mappend` x) (y `quot` 2) x               -- See Note [Half of y - 1]
      g x y z
        | even y = g (x `mappend` x) (y `quot` 2) z
        | y == 1 = x `mappend` z
        | otherwise = g (x `mappend` x) (y `quot` 2) (x `mappend` z) -- See Note [Half of y - 1]

-- this is used by the class definitionin GHC.Base;
-- it lives here to avoid cycles
stimesDefault :: (Integral b, Semigroup a) => b -> a -> a
stimesDefault y0 x0
  | y0 <= 0   = errorWithoutStackTrace "stimes: positive multiplier expected"
  | otherwise = f x0 y0
  where
    f x y
      | even y = f (x <> x) (y `quot` 2)
      | y == 1 = x
      | otherwise = g (x <> x) (y `quot` 2) x        -- See Note [Half of y - 1]
    g x y z
      | even y = g (x <> x) (y `quot` 2) z
      | y == 1 = x <> z
      | otherwise = g (x <> x) (y `quot` 2) (x <> z) -- See Note [Half of y - 1]

{- Note [Half of y - 1]
   ~~~~~~~~~~~~~~~~~~~~~
   Since y is guaranteed to be odd and positive here,
   half of y - 1 can be computed as y `quot` 2, optimising subtraction away.
-}

stimesMaybe :: (Integral b, Semigroup a) => b -> Maybe a -> Maybe a
stimesMaybe _ Nothing = Nothing
stimesMaybe n (Just a) = case compare n 0 of
    LT -> errorWithoutStackTrace "stimes: Maybe, negative multiplier"
    EQ -> Nothing
    GT -> Just (stimes n a)

stimesList  :: Integral b => b -> [a] -> [a]
stimesList n x
  | n < 0 = errorWithoutStackTrace "stimes: [], negative multiplier"
  | otherwise = rep n
  where
    rep 0 = []
    rep i = x ++ rep (i - 1)

-- | The dual of a 'Monoid', obtained by swapping the arguments of 'mappend'.
--
-- >>> getDual (mappend (Dual "Hello") (Dual "World"))
-- "WorldHello"
