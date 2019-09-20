{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE TypeOperators #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Traversable
-- Copyright   :  Conor McBride and Ross Paterson 2005
-- License     :  BSD-style (see the LICENSE file in the distribution)
--
-- Maintainer  :  libraries@haskell.org
-- Stability   :  experimental
-- Portability :  portable
--
-- Class of data structures that can be traversed from left to right,
-- performing an action on each element.
--
-- See also
--
--  * \"Applicative Programming with Effects\",
--    by Conor McBride and Ross Paterson,
--    /Journal of Functional Programming/ 18:1 (2008) 1-13, online at
--    <http://www.soi.city.ac.uk/~ross/papers/Applicative.html>.
--
--  * \"The Essence of the Iterator Pattern\",
--    by Jeremy Gibbons and Bruno Oliveira,
--    in /Mathematically-Structured Functional Programming/, 2006, online at
--    <http://web.comlab.ox.ac.uk/oucl/work/jeremy.gibbons/publications/#iterator>.
--
--  * \"An Investigation of the Laws of Traversals\",
--    by Mauro Jaskelioff and Ondrej Rypacek,
--    in /Mathematically-Structured Functional Programming/, 2012, online at
--    <http://arxiv.org/pdf/1202.2919>.
--
-----------------------------------------------------------------------------

module C.Data.Traversable (
    -- * The 'Traversable' class
    Traversable(..),
    -- * Utility functions
    for,
    forM,
    mapAccumL,
    mapAccumR,
    -- * General definitions for superclass methods
    fmapDefault,
    foldMapDefault,
    ) where

-- It is convenient to use 'Const' here but this means we must
-- define a few instances here which really belong in Control.Applicative
import C.Control.Applicative ( Const(..), ZipList(..) )
import Data.Coerce
import C.Data.Either ( Either(..) )
import C.Data.Foldable ( Foldable )
import C.Data.Functor
import C.Data.Functor.Identity ( Identity(..) )
import C.Data.Functor.Utils ( StateL(..), StateR(..) )
import C.Data.Monoid ( Dual(..), Sum(..), Product(..),
                     First(..), Last(..), Alt(..), Ap(..) )
import C.Data.Ord ( Down(..) )
import Data.Proxy ( Proxy(..) )

import GHC.Arr
import C.GHC.Base ( Applicative(..), Monad(..), Monoid, Maybe(..), NonEmpty(..),
                  ($), (.), id, flip )
import GHC.Generics
import qualified GHC.List as List ( foldr )

-- | Functors representing data structures that can be traversed from
-- left to right.
--
-- A definition of 'traverse' must satisfy the following laws:
--
-- [Naturality]
--   @t . 'traverse' f = 'traverse' (t . f)@
--   for every applicative transformation @t@
--
-- [Identity]
--   @'traverse' 'Identity' = 'Identity'@
--
-- [Composition]
--   @'traverse' ('Data.Functor.Compose.Compose' . 'fmap' g . f)
--     = 'Data.Functor.Compose.Compose' . 'fmap' ('traverse' g) . 'traverse' f@
--
-- A definition of 'sequenceA' must satisfy the following laws:
--
-- [Naturality]
--   @t . 'sequenceA' = 'sequenceA' . 'fmap' t@
--   for every applicative transformation @t@
--
-- [Identity]
--   @'sequenceA' . 'fmap' 'Identity' = 'Identity'@
--
-- [Composition]
--   @'sequenceA' . 'fmap' 'Data.Functor.Compose.Compose'
--     = 'Data.Functor.Compose.Compose' . 'fmap' 'sequenceA' . 'sequenceA'@
--
-- where an /applicative transformation/ is a function
--
-- @t :: (Applicative f, Applicative g) => f a -> g a@
--
-- preserving the 'Applicative' operations, i.e.
--
-- @
-- t ('pure' x) = 'pure' x
-- t (f '<*>' x) = t f '<*>' t x
-- @
--
-- and the identity functor 'Identity' and composition functors
-- 'Data.Functor.Compose.Compose' are from "Data.Functor.Identity" and
-- "Data.Functor.Compose".
--
-- A result of the naturality law is a purity law for 'traverse'
--
-- @'traverse' 'pure' = 'pure'@
--
-- (The naturality law is implied by parametricity and thus so is the
-- purity law [1, p15].)
--
-- Instances are similar to 'Functor', e.g. given a data type
--
-- > data Tree a = Empty | Leaf a | Node (Tree a) a (Tree a)
--
-- a suitable instance would be
--
-- > instance Traversable Tree where
-- >    traverse f Empty = pure Empty
-- >    traverse f (Leaf x) = Leaf <$> f x
-- >    traverse f (Node l k r) = Node <$> traverse f l <*> f k <*> traverse f r
--
-- This is suitable even for abstract types, as the laws for '<*>'
-- imply a form of associativity.
--
-- The superclass instances should satisfy the following:
--
--  * In the 'Functor' instance, 'fmap' should be equivalent to traversal
--    with the identity applicative functor ('fmapDefault').
--
--  * In the 'Foldable' instance, 'Data.Foldable.foldMap' should be
--    equivalent to traversal with a constant applicative functor
--    ('foldMapDefault').
--
-- References:
-- [1] The Essence of the Iterator Pattern, Jeremy Gibbons and Bruno C. d. S. Oliveira
class (Functor t, Foldable t) => Traversable t where
    {-# MINIMAL traverse | sequenceA #-}

    -- | Map each element of a structure to an action, evaluate these actions
    -- from left to right, and collect the results. For a version that ignores
    -- the results see 'Data.Foldable.traverse_'.
    traverse :: Applicative f => (a -> f b) -> t a -> f (t b)
    {-# INLINE traverse #-}  -- See Note [Inline default methods]
    traverse f = sequenceA . fmap f

    -- | Evaluate each action in the structure from left to right, and
    -- collect the results. For a version that ignores the results
    -- see 'Data.Foldable.sequenceA_'.
    sequenceA :: Applicative f => t (f a) -> f (t a)
    {-# INLINE sequenceA #-}  -- See Note [Inline default methods]
    sequenceA = traverse id

    -- | Map each element of a structure to a monadic action, evaluate
    -- these actions from left to right, and collect the results. For
    -- a version that ignores the results see 'Data.Foldable.mapM_'.
    mapM :: Monad m => (a -> m b) -> t a -> m (t b)
    {-# INLINE mapM #-}  -- See Note [Inline default methods]
    mapM = traverse

    -- | Evaluate each monadic action in the structure from left to
    -- right, and collect the results. For a version that ignores the
    -- results see 'Data.Foldable.sequence_'.
    sequence :: Monad m => t (m a) -> m (t a)
    {-# INLINE sequence #-}  -- See Note [Inline default methods]
    sequence = sequenceA

{- Note [Inline default methods]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider

   class ... => Traversable t where
       ...
       mapM :: Monad m => (a -> m b) -> t a -> m (t b)
       mapM = traverse   -- Default method

   instance Traversable [] where
       {-# INLINE traverse #-}
       traverse = ...code for traverse on lists ...

This gives rise to a list-instance of mapM looking like this

  $fTraversable[]_$ctraverse = ...code for traverse on lists...
       {-# INLINE $fTraversable[]_$ctraverse #-}
  $fTraversable[]_$cmapM    = $fTraversable[]_$ctraverse

Now the $ctraverse obediently inlines into the RHS of $cmapM, /but/
that's all!  We get

  $fTraversable[]_$cmapM = ...code for traverse on lists...

with NO INLINE pragma!  This happens even though 'traverse' had an
INLINE pragma because the author knew it should be inlined pretty
vigorously.

Indeed, it turned out that the rhs of $cmapM was just too big to
inline, so all uses of mapM on lists used a terribly inefficient
dictionary-passing style, because of its 'Monad m =>' type.  Disaster!

Solution: add an INLINE pragma on the default method:

   class ... => Traversable t where
       ...
       mapM :: Monad m => (a -> m b) -> t a -> m (t b)
       {-# INLINE mapM #-}     -- VERY IMPORTANT!
       mapM = traverse
-}

-- instances for Prelude types

-- | @since 2.01
instance Traversable Maybe where
    traverse _ Nothing = pure Nothing
    traverse f (Just x) = Just <$> f x

-- | @since 2.01
instance Traversable [] where
    {-# INLINE traverse #-} -- so that traverse can fuse
    traverse f = List.foldr cons_f (pure [])
      where cons_f x ys = liftA2 (:) (f x) ys

-- | @since 4.9.0.0
instance Traversable NonEmpty where
  traverse f ~(a :| as) = liftA2 (:|) (f a) (traverse f as)

-- | @since 4.7.0.0
instance Traversable (Either a) where
    traverse _ (Left x) = pure (Left x)
    traverse f (Right y) = Right <$> f y

-- | @since 4.7.0.0
instance Traversable ((,) a) where
    traverse f (x, y) = (,) x <$> f y

-- | @since 2.01
-- instance Ix i => Traversable (Array i) where
--     traverse f arr = listArray (bounds arr) `fmap` traverse f (elems arr)

-- | @since 4.7.0.0
-- instance Traversable Proxy where
--     traverse _ _ = pure Proxy
--     {-# INLINE traverse #-}
--     sequenceA _ = pure Proxy
--     {-# INLINE sequenceA #-}
--     mapM _ _ = pure Proxy
--     {-# INLINE mapM #-}
--     sequence _ = pure Proxy
--     {-# INLINE sequence #-}

-- | @since 4.7.0.0
instance Traversable (Const m) where
    traverse _ (Const m) = pure $ Const m

-- | @since 4.8.0.0
instance Traversable Dual where
    traverse f (Dual x) = Dual <$> f x

-- | @since 4.8.0.0
instance Traversable Sum where
    traverse f (Sum x) = Sum <$> f x

-- | @since 4.8.0.0
instance Traversable Product where
    traverse f (Product x) = Product <$> f x

-- | @since 4.8.0.0
instance Traversable First where
    traverse f (First x) = First <$> traverse f x

-- | @since 4.8.0.0
instance Traversable Last where
    traverse f (Last x) = Last <$> traverse f x

-- | @since 4.12.0.0
instance (Traversable f) => Traversable (Alt f) where
    traverse f (Alt x) = Alt <$> traverse f x

-- | @since 4.12.0.0
instance (Traversable f) => Traversable (Ap f) where
    traverse f (Ap x) = Ap <$> traverse f x

-- | @since 4.9.0.0
instance Traversable ZipList where
    traverse f (ZipList x) = ZipList <$> traverse f x

-- | @since 4.9.0.0
-- deriving instance Traversable Identity


-- Instances for GHC.Generics
-- | @since 4.9.0.0
-- instance Traversable U1 where
--     traverse _ _ = pure U1
--     {-# INLINE traverse #-}
--     sequenceA _ = pure U1
--     {-# INLINE sequenceA #-}
--     mapM _ _ = pure U1
--     {-# INLINE mapM #-}
--     sequence _ = pure U1
--     {-# INLINE sequence #-}

-- -- | @since 4.9.0.0
-- deriving instance Traversable V1

-- -- | @since 4.9.0.0
-- deriving instance Traversable Par1

-- -- | @since 4.9.0.0
-- deriving instance Traversable f => Traversable (Rec1 f)

-- -- | @since 4.9.0.0
-- deriving instance Traversable (K1 i c)

-- -- | @since 4.9.0.0
-- deriving instance Traversable f => Traversable (M1 i c f)

-- -- | @since 4.9.0.0
-- deriving instance (Traversable f, Traversable g) => Traversable (f :+: g)

-- -- | @since 4.9.0.0
-- deriving instance (Traversable f, Traversable g) => Traversable (f :*: g)

-- -- | @since 4.9.0.0
-- deriving instance (Traversable f, Traversable g) => Traversable (f :.: g)

-- -- | @since 4.9.0.0
-- deriving instance Traversable UAddr

-- -- | @since 4.9.0.0
-- deriving instance Traversable UChar

-- -- | @since 4.9.0.0
-- deriving instance Traversable UDouble

-- -- | @since 4.9.0.0
-- deriving instance Traversable UFloat

-- -- | @since 4.9.0.0
-- deriving instance Traversable UInt

-- -- | @since 4.9.0.0
-- deriving instance Traversable UWord

-- -- Instance for Data.Ord
-- -- | @since 4.12.0.0
-- deriving instance Traversable Down

-- general functions

-- | 'for' is 'traverse' with its arguments flipped. For a version
-- that ignores the results see 'Data.Foldable.for_'.
for :: (Traversable t, Applicative f) => t a -> (a -> f b) -> f (t b)
{-# INLINE for #-}
for = flip traverse

-- | 'forM' is 'mapM' with its arguments flipped. For a version that
-- ignores the results see 'Data.Foldable.forM_'.
forM :: (Traversable t, Monad m) => t a -> (a -> m b) -> m (t b)
{-# INLINE forM #-}
forM = flip mapM

-- |The 'mapAccumL' function behaves like a combination of 'fmap'
-- and 'Data.Foldable.foldl'; it applies a function to each element of a structure,
-- passing an accumulating parameter from left to right, and returning
-- a final value of this accumulator together with the new structure.
mapAccumL :: Traversable t => (a -> b -> (a, c)) -> a -> t b -> (a, t c)
mapAccumL f s t = runStateL (traverse (StateL . flip f) t) s

-- |The 'mapAccumR' function behaves like a combination of 'fmap'
-- and 'Data.Foldable.foldr'; it applies a function to each element of a structure,
-- passing an accumulating parameter from right to left, and returning
-- a final value of this accumulator together with the new structure.
mapAccumR :: Traversable t => (a -> b -> (a, c)) -> a -> t b -> (a, t c)
mapAccumR f s t = runStateR (traverse (StateR . flip f) t) s

-- | This function may be used as a value for `fmap` in a `Functor`
--   instance, provided that 'traverse' is defined. (Using
--   `fmapDefault` with a `Traversable` instance defined only by
--   'sequenceA' will result in infinite recursion.)
--
-- @
-- 'fmapDefault' f ≡ 'runIdentity' . 'traverse' ('Identity' . f)
-- @
fmapDefault :: forall t a b . Traversable t
            => (a -> b) -> t a -> t b
{-# INLINE fmapDefault #-}
-- See Note [Function coercion] in Data.Functor.Utils.
fmapDefault = coerce (traverse :: (a -> Identity b) -> t a -> Identity (t b))

-- | This function may be used as a value for `Data.Foldable.foldMap`
-- in a `Foldable` instance.
--
-- @
-- 'foldMapDefault' f ≡ 'getConst' . 'traverse' ('Const' . f)
-- @
foldMapDefault :: forall t m a . (Traversable t, Monoid m)
               => (a -> m) -> t a -> m
{-# INLINE foldMapDefault #-}
-- See Note [Function coercion] in Data.Functor.Utils.
foldMapDefault = coerce (traverse :: (a -> Const m ()) -> t a -> Const m (t ()))
