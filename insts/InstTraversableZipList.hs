-- Instance of class Traversable for ZipList ()
module InstTraversableZipList.hs where
import Control.Applicative ( Const(..), ZipList(..) )
import Data.Coerce
import Data.Either ( Either(..) )
import Data.Foldable ( Foldable )
import Data.Functor
import Data.Functor.Identity ( Identity(..) )
import Data.Functor.Utils ( StateL(..), StateR(..) )
import Data.Monoid ( Dual(..), Sum(..), Product(..),
                     First(..), Last(..), Alt(..), Ap(..) )
import Data.Ord ( Down(..) )
import Data.Proxy ( Proxy(..) )

import GHC.Arr
import GHC.Base ( Applicative(..), Monad(..), Monoid, Maybe(..), NonEmpty(..),
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

import Data.Traversable ( Traversable(..) )

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
