-- Instance of class Contravariant for K1 ( i c)
module InstContravariantK1ic where
import Control.Applicative
import Control.Category
import Data.Function (on)

import Data.Functor.Product
import Data.Functor.Sum
import Data.Functor.Compose

import Data.Monoid (Alt(..))
import Data.Proxy
import GHC.Generics

import Prelude hiding ((.),id)

-- | The class of contravariant functors.
--
-- Whereas in Haskell, one can think of a 'Functor' as containing or producing
-- values, a contravariant functor is a functor that can be thought of as
-- /consuming/ values.
--
-- As an example, consider the type of predicate functions  @a -> Bool@. One
-- such predicate might be @negative x = x < 0@, which
-- classifies integers as to whether they are negative. However, given this
-- predicate, we can re-use it in other situations, providing we have a way to
-- map values /to/ integers. For instance, we can use the @negative@ predicate
-- on a person's bank balance to work out if they are currently overdrawn:
--
-- @
-- newtype Predicate a = Predicate { getPredicate :: a -> Bool }
--
-- instance Contravariant Predicate where
--   contramap f (Predicate p) = Predicate (p . f)
--                                          |   `- First, map the input...
--                                          `----- then apply the predicate.
--
-- overdrawn :: Predicate Person
-- overdrawn = contramap personBankBalance negative
-- @
--
-- Any instance should be subject to the following laws:
--
-- [Identity]    @'contramap' 'id' = 'id'@
-- [Composition] @'contramap' (g . f) = 'contramap' f . 'contramap' g@
--
-- Note, that the second law follows from the free theorem of the type of
-- 'contramap' and the first law, so you need only check that the former
-- condition holds.


import Data.Functor.Contravariant ( Contravariant(..) )

contramap _ (K1 c) = K1 c

