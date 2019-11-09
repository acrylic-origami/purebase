-- Instance of class Applicative for Option ()
module InstApplicativeOption.hs where
import           Prelude             hiding (foldr1)

import {-# SOURCE #-} GHC.Base (Semigroup(..))

import           Data.Semigroup.Internal

import           Control.Applicative
import           Control.Monad
import           Control.Monad.Fix
import           Data.Bifoldable
import           Data.Bifunctor
import           Data.Bitraversable
import           Data.Coerce
import           Data.Data
import           GHC.Generics

-- | A generalization of 'Data.List.cycle' to an arbitrary 'Semigroup'.
-- May fail to terminate for some values in some semigroups.

import Data.Semigroup ( Option(..) )

  pure a = Option (Just a)
  Option a <*> Option b = Option (a <*> b)
  liftA2 f (Option x) (Option y) = Option (liftA2 f x y)

  Option Nothing  *>  _ = Option Nothing
  _               *>  b = b

-- | @since 4.9.0.0
