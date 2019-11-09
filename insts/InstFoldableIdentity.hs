-- Instance of class Foldable for Identity ()
module InstFoldableIdentity where
import Control.Monad.Fix
import Data.Bits (Bits, FiniteBits)
import Data.Coerce
import Data.Foldable
import Data.Functor.Utils ((#.))
import Foreign.Storable (Storable)
import GHC.Arr (Ix)
import GHC.Base ( Applicative(..), Eq(..), Functor(..), Monad(..)
                , Semigroup, Monoid, Ord(..), ($), (.) )
import GHC.Enum (Bounded, Enum)
import GHC.Float (Floating, RealFloat)
import GHC.Generics (Generic, Generic1)
import GHC.Num (Num)
import GHC.Read (Read(..), lex, readParen)
import GHC.Real (Fractional, Integral, Real, RealFrac)
import GHC.Show (Show(..), showParen, showString)
import GHC.Types (Bool(..))

-- | Identity functor and monad. (a non-strict monad)
--
-- @since 4.8.0.0

import Data.Functor.Identity ( Identity(..) )

foldMap :: Monoid m => (a -> m) -> Identity a -> m
foldMap            = coerce

elem :: Eq a => a -> Identity a -> Bool
elem               = (. runIdentity) #. (==)
foldl :: (b -> a -> b) -> b -> Identity a -> b
foldl              = coerce
foldl' :: (b -> a -> b) -> b -> Identity a -> b
foldl'             = coerce
foldl1 _           = runIdentity
foldr f z (Identity x) = f x z
foldr' :: (a -> b -> b) -> b -> Identity a -> b
foldr'             = InstFoldableIdentity.foldr
foldr1 _           = runIdentity
length _           = 1
maximum            = runIdentity
minimum            = runIdentity
null _             = False
product            = runIdentity
sum                = runIdentity
toList (Identity x)    = [x]

-- | @since 4.8.0.0
