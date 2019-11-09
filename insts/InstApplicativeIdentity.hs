-- Instance of class Applicative for Identity ()
module InstApplicativeIdentity.hs where
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

    pure     = Identity
    (<*>)    = coerce
    liftA2   = coerce

-- | @since 4.8.0.0
