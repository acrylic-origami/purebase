-- Instance of class Read for Identity (a)
module InstReadIdentitya.hs where
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

    readsPrec d = readParen (d > 10) $ \ r ->
        [(Identity x,t) | ("Identity",s) <- lex r, (x,t) <- readsPrec 11 s]

-- | This instance would be equivalent to the derived instances of the
-- 'Identity' newtype if the 'runIdentity' field were removed
--
-- @since 4.8.0.0
