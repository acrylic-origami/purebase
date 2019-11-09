-- Instance of class Functor for Const ( m)
module InstFunctorConstm.hs where
import Data.Bits (Bits, FiniteBits)
import Data.Foldable (Foldable(foldMap))
import Foreign.Storable (Storable)

import GHC.Arr (Ix)
import GHC.Base
import GHC.Enum (Bounded, Enum)
import GHC.Float (Floating, RealFloat)
import GHC.Generics (Generic, Generic1)
import GHC.Num (Num)
import GHC.Real (Fractional, Integral, Real, RealFrac)
import GHC.Read (Read(readsPrec), readParen, lex)
import GHC.Show (Show(showsPrec), showParen, showString)

-- | The 'Const' functor.

import Data.Functor.Const ( Const(..) )

    fmap _ (Const v) = Const v

-- | @since 2.0.1
