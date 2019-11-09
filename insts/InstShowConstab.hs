-- Instance of class Show for Const (a b)
module InstShowConstab.hs where
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

    showsPrec d (Const x) = showParen (d > 10) $
                            showString "Const " . showsPrec 11 x

-- | @since 4.7.0.0
