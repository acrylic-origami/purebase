-- Instance of class Applicative for Const (m)
module InstApplicativeConstm where
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

pure _ = Const mempty
liftA2 _ (Const x) (Const y) = Const (x `mappend` y)
(<*>) :: (Monoid f) => Const f (a -> b) -> Const f a -> Const f b
Const f <*> Const v = Const (f `mappend` v)

-- This is pretty much the same as
-- Const f <*> Const v = Const (f `mappend` v)
-- but guarantees that mappend for Const a b will have the same arity
-- as the one for a; it won't create a closure to raise the arity
-- to 2.
