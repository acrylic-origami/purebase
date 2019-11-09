-- Instance of class Functor for Down ()
module InstFunctorDown where
import Data.Bits (Bits, FiniteBits)
import Foreign.Storable (Storable)
import GHC.Arr (Ix)
import GHC.Base
import GHC.Enum (Bounded, Enum)
import GHC.Float (Floating, RealFloat)
import GHC.Num
import GHC.Read
import GHC.Real (Fractional, Integral, Real, RealFrac)
import GHC.Show

-- |
-- > comparing p x y = compare (p x) (p y)
--
-- Useful combinator for use in conjunction with the @xxxBy@ family
-- of functions from "Data.List", for example:
--
-- >   ... sortBy (comparing fst) ...

import Data.Ord ( Down(..) )

fmap :: (a -> b) -> Down a -> Down b
fmap = coerce

-- | @since 4.11.0.0
