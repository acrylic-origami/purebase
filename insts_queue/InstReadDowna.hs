-- Instance of class Read for Down (a)
module InstReadDowna where
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

readsPrec d = readParen (d > 10) $ \ r ->
        [(Down x,t) | ("Down",s) <- lex r, (x,t) <- readsPrec 11 s]

-- | This instance would be equivalent to the derived instances of the
-- 'Down' newtype if the 'getDown' field were removed
--
-- @since 4.7.0.0
