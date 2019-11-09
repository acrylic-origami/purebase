-- Instance of class Fractional for Complex (a)
module InstFractionalComplexa where
import GHC.Base (Applicative (..))
import GHC.Generics (Generic, Generic1)
import GHC.Float (Floating(..))
import Data.Data (Data)
import Foreign (Storable, castPtr, peek, poke, pokeElemOff, peekElemOff, sizeOf,
  alignment)


import Data.Complex ( Complex(..) )

(x:+y) / (x':+y')   =  (x*x''+y*y'') Prelude./ d :+ (y*x''-x*y'') Prelude./ d
  where x'' = scaleFloat k x'
        y'' = scaleFloat k y'
        k   = - max (exponent x') (exponent y')
        d   = x'*x'' + y'*y''

fromRational a      =  Prelude.fromRational a :+ 0

-- | @since 2.01
