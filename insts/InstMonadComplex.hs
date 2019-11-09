-- Instance of class Monad for Complex ()
module InstMonadComplex where
import GHC.Base (Applicative (..))
import GHC.Generics (Generic, Generic1)
import GHC.Float (Floating(..))
import Data.Data (Data)
import Foreign (Storable, castPtr, peek, poke, pokeElemOff, peekElemOff, sizeOf,
  alignment)


import Data.Complex ( Complex(..), realPart, imagPart )

(a :+ b) >>= f = realPart (f a) :+ imagPart (f b)

-- -----------------------------------------------------------------------------
-- Rules on Complex

