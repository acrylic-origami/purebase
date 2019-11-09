-- Instance of class Applicative for Complex ()
module InstApplicativeComplex where
import GHC.Base (Applicative (..))
import GHC.Generics (Generic, Generic1)
import GHC.Float (Floating(..))
import Data.Data (Data)
import Foreign (Storable, castPtr, peek, poke, pokeElemOff, peekElemOff, sizeOf,
  alignment)


import Data.Complex ( Complex(..) )

pure a = a :+ a
(f :+ g) <*> (a :+ b) = f a :+ g b
liftA2 f (x :+ y) (a :+ b) = f x a :+ f y b

-- | @since 4.9.0.0
