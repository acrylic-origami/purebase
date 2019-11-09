-- Instance of class Num for Complex (a)
module InstNumComplexa.hs where
import GHC.Base (Applicative (..))
import GHC.Generics (Generic, Generic1)
import GHC.Float (Floating(..))
import Data.Data (Data)
import Foreign (Storable, castPtr, peek, poke, pokeElemOff, peekElemOff, sizeOf,
                alignment)


import Data.Complex ( Complex(..) )

    {-# SPECIALISE instance Num (Complex Float) #-}
    {-# SPECIALISE instance Num (Complex Double) #-}
    (x:+y) + (x':+y')   =  (x+x') :+ (y+y')
    (x:+y) - (x':+y')   =  (x-x') :+ (y-y')
    (x:+y) * (x':+y')   =  (x*x'-y*y') :+ (x*y'+y*x')
    negate (x:+y)       =  negate x :+ negate y
    abs z               =  magnitude z :+ 0
    signum (0:+0)       =  0
    signum z@(x:+y)     =  x/r :+ y/r  where r = magnitude z
    fromInteger n       =  fromInteger n :+ 0

-- | @since 2.01
