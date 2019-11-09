-- Instance of class Num for Ratio (a)
module InstNumRatioa.hs where
import GHC.Base
import GHC.Num
import GHC.List
import GHC.Enum
import GHC.Show
import  GHC.Exception( divZeroException, overflowException
                                   , underflowException
                                   , ratioZeroDenomException )

#if defined(MIN_VERSION_integer_gmp)
import GHC.Integer.GMP.Internals
#endif


import GHC.Real ( Ratio(..) )

    {-# SPECIALIZE instance Num Rational #-}
    (x:%y) + (x':%y')   =  reduce (x*y' + x'*y) (y*y')
    (x:%y) - (x':%y')   =  reduce (x*y' - x'*y) (y*y')
    (x:%y) * (x':%y')   =  reduce (x * x') (y * y')
    negate (x:%y)       =  (-x) :% y
    abs (x:%y)          =  abs x :% y
    signum (x:%_)       =  signum x :% 1
    fromInteger x       =  fromInteger x :% 1

-- | @since 2.0.1
