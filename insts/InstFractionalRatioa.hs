-- Instance of class Fractional for Ratio (a)
module InstFractionalRatioa.hs where
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


import GHC.Real ( Fractional(..) )

    {-# SPECIALIZE instance Fractional Rational #-}
    (x:%y) / (x':%y')   =  (x*y') % (y*x')
    recip (0:%_)        = ratioZeroDenominatorError
    recip (x:%y)
        | x < 0         = negate y :% negate x
        | otherwise     = y :% x
    fromRational (x:%y) =  fromInteger x % fromInteger y

-- | @since 2.0.1
