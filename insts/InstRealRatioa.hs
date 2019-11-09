-- Instance of class Real for Ratio (a)
module InstRealRatioa.hs where
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


import GHC.Real ( Real(..) )

    {-# SPECIALIZE instance Real Rational #-}
    toRational (x:%y)   =  toInteger x :% toInteger y

-- | @since 2.0.1
