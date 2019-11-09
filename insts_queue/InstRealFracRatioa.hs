-- Instance of class RealFrac for Ratio (a)
module InstRealFracRatioa where
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


import GHC.Real ( RealFrac(..) )

{-# SPECIALIZE instance RealFrac Rational #-}
properFraction (x:%y) = (fromInteger (toInteger q), r:%y)
where (q,r) = quotRem x y

-- | @since 2.0.1
