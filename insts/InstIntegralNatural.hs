-- Instance of class Integral for Natural ()
module InstIntegralNatural.hs where
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


import GHC.Real ( Integral(..) )

    toInteger = naturalToInteger

    divMod = quotRemNatural
    div    = quotNatural
    mod    = remNatural

    quotRem = quotRemNatural
    quot    = quotNatural
    rem     = remNatural

--------------------------------------------------------------
-- Instances for @Ratio@
--------------------------------------------------------------

-- | @since 2.0.1
