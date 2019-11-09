-- Instance of class Show for Ratio (a)
module InstShowRatioa where
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

{-# SPECIALIZE instance Show Rational #-}
showsPrec p (x:%y)  =  showParen (p > ratioPrec) $
showsPrec ratioPrec1 x .
showString " % " .
                           -- H98 report has spaces round the %
                           -- but we removed them [May 04]
                           -- and added them again for consistency with
                           -- Haskell 98 [Sep 08, #1920]
showsPrec ratioPrec1 y

-- | @since 2.0.1
