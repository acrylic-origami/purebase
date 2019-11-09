-- Instance of class Integral for Int ()
module InstIntegralInt.hs where
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

    toInteger (I# i) = smallInteger i

    a `quot` b
     | b == 0                     = divZeroError
     | b == (-1) && a == minBound = overflowError -- Note [Order of tests]
                                                  -- in GHC.Int
     | otherwise                  =  a `quotInt` b

    a `rem` b
     | b == 0                     = divZeroError
       -- The quotRem CPU instruction fails for minBound `quotRem` -1,
       -- but minBound `rem` -1 is well-defined (0). We therefore
       -- special-case it.
     | b == (-1)                  = 0
     | otherwise                  =  a `remInt` b

    a `div` b
     | b == 0                     = divZeroError
     | b == (-1) && a == minBound = overflowError -- Note [Order of tests]
                                                  -- in GHC.Int
     | otherwise                  =  a `divInt` b

    a `mod` b
     | b == 0                     = divZeroError
       -- The divMod CPU instruction fails for minBound `divMod` -1,
       -- but minBound `mod` -1 is well-defined (0). We therefore
       -- special-case it.
     | b == (-1)                  = 0
     | otherwise                  =  a `modInt` b

    a `quotRem` b
     | b == 0                     = divZeroError
       -- Note [Order of tests] in GHC.Int
     | b == (-1) && a == minBound = (overflowError, 0)
     | otherwise                  =  a `quotRemInt` b

    a `divMod` b
     | b == 0                     = divZeroError
       -- Note [Order of tests] in GHC.Int
     | b == (-1) && a == minBound = (overflowError, 0)
     | otherwise                  =  a `divModInt` b

--------------------------------------------------------------
-- Instances for @Word@
--------------------------------------------------------------

-- | @since 2.01
