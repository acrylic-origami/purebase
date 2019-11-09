-- Instance of class Integral for Integer ()
module InstIntegralInteger.hs where
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

    toInteger n      = n

    {-# INLINE quot #-}
    _ `quot` 0 = divZeroError
    n `quot` d = n `quotInteger` d

    {-# INLINE rem #-}
    _ `rem` 0 = divZeroError
    n `rem` d = n `remInteger` d

    {-# INLINE div #-}
    _ `div` 0 = divZeroError
    n `div` d = n `divInteger` d

    {-# INLINE mod #-}
    _ `mod` 0 = divZeroError
    n `mod` d = n `modInteger` d

    {-# INLINE divMod #-}
    _ `divMod` 0 = divZeroError
    n `divMod` d = case n `divModInteger` d of
                     (# x, y #) -> (x, y)

    {-# INLINE quotRem #-}
    _ `quotRem` 0 = divZeroError
    n `quotRem` d = case n `quotRemInteger` d of
                      (# q, r #) -> (q, r)

-- | @since 4.8.0.0
