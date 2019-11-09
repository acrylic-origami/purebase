-- Instance of class Integral for Word ()
module InstIntegralWord.hs where
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

    quot    (W# x#) y@(W# y#)
        | y /= 0                = W# (x# `quotWord#` y#)
        | otherwise             = divZeroError
    rem     (W# x#) y@(W# y#)
        | y /= 0                = W# (x# `remWord#` y#)
        | otherwise             = divZeroError
    div     (W# x#) y@(W# y#)
        | y /= 0                = W# (x# `quotWord#` y#)
        | otherwise             = divZeroError
    mod     (W# x#) y@(W# y#)
        | y /= 0                = W# (x# `remWord#` y#)
        | otherwise             = divZeroError
    quotRem (W# x#) y@(W# y#)
        | y /= 0                = case x# `quotRemWord#` y# of
                                  (# q, r #) ->
                                      (W# q, W# r)
        | otherwise             = divZeroError
    divMod  (W# x#) y@(W# y#)
        | y /= 0                = (W# (x# `quotWord#` y#), W# (x# `remWord#` y#))
        | otherwise             = divZeroError
    toInteger (W# x#)           = wordToInteger x#

--------------------------------------------------------------
-- Instances for Integer
--------------------------------------------------------------

-- | @since 2.0.1
