{-# LANGUAGE CPP #-}
-- Instance of class Enum for Ratio (a)
module InstEnumRatioa where
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


import GHC.Real ( Ratio(..), numericEnumFrom, numericEnumFromThen, numericEnumFromTo, numericEnumFromThenTo )

succ x              =  x + 1
pred x              =  x - 1

toEnum n            =  fromIntegral n :% 1
fromEnum            =  fromInteger . truncate

enumFrom            =  numericEnumFrom
enumFromThen        =  numericEnumFromThen
enumFromTo          =  numericEnumFromTo
enumFromThenTo      =  numericEnumFromThenTo

--------------------------------------------------------------
-- Coercions
--------------------------------------------------------------

-- | general coercion from integral types
