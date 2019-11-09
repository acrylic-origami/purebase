-- Instance of class Real for Natural ()
module InstRealNatural where
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

toRational n = naturalToInteger n :% 1

-- Note [Integer division constant folding]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
--
-- Constant folding of quot, rem, div, mod, divMod and quotRem for
-- Integer arguments depends crucially on inlining. Constant folding
-- rules defined in compiler/prelude/PrelRules.hs trigger for
-- quotInteger, remInteger and so on. So if calls to quot, rem and so on
-- were not inlined the rules would not fire. The rules would also not
-- fire if calls to quotInteger and so on were inlined, but this does not
-- happen because they are all marked with NOINLINE pragma - see documentation
-- of integer-gmp or integer-simple.

-- | @since 2.0.1
