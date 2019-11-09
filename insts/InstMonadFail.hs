-- Instance of class MonadFail for  ([])
module InstMonadFail.hs where
import GHC.Base (String, Monad(), Maybe(Nothing), IO())
import  GHC.IO (failIO)

-- | When a value is bound in @do@-notation, the pattern on the left
-- hand side of @<-@ might not match. In this case, this class
-- provides a function to recover.
--
-- A 'Monad' without a 'MonadFail' instance may only be used in conjunction
-- with pattern that always match, such as newtypes, tuples, data types with
-- only a single data constructor, and irrefutable patterns (@~pat@).
--
-- Instances of 'MonadFail' should satisfy the following law: @fail s@ should
-- be a left zero for 'Control.Monad.>>=',
--
-- @
-- fail s >>= f  =  fail s
-- @
--
-- If your 'Monad' is also 'Control.Monad.MonadPlus', a popular definition is
--
-- @
-- fail _ = mzero
-- @
--
-- @since 4.9.0.0

import Control.Monad.Fail ( MonadFail(..) )

    {-# INLINE fail #-}
    fail _ = []

-- | @since 4.9.0.0
