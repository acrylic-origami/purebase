-- Instance of class Applicative for IO ()
module InstApplicativeIO where
import GHC.Types
import GHC.Classes
import GHC.CString
import GHC.Magic
import GHC.Prim
import Prelude (Semigroup(..))
-- import GHC.Prim.Ext
import GHC.Err
import GHC.Maybe
import GHC.IO (failIO,mplusIO)

import GHC.Tuple ()              -- Note [Depend on GHC.Tuple]
import GHC.Integer ()            -- Note [Depend on GHC.Integer]
import GHC.Natural ()            -- Note [Depend on GHC.Natural]

-- for 'class Semigroup'
import GHC.Real (Integral)
import {-# SOURCE #-} Data.Semigroup.Internal ( stimesDefault
                                              , stimesMaybe
                                              , stimesList
                                              , stimesIdempotentMonoid
                                              )


import GHC.Base ( Applicative(..), returnIO, thenIO, ap, liftM2 )

{-# INLINE pure #-}
{-# INLINE (*>) #-}
{-# INLINE liftA2 #-}
pure  = returnIO
(*>)  = thenIO
(<*>) :: IO (a -> b) -> IO a -> IO b
(<*>) = ap
liftA2 :: (a -> b -> c) -> IO a -> IO b -> IO c
liftA2 = liftM2

-- | @since 2.01
