-- Instance of class Applicative for NonEmpty ()
module InstApplicativeNonEmpty where
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


import GHC.Base ( Applicative(..), NonEmpty(..), ap, liftM2 )

pure a = a :| []
(<*>) :: NonEmpty (a -> b) -> NonEmpty a -> NonEmpty b
(<*>) = ap
liftA2 :: (a -> b -> c) -> NonEmpty a -> NonEmpty b -> NonEmpty c
liftA2 = liftM2

-- | @since 4.9.0.0
