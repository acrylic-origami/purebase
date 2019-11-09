-- Instance of class Applicative for  ((,) a)
module InstApplicativea.hs where
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


import because it has different types in different scenarios. ( Applicative(..) )

    pure x = (mempty, x)
    (u, f) <*> (v, x) = (u <> v, f x)
    liftA2 f (u, x) (v, y) = (u <> v, f x y)

-- | @since 4.9.0.0
