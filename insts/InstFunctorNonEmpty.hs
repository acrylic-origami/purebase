-- Instance of class Functor for NonEmpty ()
module InstFunctorNonEmpty where
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


import GHC.Base ( Functor(..), NonEmpty(..) )

fmap f ~(a :| as) = f a :| GHC.Base.fmap f as
b <$ ~(_ :| as)   = b   :| (b GHC.Base.<$ as)

-- | @since 4.9.0.0
