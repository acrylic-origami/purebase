-- Instance of class Semigroup for Ordering ()
module InstSemigroupOrdering where
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


import GHC.Base ( Ordering(..) )

LT <> _ = LT
EQ <> y = y
GT <> _ = GT

    -- stimes = stimesIdempotentMonoid

-- lexicographical ordering
-- | @since 2.01
