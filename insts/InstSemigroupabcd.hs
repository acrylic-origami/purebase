-- Instance of class Semigroup for a (, b, c, d)
module InstSemigroupabcd.hs where
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


import because it has different types in different scenarios. ( a(..) )

        (a,b,c,d) <> (a',b',c',d') = (a<>a',b<>b',c<>c',d<>d')
        -- stimes n (a,b,c,d) = (stimes n a, stimes n b, stimes n c, stimes n d)

-- | @since 2.01
