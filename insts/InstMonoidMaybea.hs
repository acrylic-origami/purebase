-- Instance of class Monoid for Maybe (a)
module InstMonoidMaybea where
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


import GHC.Base ( Monoid(..) )

mempty = Nothing

-- | For tuples, the 'Monoid' constraint on @a@ determines
-- how the first values merge.
-- For example, 'String's concatenate:
--
-- > ("hello ", (+15)) <*> ("world!", 2002)
-- > ("hello world!",2017)
--
-- @since 2.01
