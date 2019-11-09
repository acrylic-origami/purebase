-- Instance of class Applicative for Maybe ()
module InstApplicativeMaybe where
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


import GHC.Base ( Applicative(..), Functor(..) )

pure = Just

Just f  <*> m       = fmap f m
Nothing <*> _m      = Nothing

liftA2 f (Just x) (Just y) = Just (f x y)
liftA2 _ _ _ = Nothing

Just _m1 *> m2      = m2
Nothing  *> _m2     = Nothing

-- | @since 2.01
