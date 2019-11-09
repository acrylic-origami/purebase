-- Instance of class Foldable for U1 ()
module InstFoldableU1.hs where
import Data.Bool
import Data.Either
import Data.Eq
import Data.Functor.Utils (Max(..), Min(..), (#.))
import qualified GHC.List as List
import Data.Maybe
import Data.Monoid
import Data.Ord
import Data.Proxy

import GHC.Arr  ( Array(..), elems, numElements,
                  foldlElems, foldrElems,
                  foldlElems', foldrElems',
                  foldl1Elems, foldr1Elems)
import GHC.Base hiding ( foldr )
import GHC.Generics
import GHC.Num  ( Num(..) )


import Data.Foldable ( Foldable(..) )

    foldMap _ _ = mempty
    {-# INLINE foldMap #-}
    fold _ = mempty
    {-# INLINE fold #-}
    foldr _ z _ = z
    {-# INLINE foldr #-}
    foldl _ z _ = z
    {-# INLINE foldl #-}
    foldl1 _ _ = errorWithoutStackTrace "foldl1: U1"
    foldr1 _ _ = errorWithoutStackTrace "foldr1: U1"
    length _   = 0
    null _     = True
    elem _ _   = False
    sum _      = 0
    product _  = 1

-- | @since 4.9.0.0
-- deriving instance Foldable V1

-- -- | @since 4.9.0.0
-- deriving instance Foldable Par1

-- -- | @since 4.9.0.0
-- deriving instance Foldable f => Foldable (Rec1 f)

-- -- | @since 4.9.0.0
-- deriving instance Foldable (K1 i c)

-- -- | @since 4.9.0.0
-- deriving instance Foldable f => Foldable (M1 i c f)

-- -- | @since 4.9.0.0
-- deriving instance (Foldable f, Foldable g) => Foldable (f :+: g)

-- -- | @since 4.9.0.0
-- deriving instance (Foldable f, Foldable g) => Foldable (f :*: g)

-- -- | @since 4.9.0.0
-- deriving instance (Foldable f, Foldable g) => Foldable (f :.: g)

-- -- | @since 4.9.0.0
-- deriving instance Foldable UAddr

-- -- | @since 4.9.0.0
-- deriving instance Foldable UChar

-- -- | @since 4.9.0.0
-- deriving instance Foldable UDouble

-- -- | @since 4.9.0.0
-- deriving instance Foldable UFloat

-- -- | @since 4.9.0.0
-- deriving instance Foldable UInt

-- -- | @since 4.9.0.0
-- deriving instance Foldable UWord

-- -- Instances for Data.Ord
-- -- | @since 4.12.0.0
-- deriving instance Foldable Down

-- | Monadic fold over the elements of a structure,
-- associating to the right, i.e. from right to left.
