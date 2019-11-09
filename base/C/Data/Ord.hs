{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Ord
-- Copyright   :  (c) The University of Glasgow 2005
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  libraries@haskell.org
-- Stability   :  stable
-- Portability :  portable
--
-- Orderings
--
-----------------------------------------------------------------------------

module C.Data.Ord (
   Ord(..),
   Ordering(..),
   Down(..),
   comparing,
 ) where

import Data.Bits (Bits, FiniteBits)
import Foreign.Storable (Storable)
import GHC.Arr (Ix)
import GHC.Base
import GHC.Enum (Bounded, Enum)
import GHC.Float (Floating, RealFloat)
import GHC.Num
import GHC.Read
import GHC.Real (Fractional, Integral, Real, RealFrac)
import GHC.Show

-- |
-- > comparing p x y = compare (p x) (p y)
--
-- Useful combinator for use in conjunction with the @xxxBy@ family
-- of functions from "Data.List", for example:
--
-- >   ... sortBy (comparing fst) ...
import Data.Ord ( Down(..) )

comparing :: (Ord a) => (b -> a) -> b -> b -> Ordering
comparing p x y = compare (p x) (p y)

-- | The 'Down' type allows you to reverse sort order conveniently.  A value of type
-- @'Down' a@ contains a value of type @a@ (represented as @'Down' a@).
-- If @a@ has an @'Ord'@ instance associated with it then comparing two
-- values thus wrapped will give you the opposite of their normal sort order.
-- This is particularly useful when sorting in generalised list comprehensions,
-- as in: @then sortWith by 'Down' x@
--
-- @since 4.6.0.0
