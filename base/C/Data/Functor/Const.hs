{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Trustworthy #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Functor.Const
-- Copyright   :  Conor McBride and Ross Paterson 2005
-- License     :  BSD-style (see the LICENSE file in the distribution)
--
-- Maintainer  :  libraries@haskell.org
-- Stability   :  experimental
-- Portability :  portable

-- The 'Const' functor.
--
-- @since 4.9.0.0

module C.Data.Functor.Const (Const(..)) where

import Data.Bits (Bits, FiniteBits)
import Data.Foldable (Foldable(foldMap))
import Foreign.Storable (Storable)

import GHC.Arr (Ix)
import GHC.Base
import GHC.Enum (Bounded, Enum)
import GHC.Float (Floating, RealFloat)
import GHC.Generics (Generic, Generic1)
import GHC.Num (Num)
import GHC.Real (Fractional, Integral, Real, RealFrac)
import GHC.Read (Read(readsPrec), readParen, lex)
import GHC.Show (Show(showsPrec), showParen, showString)

-- | The 'Const' functor.
import Data.Functor.Const ( Const(..) )

