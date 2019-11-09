{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE Safe #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Functor.Sum
-- Copyright   :  (c) Ross Paterson 2014
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  libraries@haskell.org
-- Stability   :  experimental
-- Portability :  portable
--
-- Sums, lifted to functors.
--
-- @since 4.9.0.0
-----------------------------------------------------------------------------

module C.Data.Functor.Sum (
    Sum(..),
  ) where

import Control.Applicative ((<|>))
import Data.Data (Data)
import Data.Functor.Classes
import GHC.Generics (Generic, Generic1)
import Text.Read (Read(..), readListDefault, readListPrecDefault)

-- | Lifted sum of functors.
import Data.Functor.Sum ( Sum(..) )

