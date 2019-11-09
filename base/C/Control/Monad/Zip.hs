{-# LANGUAGE Safe #-}
{-# LANGUAGE TypeOperators #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Monad.Zip
-- Copyright   :  (c) Nils Schweinsberg 2011,
--                (c) George Giorgidze 2011
--                (c) University Tuebingen 2011
-- License     :  BSD-style (see the file libraries/base/LICENSE)
-- Maintainer  :  libraries@haskell.org
-- Stability   :  experimental
-- Portability :  portable
--
-- Monadic zipping (used for monad comprehensions)
--
-----------------------------------------------------------------------------

module C.Control.Monad.Zip where

import Control.Monad (liftM, liftM2)
import Data.Functor.Identity
import Data.Monoid
import Data.Ord ( Down(..) )
import Data.Proxy
import qualified Data.List.NonEmpty as NE
import GHC.Generics

-- | Instances should satisfy the laws:
--
-- [Naturality]
--
--     @'liftM' (f 'Control.Arrow.***' g) ('mzip' ma mb)
--         = 'mzip' ('liftM' f ma) ('liftM' g mb)@
--
-- [Information Preservation]
--
--     @'liftM' ('Prelude.const' ()) ma = 'liftM' ('Prelude.const' ()) mb@
--         implies
--     @'munzip' ('mzip' ma mb) = (ma, mb)@
--
import Control.Monad.Zip ( MonadZip(..) )

