{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeOperators #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Monad.Fix
-- Copyright   :  (c) Andy Gill 2001,
--                (c) Oregon Graduate Institute of Science and Technology, 2002
-- License     :  BSD-style (see the file libraries/base/LICENSE)
-- Maintainer  :  libraries@haskell.org
-- Stability   :  experimental
-- Portability :  portable
--
-- Monadic fixpoints.
--
-- For a detailed discussion, see Levent Erkok's thesis,
-- /Value Recursion in Monadic Computations/, Oregon Graduate Institute, 2002.
--
-----------------------------------------------------------------------------

module C.Control.Monad.Fix (
        MonadFix(mfix),
        fix
  ) where

import Data.Either
import Data.Function ( fix )
import Data.Maybe
import Data.Monoid ( Dual(..), Sum(..), Product(..)
                   , First(..), Last(..), Alt(..), Ap(..) )
import Data.Ord ( Down(..) )
import GHC.Base ( Monad, NonEmpty(..), errorWithoutStackTrace, (.) )
import GHC.Generics
import GHC.List ( head, tail )
import Control.Monad.ST.Imp
import System.IO

-- | Monads having fixed points with a \'knot-tying\' semantics.
-- Instances of 'MonadFix' should satisfy the following laws:
--
-- [Purity]
--      @'mfix' ('Control.Monad.return' . h)  =  'Control.Monad.return' ('fix' h)@
--
-- [Left shrinking (or Tightening)]
--      @'mfix' (\\x -> a >>= \\y -> f x y)  =  a >>= \\y -> 'mfix' (\\x -> f x y)@
--
-- [Sliding]
--      @'mfix' ('Control.Monad.liftM' h . f)  =  'Control.Monad.liftM' h ('mfix' (f . h))@,
--      for strict @h@.
--
-- [Nesting]
--      @'mfix' (\\x -> 'mfix' (\\y -> f x y))  =  'mfix' (\\x -> f x x)@
--
-- This class is used in the translation of the recursive @do@ notation
-- supported by GHC and Hugs.    mfix f = \ r -> let a = f a r in a

-- | @since 4.3.0.0
import Control.Monad.Fix ( MonadFix(..) )

