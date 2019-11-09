-- Instance of class MonadFix for First ()
module InstMonadFixFirst.hs where
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
-- supported by GHC and Hugs.

import Control.Monad.Fix ( MonadFix(..) )

    mfix f   = First (mfix (getFirst . f))

-- | @since 4.8.0.0
