-- Instance of class MonadFix for Ap (f)
module InstMonadFixApf where
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

mfix f   = Ap (Control.Monad.Fix.mfix (getAp . f))

-- Instances for GHC.Generics
-- | @since 4.9.0.0
-- instance MonadFix Par1 where
--     mfix f = Par1 (fix (unPar1 . f))

-- | @since 4.9.0.0
-- instance MonadFix f => MonadFix (Rec1 f) where
--     mfix f = Rec1 (mfix (unRec1 . f))

-- | @since 4.9.0.0
-- instance MonadFix f => MonadFix (M1 i c f) where
--     mfix f = M1 (mfix (unM1. f))

-- | @since 4.9.0.0
-- instance (MonadFix f, MonadFix g) => MonadFix (f :*: g) where
--     mfix f = (mfix (fstP . f)) :*: (mfix (sndP . f))
--       where
--         fstP (a :*: _) = a
--         sndP (_ :*: b) = b

-- Instances for Data.Ord

-- | @since 4.12.0.0
