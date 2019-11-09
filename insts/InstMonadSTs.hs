-- Instance of class Monad for ST ( s)
module InstMonadSTs.hs where
import Control.Monad.Fix

import qualified Control.Monad.ST as ST
import qualified Control.Monad.ST.Unsafe as ST

import qualified GHC.ST as GHC.ST
import GHC.Base
import qualified Control.Monad.Fail as Fail

-- | The lazy @'ST' monad.
-- The ST monad allows for destructive updates, but is escapable (unlike IO).
-- A computation of type @'ST' s a@ returns a value of type @a@, and
-- execute in "thread" @s@. The @s@ parameter is either
--
-- * an uninstantiated type variable (inside invocations of 'runST'), or
--
-- * 'RealWorld' (inside invocations of 'stToIO').
--
-- It serves to keep the internal states of different invocations of
-- 'runST' separate from each other and from invocations of 'stToIO'.
--
-- The '>>=' and '>>' operations are not strict in the state.  For example,
--
-- @'runST' (writeSTRef _|_ v >>= readSTRef _|_ >> return 2) = 2@

import Control.Monad.ST.Lazy.Imp ( ST(..) )

    (>>) = (*>)

    m >>= k = ST $ \ s ->
       let
         -- See Note [Lazy ST and multithreading]
         {-# NOINLINE res #-}
         res = noDup (unST m s)
         (r,new_s) = res
       in
         unST (k r) new_s

-- | @since 4.10
