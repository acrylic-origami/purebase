-- Instance of class Applicative for ST ( s)
module InstApplicativeSTs.hs where
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

    pure a = ST $ \ s -> (a,s)

    fm <*> xm = ST $ \ s ->
       let
         {-# NOINLINE res1 #-}
         !res1 = unST fm s
         !(f, s') = res1

         {-# NOINLINE res2 #-}
         -- See Note [Lazy ST and multithreading]
         res2 = noDup (unST xm s')
         (x, s'') = res2
       in (f x, s'')
    -- Why can we use a strict binding for res1? If someone
    -- forces the (f x, s'') pair, then they must need
    -- f or s''. To get s'', they need s'.

    liftA2 f m n = ST $ \ s ->
      let
        {-# NOINLINE res1 #-}
        -- See Note [Lazy ST and multithreading]
        res1 = noDup (unST m s)
        (x, s') = res1

        {-# NOINLINE res2 #-}
        res2 = noDup (unST n s')
        (y, s'') = res2
      in (f x y, s'')
    -- We don't get to be strict in liftA2, but we clear out a
    -- NOINLINE in comparison to the default definition, which may
    -- help the simplifier.

    m *> n = ST $ \s ->
       let
         {-# NOINLINE s' #-}
         -- See Note [Lazy ST and multithreading]
         s' = noDup (snd (unST m s))
       in unST n s'

    m <* n = ST $ \s ->
       let
         {-# NOINLINE res1 #-}
         !res1 = unST m s
         !(mr, s') = res1

         {-# NOINLINE s'' #-}
         -- See Note [Lazy ST and multithreading]
         s'' = noDup (snd (unST n s'))
       in (mr, s'')
    -- Why can we use a strict binding for res1? The same reason as
    -- in <*>. If someone demands the (mr, s'') pair, then they will
    -- force mr or s''. To get s'', they need s'.

-- | @since 2.01
