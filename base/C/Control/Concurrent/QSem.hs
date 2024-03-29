{-# LANGUAGE Safe #-}
{-# LANGUAGE BangPatterns #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Concurrent.QSem
-- Copyright   :  (c) The University of Glasgow 2001
-- License     :  BSD-style (see the file libraries/base/LICENSE)
-- 
-- Maintainer  :  libraries@haskell.org
-- Stability   :  experimental
-- Portability :  non-portable (concurrency)
--
-- Simple quantity semaphores.
--
-----------------------------------------------------------------------------

module C.Control.Concurrent.QSem
        ( -- * Simple Quantity Semaphores
          QSem,         -- abstract
          newQSem,      -- :: Int  -> IO QSem
          waitQSem,     -- :: QSem -> IO ()
          signalQSem    -- :: QSem -> IO ()
        ) where

import Control.Concurrent.MVar ( MVar, newEmptyMVar, takeMVar, tryTakeMVar
                          , putMVar, newMVar, tryPutMVar)
import Control.Exception
import Data.Maybe

-- | 'QSem' is a quantity semaphore in which the resource is acquired
-- and released in units of one. It provides guaranteed FIFO ordering
-- for satisfying blocked `waitQSem` calls.
--
-- The pattern
--
-- >   bracket_ waitQSem signalQSem (...)
--
-- is safe; it never loses a unit of the resource.
--
import Control.Concurrent.QSem ( QSem(..) )

newQSem :: Int -> IO QSem
newQSem initial
  | initial < 0 = fail "newQSem: Initial quantity must be non-negative"
  | otherwise   = do
      sem <- newMVar (initial, [], [])
      return (QSem sem)

-- |Wait for a unit to become available
waitQSem :: QSem -> IO ()
waitQSem (QSem m) =
  mask_ $ do
    (i,b1,b2) <- takeMVar m
    if i == 0
       then do
         b <- newEmptyMVar
         putMVar m (i, b1, b:b2)
         wait b
       else do
         let !z = i-1
         putMVar m (z, b1, b2)
         return ()
  where
    wait b = takeMVar b `onException` do
                (uninterruptibleMask_ $ do -- Note [signal uninterruptible]
                   (i,b1,b2) <- takeMVar m
                   r <- tryTakeMVar b
                   r' <- if isJust r
                            then signal (i,b1,b2)
                            else do putMVar b (); return (i,b1,b2)
                   putMVar m r')

-- |Signal that a unit of the 'QSem' is available
signalQSem :: QSem -> IO ()
signalQSem (QSem m) =
  uninterruptibleMask_ $ do -- Note [signal uninterruptible]
    r <- takeMVar m
    r' <- signal r
    putMVar m r'

-- Note [signal uninterruptible]
--
--   If we have
--
--      bracket waitQSem signalQSem (...)
--
--   and an exception arrives at the signalQSem, then we must not lose
--   the resource.  The signalQSem is masked by bracket, but taking
--   the MVar might block, and so it would be interruptible.  Hence we
--   need an uninterruptibleMask here.
--
--   This isn't ideal: during high contention, some threads won't be
--   interruptible.  The QSemSTM implementation has better behaviour
--   here, but it performs much worse than this one in some
--   benchmarks.

signal :: (Int,[MVar ()],[MVar ()]) -> IO (Int,[MVar ()],[MVar ()])
signal (i,a1,a2) =
 if i == 0
   then loop a1 a2
   else let !z = i+1 in return (z, a1, a2)
 where
   loop [] [] = return (1, [], [])
   loop [] b2 = loop (reverse b2) []
   loop (b:bs) b2 = do
     r <- tryPutMVar b ()
     if r then return (0, bs, b2)
          else loop bs b2
