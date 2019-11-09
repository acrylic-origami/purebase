{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE DeriveGeneric, NoImplicitPrelude, MagicHash,
             ExistentialQuantification, ImplicitParams #-}
{-# OPTIONS_GHC -funbox-strict-fields #-}
{-# OPTIONS_HADDOCK not-home #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  GHC.IO.Exception
-- Copyright   :  (c) The University of Glasgow, 2009
-- License     :  see libraries/base/LICENSE
--
-- Maintainer  :  libraries@haskell.org
-- Stability   :  internal
-- Portability :  non-portable
--
-- IO-related Exception types and functions
--
-----------------------------------------------------------------------------

module C.GHC.IO.Exception (
  BlockedIndefinitelyOnMVar(..), blockedIndefinitelyOnMVar,
  BlockedIndefinitelyOnSTM(..), blockedIndefinitelyOnSTM,
  Deadlock(..),
  AllocationLimitExceeded(..), allocationLimitExceeded,
  AssertionFailed(..),
  CompactionFailed(..),
  cannotCompactFunction, cannotCompactPinned, cannotCompactMutable,

  SomeAsyncException(..),
  asyncExceptionToException, asyncExceptionFromException,
  AsyncException(..), stackOverflow, heapOverflow,

  ArrayException(..),
  ExitCode(..),
  FixIOException (..),

  ioException,
  ioError,
  IOError,
  IOException(..),
  IOErrorType(..),
  userError,
  assertError,
  unsupportedOperation,
  untangle,
 ) where

import GHC.Base
import GHC.Generics
import GHC.List
import GHC.IO
import GHC.Show
import GHC.Read
import GHC.Exception
import GHC.IO.Handle.Types
import GHC.OldList ( intercalate )
import  GHC.Stack.CCS
import Foreign.C.Types

import Data.Typeable ( cast )

-- ------------------------------------------------------------------------
-- Exception datatypes and operations

-- |The thread is blocked on an @MVar@, but there are no other references
-- to the @MVar@ so it can't ever continue.
-- | @since 4.1.0.0
import GHC.IO.Exception ( AssertionFailed(..), ArrayException(..), BlockedIndefinitelyOnSTM(..), SomeAsyncException(..), IOException(..), AsyncException(..), Deadlock(..), IOErrorType(..), CompactionFailed(..), AllocationLimitExceeded(..), FixIOException(..), BlockedIndefinitelyOnMVar(..) )

blockedIndefinitelyOnMVar :: SomeException -- for the RTS
blockedIndefinitelyOnMVar = toException BlockedIndefinitelyOnMVar

-----

-- |The thread is waiting to retry an STM transaction, but there are no
-- other references to any @TVar@s involved, so it can't ever continue.
-- | @since 4.1.0.0
blockedIndefinitelyOnSTM :: SomeException -- for the RTS
blockedIndefinitelyOnSTM = toException BlockedIndefinitelyOnSTM

-----

-- |There are no runnable threads, so the program is deadlocked.
-- The @Deadlock@ exception is raised in the main thread only.
-- | @since 4.1.0.0
allocationLimitExceeded :: SomeException -- for the RTS
allocationLimitExceeded = toException AllocationLimitExceeded

-----

-- | Compaction found an object that cannot be compacted.  Functions
-- cannot be compacted, nor can mutable objects or pinned objects.
-- See 'GHC.Compact.compact'.
--
-- @since 4.10.0.0
cannotCompactFunction :: SomeException -- for the RTS
cannotCompactFunction =
  toException (CompactionFailed "cannot compact functions")

cannotCompactPinned :: SomeException -- for the RTS
cannotCompactPinned =
  toException (CompactionFailed "cannot compact pinned objects")

cannotCompactMutable :: SomeException -- for the RTS
cannotCompactMutable =
  toException (CompactionFailed "cannot compact mutable objects")

-----

-- |'assert' was applied to 'False'.
-- | @since 4.1.0.0
-- |@since 4.7.0.0
asyncExceptionToException :: Exception e => e -> SomeException
asyncExceptionToException = toException . SomeAsyncException

-- |@since 4.7.0.0
asyncExceptionFromException :: Exception e => SomeException -> Maybe e
asyncExceptionFromException x = do
    SomeAsyncException a <- fromException x
    cast a


-- |Asynchronous exceptions.
-- for the RTS
stackOverflow, heapOverflow :: SomeException
stackOverflow = toException StackOverflow
heapOverflow  = toException HeapOverflow

-- | @since 4.1.0.0
-- | @since 4.11.0.0
ioException     :: IOException -> IO a
ioException err = throwIO err

-- | Raise an 'IOError' in the 'IO' monad.
ioError         :: IOError -> IO a
ioError         =  ioException

-- ---------------------------------------------------------------------------
-- IOError type

-- | The Haskell 2010 type for exceptions in the 'IO' monad.
-- Any I\/O operation may raise an 'IOError' instead of returning a result.
-- For a more general type of exception, including also those that arise
-- in pure code, see 'Control.Exception.Exception'.
--
-- In Haskell 2010, this is an opaque type.
type IOError = IOException

-- |Exceptions that occur in the @IO@ monad.
-- An @IOException@ records a more specific error type, a descriptive
-- string and maybe the handle that was used when the error was
-- flagged.
-- | @since 4.1.0.0
data IOErrorType
  -- Haskell 2010:
  = AlreadyExists
  | NoSuchThing
  | ResourceBusy
  | ResourceExhausted
  | EOF
  | IllegalOperation
  | PermissionDenied
  | UserError
  -- GHC only:
  | UnsatisfiedConstraints
  | SystemError
  | ProtocolError
  | OtherError
  | InvalidArgument
  | InappropriateType
  | HardwareFault
  | UnsupportedOperation
  | TimeExpired
  | ResourceVanished
  | Interrupted

-- | @since 4.1.0.0
userError       :: String  -> IOError
userError str   =  IOError Nothing UserError "" str Nothing Nothing

-- ---------------------------------------------------------------------------
-- Showing IOErrors

-- | @since 4.1.0.0
assertError :: (?callStack :: CallStack) => Bool -> a -> a
assertError predicate v
  | predicate = lazy v
  | otherwise = unsafeDupablePerformIO $ do
    ccsStack <- currentCallStack
    let
      implicitParamCallStack = prettyCallStackLines ?callStack
      ccsCallStack = showCCSStack ccsStack
      stack = intercalate "\n" $ implicitParamCallStack ++ ccsCallStack
    throwIO (AssertionFailed ("Assertion failed\n" ++ stack))

unsupportedOperation :: IOError
unsupportedOperation =
   (IOError Nothing UnsupportedOperation ""
        "Operation is not supported" Nothing Nothing)

{-
(untangle coded message) expects "coded" to be of the form
        "location|details"
It prints
        location message details
-}
untangle :: Addr# -> String -> String
untangle coded message
  =  location
  ++ ": "
  ++ message
  ++ details
  ++ "\n"
  where
    coded_str = unpackCStringUtf8# coded

    (location, details)
      = case (span not_bar coded_str) of { (loc, rest) ->
        case rest of
          ('|':det) -> (loc, ' ' : det)
          _         -> (loc, "")
        }
    not_bar c = c /= '|'

