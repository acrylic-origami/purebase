{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE NoImplicitPrelude
           , ExistentialQuantification
           , MagicHash
           , RecordWildCards
           , PatternSynonyms
  #-}
{-# LANGUAGE TypeInType #-}
{-# OPTIONS_HADDOCK not-home #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  GHC.Exception
-- Copyright   :  (c) The University of Glasgow, 1998-2002
-- License     :  see libraries/base/LICENSE
--
-- Maintainer  :  cvs-ghc@haskell.org
-- Stability   :  internal
-- Portability :  non-portable (GHC extensions)
--
-- Exceptions and exception-handling functions.
--
-----------------------------------------------------------------------------

module C.GHC.Exception
       ( module GHC.Exception.Type
       , throw
       -- , ErrorCall(..,ErrorCall)
       , errorCallException
       , errorCallWithCallStackException
         -- re-export CallStack and SrcLoc from GHC.Types
       , CallStack, fromCallSiteList, getCallStack, prettyCallStack
       , prettyCallStackLines, showCCSStack
       , SrcLoc(..), prettySrcLoc
       ) where

import GHC.Base
import GHC.Show
import GHC.Stack.Types
import GHC.OldList
import GHC.Prim
import GHC.IO.Unsafe
import  GHC.Stack.CCS
import GHC.Exception.Type

-- | Throw an exception.  Exceptions may be thrown from purely
-- functional code, but may only be caught within the 'IO' monad.
import GHC.Exception ( ErrorCall(..) )

throw :: forall (r :: RuntimeRep). forall (a :: TYPE r). forall e.
         Exception e => e -> a
throw e = raise# (toException e)

-- | This is thrown when the user calls 'error'. The first @String@ is the
-- argument given to 'error', second @String@ is the location.
-- pattern ErrorCall :: String -> ErrorCall
-- pattern ErrorCall err <- ErrorCallWithLocation err _ where
--  ErrorCall err = ErrorCallWithLocation err ""

-- {-# COMPLETE ErrorCall #-}

-- | @since 4.0.0.0
-- | @since 4.0.0.0
errorCallException :: String -> SomeException
errorCallException s = toException (ErrorCall s)

errorCallWithCallStackException :: String -> CallStack -> SomeException
errorCallWithCallStackException s stk = unsafeDupablePerformIO $ do
  ccsStack <- currentCallStack
  let
    implicitParamCallStack = prettyCallStackLines stk
    ccsCallStack = showCCSStack ccsStack
    stack = intercalate "\n" $ implicitParamCallStack ++ ccsCallStack
  return $ toException (ErrorCallWithLocation s stack)

showCCSStack :: [String] -> [String]
showCCSStack [] = []
showCCSStack stk = "CallStack (from -prof):" : map ("  " ++) (reverse stk)

-- prettySrcLoc and prettyCallStack are defined here to avoid hs-boot
-- files. See Note [Definition of CallStack]

-- | Pretty print a 'SrcLoc'.
--
-- @since 4.9.0.0
prettySrcLoc :: SrcLoc -> String
prettySrcLoc SrcLoc {..}
  = foldr (++) ""
      [ srcLocFile, ":"
      , show srcLocStartLine, ":"
      , show srcLocStartCol, " in "
      , srcLocPackage, ":", srcLocModule
      ]

-- | Pretty print a 'CallStack'.
--
-- @since 4.9.0.0
prettyCallStack :: CallStack -> String
prettyCallStack = intercalate "\n" . prettyCallStackLines

prettyCallStackLines :: CallStack -> [String]
prettyCallStackLines cs = case getCallStack cs of
  []  -> []
  stk -> "CallStack (from HasCallStack):"
       : map (("  " ++) . prettyCallSite) stk
  where
    prettyCallSite (f, loc) = f ++ ", called at " ++ prettySrcLoc loc
