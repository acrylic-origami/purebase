{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE NoImplicitPrelude #-}

module C.GHC.Exception.Type
  ( SomeException
  , divZeroException
  , overflowException
  , ratioZeroDenomException
  , underflowException
  ) where

import GHC.Types ()

data SomeException
divZeroException, overflowException,
  ratioZeroDenomException, underflowException :: SomeException
