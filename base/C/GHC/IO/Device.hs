{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE NoImplicitPrelude, BangPatterns #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  GHC.IO.Device
-- Copyright   :  (c) The University of Glasgow, 1994-2008
-- License     :  see libraries/base/LICENSE
--
-- Maintainer  :  libraries@haskell.org
-- Stability   :  internal
-- Portability :  non-portable
--
-- Type classes for I/O providers.
--
-----------------------------------------------------------------------------

module C.GHC.IO.Device (
        RawIO(..),
        IODevice(..),
        IODeviceType(..),
        SeekMode(..)
    ) where

import GHC.Base
import GHC.Word
import GHC.Arr
import GHC.Enum
import GHC.Read
import GHC.Show
import GHC.Ptr
import GHC.Num
import GHC.IO
import  GHC.IO.Exception ( unsupportedOperation )

-- | A low-level I/O provider where the data is bytes in memory.
import GHC.IO.Device ( IODevice(..), RawIO(..) )

ioe_unsupportedOperation :: IO a
ioe_unsupportedOperation = throwIO unsupportedOperation

-- | Type of a device that can be used to back a
-- 'GHC.IO.Handle.Handle' (see also 'GHC.IO.Handle.mkFileHandle'). The
-- standard libraries provide creation of 'GHC.IO.Handle.Handle's via
-- Posix file operations with file descriptors (see
-- 'GHC.IO.Handle.FD.mkHandleFromFD') with FD being the underlying
-- 'GHC.IO.Device.IODevice' instance.
--
-- Users may provide custom instances of 'GHC.IO.Device.IODevice'
-- which are expected to conform the following rules:

