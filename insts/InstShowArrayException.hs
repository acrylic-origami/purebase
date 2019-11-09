-- Instance of class Show for ArrayException ()
module InstShowArrayException.hs where
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

import GHC.IO.Exception ( ArrayException(..) )

  showsPrec _ (IndexOutOfBounds s)
        = showString "array index out of range"
        . (if not (null s) then showString ": " . showString s
                           else id)
  showsPrec _ (UndefinedElement s)
        = showString "undefined array element"
        . (if not (null s) then showString ": " . showString s
                           else id)

-- | The exception thrown when an infinite cycle is detected in
-- 'System.IO.fixIO'.
--
-- @since 4.11.0.0
