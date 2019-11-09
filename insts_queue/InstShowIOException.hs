-- Instance of class Show for IOException ()
module InstShowIOException where
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

import GHC.IO.Exception ( IOException(..) )

showsPrec p (IOError hdl iot loc s _ fn) =
  (case fn of
          Nothing -> case hdl of
          Nothing -> id
          Just h  -> showsPrec p h . showString ": "
          Just name -> showString name . showString ": ") .
  (case loc of
           "" -> id
           _  -> showString loc . showString ": ") .
  showsPrec p iot .
  (case s of
           "" -> id
           _  -> showString " (" . showString s . showString ")")

-- Note the use of "lazy". This means that
--     assert False (throw e)
-- will throw the assertion failure rather than e. See trac #5561.
