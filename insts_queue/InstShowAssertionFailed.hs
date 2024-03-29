-- Instance of class Show for AssertionFailed ()
module InstShowAssertionFailed where
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

import GHC.IO.Exception ( AssertionFailed(..) )

showsPrec _ (AssertionFailed err) = showString err

-----

-- |Superclass for asynchronous exceptions.
--
-- @since 4.7.0.0
