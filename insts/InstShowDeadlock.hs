-- Instance of class Show for Deadlock ()
module InstShowDeadlock.hs where
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

import GHC.IO.Exception ( Deadlock(..) )

    showsPrec _ Deadlock = showString "<<deadlock>>"

-----

-- |This thread has exceeded its allocation limit.  See
-- 'System.Mem.setAllocationCounter' and
-- 'System.Mem.enableAllocationLimit'.
--
-- @since 4.8.0.0
