-- Instance of class Eq for IOException ()
module InstEqIOException where
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

(IOError h1 e1 loc1 str1 en1 fn1) == (IOError h2 e2 loc2 str2 en2 fn2) =
  e1 GHC.Base.== e2 && str1 GHC.Base.== str2 && h1 GHC.Base.== h2 && loc1 GHC.Base.== loc2 && en1 GHC.Base.== en2 && fn1 GHC.Base.== fn2

-- | An abstract type that contains a value for each variant of 'IOError'.
