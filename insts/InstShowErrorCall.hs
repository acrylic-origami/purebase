-- Instance of class Show for ErrorCall ()
module InstShowErrorCall.hs where
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

  showsPrec _ (ErrorCallWithLocation err "") = showString err
  showsPrec _ (ErrorCallWithLocation err loc) =
      showString err . showChar '\n' . showString loc

