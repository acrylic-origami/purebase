-- Instance of class Show for SomeTypeRep ()
module InstShowSomeTypeRep.hs where
import GHC.Base
import qualified GHC.Arr as A
import GHC.Types ( TYPE )
import Data.Type.Equality
import GHC.List ( splitAt, foldl', elem )
import GHC.Word
import GHC.Show
import GHC.TypeLits ( KnownSymbol, symbolVal', AppendSymbol )
import GHC.TypeNats ( KnownNat, natVal' )
import Unsafe.Coerce ( unsafeCoerce )

import GHC.Fingerprint.Type
import  GHC.Fingerprint
   -- loop: GHC.Fingerprint -> Foreign.Ptr -> Data.Typeable
   -- Better to break the loop here, because we want non-SOURCE imports
   -- of Data.Typeable as much as possible so we can optimise the derived
   -- instances.
-- import  Debug.Trace (trace)

#include "MachDeps.h"


import Data.Typeable.Internal ( SomeTypeRep(..) )

  showsPrec p (SomeTypeRep ty) = showsPrec p ty

