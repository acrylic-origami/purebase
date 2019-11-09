-- Instance of class Read for Compose (f g a)
module InstReadComposefga where
import Data.Functor.Classes

import Control.Applicative
import Data.Coerce (coerce)
import Data.Data (Data)
import Data.Type.Equality (TestEquality(..), (:~:)(..))
import GHC.Generics (Generic, Generic1)
import Text.Read (Read(..), readListDefault, readListPrecDefault)


import Data.Functor.Compose ( Compose(..) )

readPrec = readPrec1

readListPrec = readListPrecDefault
readList     = readListDefault

-- | @since 4.9.0.0
