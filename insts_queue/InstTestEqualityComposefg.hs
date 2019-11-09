-- Instance of class TestEquality for Compose (f g)
module InstTestEqualityComposefg where
import Data.Functor.Classes

import Control.Applicative
import Data.Coerce (coerce)
import Data.Data (Data)
import Data.Type.Equality (TestEquality(..), (:~:)(..))
import GHC.Generics (Generic, Generic1)
import Text.Read (Read(..), readListDefault, readListPrecDefault)


import Data.Functor.Compose ( Compose(..) )

testEquality (Compose x) (Compose y) =
  case testEquality x y of -- :: Maybe (g x :~: g y)
    Just Refl -> Just Refl -- :: Maybe (x :~: y)
    Nothing   -> Nothing
