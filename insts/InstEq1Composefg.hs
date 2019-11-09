-- Instance of class Eq1 for Compose (f g)
module InstEq1Composefg where
import Data.Functor.Classes

import Control.Applicative
import Data.Coerce (coerce)
import Data.Data (Data)
import Data.Type.Equality (TestEquality(..), (:~:)(..))
import GHC.Generics (Generic, Generic1)
import Text.Read (Read(..), readListDefault, readListPrecDefault)


import Data.Functor.Compose ( Compose(..) )

liftEq eq (Compose x) (Compose y) = Data.Functor.Classes.liftEq (Data.Functor.Classes.liftEq eq) x y

-- | @since 4.9.0.0
