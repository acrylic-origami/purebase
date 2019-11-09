-- Instance of class Eq for Compose (f g a)
module InstEqComposefga where
import Data.Functor.Classes

import Control.Applicative
import Data.Coerce (coerce)
import Data.Data (Data)
import Data.Type.Equality (TestEquality(..), (:~:)(..))
import GHC.Generics (Generic, Generic1)
import Text.Read (Read(..), readListDefault, readListPrecDefault)


import Data.Functor.Compose ( Compose(..) )

(==) :: (Eq1 f, Eq1 g, Eq a) => Compose f g a -> Compose f g a -> Bool
(==) = eq1

-- | @since 4.9.0.0
