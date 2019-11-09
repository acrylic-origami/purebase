-- Instance of class Traversable for Compose (f g)
module InstTraversableComposefg.hs where
import Data.Functor.Classes

import Control.Applicative
import Data.Coerce (coerce)
import Data.Data (Data)
import Data.Type.Equality (TestEquality(..), (:~:)(..))
import GHC.Generics (Generic, Generic1)
import Text.Read (Read(..), readListDefault, readListPrecDefault)


import Data.Functor.Compose ( Compose(..) )

    traverse f (Compose t) = Compose <$> traverse (traverse f) t

-- | @since 4.9.0.0
