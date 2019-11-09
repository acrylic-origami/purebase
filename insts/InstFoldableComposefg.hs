-- Instance of class Foldable for Compose (f g)
module InstFoldableComposefg.hs where
import Data.Functor.Classes

import Control.Applicative
import Data.Coerce (coerce)
import Data.Data (Data)
import Data.Type.Equality (TestEquality(..), (:~:)(..))
import GHC.Generics (Generic, Generic1)
import Text.Read (Read(..), readListDefault, readListPrecDefault)


import Data.Functor.Compose ( Compose(..) )

    foldMap f (Compose t) = foldMap (foldMap f) t

-- | @since 4.9.0.0
