-- Instance of class Functor for Compose (f g)
module InstFunctorComposefg.hs where
import Data.Functor.Classes

import Control.Applicative
import Data.Coerce (coerce)
import Data.Data (Data)
import Data.Type.Equality (TestEquality(..), (:~:)(..))
import GHC.Generics (Generic, Generic1)
import Text.Read (Read(..), readListDefault, readListPrecDefault)


import Data.Functor.Compose ( Compose(..) )

    fmap f (Compose x) = Compose (fmap (fmap f) x)
    a <$ (Compose x) = Compose (fmap (a <$) x)

-- | @since 4.9.0.0
