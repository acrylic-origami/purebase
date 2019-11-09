-- Instance of class Show for Compose (f g a)
module InstShowComposefga.hs where
import Data.Functor.Classes

import Control.Applicative
import Data.Coerce (coerce)
import Data.Data (Data)
import Data.Type.Equality (TestEquality(..), (:~:)(..))
import GHC.Generics (Generic, Generic1)
import Text.Read (Read(..), readListDefault, readListPrecDefault)


import Data.Functor.Compose ( Compose(..) )

    showsPrec = showsPrec1

-- Functor instances

-- | @since 4.9.0.0
