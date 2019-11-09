-- Instance of class Applicative for Compose (f g)
module InstApplicativeComposefg where
import Data.Functor.Classes

import Control.Applicative
import Data.Coerce (coerce)
import Data.Data (Data)
import Data.Type.Equality (TestEquality(..), (:~:)(..))
import GHC.Generics (Generic, Generic1)
import Text.Read (Read(..), readListDefault, readListPrecDefault)


import Data.Functor.Compose ( Compose(..) )

pure x = Compose (Control.Applicative.pure (Control.Applicative.pure x))
Compose f <*> Compose x = Compose (Control.Applicative.liftA2 (Control.Applicative.<*>) f x)
liftA2 f (Compose x) (Compose y) = Compose (Control.Applicative.liftA2 (Control.Applicative.liftA2 f) x y)

-- | @since 4.9.0.0
