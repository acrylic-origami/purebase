-- Instance of class Alternative for Compose (f g)
module InstAlternativeComposefg where
import Data.Functor.Classes

import Control.Applicative
import Data.Coerce (coerce)
import Data.Data (Data)
import Data.Type.Equality (TestEquality(..), (:~:)(..))
import GHC.Generics (Generic, Generic1)
import Text.Read (Read(..), readListDefault, readListPrecDefault)


import Data.Functor.Compose ( Compose(..) )

empty :: Alternative f => Compose f g a
empty = Compose Control.Applicative.empty
-- (<|>) = coerce ((Control.Applicative.<|>) :: f (g a) -> f (g a) -> f (g a))
--   :: Compose f g a -> Compose f g a -> Compose f g a

-- | The deduction (via generativity) that if @g x :~: g y@ then @x :~: y@.
--
-- @since 4.14.0.0
