-- Instance of class Ord1 for Compose (f g)
module InstOrd1Composefg.hs where
import Data.Functor.Classes

import Control.Applicative
import Data.Coerce (coerce)
import Data.Data (Data)
import Data.Type.Equality (TestEquality(..), (:~:)(..))
import GHC.Generics (Generic, Generic1)
import Text.Read (Read(..), readListDefault, readListPrecDefault)


import Data.Functor.Compose ( Compose(..) )

    liftCompare comp (Compose x) (Compose y) =
        liftCompare (liftCompare comp) x y

-- | @since 4.9.0.0
