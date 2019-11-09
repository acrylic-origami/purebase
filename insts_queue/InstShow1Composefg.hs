-- Instance of class Show1 for Compose (f g)
module InstShow1Composefg where
import Data.Functor.Classes

import Control.Applicative
import Data.Coerce (coerce)
import Data.Data (Data)
import Data.Type.Equality (TestEquality(..), (:~:)(..))
import GHC.Generics (Generic, Generic1)
import Text.Read (Read(..), readListDefault, readListPrecDefault)


import Data.Functor.Compose ( Compose(..) )

liftShowsPrec sp sl d (Compose x) =
showsUnaryWith (liftShowsPrec sp' sl') "Compose" d x
where
sp' = liftShowsPrec sp sl
sl' = liftShowList sp sl

-- Instances of Prelude classes

-- | @since 4.9.0.0
