-- Instance of class Show1 for Sum (f g)
module InstShow1Sumfg where
import Control.Applicative ((<|>))
import Data.Data (Data)
import Data.Functor.Classes
import GHC.Generics (Generic, Generic1)
import Text.Read (Read(..), readListDefault, readListPrecDefault)

-- | Lifted sum of functors.

import Data.Functor.Sum ( Sum(..) )

liftShowsPrec sp sl d (InL x) =
showsUnaryWith (liftShowsPrec sp sl) "InL" d x
liftShowsPrec sp sl d (InR y) =
showsUnaryWith (liftShowsPrec sp sl) "InR" d y

-- | @since 4.9.0.0
