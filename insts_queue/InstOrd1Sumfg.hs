-- Instance of class Ord1 for Sum (f g)
module InstOrd1Sumfg where
import Control.Applicative ((<|>))
import Data.Data (Data)
import Data.Functor.Classes
import GHC.Generics (Generic, Generic1)
import Text.Read (Read(..), readListDefault, readListPrecDefault)

-- | Lifted sum of functors.

import Data.Functor.Sum ( Sum(..) )

liftCompare comp (InL x1) (InL x2) = liftCompare comp x1 x2
liftCompare _ (InL _) (InR _) = LT
liftCompare _ (InR _) (InL _) = GT
liftCompare comp (InR y1) (InR y2) = liftCompare comp y1 y2

-- | @since 4.9.0.0
