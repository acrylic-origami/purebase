-- Instance of class Eq1 for Sum (f g)
module InstEq1Sumfg.hs where
import Control.Applicative ((<|>))
import Data.Data (Data)
import Data.Functor.Classes
import GHC.Generics (Generic, Generic1)
import Text.Read (Read(..), readListDefault, readListPrecDefault)

-- | Lifted sum of functors.

import Data.Functor.Sum ( Sum(..) )

    liftEq eq (InL x1) (InL x2) = liftEq eq x1 x2
    liftEq _ (InL _) (InR _) = False
    liftEq _ (InR _) (InL _) = False
    liftEq eq (InR y1) (InR y2) = liftEq eq y1 y2

-- | @since 4.9.0.0
