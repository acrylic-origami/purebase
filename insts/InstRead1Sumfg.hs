-- Instance of class Read1 for Sum (f g)
module InstRead1Sumfg.hs where
import Control.Applicative ((<|>))
import Data.Data (Data)
import Data.Functor.Classes
import GHC.Generics (Generic, Generic1)
import Text.Read (Read(..), readListDefault, readListPrecDefault)

-- | Lifted sum of functors.

import Data.Functor.Sum ( Sum(..) )

    liftReadPrec rp rl = readData $
        readUnaryWith (liftReadPrec rp rl) "InL" InL <|>
        readUnaryWith (liftReadPrec rp rl) "InR" InR

    liftReadListPrec = liftReadListPrecDefault
    liftReadList     = liftReadListDefault

-- | @since 4.9.0.0
