-- Instance of class Foldable for Sum (f g)
module InstFoldableSumfg where
import Control.Applicative ((<|>))
import Data.Data (Data)
import Data.Functor.Classes
import GHC.Generics (Generic, Generic1)
import Text.Read (Read(..), readListDefault, readListPrecDefault)

-- | Lifted sum of functors.

import Data.Functor.Sum ( Sum(..) )

foldMap f (InL x) = Prelude.foldMap f x
foldMap f (InR y) = Prelude.foldMap f y

-- | @since 4.9.0.0
