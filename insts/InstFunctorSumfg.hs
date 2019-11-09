-- Instance of class Functor for Sum (f g)
module InstFunctorSumfg where
import Control.Applicative ((<|>))
import Data.Data (Data)
import Data.Functor.Classes
import GHC.Generics (Generic, Generic1)
import Text.Read (Read(..), readListDefault, readListPrecDefault)

-- | Lifted sum of functors.

import Data.Functor.Sum ( Sum(..) )

fmap f (InL x) = InL (Prelude.fmap f x)
fmap f (InR y) = InR (Prelude.fmap f y)

a <$ (InL x) = InL (a Prelude.<$ x)
a <$ (InR y) = InR (a Prelude.<$ y)

-- | @since 4.9.0.0
