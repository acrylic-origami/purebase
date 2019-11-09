-- Instance of class Semigroup for Endo ( a)
module InstSemigroupEndoa.hs where
import GHC.Base hiding (Any)
import GHC.Enum
import GHC.Num
import GHC.Read
import GHC.Show
import GHC.Generics
import GHC.Real

-- | This is a valid definition of 'stimes' for an idempotent 'Semigroup'.
--
-- When @x <> x = x@, this definition should be preferred, because it
-- works in \(\mathcal{O}(1)\) rather than \(\mathcal{O}(\log n)\).

import Data.Semigroup.Internal ( Endo(..) )

        (<>) = coerce ((.) :: (a -> a) -> (a -> a) -> (a -> a))
        stimes = stimesMonoid

-- | @since 2.01