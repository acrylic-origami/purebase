-- Instance of class Semigroup for Dual (a)
module InstSemigroupDuala where
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

import Data.Semigroup.Internal ( Dual(..) )

Dual a <> Dual b = Dual (b <> a)
stimes n (Dual a) = Dual (stimes n a)

-- | @since 2.01
