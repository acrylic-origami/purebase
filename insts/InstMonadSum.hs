-- Instance of class Monad for Sum ()
module InstMonadSum.hs where
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

import Data.Semigroup.Internal ( Sum(..) )

    m >>= k  = k (getSum m)

-- | Monoid under multiplication.
--
-- >>> getProduct (Product 3 <> Product 4 <> mempty)
-- 12
