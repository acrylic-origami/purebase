-- Instance of class Monoid for All ()
module InstMonoidAll where
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

import Data.Semigroup.Internal ( All(..) )

mempty = All True

-- | Boolean monoid under disjunction ('||').
--
-- >>> getAny (Any True <> mempty <> Any False)
-- True
--
-- >>> getAny (mconcat (map (\x -> Any (even x)) [2,4,6,7,8]))
-- True
