-- Instance of class Functor for StateL ( s)
module InstFunctorStateLs.hs where
import Data.Coerce (Coercible, coerce)
import GHC.Base ( Applicative(..), Functor(..), Maybe(..), Monoid(..), Ord(..)
                , Semigroup(..), ($), otherwise )

-- We don't expose Max and Min because, as Edward Kmett pointed out to me,
-- there are two reasonable ways to define them. One way is to use Maybe, as we
-- do here; the other way is to impose a Bounded constraint on the Monoid
-- instance. We may eventually want to add both versions, but we don't want to
-- trample on anyone's toes by imposing Max = MaxMaybe.


import Data.Functor.Utils ( StateL(..) )

    fmap f (StateL k) = StateL $ \ s -> let (s', v) = k s in (s', f v)

-- | @since 4.0
