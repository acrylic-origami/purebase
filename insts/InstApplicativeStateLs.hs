-- Instance of class Applicative for StateL ( s)
module InstApplicativeStateLs.hs where
import Data.Coerce (Coercible, coerce)
import GHC.Base ( Applicative(..), Functor(..), Maybe(..), Monoid(..), Ord(..)
                , Semigroup(..), ($), otherwise )

-- We don't expose Max and Min because, as Edward Kmett pointed out to me,
-- there are two reasonable ways to define them. One way is to use Maybe, as we
-- do here; the other way is to impose a Bounded constraint on the Monoid
-- instance. We may eventually want to add both versions, but we don't want to
-- trample on anyone's toes by imposing Max = MaxMaybe.


import Data.Functor.Utils ( StateL(..) )

    pure x = StateL (\ s -> (s, x))
    StateL kf <*> StateL kv = StateL $ \ s ->
        let (s', f) = kf s
            (s'', v) = kv s'
        in (s'', f v)
    liftA2 f (StateL kx) (StateL ky) = StateL $ \s ->
        let (s', x) = kx s
            (s'', y) = ky s'
        in (s'', f x y)

-- right-to-left state-transforming monad
