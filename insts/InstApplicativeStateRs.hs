-- Instance of class Applicative for StateR ( s)
module InstApplicativeStateRs where
import Data.Coerce (Coercible, coerce)
import GHC.Base ( Applicative(..), Functor(..), Maybe(..), Monoid(..), Ord(..)
                , Semigroup(..), ($), otherwise )

-- We don't expose Max and Min because, as Edward Kmett pointed out to me,
-- there are two reasonable ways to define them. One way is to use Maybe, as we
-- do here; the other way is to impose a Bounded constraint on the Monoid
-- instance. We may eventually want to add both versions, but we don't want to
-- trample on anyone's toes by imposing Max = MaxMaybe.


import Data.Functor.Utils ( StateR(..) )

pure x = StateR (\ s -> (s, x))
StateR kf <*> StateR kv = StateR $ \ s ->
  let (s', v) = kv s
      (s'', f) = kf s'
  in (s'', f v)
liftA2 f (StateR kx) (StateR ky) = StateR $ \ s ->
  let (s', y) = ky s
      (s'', x) = kx s'
  in (s'', f x y)

-- See Note [Function coercion]
