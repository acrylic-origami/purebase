-- Instance of class ArrowChoice for Kleisli (m)
module InstArrowChoiceKleislim.hs where
import Data.Tuple ( fst, snd, uncurry )
import Data.Either
import Control.Monad.Fix
import Control.Category
import GHC.Base hiding ( (.), id )
import GHC.Generics (Generic, Generic1)


import Control.Arrow ( ArrowChoice(..) )

    left f = f +++ arr id
    right f = arr id +++ f
    f +++ g = (f >>> arr Left) ||| (g >>> arr Right)
    Kleisli f ||| Kleisli g = Kleisli (either f g)

-- | Some arrows allow application of arrow inputs to other inputs.
-- Instances should satisfy the following laws:
--
--  * @'first' ('arr' (\\x -> 'arr' (\\y -> (x,y)))) >>> 'app' = 'id'@
--
--  * @'first' ('arr' (g >>>)) >>> 'app' = 'second' g >>> 'app'@
--
--  * @'first' ('arr' (>>> h)) >>> 'app' = 'app' >>> h@
--
-- Such arrows are equivalent to monads (see 'ArrowMonad').

