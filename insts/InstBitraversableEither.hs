-- Instance of class Bitraversable for Either ()
module InstBitraversableEither.hs where
import Control.Applicative
import Data.Bifunctor
import Data.Bifoldable
import Data.Coerce
import Data.Functor.Identity (Identity(..))
import Data.Functor.Utils (StateL(..), StateR(..))
import GHC.Generics (K1(..))

-- | 'Bitraversable' identifies bifunctorial data structures whose elements can
-- be traversed in order, performing 'Applicative' or 'Monad' actions at each
-- element, and collecting a result structure with the same shape.
--
-- As opposed to 'Traversable' data structures, which have one variety of
-- element on which an action can be performed, 'Bitraversable' data structures
-- have two such varieties of elements.
--
-- A definition of 'bitraverse' must satisfy the following laws:
--
-- [Naturality]
--   @'bitraverse' (t . f) (t . g) ≡ t . 'bitraverse' f g@
--   for every applicative transformation @t@
--
-- [Identity]
--   @'bitraverse' 'Identity' 'Identity' ≡ 'Identity'@
--
-- [Composition]
--   @'Data.Functor.Compose.Compose' .
--    'fmap' ('bitraverse' g1 g2) .
--    'bitraverse' f1 f2
--     ≡ 'bitraverse' ('Data.Functor.Compose.Compose' . 'fmap' g1 . f1)
--                  ('Data.Functor.Compose.Compose' . 'fmap' g2 . f2)@
--
-- where an /applicative transformation/ is a function
--
-- @t :: ('Applicative' f, 'Applicative' g) => f a -> g a@
--
-- preserving the 'Applicative' operations:
--
-- @
-- t ('pure' x) = 'pure' x
-- t (f '<*>' x) = t f '<*>' t x
-- @
--
-- and the identity functor 'Identity' and composition functors
-- 'Data.Functor.Compose.Compose' are from "Data.Functor.Identity" and
-- "Data.Functor.Compose".
--
-- Some simple examples are 'Either' and '(,)':
--
-- > instance Bitraversable Either where
-- >   bitraverse f _ (Left x) = Left <$> f x
-- >   bitraverse _ g (Right y) = Right <$> g y
-- >
-- > instance Bitraversable (,) where
-- >   bitraverse f g (x, y) = (,) <$> f x <*> g y
--
-- 'Bitraversable' relates to its superclasses in the following ways:
--
-- @
-- 'bimap' f g ≡ 'runIdentity' . 'bitraverse' ('Identity' . f) ('Identity' . g)
-- 'bifoldMap' f g = 'getConst' . 'bitraverse' ('Const' . f) ('Const' . g)
-- @
--
-- These are available as 'bimapDefault' and 'bifoldMapDefault' respectively.
--
-- @since 4.10.0.0

import Data.Bitraversable ( Bitraversable(..) )

  bitraverse f _ (Left a) = Left <$> f a
  bitraverse _ g (Right b) = Right <$> g b

-- | @since 4.10.0.0
