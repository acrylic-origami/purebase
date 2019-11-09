-- Instance of class Bifoldable for Const ()
module InstBifoldableConst.hs where
import Control.Applicative
import Data.Functor.Utils (Max(..), Min(..), (#.))
import Data.Maybe (fromMaybe)
import Data.Monoid
import GHC.Generics (K1(..))

-- | 'Bifoldable' identifies foldable structures with two different varieties
-- of elements (as opposed to 'Foldable', which has one variety of element).
-- Common examples are 'Either' and '(,)':
--
-- > instance Bifoldable Either where
-- >   bifoldMap f _ (Left  a) = f a
-- >   bifoldMap _ g (Right b) = g b
-- >
-- > instance Bifoldable (,) where
-- >   bifoldr f g z (a, b) = f a (g b z)
--
-- A minimal 'Bifoldable' definition consists of either 'bifoldMap' or
-- 'bifoldr'. When defining more than this minimal set, one should ensure
-- that the following identities hold:
--
-- @
-- 'bifold' ≡ 'bifoldMap' 'id' 'id'
-- 'bifoldMap' f g ≡ 'bifoldr' ('mappend' . f) ('mappend' . g) 'mempty'
-- 'bifoldr' f g z t ≡ 'appEndo' ('bifoldMap' (Endo . f) (Endo . g) t) z
-- @
--
-- If the type is also a 'Data.Bifunctor.Bifunctor' instance, it should satisfy:
--
-- @
-- 'bifoldMap' f g ≡ 'bifold' . 'Data.Bifunctor.bimap' f g
-- @
--
-- which implies that
--
-- @
-- 'bifoldMap' f g . 'Data.Bifunctor.bimap' h i ≡ 'bifoldMap' (f . h) (g . i)
-- @
--
-- @since 4.10.0.0

import Data.Bifoldable ( Bifoldable(..) )

  bifoldMap f _ (Const a) = f a

-- | @since 4.10.0.0
