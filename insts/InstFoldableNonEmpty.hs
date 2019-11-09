-- Instance of class Foldable for NonEmpty ()
module InstFoldableNonEmpty where
import Data.Bool
import Data.Either
import Data.Eq
import Data.Functor.Utils (Max(..), Min(..), (#.))
import qualified GHC.List as List
import Data.Maybe
import Data.Monoid
import Data.Ord
import Data.Proxy

import GHC.Arr  ( Array(..), elems, numElements,
  foldlElems, foldrElems,
  foldlElems', foldrElems',
  foldl1Elems, foldr1Elems)
import GHC.Base hiding ( foldr )
import GHC.Generics
import GHC.Num  ( Num(..) )


import Data.Foldable ( Foldable(..) )

foldr f z ~(a :| as) = f a (List.foldr f z as)
foldl f z (a :| as) = List.foldl f (f z a) as
foldl1 f (a :| as) = List.foldl f a as

  -- GHC isn't clever enough to transform the default definition
  -- into anything like this, so we'd end up shuffling a bunch of
  -- Maybes around.
foldr1 f (p :| ps) = Data.Foldable.foldr go id ps p
  where
    go x r prev = f prev (r x)

  -- We used to say
  --
  --   length (_ :| as) = 1 + length as
  --
  -- but the default definition is better, counting from 1.
  --
  -- The default definition also works great for null and foldl'.
  -- As usual for cons lists, foldr' is basically hopeless.

foldMap f ~(a :| as) = f a `mappend` Data.Foldable.foldMap f as
fold ~(m :| ms) = m `mappend` Data.Foldable.fold ms
toList ~(a :| as) = a : as

-- | @since 4.7.0.0
