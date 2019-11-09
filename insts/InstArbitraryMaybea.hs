-- Instance of class Arbitrary for Maybe (a)
module InstArbitraryMaybea.hs where
import GHC.Base hiding (Any)
import GHC.Enum
import GHC.Generics
import GHC.Num
import GHC.Read
import GHC.Show

import Control.Monad.Fail (MonadFail)

import Data.Semigroup.Internal

-- $MaybeExamples
-- To implement @find@ or @findLast@ on any 'Data.Foldable.Foldable':
--
-- @
-- findLast :: Foldable t => (a -> Bool) -> t a -> Maybe a
-- findLast pred = getLast . foldMap (\x -> if pred x
--                                            then Last (Just x)
--                                            else Last Nothing)
-- @
--
-- Much of 'Data.Map.Lazy.Map's interface can be implemented with
-- 'Data.Map.Lazy.alter'. Some of the rest can be implemented with a new
-- 'Data.Map.Lazy.alterF' function and either 'First' or 'Last':
--
-- > alterF :: (Functor f, Ord k) =>
-- >           (Maybe a -> f (Maybe a)) -> k -> Map k a -> f (Map k a)
-- >
-- > instance Monoid a => Functor ((,) a)  -- from Data.Functor
--
-- @
-- insertLookupWithKey :: Ord k => (k -> v -> v -> v) -> k -> v
--                     -> Map k v -> (Maybe v, Map k v)
-- insertLookupWithKey combine key value =
--   Arrow.first getFirst . 'Data.Map.Lazy.alterF' doChange key
--   where
--   doChange Nothing = (First Nothing, Just value)
--   doChange (Just oldValue) =
--     (First (Just oldValue),
--      Just (combine key value oldValue))
-- @


-- | Maybe monoid returning the leftmost non-Nothing value.
--
-- @'First' a@ is isomorphic to @'Alt' 'Maybe' a@, but precedes it
-- historically.
--
-- >>> getFirst (First (Just "hello") <> First Nothing <> First (Just "world"))
-- Just "hello"
--
-- Use of this type is discouraged. Note the following equivalence:
--
-- > Data.Monoid.First x === Maybe (Data.Semigroup.First x)
--
-- In addition to being equivalent in the structural sense, the two
-- also have 'Monoid' instances that behave the same. This type will
-- be marked deprecated in GHC 8.8, and removed in GHC 8.10.
-- Users are advised to use the variant from "Data.Semigroup" and wrap
-- it in 'Maybe'.

import Data.Monoid ( Maybe(..) )

  arbitrary = oneof [return Nothing, Just `fmap` arbitrary]

