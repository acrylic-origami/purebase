{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-inline-rule-shadowing #-}
    -- The RULES for the methods of class Arrow may never fire
    -- e.g. compose/arr;  see #10528

-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Arrow
-- Copyright   :  (c) Ross Paterson 2002
-- License     :  BSD-style (see the LICENSE file in the distribution)
--
-- Maintainer  :  libraries@haskell.org
-- Stability   :  provisional
-- Portability :  portable
--
-- Basic arrow definitions, based on
--
--  * /Generalising Monads to Arrows/, by John Hughes,
--    /Science of Computer Programming/ 37, pp67-111, May 2000.
--
-- plus a couple of definitions ('returnA' and 'loop') from
--
--  * /A New Notation for Arrows/, by Ross Paterson, in /ICFP 2001/,
--    Firenze, Italy, pp229-240.
--
-- These papers and more information on arrows can be found at
-- <http://www.haskell.org/arrows/>.

module C.Control.Arrow (
    -- * Arrows
    Arrow(..), Kleisli(..),
    -- ** Derived combinators
    returnA,
    (^>>), (>>^),
    (>>>), (<<<), -- reexported
    -- ** Right-to-left variants
    (<<^), (^<<),
    -- * Monoid operations
    ArrowZero(..), ArrowPlus(..),
    -- * Conditionals
    ArrowChoice(..),
    -- * Arrow application
    ArrowApply(..), ArrowMonad(..), leftApp,
    -- * Feedback
    ArrowLoop(..)
    ) where

import Data.Tuple ( fst, snd, uncurry )
import Data.Either
import Control.Monad.Fix
import Control.Category
import GHC.Base hiding ( (.), id )
import GHC.Generics (Generic, Generic1)

import Control.Arrow ( ArrowMonad(..), ArrowPlus(..), ArrowApply(..), Arrow(..), ArrowLoop(..), ArrowZero(..), ArrowChoice(..), Kleisli(..) )

-- infixr 5 <+>
-- infixr 3 ***
-- infixr 3 &&&
-- infixr 2 +++
-- infixr 2 |||
infixr 1 ^>>, >>^
infixr 1 ^<<, <<^

-- | The basic arrow class.
--
-- Instances should satisfy the following laws:
--
--  * @'arr' id = 'id'@
--
--  * @'arr' (f >>> g) = 'arr' f >>> 'arr' g@
--
--  * @'first' ('arr' f) = 'arr' ('first' f)@
--
--  * @'first' (f >>> g) = 'first' f >>> 'first' g@
--
--  * @'first' f >>> 'arr' 'fst' = 'arr' 'fst' >>> f@
--
--  * @'first' f >>> 'arr' ('id' *** g) = 'arr' ('id' *** g) >>> 'first' f@
--
--  * @'first' ('first' f) >>> 'arr' assoc = 'arr' assoc >>> 'first' f@
--
-- where
--
-- > assoc ((a,b),c) = (a,(b,c))
--
-- The other combinators have sensible default definitions,
-- which may be overridden for efficiency.

{-# RULES
"compose/arr"   forall f g .
                (arr f) . (arr g) = arr (f . g)
"first/arr"     forall f .
                first (arr f) = arr (first f)
"second/arr"    forall f .
                second (arr f) = arr (second f)
"product/arr"   forall f g .
                arr f *** arr g = arr (f *** g)
"fanout/arr"    forall f g .
                arr f &&& arr g = arr (f &&& g)
"compose/first" forall f g .
                (first f) . (first g) = first (f . g)
"compose/second" forall f g .
                (second f) . (second g) = second (f . g)
 #-}

-- Ordinary functions are arrows.

-- | @since 2.01
deriving instance Generic (Kleisli m a b)

-- | @since 4.14.0.0
deriving instance Generic1 (Kleisli m a)

-- | @since 4.14.0.0
-- deriving instance Functor m => Functor (Kleisli m a)
returnA :: Arrow a => a b b
returnA = arr id

-- | Precomposition with a pure function.
(^>>) :: Arrow a => (b -> c) -> a c d -> a b d
f ^>> a = arr f >>> a

-- | Postcomposition with a pure function.
(>>^) :: Arrow a => a b c -> (c -> d) -> a b d
a >>^ f = a >>> arr f

-- | Precomposition with a pure function (right-to-left variant).
(<<^) :: Arrow a => a c d -> (b -> c) -> a b d
a <<^ f = a <<< arr f

-- | Postcomposition with a pure function (right-to-left variant).
(^<<) :: Arrow a => (c -> d) -> a b c -> a b d
f ^<< a = arr f <<< a

{-# RULES
"left/arr"      forall f .
                left (arr f) = arr (left f)
"right/arr"     forall f .
                right (arr f) = arr (right f)
"sum/arr"       forall f g .
                arr f +++ arr g = arr (f +++ g)
"fanin/arr"     forall f g .
                arr f ||| arr g = arr (f ||| g)
"compose/left"  forall f g .
                left f . left g = left (f . g)
"compose/right" forall f g .
                right f . right g = right (f . g)
 #-}

-- | @since 2.01
-- | Any instance of 'ArrowApply' can be made into an instance of
--   'ArrowChoice' by defining 'left' = 'leftApp'.

leftApp :: ArrowApply a => a b c -> a (Either b d) (Either c d)
leftApp f = arr ((\b -> (arr (\() -> b) >>> f >>> arr Left, ())) |||
             (\d -> (arr (\() -> d) >>> arr Right, ()))) >>> app

-- | The 'loop' operator expresses computations in which an output value
-- is fed back as input, although the computation occurs only once.
-- It underlies the @rec@ value recursion construct in arrow notation.
-- 'loop' should satisfy the following laws:
--
-- [/extension/]
--      @'loop' ('arr' f) = 'arr' (\\ b -> 'fst' ('fix' (\\ (c,d) -> f (b,d))))@
--
-- [/left tightening/]
--      @'loop' ('first' h >>> f) = h >>> 'loop' f@
--
-- [/right tightening/]
--      @'loop' (f >>> 'first' h) = 'loop' f >>> h@
--
-- [/sliding/]
--      @'loop' (f >>> 'arr' ('id' *** k)) = 'loop' ('arr' ('id' *** k) >>> f)@
--
-- [/vanishing/]
--      @'loop' ('loop' f) = 'loop' ('arr' unassoc >>> f >>> 'arr' assoc)@
--
-- [/superposing/]
--      @'second' ('loop' f) = 'loop' ('arr' assoc >>> 'second' f >>> 'arr' unassoc)@
--
-- where
--
-- > assoc ((a,b),c) = (a,(b,c))
-- > unassoc (a,(b,c)) = ((a,b),c)
--
