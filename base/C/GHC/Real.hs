{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE CPP, NoImplicitPrelude, MagicHash, UnboxedTuples, BangPatterns #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_HADDOCK not-home #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  GHC.Real
-- Copyright   :  (c) The University of Glasgow, 1994-2002
-- License     :  see libraries/base/LICENSE
--
-- Maintainer  :  cvs-ghc@haskell.org
-- Stability   :  internal
-- Portability :  non-portable (GHC Extensions)
--
-- The types 'Ratio' and 'Rational', and the classes 'Real', 'Fractional',
-- 'Integral', and 'RealFrac'.
--
-----------------------------------------------------------------------------

module C.GHC.Real where

#include "MachDeps.h"

import GHC.Base
import GHC.Num
import GHC.List
import GHC.Enum
import GHC.Show
import  GHC.Exception( divZeroException, overflowException
                                   , underflowException
                                   , ratioZeroDenomException )

#if defined(MIN_VERSION_integer_gmp)
import GHC.Integer.GMP.Internals
#endif

import GHC.Real ( Integral(..), RealFrac(..), Real(..), Fractional(..), Ratio(..) )

infixr 8  ^, ^^
-- infixl 7  /, `quot`, `rem`, `div`, `mod`
infixl 7  %

default ()              -- Double isn't available yet,
                        -- and we shouldn't be using defaults anyway

------------------------------------------------------------------------
-- Divide by zero and arithmetic overflow
------------------------------------------------------------------------

-- We put them here because they are needed relatively early
-- in the libraries before the Exception type has been defined yet.

{-# NOINLINE divZeroError #-}
divZeroError :: a
divZeroError = raise# divZeroException

{-# NOINLINE ratioZeroDenominatorError #-}
ratioZeroDenominatorError :: a
ratioZeroDenominatorError = raise# ratioZeroDenomException

{-# NOINLINE overflowError #-}
overflowError :: a
overflowError = raise# overflowException

{-# NOINLINE underflowError #-}
underflowError :: a
underflowError = raise# underflowException


--------------------------------------------------------------
-- The Ratio and Rational types
--------------------------------------------------------------

-- | Rational numbers, with numerator and denominator of some 'Integral' type.
--
-- Note that `Ratio`'s instances inherit the deficiencies from the type
-- parameter's. For example, @Ratio Natural@'s 'Num' instance has similar
-- problems to `Numeric.Natural.Natural`'s.
type  Rational          =  Ratio Integer

ratioPrec, ratioPrec1 :: Int
ratioPrec  = 7  -- Precedence of ':%' constructor
ratioPrec1 = ratioPrec + 1

infinity, notANumber :: Rational
infinity   = 1 :% 0
notANumber = 0 :% 0

-- Use :%, not % for Inf/NaN; the latter would
-- immediately lead to a runtime error, because it normalises.

-- | Forms the ratio of two integral numbers.
{-# SPECIALISE (%) :: Integer -> Integer -> Rational #-}
(%)                     :: (Integral a) => a -> a -> Ratio a

-- | Extract the numerator of the ratio in reduced form:
-- the numerator and denominator have no common factor and the denominator
-- is positive.
numerator       :: Ratio a -> a

-- | Extract the denominator of the ratio in reduced form:
-- the numerator and denominator have no common factor and the denominator
-- is positive.
denominator     :: Ratio a -> a


-- | 'reduce' is a subsidiary function used only in this module.
-- It normalises a ratio by dividing both numerator and denominator by
-- their greatest common divisor.
reduce ::  (Integral a) => a -> a -> Ratio a
{-# SPECIALISE reduce :: Integer -> Integer -> Rational #-}
reduce _ 0              =  ratioZeroDenominatorError
reduce x y              =  (x `quot` d) :% (y `quot` d)
                           where d = gcd x y

x % y                   =  reduce (x * signum y) (abs y)

numerator   (x :% _)    =  x
denominator (_ :% y)    =  y

--------------------------------------------------------------
-- Standard numeric classes
--------------------------------------------------------------

numericEnumFrom         :: (Fractional a) => a -> [a]
numericEnumFrom n       = go 0
  where
    -- See Note [Numeric Stability of Enumerating Floating Numbers]
    go !k = let !n' = n + k
             in n' : go (k + 1)

numericEnumFromThen     :: (Fractional a) => a -> a -> [a]
numericEnumFromThen n m = go 0
  where
    step = m - n
    -- See Note [Numeric Stability of Enumerating Floating Numbers]
    go !k = let !n' = n + k * step
             in n' : go (k + 1)

numericEnumFromTo       :: (Ord a, Fractional a) => a -> a -> [a]
numericEnumFromTo n m   = takeWhile (<= m + 1/2) (numericEnumFrom n)

numericEnumFromThenTo   :: (Ord a, Fractional a) => a -> a -> a -> [a]
numericEnumFromThenTo e1 e2 e3
    = takeWhile predicate (numericEnumFromThen e1 e2)
                                where
                                 mid = (e2 - e1) / 2
                                 predicate | e2 >= e1  = (<= e3 + mid)
                                           | otherwise = (>= e3 + mid)

{- Note [Numeric Stability of Enumerating Floating Numbers]
-----------------------------------------------------------
When enumerate floating numbers, we could add the increment to the last number
at every run (as what we did previously):

    numericEnumFrom n =  n `seq` (n : numericEnumFrom (n + 1))

This approach is concise and really fast, only needs an addition operation.
However when a floating number is large enough, for `n`, `n` and `n+1` will
have the same binary representation. For example (all number has type
`Double`):

    9007199254740990                 is: 0x433ffffffffffffe
    9007199254740990 + 1             is: 0x433fffffffffffff
    (9007199254740990 + 1) + 1       is: 0x4340000000000000
    ((9007199254740990 + 1) + 1) + 1 is: 0x4340000000000000

When we evaluate ([9007199254740990..9007199254740991] :: Double), we would
never reach the condition in `numericEnumFromTo`

    9007199254740990 + 1 + 1 + ... > 9007199254740991 + 1/2

We would fall into infinite loop (as reported in #15081).

To remedy the situation, we record the number of `1` that needed to be added
to the start number, rather than increasing `1` at every time. This approach
can improvement the numeric stability greatly at the cost of a multiplication.

Furthermore, we use the type of the enumerated number, `Fractional a => a`,
as the type of multiplier. In rare situations, the multiplier could be very
large and will lead to the enumeration to infinite loop, too, which should
be very rare. Consider the following example:

    [1..9007199254740994]

We could fix that by using an Integer as multiplier but we don't do that.
The benchmark on T7954.hs shows that this approach leads to significant
degeneration on performance (33% increase allocation and 300% increase on
elapsed time).

See #15081 and Phab:D4650 for the related discussion about this problem.
-}

--------------------------------------------------------------
-- Instances for Int
--------------------------------------------------------------

-- | @since 2.0.1
{-# RULES "fromRational/id" fromRational = id :: Rational -> Rational #-}
{-# NOINLINE [1] fromIntegral #-}
fromIntegral :: (Integral a, Num b) => a -> b
fromIntegral = fromInteger . toInteger

{-# RULES
"fromIntegral/Int->Int" fromIntegral = id :: Int -> Int
    #-}

{-# RULES
"fromIntegral/Int->Word"  fromIntegral = \(I# x#) -> W# (int2Word# x#)
"fromIntegral/Word->Int"  fromIntegral = \(W# x#) -> I# (word2Int# x#)
"fromIntegral/Word->Word" fromIntegral = id :: Word -> Word
    #-}

{-# RULES
"fromIntegral/Natural->Natural"  fromIntegral = id :: Natural -> Natural
"fromIntegral/Natural->Integer"  fromIntegral = toInteger :: Natural->Integer
"fromIntegral/Natural->Word"     fromIntegral = naturalToWord
  #-}

{-# RULES
"fromIntegral/Word->Natural"     fromIntegral = wordToNatural
"fromIntegral/Int->Natural"     fromIntegral = intToNatural
  #-}

-- | general coercion to fractional types
realToFrac :: (Real a, Fractional b) => a -> b
{-# NOINLINE [1] realToFrac #-}
realToFrac = fromRational . toRational

--------------------------------------------------------------
-- Overloaded numeric functions
--------------------------------------------------------------

-- | Converts a possibly-negative 'Real' value to a string.
showSigned :: (Real a)
  => (a -> ShowS)       -- ^ a function that can show unsigned values
  -> Int                -- ^ the precedence of the enclosing context
  -> a                  -- ^ the value to show
  -> ShowS
showSigned showPos p x
   | x < 0     = showParen (p > 6) (showChar '-' . showPos (-x))
   | otherwise = showPos x

even, odd       :: (Integral a) => a -> Bool
even n          =  n `rem` 2 == 0
odd             =  not . even
{-# INLINABLE even #-}
{-# INLINABLE odd  #-}

-------------------------------------------------------
-- | raise a number to a non-negative integral power
{-# SPECIALISE [1] (^) ::
        Integer -> Integer -> Integer,
        Integer -> Int -> Integer,
        Int -> Int -> Int #-}
{-# INLINABLE [1] (^) #-}    -- See Note [Inlining (^)]
(^) :: (Num a, Integral b) => a -> b -> a
x0 ^ y0 | y0 < 0    = errorWithoutStackTrace "Negative exponent"
        | y0 == 0   = 1
        | otherwise = f x0 y0
    where -- f : x0 ^ y0 = x ^ y
          f x y | even y    = f (x * x) (y `quot` 2)
                | y == 1    = x
                | otherwise = g (x * x) (y `quot` 2) x         -- See Note [Half of y - 1]
          -- g : x0 ^ y0 = (x ^ y) * z
          g x y z | even y = g (x * x) (y `quot` 2) z
                  | y == 1 = x * z
                  | otherwise = g (x * x) (y `quot` 2) (x * z) -- See Note [Half of y - 1]

-- | raise a number to an integral power
(^^)            :: (Fractional a, Integral b) => a -> b -> a
{-# INLINABLE [1] (^^) #-}         -- See Note [Inlining (^)
x ^^ n          =  if n >= 0 then x^n else recip (x^(negate n))

{- Note [Half of y - 1]
   ~~~~~~~~~~~~~~~~~~~~~
   Since y is guaranteed to be odd and positive here,
   half of y - 1 can be computed as y `quot` 2, optimising subtraction away.
-}

{- Note [Inlining (^)
   ~~~~~~~~~~~~~~~~~~~~~
   The INLINABLE pragma allows (^) to be specialised at its call sites.
   If it is called repeatedly at the same type, that can make a huge
   difference, because of those constants which can be repeatedly
   calculated.

   Currently the fromInteger calls are not floated because we get
             \d1 d2 x y -> blah
   after the gentle round of simplification. -}

{- Rules for powers with known small exponent
    see #5237
    For small exponents, (^) is inefficient compared to manually
    expanding the multiplication tree.
    Here, rules for the most common exponent types are given.
    The range of exponents for which rules are given is quite
    arbitrary and kept small to not unduly increase the number of rules.
    0 and 1 are excluded based on the assumption that nobody would
    write x^0 or x^1 in code and the cases where an exponent could
    be statically resolved to 0 or 1 are rare.

    It might be desirable to have corresponding rules also for
    exponents of other types (e. g., Word), but it's doubtful they
    would fire, since the exponents of other types tend to get
    floated out before the rule has a chance to fire.

    Also desirable would be rules for (^^), but I haven't managed
    to get those to fire.

    Note: Trying to save multiplications by sharing the square for
    exponents 4 and 5 does not save time, indeed, for Double, it is
    up to twice slower, so the rules contain flat sequences of
    multiplications.
-}

{-# RULES
"^2/Int"        forall x. x ^ (2 :: Int) = let u = x in u*u
"^3/Int"        forall x. x ^ (3 :: Int) = let u = x in u*u*u
"^4/Int"        forall x. x ^ (4 :: Int) = let u = x in u*u*u*u
"^5/Int"        forall x. x ^ (5 :: Int) = let u = x in u*u*u*u*u
"^2/Integer"    forall x. x ^ (2 :: Integer) = let u = x in u*u
"^3/Integer"    forall x. x ^ (3 :: Integer) = let u = x in u*u*u
"^4/Integer"    forall x. x ^ (4 :: Integer) = let u = x in u*u*u*u
"^5/Integer"    forall x. x ^ (5 :: Integer) = let u = x in u*u*u*u*u
  #-}

-------------------------------------------------------
-- Special power functions for Rational
--
-- see #4337
--
-- Rationale:
-- For a legitimate Rational (n :% d), the numerator and denominator are
-- coprime, i.e. they have no common prime factor.
-- Therefore all powers (n ^ a) and (d ^ b) are also coprime, so it is
-- not necessary to compute the greatest common divisor, which would be
-- done in the default implementation at each multiplication step.
-- Since exponentiation quickly leads to very large numbers and
-- calculation of gcds is generally very slow for large numbers,
-- avoiding the gcd leads to an order of magnitude speedup relatively
-- soon (and an asymptotic improvement overall).
--
-- Note:
-- We cannot use these functions for general Ratio a because that would
-- change results in a multitude of cases.
-- The cause is that if a and b are coprime, their remainders by any
-- positive modulus generally aren't, so in the default implementation
-- reduction occurs.
--
-- Example:
-- (17 % 3) ^ 3 :: Ratio Word8
-- Default:
-- (17 % 3) ^ 3 = ((17 % 3) ^ 2) * (17 % 3)
--              = ((289 `mod` 256) % 9) * (17 % 3)
--              = (33 % 9) * (17 % 3)
--              = (11 % 3) * (17 % 3)
--              = (187 % 9)
-- But:
-- ((17^3) `mod` 256) % (3^3)   = (4913 `mod` 256) % 27
--                              = 49 % 27
--
-- TODO:
-- Find out whether special-casing for numerator, denominator or
-- exponent = 1 (or -1, where that may apply) gains something.

-- Special version of (^) for Rational base
{-# RULES "(^)/Rational"    (^) = (^%^) #-}
(^%^)           :: Integral a => Rational -> a -> Rational
(n :% d) ^%^ e
    | e < 0     = errorWithoutStackTrace "Negative exponent"
    | e == 0    = 1 :% 1
    | otherwise = (n ^ e) :% (d ^ e)

-- Special version of (^^) for Rational base
{-# RULES "(^^)/Rational"   (^^) = (^^%^^) #-}
(^^%^^)         :: Integral a => Rational -> a -> Rational
(n :% d) ^^%^^ e
    | e > 0     = (n ^ e) :% (d ^ e)
    | e == 0    = 1 :% 1
    | n > 0     = (d ^ (negate e)) :% (n ^ (negate e))
    | n == 0    = ratioZeroDenominatorError
    | otherwise = let nn = d ^ (negate e)
                      dd = (negate n) ^ (negate e)
                  in if even e then (nn :% dd) else (negate nn :% dd)

-------------------------------------------------------
-- | @'gcd' x y@ is the non-negative factor of both @x@ and @y@ of which
-- every common factor of @x@ and @y@ is also a factor; for example
-- @'gcd' 4 2 = 2@, @'gcd' (-4) 6 = 2@, @'gcd' 0 4@ = @4@. @'gcd' 0 0@ = @0@.
-- (That is, the common divisor that is \"greatest\" in the divisibility
-- preordering.)
--
-- Note: Since for signed fixed-width integer types, @'abs' 'minBound' < 0@,
-- the result may be negative if one of the arguments is @'minBound'@ (and
-- necessarily is if the other is @0@ or @'minBound'@) for such types.
gcd             :: (Integral a) => a -> a -> a
{-# NOINLINE [1] gcd #-}
gcd x y         =  gcd' (abs x) (abs y)
                   where gcd' a 0  =  a
                         gcd' a b  =  gcd' b (a `rem` b)

-- | @'lcm' x y@ is the smallest positive integer that both @x@ and @y@ divide.
lcm             :: (Integral a) => a -> a -> a
{-# SPECIALISE lcm :: Int -> Int -> Int #-}
{-# SPECIALISE lcm :: Word -> Word -> Word #-}
{-# NOINLINE [1] lcm #-}
lcm _ 0         =  0
lcm 0 _         =  0
lcm x y         =  abs ((x `quot` (gcd x y)) * y)

{-# RULES
"gcd/Integer->Integer->Integer" gcd = gcdInteger
"lcm/Integer->Integer->Integer" lcm = lcmInteger
"gcd/Natural->Natural->Natural" gcd = gcdNatural
"lcm/Natural->Natural->Natural" lcm = lcmNatural
 #-}

#if defined(MIN_VERSION_integer_gmp)
-- GMP defines a more efficient Int# and Word# GCD

gcdInt' :: Int -> Int -> Int
gcdInt' (I# x) (I# y) = I# (gcdInt x y)

gcdWord' :: Word -> Word -> Word
gcdWord' (W# x) (W# y) = W# (gcdWord x y)

{-# RULES
"gcd/Int->Int->Int"             gcd = gcdInt'
"gcd/Word->Word->Word"          gcd = gcdWord'
 #-}

#endif

integralEnumFrom :: (Integral a, Bounded a) => a -> [a]
integralEnumFrom n = map fromInteger [toInteger n .. toInteger (maxBound `asTypeOf` n)]

integralEnumFromThen :: (Integral a, Bounded a) => a -> a -> [a]
integralEnumFromThen n1 n2
  | i_n2 >= i_n1  = map fromInteger [i_n1, i_n2 .. toInteger (maxBound `asTypeOf` n1)]
  | otherwise     = map fromInteger [i_n1, i_n2 .. toInteger (minBound `asTypeOf` n1)]
  where
    i_n1 = toInteger n1
    i_n2 = toInteger n2

integralEnumFromTo :: Integral a => a -> a -> [a]
integralEnumFromTo n m = map fromInteger [toInteger n .. toInteger m]

integralEnumFromThenTo :: Integral a => a -> a -> a -> [a]
integralEnumFromThenTo n1 n2 m
  = map fromInteger [toInteger n1, toInteger n2 .. toInteger m]
