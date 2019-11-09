{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Complex
-- Copyright   :  (c) The University of Glasgow 2001
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  libraries@haskell.org
-- Stability   :  provisional
-- Portability :  portable
--
-- Complex numbers.
--
-----------------------------------------------------------------------------

module C.Data.Complex
        (
        -- * Rectangular form
          Complex((:+))

        , realPart
        , imagPart
        -- * Polar form
        , mkPolar
        , cis
        , polar
        , magnitude
        , phase
        -- * Conjugate
        , conjugate

        )  where

import GHC.Base (Applicative (..))
import GHC.Generics (Generic, Generic1)
import GHC.Float (Floating(..))
import Data.Data (Data)
import Foreign (Storable, castPtr, peek, poke, pokeElemOff, peekElemOff, sizeOf,
                alignment)

import Data.Complex ( Complex(..) )

-- infix  6  :+

-- -----------------------------------------------------------------------------
-- The Complex type

-- | Complex numbers are an algebraic type.
--
-- For a complex number @z@, @'abs' z@ is a number with the magnitude of @z@,
-- but oriented in the positive real direction, whereas @'signum' z@
-- has the phase of @z@, but unit magnitude.
--
-- The 'Foldable' and 'Traversable' instances traverse the real part first.
--
-- Note that `Complex`'s instances inherit the deficiencies from the type
-- parameter's. For example, @Complex Float@'s 'Ord' instance has similar
-- problems to `Float`'s.
realPart :: Complex a -> a
realPart (x :+ _) =  x

-- | Extracts the imaginary part of a complex number.
imagPart :: Complex a -> a
imagPart (_ :+ y) =  y

-- | The conjugate of a complex number.
{-# SPECIALISE conjugate :: Complex Double -> Complex Double #-}
conjugate        :: Num a => Complex a -> Complex a
conjugate (x:+y) =  x :+ (-y)

-- | Form a complex number from polar components of magnitude and phase.
{-# SPECIALISE mkPolar :: Double -> Double -> Complex Double #-}
mkPolar          :: Floating a => a -> a -> Complex a
mkPolar r theta  =  r * cos theta :+ r * sin theta

-- | @'cis' t@ is a complex value with magnitude @1@
-- and phase @t@ (modulo @2*'pi'@).
{-# SPECIALISE cis :: Double -> Complex Double #-}
cis              :: Floating a => a -> Complex a
cis theta        =  cos theta :+ sin theta

-- | The function 'polar' takes a complex number and
-- returns a (magnitude, phase) pair in canonical form:
-- the magnitude is nonnegative, and the phase in the range @(-'pi', 'pi']@;
-- if the magnitude is zero, then so is the phase.
{-# SPECIALISE polar :: Complex Double -> (Double,Double) #-}
polar            :: (RealFloat a) => Complex a -> (a,a)
polar z          =  (magnitude z, phase z)

-- | The nonnegative magnitude of a complex number.
{-# SPECIALISE magnitude :: Complex Double -> Double #-}
magnitude :: (RealFloat a) => Complex a -> a
magnitude (x:+y) =  scaleFloat k
                     (sqrt (sqr (scaleFloat mk x) + sqr (scaleFloat mk y)))
                    where k  = max (exponent x) (exponent y)
                          mk = - k
                          sqr z = z * z

-- | The phase of a complex number, in the range @(-'pi', 'pi']@.
-- If the magnitude is zero, then so is the phase.
{-# SPECIALISE phase :: Complex Double -> Double #-}
phase :: (RealFloat a) => Complex a -> a
phase (0 :+ 0)   = 0            -- SLPJ July 97 from John Peterson
phase (x:+y)     = atan2 y x


-- -----------------------------------------------------------------------------
-- Instances of Complex

-- | @since 2.01
{-# RULES

"realToFrac/a->Complex Double"
  realToFrac = \x -> realToFrac x :+ (0 :: Double)

"realToFrac/a->Complex Float"
  realToFrac = \x -> realToFrac x :+ (0 :: Float)

  #-}
