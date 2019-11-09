-- Instance of class Floating for Complex (a)
module InstFloatingComplexa.hs where
import GHC.Base (Applicative (..))
import GHC.Generics (Generic, Generic1)
import GHC.Float (Floating(..))
import Data.Data (Data)
import Foreign (Storable, castPtr, peek, poke, pokeElemOff, peekElemOff, sizeOf,
                alignment)


import Data.Complex ( Complex(..) )

    {-# SPECIALISE instance Floating (Complex Float) #-}
    {-# SPECIALISE instance Floating (Complex Double) #-}
    pi             =  pi :+ 0
    exp (x:+y)     =  expx * cos y :+ expx * sin y
                      where expx = exp x
    log z          =  log (magnitude z) :+ phase z

    x ** y = case (x,y) of
      (_ , (0:+0))  -> 1 :+ 0
      ((0:+0), (exp_re:+_)) -> case compare exp_re 0 of
                 GT -> 0 :+ 0
                 LT -> inf :+ 0
                 EQ -> nan :+ nan
      ((re:+im), (exp_re:+_))
        | (isInfinite re || isInfinite im) -> case compare exp_re 0 of
                 GT -> inf :+ 0
                 LT -> 0 :+ 0
                 EQ -> nan :+ nan
        | otherwise -> exp (log x * y)
      where
        inf = 1/0
        nan = 0/0

    sqrt (0:+0)    =  0
    sqrt z@(x:+y)  =  u :+ (if y < 0 then -v else v)
                      where (u,v) = if x < 0 then (v',u') else (u',v')
                            v'    = abs y / (u'*2)
                            u'    = sqrt ((magnitude z + abs x) / 2)

    sin (x:+y)     =  sin x * cosh y :+ cos x * sinh y
    cos (x:+y)     =  cos x * cosh y :+ (- sin x * sinh y)
    tan (x:+y)     =  (sinx*coshy:+cosx*sinhy)/(cosx*coshy:+(-sinx*sinhy))
                      where sinx  = sin x
                            cosx  = cos x
                            sinhy = sinh y
                            coshy = cosh y

    sinh (x:+y)    =  cos y * sinh x :+ sin  y * cosh x
    cosh (x:+y)    =  cos y * cosh x :+ sin y * sinh x
    tanh (x:+y)    =  (cosy*sinhx:+siny*coshx)/(cosy*coshx:+siny*sinhx)
                      where siny  = sin y
                            cosy  = cos y
                            sinhx = sinh x
                            coshx = cosh x

    asin z@(x:+y)  =  y':+(-x')
                      where  (x':+y') = log (((-y):+x) + sqrt (1 - z*z))
    acos z         =  y'':+(-x'')
                      where (x'':+y'') = log (z + ((-y'):+x'))
                            (x':+y')   = sqrt (1 - z*z)
    atan z@(x:+y)  =  y':+(-x')
                      where (x':+y') = log (((1-y):+x) / sqrt (1+z*z))

    asinh z        =  log (z + sqrt (1+z*z))
    -- Take care to allow (-1)::Complex, fixing #8532
    acosh z        =  log (z + (sqrt $ z+1) * (sqrt $ z-1))
    atanh z        =  0.5 * log ((1.0+z) / (1.0-z))

    log1p x@(a :+ b)
      | abs a < 0.5 && abs b < 0.5
      , u <- 2*a + a*a + b*b = log1p (u/(1 + sqrt(u+1))) :+ atan2 (1 + a) b
      | otherwise = log (1 + x)
    {-# INLINE log1p #-}

    expm1 x@(a :+ b)
      | a*a + b*b < 1
      , u <- expm1 a
      , v <- sin (b/2)
      , w <- -2*v*v = (u*w + u + w) :+ (u+1)*sin b
      | otherwise = exp x - 1
    {-# INLINE expm1 #-}

-- | @since 4.8.0.0
