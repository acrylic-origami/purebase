-- Instance of class Floating for Complex (a)
module InstFloatingComplexa where
import GHC.Base (Applicative (..))
import GHC.Generics (Generic, Generic1)
import GHC.Float (Floating(..))
import Data.Data (Data)
import Foreign (Storable, castPtr, peek, poke, pokeElemOff, peekElemOff, sizeOf,
  alignment)


import Data.Complex ( Complex(..), magnitude, phase )

pi             =  GHC.Float.pi :+ 0
exp (x:+y)     =  expx * GHC.Float.cos y :+ expx * GHC.Float.sin y
  where expx = GHC.Float.exp x
log z          =  GHC.Float.log (magnitude z) :+ phase z

-- x ** y = case (x,y) of
--   (_ , (0:+0))  -> 1 :+ 0
--   ((0:+0), (exp_re:+_)) -> case compare exp_re 0 of
--     GT -> 0 :+ 0
--     LT -> inf :+ 0
--     EQ -> nan :+ nan
--   ((re:+im), (exp_re:+_))
--           | (isInfinite re || isInfinite im) -> case compare exp_re 0 of
--             GT -> inf :+ 0
--             LT -> 0 :+ 0
--             EQ -> nan :+ nan
--           | otherwise -> GHC.Float.exp (GHC.Float.log x * y)
--   where
--     inf = 1/0
--     nan = 0/0

--     sqrt (0:+0)    =  0
--     sqrt z@(x:+y)  =  u :+ (if y < 0 then -v else v)
--       where (u,v) = if x < 0 then (v',u') else (u',v')
--             v'    = abs y / (u'*2)
--             u'    = sqrt ((magnitude z + abs x) / 2)
-- TODO

sin (x:+y)     =  GHC.Float.sin x * GHC.Float.cosh y :+ GHC.Float.cos x * GHC.Float.sinh y
cos (x:+y)     =  GHC.Float.cos x * GHC.Float.cosh y :+ (- GHC.Float.sin x * GHC.Float.sinh y)
tan (x:+y)     =  (sinx*coshy:+cosx*sinhy)/(cosx*coshy:+(-sinx*sinhy))
  where sinx  = GHC.Float.sin x
        cosx  = GHC.Float.cos x
        sinhy = GHC.Float.sinh y
        coshy = GHC.Float.cosh y

sinh (x:+y)    =  GHC.Float.cos y * GHC.Float.sinh x :+ GHC.Float.sin  y * GHC.Float.cosh x
cosh (x:+y)    =  GHC.Float.cos y * GHC.Float.cosh x :+ GHC.Float.sin y * GHC.Float.sinh x
tanh (x:+y)    =  (cosy*sinhx:+siny*coshx)/(cosy*coshx:+siny*sinhx)
  where siny  = GHC.Float.sin y
        cosy  = GHC.Float.cos y
        sinhx = GHC.Float.sinh x
        coshx = GHC.Float.cosh x

asin z@(x:+y)  =  y':+(-x')
  where  (x':+y') = GHC.Float.log (((-y):+x) + sqrt (1 - z*z))
acos z         =  y'':+(-x'')
  where (x'':+y'') = GHC.Float.log (z + ((-y'):+x'))
        (x':+y')   = sqrt (1 - z*z)
atan z@(x:+y)  =  y':+(-x')
  where (x':+y') = GHC.Float.log (((1-y):+x) / sqrt (1+z*z))

asinh z        =  GHC.Float.log (z + sqrt (1+z*z))
    -- Take care to allow (-1)::Complex, fixing #8532
acosh z        =  GHC.Float.log (z + (sqrt $ z+1) * (sqrt $ z-1))
atanh z        =  0.5 * GHC.Float.log ((1.0+z) / (1.0-z))

-- log1p x@(a :+ b)
--       | abs a < 0.5 && abs b < 0.5
--       , u <- 2*a + a*a + b*b = InstFloatingComplexa.log1p (u/(1 + sqrt(u+1))) :+ atan2 (1 + a) b
--       | otherwise = GHC.Float.log (1 + x)
-- {-# INLINE log1p #-}
-- TODO

-- expm1 x@(a :+ b)
--       | a*a + b*b < 1
--       , u <- InstFloatingComplexa.expm1 a
--       , v <- GHC.Float.sin (b/2)
--       , w <- -2*v*v = (u*w + u + w) :+ (u+1)*GHC.Float.sin b
--       | otherwise = GHC.Float.exp x - 1
-- {-# INLINE expm1 #-}
-- TODO

-- | @since 4.8.0.0
