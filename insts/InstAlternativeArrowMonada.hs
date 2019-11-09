-- Instance of class Alternative for ArrowMonad (a)
module InstAlternativeArrowMonada where
import Data.Tuple ( fst, snd, uncurry )
import Data.Either
import Control.Monad.Fix
import Control.Category
import GHC.Base hiding ( (.), id )
import GHC.Generics (Generic, Generic1)


import Control.Arrow ( ArrowMonad(..), ArrowZero(..), (<+>), zeroArrow )

empty :: ArrowZero a => ArrowMonad a b
empty = ArrowMonad zeroArrow
ArrowMonad x <|> ArrowMonad y = ArrowMonad (x <+> y)

-- | @since 4.6.0.0
