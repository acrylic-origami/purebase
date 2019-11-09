-- Instance of class Semigroup for Either ( a b)
module InstSemigroupEitherab where
import GHC.Base
import GHC.Show
import GHC.Read

-- $setup
-- Allow the use of some Prelude functions in doctests.
-- >>> import Prelude ( (+), (*), length, putStrLn )


import Data.Either ( Either(..) )

Left _ <> b = b
a      <> _ = a
#if !defined(__HADDOCK_VERSION__)
    -- workaround https://github.com/haskell/haddock/issues/680
stimes n x
      | n <= 0 = errorWithoutStackTrace "stimes: positive multiplier expected"
      | otherwise = x
#endif

-- | @since 3.0
