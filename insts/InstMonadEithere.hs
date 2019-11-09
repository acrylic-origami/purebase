-- Instance of class Monad for Either ( e)
module InstMonadEithere where
import GHC.Base
import GHC.Show
import GHC.Read

-- $setup
-- Allow the use of some Prelude functions in doctests.
-- >>> import Prelude ( (+), (*), length, putStrLn )


import Data.Either ( Either(..) )

Left  l >>= _ = Left l
Right r >>= k = k r

-- | Case analysis for the 'Either' type.
-- If the value is @'Left' a@, apply the first function to @a@;
-- if it is @'Right' b@, apply the second function to @b@.
--
-- ==== __Examples__
--
-- We create two values of type @'Either' 'String' 'Int'@, one using the
-- 'Left' constructor and another using the 'Right' constructor. Then
-- we apply \"either\" the 'Prelude.length' function (if we have a 'String')
-- or the \"times-two\" function (if we have an 'Int'):
--
-- >>> let s = Left "foo" :: Either String Int
-- >>> let n = Right 3 :: Either String Int
-- >>> either length (*2) s
-- 3
-- >>> either length (*2) n
-- 6
--
