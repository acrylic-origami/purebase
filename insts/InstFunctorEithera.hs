-- Instance of class Functor for Either ( a)
module InstFunctorEithera where
import GHC.Base
import GHC.Show
import GHC.Read

-- $setup
-- Allow the use of some Prelude functions in doctests.
-- >>> import Prelude ( (+), (*), length, putStrLn )


import Data.Either ( Either(..) )

fmap _ (Left x) = Left x
fmap f (Right y) = Right (f y)

-- | @since 4.9.0.0
