-- Instance of class Applicative for Either ( e)
module InstApplicativeEithere where
import GHC.Base
import GHC.Show
import GHC.Read

-- $setup
-- Allow the use of some Prelude functions in doctests.
-- >>> import Prelude ( (+), (*), length, putStrLn )


import Data.Either ( Either(..) )

pure          = Right
Left  e <*> _ = Left e
Right f <*> r = fmap f r

-- | @since 4.4.0.0
