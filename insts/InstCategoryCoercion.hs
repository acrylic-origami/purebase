-- Instance of class Category for Coercion ()
module InstCategoryCoercion.hs where
import qualified GHC.Base (id,(.))
import Data.Type.Coercion
import Data.Type.Equality
import Data.Coerce (coerce)


import Control.Category ( Category(..) )

  id = Coercion
  (.) Coercion = coerce

-- | Right-to-left composition
