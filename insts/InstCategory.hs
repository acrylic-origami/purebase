-- Instance of class Category for  (:~~:)
module InstCategory.hs where
import qualified GHC.Base (id,(.))
import Data.Type.Coercion
import Data.Type.Equality
import Data.Coerce (coerce)


import Control.Category ( Category(..) )

  id            = HRefl
  HRefl . HRefl = HRefl

-- | @since 4.7.0.0
