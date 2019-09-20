{-# LANGUAGE NoImplicitPrelude #-}

module C.GHC.Base (Maybe, Semigroup, Monoid) where

import C.GHC.Maybe (Maybe)
import GHC.Types ()

class Semigroup a
class Monoid a
