-- Instance of class Eq for MVar ( a)
module InstEqMVara.hs where
import GHC.Base


import GHC.MVar ( MVar(..) )

        (MVar mvar1#) == (MVar mvar2#) = isTrue# (sameMVar# mvar1# mvar2#)

