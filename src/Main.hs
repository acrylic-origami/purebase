module Main where

import DynFlags
import GHC
import GHC.Paths ( libdir )
import Control.Monad ( void )
  
main :: IO ()
main = void $ runGhc (Just libdir) $ do
  dflags <- getSessionDynFlags
  let inc_paths = includePaths dflags
  setSessionDynFlags $ dflags { includePaths = inc_paths { includePathsGlobal = "base/C/include/":(includePathsGlobal inc_paths) }, importPaths = "hiddens":"base/":(importPaths dflags) }
  
  target <- guessTarget ("target/A.hs") Nothing
  setTargets [target] 
  load LoadAllTargets