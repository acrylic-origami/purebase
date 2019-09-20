{-# LANGUAGE NoImplicitPrelude #-}

module A () where
import C.Prelude  

foo = putStrLn $ show $ (+1) <$> ([3] ++ (1 : [2]) ++ [4])
bar = (do Nothing) >>= Just