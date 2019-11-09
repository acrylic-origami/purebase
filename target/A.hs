module A () where
import C.Union

foo = putStrLn $ show $ (+1) <$> ([3] ++ (1 : [2]) ++ [4])
bar = (do Nothing) >>= Just