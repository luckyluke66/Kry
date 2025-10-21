module Eukleid where

eukleid :: Int -> Int -> (Int, Int, Int)
eukleid a 0 = (a, 1, 0)
eukleid a b = (c, u, v)
            where
                (c, u1, v1) = eukleid b (mod a b)
                u = v1
                v = u1 - (quot a b) * v1
