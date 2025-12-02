module Polynom where
import  Data.Maybe
import Data.List

plusMod2 :: Int -> Int -> Int
plusMod2 x y = (x + y) `mod` 2

irrPoly :: [Int]
irrPoly = [1, 0, 0, 0, 1, 1, 0, 1, 1]

newtype Polynom = P {
    coefficients :: [Int]
} deriving(Show)

instance Num Polynom where
    (+) (P coeffs1) (P coeffs2) = P (zipWith plusMod2 coeffs1 coeffs2)
    (-) = (+)
    (*) (P a) (P b) = P $ polyMod (mulMod2 a b) irrPoly
    abs = id
    signum = id
    fromInteger x = P [0, 0 ,0, 0, 0, 0, 0, 0, fromIntegral x]



xor2 :: Int -> Int -> Int
xor2 a b = (a + b) `mod` 2

trim :: [Int] -> [Int]
trim xs = let t = dropWhile (==0) xs
          in if null t then [0] else t

pad9 :: [Int] -> [Int]
pad9 xs = replicate (9 - length xs) 0 ++ xs

polyMod :: [Int] -> [Int] -> [Int]
polyMod a b = pad9 (go (trim a))
  where
    lb = length (trim b)
    go r
      | length r < lb = r
      | otherwise =
          let shift = length r - lb
              sub   = replicate shift 0 ++ trim b
              r'    = trim (zipWith xor2 r sub)
          in go r'

mulMod2 :: [Int] -> [Int] -> [Int]
mulMod2 xs ys =
    let lx = length xs
        ly = length ys
    in [ sum [ xs !! i * ys !! (k-i)
             | i <- [0 .. lx-1]
             , let j = k - i
             , j >= 0, j < ly
             ] `mod` 2
       | k <- [0 .. lx + ly - 2]
       ]