module ModularAlgebra where


import Data.Maybe
import Data.List
import Eukleid
-- definition for numbers 

instance Num Char where
  (+) x y = intToChar $ (charToInt x + charToInt y) `mod` 26
  (*) x y = intToChar $ (charToInt x * charToInt y) `mod` 26
  (-) x y = intToChar $ (charToInt x - charToInt y) `mod` 26
  fromInteger = intToChar . fromInteger
  -- for this instance the following functions dont make sence 
  abs a = a
  signum a = a

alphabet :: String
alphabet = take 26 ['a'..]

charToInt :: Char -> Int
charToInt char = fromJust $ elemIndex char alphabet

intToChar :: Int -> Char
intToChar i = alphabet !! (abs i)

inverse :: Int -> Int -> Maybe Int
inverse a n
  | g /= 1    = Nothing 
  | otherwise = Just (x `mod` n)
    where (g, x, _) = eukleid a n
       
inv :: Int -> Int 
inv a = fromJust (inverse a 26)