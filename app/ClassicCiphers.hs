module ClassicCiphers where

import ModularAlgebra
import Data.Maybe
import Eukleid


-- ciphers 
caesar :: String -> Int -> String
caesar msg z
    | z >= 0    = map (\y -> intToChar x + y) msg
    | otherwise = map (\y -> y - intToChar x) msg
        where x = abs z

encodeAffine :: String -> (Int, Int) -> String
encodeAffine msg (a, b) =  sub msg f
    where
        f x = (intToChar a) * x + (intToChar b)

decodeAffine :: String -> (Int, Int) -> String
decodeAffine msg (a, b) = sub msg f
    where f x = (intToChar (inv a)) * (x - (intToChar b))

-- substitution 
sub :: String -> (Char -> Char) -> String
sub msg key = map key msg

-- test key for substitution
subKey :: [(Char, Char)]
subKey = zip alphabet "mnbvcxzlkjhgfdsapoiuytrewq"
-- inverse key
inverseKey :: [(Char, Char)] -> [(Char, Char)]
inverseKey = map (\(a, b) -> (b, a))

-- Encryption function
f :: Char -> Char
f c = fromMaybe c $ lookup c subKey

-- Decryption function
f' :: Char -> Char
f' c = fromMaybe c $ lookup c (inverseKey subKey)

encodeVigenere :: String -> String -> String
encodeVigenere msg key = zipWith (+) msg (cycle key)

decodeVigenere :: String -> String -> String
decodeVigenere msg key = zipWith (-) msg (cycle key)
