import Data.Maybe
import Data.List


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

-- euklid 
eukleid :: Int -> Int -> (Int, Int, Int)
eukleid a 0 = (a, 1, 0)
eukleid a b = (c, u, v)
              where
                (c, u1, v1) = eukleid b (mod a b)
                u = v1
                v = u1 - (quot a b) * v1
                

inverse :: Int -> Int -> Maybe Int
inverse a n
  | g /= 1    = Nothing 
  | otherwise = Just (x `mod` n)
    where (g, x, _) = eukleid a n
       
inv :: Int -> Int 
inv a = fromJust (inverse a 26)

-- ciphers 
caesar :: String -> Int -> String
caesar mes z
  | z >= 0    = map (\y -> (intToChar x) + y) mes
  | otherwise = map (\y -> y - (intToChar x)) mes
  where x = abs z


encodeAffine :: String -> (Int, Int) -> String
encodeAffine mes (a, b) =  sub mes f
  where 
    f x = (intToChar a) * x + (intToChar b)
  
decodeAffine :: String -> (Int, Int) -> String
decodeAffine mes (a, b) = sub mes f
  where f x = (intToChar (inv a)) * (x - (intToChar b))

-- substitution 
sub :: String -> (Char -> Char) -> String
sub mes key = map key mes

subKey :: [(Char, Char)]
subKey = [('a', 'm'), ('b', 'n'), ('c', 'b'), ('d', 'v'), ('e', 'c'), ('f', 'x'), ('g', 'z'), ('h', 'l'), ('i', 'k'), ('j', 'j'), ('k', 'h'), ('l', 'g'), ('m', 'f'), ('n', 'd'), ('o', 's'), ('p', 'a'), ('q', 'p'), ('r', 'o'), ('s', 'i'), ('t', 'u'), ('u', 'y'), ('v', 't'), ('w', 'r'), ('x', 'e'), ('y', 'w'), ('z', 'q')]

inverseKey :: [(Char, Char)] -> [(Char, Char)]
inverseKey = map (\(a, b) -> (b, a))

-- Encryption function: f :: Char -> Char
f :: Char -> Char
f c = fromMaybe c $ lookup c subKey

-- Decryption function: f' :: Char -> Char
f' :: Char -> Char
f' c = fromMaybe c $ lookup c (inverseKey subKey)




    
main :: IO ()
main = do
  print $ caesar (caesar "hello" 3) (-3) -- hello
  
  print $ decodeAffine (encodeAffine "hello" (3, 4)) (3, 4)
  
  print $ sub (sub "hello" f) f'
  