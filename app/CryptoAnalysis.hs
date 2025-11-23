module CryptoAnalysis where

import ModularAlgebra
import Data.Foldable (Foldable(length))
import Data.List


lettersOnly:: String -> String
lettersOnly = filter (`elem` alphabet)

countLetter :: Eq a => a -> [a] -> Int
countLetter letter = length . filter (==letter)

coincidenceIndex :: String -> Double
coincidenceIndex raw 
    | denom == 0 = 0 
    | otherwise     = numer / denom
        where 
            cg    = lettersOnly raw
            freqs = map (\c -> fromIntegral (countLetter c cg) :: Double) alphabet
            n     = length cg
            numer = sum $ (map (\x -> x * (x - 1)) freqs)
            denom = fromIntegral (n * (n - 1)) :: Double


rmdups :: (Ord a) => [a] -> [a]
rmdups = map head . group . sort

substringsOfLen :: String -> Int -> [(String, Int)]
substringsOfLen cg n =
    [(take n t, i) | (t, i) <- zip (tails cg) [0..], length t >= n]

groupSubs :: [(String, Int)] -> [(String, [Int])]
groupSubs xs = zip names dists
    where
        dists = map (map snd) $ groupBy (\(x1, _) (x2, _) -> x1 == x2) xs
        names = rmdups $ map fst xs

distances :: [Int] -> [Int]
distances xs = 
    zipWith (-) (tail xs) xs 

kasiski :: String -> Int -> [(String, [Int])]
kasiski raw n = [(s, distances ds) | (s, ds) <- groupSubs subs, length ds > 1]
    where
        cg     = lettersOnly raw
        subs   = sortOn fst $ substringsOfLen cg n

