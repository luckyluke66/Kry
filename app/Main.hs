module Main where

import ClassicCiphers
import Eukleid

import CryptoAnalysis
import ModularAlgebra

main :: IO ()
main = do
    -- ukol1
    let list = [(6, 3), (10, 11), (2, 48), (64, 16), (105, 25)]
    mapM_ (print . (uncurry eukleid)) list


    -- ukol 2 - 5
    print $ caesar (caesar "hello" 3) (-3)
  
    print $ decodeAffine (encodeAffine "hello" (3, 4)) (3, 4)
  
    print $ sub (sub "hello" f) f'

    print $ decodeVigenere (encodeVigenere "hello" "cryptii") "cryptii" 

    -- ukol 6 - 7

    print $ coincidenceIndex "When he had to picnic on the beach, he purposely put sand in other people’s food."

    print $ coincidenceIndex "The white water rafting trip was suddenly halted by the unexpected brick wall."

    print $ coincidenceIndex "qwertyuiopasdfghjklzxcvbnm"

    print $ kasiski "abababababab" 3

    print $ kasiski "abdfrabdghcdabdhjkabd" 3
