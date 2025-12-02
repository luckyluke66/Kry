module Main where

import ClassicCiphers
import Eukleid

import CryptoAnalysis
import ModularAlgebra
import Polynom

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

    print $ plusMod2 3 4 

    -- ukol 8 

    let a = P [1,0,1,0,0,0,1,1,0]
    let b = P [0,1,0,1,1,0,0,1,1]

    print $ a + b

    let a1 = P [0,0,0,0,0,0,0,0,1]
    let b1 = P [0,0,1,0,0,1,0,1,1]

    print $ a1 * b1

    let a2 = P [0,0,0,0,0,0,0,1,0]
    let b2 = P [0,0,0,0,0,0,1,0,1]
    
    print $ a2 * b2
    print $ b2 * a2
