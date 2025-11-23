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

    print $ coincidenceIndex "thisisanexampleofenglishtextwithnormalfrequencies"

    print $ coincidenceIndex "thequickbrownfoxjumpsoverthelazydogandthisisarepeatedenglishtextexample"

    print $ coincidenceIndex "qwertyuiopasdfghjklzxcvbnm"

    print $ kasiski "abababababab" 3

    print $ kasiski "abdfrabdghcdabdhjkabd" 3
