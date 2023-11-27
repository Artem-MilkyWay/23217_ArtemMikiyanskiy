module Main (main) where

import System.Environment
import Data.List
import Data.Char
 
main :: IO ()
main = do
  args <- getArgs
  case args of
    [mode, key, filepath] -> do
      content <- readFile filepath
      let res = case mode of
                     "e" -> encrypt key content
                     "d" -> decrypt key content     
      putStrLn res 
                      
encrypt :: String -> String -> String       -- шифрование 
encrypt char key = vigenere encryptChar char key

decrypt :: String -> String -> String      -- обратное шифрование 
decrypt char key = vigenere decryptChar char key

vigenere :: ((Char, Char) -> Char) -> String -> String -> String          
vigenere func key str = zipWith(\x k -> if isLetterOrDigit x then func (toUpper x, toUpper k) else ' ') str (cycle key)

decryptChar :: (Char, Char) -> Char -- сдвиг влево
decryptChar = shiftChar (-1)
encryptChar :: (Char,Char) -> Char -- сдвиг вправо
encryptChar = shiftChar 1

isLetterOrDigit :: Char -> Bool
isLetterOrDigit c = isLetter c || isDigit c

shiftChar :: Int -> (Char, Char) -> Char 
shiftChar multiplier (stringChar, keyChar)
      | isLetter stringChar = chr $ ((ord stringChar + multiplier * ord keyChar - 2 * ord 'A') `mod` 26) + ord 'A'
      | otherwise = ' '