module Utils(permitirCaracteresUTF8, regexOperador, regexOperando, ehOperando, ehOperacao, ehExpressao, trim) where
import System.IO (hSetEncoding, stdin, stdout, stderr, utf8) 
import Text.Regex.TDFA ((=~))
import Data.Char (isSpace)
import Data.List (dropWhileEnd)

permitirCaracteresUTF8 :: IO()
permitirCaracteresUTF8 = do
    hSetEncoding stdin utf8
    hSetEncoding stdout utf8
    hSetEncoding stderr utf8

regexOperador :: String
regexOperador = "[-+*/]"

regexLetra :: String
regexLetra = "[a-zA-Z]"

regexDigito :: String
regexDigito = "[0-9]"

regexOperando :: String
regexOperando = "(" ++ regexLetra ++ "+[0-9]*|[0-9]+)"

regexOperacao :: String
regexOperacao = regexOperando ++ regexOperador ++ regexOperando

regexExpressao :: String
regexExpressao = regexOperando ++ "(" ++ regexOperador ++ regexOperando ++ ")+"

ehOperando :: String -> Bool
ehOperando s
    | null s = error "A string não pode ser vazia."
    | otherwise = s =~ ("^" ++ regexOperando ++ "$")

ehOperacao :: String -> Bool
ehOperacao s
    | null s = error "A string não pode ser vazia."
    | otherwise = s =~ ("^" ++ regexOperacao ++ "$")

ehExpressao :: String -> Bool
ehExpressao s
    | null s = error "A string não pode ser vazia."
    | otherwise = trim s =~ ("^" ++ regexExpressao ++ "$")

trim :: String -> String
trim str
    | null str = error  "A string não pode ser vazia."
    | otherwise = filter (not . isSpace) str