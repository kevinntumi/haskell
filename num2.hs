import Data.Char

isConIdIdentifier :: String -> Bool
isConIdIdentifier [] = False
isConIdIdentifier (head:tail) = isUpper head && isConIdTail tail

isConIdTail :: String -> Bool
isConIdTail = hasOnlySmallLargeDigitNSingleComma

hasOnlySmallLargeDigitNSingleComma :: String -> Bool
hasOnlySmallLargeDigitNSingleComma = all (\c -> isLower c || isUpper c || isDigit c || c == '\'')

main :: IO ()
main = do
    str <- getLine
    let prefixo = "O identificador de construtor é "
    putStrLn $ if isConIdIdentifier str then prefixo ++ "valido" else prefixo ++ "invalido"
