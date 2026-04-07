import Data.Char

isConIdIdentifier :: String -> Bool
isConIdIdentifier str = not (null str) && isUpper (head str) && isConIdTail (tail str)

isConIdTail :: String -> Bool
isConIdTail = hasOnlySmallLargeDigitNSingleComma

hasOnlySmallLargeDigitNSingleComma :: String -> Bool
hasOnlySmallLargeDigitNSingleComma str = length str - lowerCaseLength str - upperCaseLength str - digitLength str - singleCommasLength str == 0

lowerCaseLength :: String -> Int
lowerCaseLength str = length (filter isLower str)

upperCaseLength :: String -> Int
upperCaseLength str = length (filter isUpper str)

digitLength :: String -> Int
digitLength str = length (filter isDigit str)

singleCommasLength :: String -> Int
singleCommasLength str = length (filter (== '\'') str)

main :: IO ()
main = do
    str <- getLine
    let prefixo = "O identificador de construtor é "
    putStrLn $ if isConIdIdentifier str then prefixo ++ "valido" else prefixo ++ "invalido"