import Data.Char
import Control.Monad.RWS.Class (MonadState(put))

isConIdIdentifier :: String -> Bool
isConIdIdentifier str = not (null str) && isUpper (head str) && isConNVarIdTail (tail str)

isConNVarIdTail :: String -> Bool
isConNVarIdTail = all (\c -> isLower c || isUpper c || isDigit c || c == '\'')

isVarSymTail :: String -> Bool
isVarSymTail = all (\c -> (isSymbol c && c /= '-') || c == ':')

isVarSymIdentifier :: String -> Bool
isVarSymIdentifier str = not (null str) && isSymbol (head str) && isVarSymTail (tail str) && not (isReservedOp str)

isVarIdIdentifier :: String -> Bool
isVarIdIdentifier str = not (null str) && isLower (head str) && isConNVarIdTail (tail str) && not (isReservedId str)

isConSymIdentifier :: String -> Bool
isConSymIdentifier str = not (null str) && head str == ':' && isConSymTail (tail str) && not (isReservedOp str)

isConSymTail :: String -> Bool
isConSymTail = all (\c -> isSymbol c || c == ':')

isReservedId :: String -> Bool
isReservedId str = str `elem` getReservedIdentifiers

isReservedOp :: String -> Bool
isReservedOp str = str `elem` getReservedOperators

getReservedOperators :: [String]
getReservedOperators = ["..", ":", "::", "=", "\\", "|", "<-", "->", "@", "~", "=>"]

getReservedIdentifiers :: [String]
getReservedIdentifiers = ["case", "class", "data", "default", "deriving", "do", "else", "if", "import", "in", "infix", "infixl", "infixr", "instance", "let", "module", "newtype", "of", "then", "type", "where", "foreign", "forall", "mdo", "family", "role", "pattern", "static", "group", "by", "using", "qualified", "as", "hiding"]
    
classificarLexema :: String -> IO ()
classificarLexema str
    | isConIdIdentifier str = putStrLn "ConId"
    | isVarSymIdentifier str = putStrLn "VarSym"
    | isVarIdIdentifier str = putStrLn "VarId"
    | isConSymIdentifier str = putStrLn "ConSym"
    | otherwise = putStrLn "Não é um identificador válido."

main :: IO ()
main = do
    putStrLn "Digite um lexema para classificar:"
    str <- getLine
    classificarLexema str
