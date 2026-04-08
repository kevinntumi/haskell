import Data.Char (isUpper, isLower, isDigit)
import Control.Monad (when)

isConIdIdentifier :: String -> Bool
isConIdIdentifier str = not (null str) && isUpper (head str) && isConNVarIdTail (tail str)

isConNVarIdTail :: String -> Bool
isConNVarIdTail = all (\c -> isSmall c || isUpper c || isDigit c || c == '\'')

isVarSymTail :: String -> Bool
isVarSymTail = all isSymbol

isVarSymIdentifier :: String -> Bool
isVarSymIdentifier str = not (null str) && isSymbol (head str) && isVarSymTail (tail str) && not (isReservedOp str)

isVarIdIdentifier :: String -> Bool
isVarIdIdentifier str = not (null str) && isSmall (head str) && isConNVarIdTail (tail str) && not (isReservedId str)

isConSymIdentifier :: String -> Bool
isConSymIdentifier str = not (null str) && head str == ':' && isConSymTail (tail str) && not (isReservedOp str)

isConSymTail :: String -> Bool
isConSymTail = all (\c -> isSymbol c || c == ':')

isSmall :: Char -> Bool
isSmall c = isLower c || c == '_'

isReservedId :: String -> Bool
isReservedId str = str `elem` getReservedIdentifiers

isSymbol :: Char -> Bool
isSymbol c = c `elem` symbols

isReservedOp :: String -> Bool
isReservedOp str = str `elem` reservedOperators

reservedOperators :: [String]
reservedOperators = 
    [
        "..", 
        ":", 
        "::", 
        "=", 
        "\\", 
        "|", 
        "<-", 
        "->", 
        "@", 
        "~", 
        "=>"
    ]

getReservedIdentifiers :: [String]
getReservedIdentifiers = 
    [
        "case", 
        "class", 
        "data", 
        "default", 
        "deriving", 
        "do", 
        "else", 
        "if", 
        "import", 
        "in", 
        "infix", 
        "infixl", 
        "infixr", 
        "instance", 
        "let", 
        "module", 
        "newtype", 
        "of", 
        "then", 
        "type", 
        "where", 
        "foreign", 
        "forall", 
        "MDo", 
        "family", 
        "role", 
        "pattern", 
        "static", 
        "group", 
        "by", 
        "using", 
        "qualified", 
        "as", 
        "hiding"
    ]

symbols :: [Char]
symbols = "!#$%&*+./<=>?@\\^|-~"

classificarLexema :: String -> IO ()
classificarLexema str
    | isConIdIdentifier str = putStrLn "ConId"
    | isVarSymIdentifier str = putStrLn "VarSym"
    | isVarIdIdentifier str = putStrLn "VarId"
    | isConSymIdentifier str = putStrLn "ConSym"
    | otherwise = putStrLn "Não é um identificador válido."

mostrarMenu :: IO ()
mostrarMenu = do
    putStrLn "Digite um lexema para classificar:"
    str <- getLine
    classificarLexema str

    putStrLn "\nDeseja classificar outro lexema? (s/n)"
    resposta <- getLine

    when (foiAceiteResposta resposta) mostrarMenu

foiAceiteResposta :: String -> Bool
foiAceiteResposta [] = False
foiAceiteResposta (x:_) = x `elem` "sS"
            
main :: IO ()
main = do
    mostrarMenu
