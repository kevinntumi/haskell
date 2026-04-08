import Data.Char (isNumber)

fatorial :: Integer -> Integer
fatorial 0 = 1
fatorial n = n * fatorial (n - 1)

numTelefone :: String -> String
numTelefone str
    | null str = "O número de telefone não pode ser vazio."
    | length str /= 9 = "O número de telefone deve conter exatamente 9 dígitos."
    | not (saoDigitos str) = "O número de telefone deve conter apenas dígitos."
numTelefone ('8':d:_)
    | d `elem` "23" = "Número de telefone da Tmcel"
    | d `elem` "45" = "Número de telefone da Vodacom"
    | d `elem` "67" = "Número de telefone da Movitel"
    | otherwise     = "Número de telefone de operadora desconhecida"
numTelefone _ = "Número de telefone de operadora estrangeira"

saoDigitos :: String -> Bool
saoDigitos = all isNumber

imprimir :: String -> IO ()
imprimir = putStrLn
    
percorrerLista :: [String] -> IO ()
percorrerLista [] = return ()
percorrerLista (x:xs) = do
    putStrLn x
    percorrerLista xs

opcoes :: [String]
opcoes = ["1. Imprimir fatorial", "2."]

foiAceiteResposta :: String -> Bool
foiAceiteResposta [] = False
foiAceiteResposta (x:_) = x `elem` "sS"

mostrarMenu :: IO ()
mostrarMenu = do
    putStrLn "Bem vindo ao menu:"
    percorrerLista opcoes

    putStrLn "\nDeseja voltar ao menu principal? (s/n)"
    resposta <- getLine

    when (foiAceiteResposta resposta) mostrarMenu

main :: IO ()
main = mostrarMenu