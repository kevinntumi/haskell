import System.IO (hSetEncoding, stdin, stdout, stderr, utf8)
import Data.Char (isDigit)
import Control.Monad (when)
import Text.Read (readMaybe)

fatorial :: Integer -> Integer
fatorial 0 = 1
fatorial n = n * fatorial (n - 1)

numTelefone :: String -> String
numTelefone [] = "O número de telefone não pode ser vazio."
numTelefone str
    | length str /= 9 = "O número de telefone deve conter exatamente 9 dígitos."
    | not (saoDigitos str) = "O número de telefone deve conter apenas dígitos."
numTelefone ('8':d:_)
    | d `elem` "23" = "Número de telefone da Tmcel"
    | d `elem` "45" = "Número de telefone da Vodacom"
    | d `elem` "67" = "Número de telefone da Movitel"
    | otherwise     = "Número de telefone de operadora desconhecida"
numTelefone _ = "Número de telefone de operadora estrangeira"

nEsimoCodigoEstudanteSIGA :: Int -> [Integer]
nEsimoCodigoEstudanteSIGA n
    | n <= 0 = error "O número deve ser maior que zero."
    | otherwise = take n codigoEstudantesSIGA

codigoEstudantesSIGA :: [Integer]
codigoEstudantesSIGA = [20230000..]

saoDigitos :: String -> Bool
saoDigitos = all isDigit

imprimir :: String -> IO ()
imprimir = putStrLn
    
percorrerLista :: [String] -> IO ()
percorrerLista [] = return ()
percorrerLista (x:xs) = do
    putStrLn x
    percorrerLista xs

opcoes :: [String]
opcoes = 
    [
        "1. Imprimir o n-ésimo fatorial com recursão", 
        "2. Identificar operadora de número de telefones com guards e pattern matching",
        "3. Gerar os n primeiros códigos de estudantes do SIGA com lazy lists",
        "4. Demonstrar o input com Maybe",
        "5. Gerar os n primeiros códigos de estudantes pares do SIGA com lazy lists e funções de alta ordem",
        "6. Sair"
    ]

foiAceiteResposta :: String -> Bool
foiAceiteResposta [] = False
foiAceiteResposta (x:_) = x `elem` "sS"

mostrarMenu :: IO ()
mostrarMenu = do
    putStrLn "Bem vindo ao menu:"
    percorrerLista opcoes
    opcaoStr <- getLine
    let opcao = read opcaoStr :: Integer

    case opcao of
        1 -> 
            do
                putStrLn "Digite um número para calcular o fatorial:"
                numStr <- getLine

                if all isDigit numStr then 
                    do
                        let n = read numStr :: Integer
                        putStrLn $ "O fatorial de " ++ show n ++ " é: " ++ show (fatorial n)
                else 
                    putStrLn "Entrada inválida. Por favor, digite um número inteiro."
        2 -> 
            do
                putStrLn "Digite um número de telefone para obter a operadora:"
                numTelStr <- getLine
                putStrLn $ numTelefone numTelStr
        3 -> 
            do
                putStrLn "Digite o número de códigos de estudantes do SIGA que deseja imprimir:"
                nEsimoStr <- getLine

                if all isDigit nEsimoStr then 
                    do
                        let n = read nEsimoStr :: Int
                        putStrLn $ "Os " ++ show n ++ " primeiros códigos de estudantes SIGA são: " ++ show (nEsimoCodigoEstudanteSIGA n)
                else 
                    putStrLn "Entrada inválida. Por favor, digite um número inteiro."
        4 -> 
            do
                putStrLn "Digite um número qualquer:"
                numStr <- getLine

                let possivelNumero = readMaybe numStr :: Maybe Integer

                case possivelNumero of
                    Just n -> putStrLn $ "O número digitado é: " ++ show n
                    Nothing -> putStrLn "Por favor, digite um número"
        5 ->
            do
                putStrLn "Digite o número de códigos de estudantes pares do SIGA que deseja imprimir:"
                nEsimoParStr <- getLine

                if all isDigit nEsimoParStr then 
                    do
                        let n = read nEsimoParStr :: Int
                        let codigosPares = filter even codigoEstudantesSIGA
                        putStrLn $ "Os " ++ show n ++ " primeiros códigos de estudantes pares do SIGA são: " ++ show (take n codigosPares)
                else 
                    putStrLn "Entrada inválida. Por favor, digite um número inteiro."
        6 -> 
            return ()
        _ -> 
            do
                putStrLn "Opção inválida. Por favor, escolha uma opção válida."
                mostrarMenu

    putStrLn "\nDeseja voltar ao menu principal? (s/n)"
    resposta <- getLine

    when (foiAceiteResposta resposta) mostrarMenu

main :: IO ()
main = do 
    hSetEncoding stdin utf8
    hSetEncoding stdout utf8
    hSetEncoding stderr utf8
    mostrarMenu
