import Data.Char (isNumber)
import Control.Monad (when)

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

nEsimoCodigoEstudanteSIGA :: Int -> [Integer]
nEsimoCodigoEstudanteSIGA n
    | n <= 0 = error "O número deve ser maior que zero."
    | otherwise = take n codigoEstudantesSIGA

codigoEstudantesSIGA :: [Integer]
codigoEstudantesSIGA = [20230000..]

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
opcoes = 
    [
        "1. Imprimir o n-ésimo fatorial", 
        "2. Identificar operadora de número de telefones",
        "3. Gerar os n primeiros códigos de estudantes do SIGA",
        "4. ",
        "7. Sair"
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

                if all isNumber numStr then 
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

                if all isNumber nEsimoStr then 
                    do
                        let n = read nEsimoStr :: Int
                        putStrLn $ "Os " ++ show n ++ " primeiros códigos de estudantes SIGA são: " ++ show (nEsimoCodigoEstudanteSIGA n)
                else 
                    putStrLn "Entrada inválida. Por favor, digite um número inteiro."
        7 -> 
            return ()
        _ -> 
            do
                putStrLn "Opção inválida. Por favor, escolha uma opção válida."
                mostrarMenu

    putStrLn "\nDeseja voltar ao menu principal? (s/n)"
    resposta <- getLine

    when (foiAceiteResposta resposta) mostrarMenu

main :: IO ()
main = mostrarMenu
