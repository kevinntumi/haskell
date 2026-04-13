{- HLINT ignore "Use unless" -}
module AvaliadorExpressao (avaliarExpressao) where
import Utils (ehExpressao, regexOperador, regexOperando, trim, ehOperacao)
import System.Random (randomRIO)
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as Text
import Data.Char (isSpace, chr)
import Control.Monad (when)
import Data.List (dropWhileEnd, nub)
import Text.Regex.TDFA ((=~))

data Operador = Adicao | Subtraccao | Multiplicacao | Divisao deriving (Eq, Ord, Show)

avaliarExpressao :: String -> IO ()
avaliarExpressao expressao
    | null expressao || not (Utils.ehExpressao expressao) = return ()
    | otherwise = do
        let operadores = obterOperadores expressao

        if Set.null operadores then
            return ()
        else do
            when (Set.size operadores <= 2) $ do
                let variaveisExpressao = extrairVariaveis expressao
                variavel <- obterVariavelGeradaAleatoriamente variaveisExpressao
                putStrLn (variavel ++ " = " ++ expressao)
            when (Set.size operadores <= 1) $ return ()

        verificarPrecedencia operadores Multiplicacao expressao

imprimirVariavelEOperacao :: String -> Set.Set String -> IO()
imprimirVariavelEOperacao operacao variaveis
    | null operacao || not (Utils.ehOperacao operacao) || Set.null variaveis = return ()
    | otherwise = do
        variavel <- obterVariavelGeradaAleatoriamente variaveis
        putStrLn (variavel ++ " = " ++ operacao)

verificarPrecedencia :: Set.Set Operador -> Operador -> String -> IO()
verificarPrecedencia operadores operador str
    | Set.null operadores || null str = return ()
    | otherwise = do
        if existeOperadorNoSet operadores operador then do
            let operacaoPrioritaria = obterOperacaoPrioritaria str operador

            if null operacaoPrioritaria then do
                aceitar operadores operador str
            else do
                let variaveisExpressao = extrairVariaveis str
                variavel <- obterVariavelGeradaAleatoriamente variaveisExpressao
                putStrLn (variavel ++ " = " ++ operacaoPrioritaria)
                let novaExpressao = Text.unpack $ Text.replace (Text.pack operacaoPrioritaria) (Text.pack variavel) (Text.pack (Utils.trim str))
                avaliarExpressao novaExpressao
        else
            aceitar operadores operador str

obterOperacaoPrioritaria :: String -> Operador -> String
obterOperacaoPrioritaria str operador = Utils.trim str =~ (regexOperando ++ "[" ++ obterOperador operador ++ "]" ++ regexOperando) :: String

obterOperador :: Operador -> String
obterOperador op
    | op == Adicao = "+"
    | op == Subtraccao = "-"
    | op == Multiplicacao = "*"
    | op == Divisao = "/"
    | otherwise = error "Ocorreu um erro"

aceitar :: Set.Set Operador -> Operador -> String ->  IO ()
aceitar operadores op str
    | Set.null operadores || null str = return ()
    | otherwise = case op of
        Adicao        -> verificarPrecedencia operadores Subtraccao str
        Multiplicacao -> verificarPrecedencia operadores Divisao str
        Divisao       -> verificarPrecedencia operadores Adicao str
        Subtraccao    -> putStr "Subtraccao"

existeOperadorNoSet :: Set.Set Operador -> Operador -> Bool
existeOperadorNoSet operadores operador = Set.member operador operadores

obterVariavelGeradaAleatoriamente :: Set.Set String -> IO String
obterVariavelGeradaAleatoriamente set = do
    x <- gerarIndiceLetra

    let novaVariavel = chr x : show (Set.size set + 1)

    if Set.member novaVariavel set
        then obterVariavelGeradaAleatoriamente set
        else return novaVariavel

gerarIndiceLetra :: IO Int
gerarIndiceLetra = do randomRIO (97, 122)

obterOperadores :: String -> Set.Set Operador
obterOperadores str =
    let oprdrs = extrairOperadores str
    in Set.fromList $
        concat [
            [Adicao        | temOperadorAdicao oprdrs],
            [Subtraccao    | temOperadorSubtraccao oprdrs],
            [Multiplicacao | temOperadorMultiplicacao oprdrs],
            [Divisao       | temOperadorDivisao oprdrs]
        ]

extrairOperadores :: String -> [String]
extrairOperadores str =
    concat
    matches
    where
    matches :: [[String]]
    matches = str =~ Utils.regexOperador

extrairVariaveis :: String -> Set.Set String
extrairVariaveis str
    | null str = Set.empty
    | otherwise =
        Set.fromList (concat matches)
        where
        matches :: [[String]]
        matches = str =~ Utils.regexOperando

temOperadorAdicao :: [String] -> Bool
temOperadorAdicao [] = False
temOperadorAdicao str = "+" `elem` str

temOperadorSubtraccao :: [String] -> Bool
temOperadorSubtraccao [] = False
temOperadorSubtraccao str = "-" `elem` str

temOperadorMultiplicacao :: [String] -> Bool
temOperadorMultiplicacao [] = False
temOperadorMultiplicacao str = "*" `elem` str

temOperadorDivisao :: [String] -> Bool
temOperadorDivisao [] = False
temOperadorDivisao str = "/" `elem` str