{- HLINT ignore "Use unless" -}
{-# LANGUAGE BlockArguments #-}
module AvaliadorExpressao (avaliarExpressao) where
import Utils
import System.Random
import qualified Data.Map as Map
import qualified Data.Set as Set
import Control.Monad.ST
import qualified Data.Text as Text
import Data.STRef
import Data.Char (isSpace, chr)
import Text.ParserCombinators.ReadP (string)
import Control.Monad (when)
import Data.List (dropWhileEnd)
import Text.Regex.TDFA ((=~))
import GHC.IO.Device (IODevice(dup))
import Data.Functor.Contravariant (Op)

data Operador = Adicao | Subtraccao | Multiplicacao | Divisao deriving (Eq, Ord, Show)

avaliarExpressao :: String -> Map.Map String String -> IO ()
avaliarExpressao expressao map
    | null expressao || not (Utils.ehExpressao expressao) =  return ()
    | otherwise = do
        let operadores = obterOperadores expressao

        if Set.null operadores then
            return ()
        else when (Set.size operadores <= 2) $ do
            print expressao
            when (Set.size operadores <= 1)
                $ return ()

        verificarPrecedencia operadores Multiplicacao expressao map

verificarPrecedencia :: Set.Set Operador -> Operador -> String -> Map.Map String String -> IO()
verificarPrecedencia operadores operador str map
    | Set.null operadores || null str = return ()
    | otherwise = do
        if existeOperadorNoSet operadores operador then do
            let operacaoPrioritaria = obterOperacaoPrioritaria str operador
            
            if null operacaoPrioritaria then do
                aceitar operadores operador str map
            else do
                variavel <- obterVariavelGeradaAleatoriamente map
                let novoMapa = adicionarAoMap variavel operacaoPrioritaria map
                putStrLn (variavel ++ " = " ++ operacaoPrioritaria)
                let novaExpressao = Text.unpack $ Text.replace (Text.pack operacaoPrioritaria) (Text.pack variavel) (Text.pack (Utils.trim str))
                avaliarExpressao novaExpressao novoMapa
        else
            aceitar operadores operador str map

adicionarAoMap :: String -> String -> Map.Map String String -> Map.Map String String
adicionarAoMap variavel operacao map
    | not (Map.null map) && not (contemVariavel variavel map) = Map.insert variavel operacao map
    | null variavel || null operacao || contemVariavel variavel map = map
    | otherwise = map

contemVariavel :: String -> Map.Map String String  -> Bool
contemVariavel variavel map =
    case Map.lookup variavel map of
        Just v -> True
        Nothing -> False

encontrarVariavelPeloValor :: String -> Map.Map String String -> [String]
encontrarVariavelPeloValor valor map = Map.keys (Map.filter (== valor) map)

obterOperacaoPrioritaria :: String -> Operador -> String
obterOperacaoPrioritaria str operador = Utils.trim str =~ (regexOperando ++ "[" ++ obterOperador operador ++ "]" ++ regexOperando) :: String

obterOperador :: Operador -> String
obterOperador op
    | op == Adicao = "+"
    | op == Subtraccao = "-"
    | op == Multiplicacao = "*"
    | op == Divisao = "/"
    | otherwise = error "Ocorreu um erro"

aceitar :: Set.Set Operador -> Operador -> String -> Map.Map String String -> IO ()
aceitar operadores op str mapa
    | Set.null operadores || null str = return ()
    | otherwise = case op of
        Adicao        -> verificarPrecedencia operadores Subtraccao str mapa
        Multiplicacao -> verificarPrecedencia operadores Divisao str mapa
        Divisao       -> verificarPrecedencia operadores Adicao str mapa
        Subtraccao    -> verificarPrecedencia operadores Adicao str mapa

existeOperadorNoSet :: Set.Set Operador -> Operador -> Bool
existeOperadorNoSet operadores operador = Set.member operador operadores

obterVariavelGeradaAleatoriamente :: Map.Map String String -> IO String
obterVariavelGeradaAleatoriamente mapa = do
    x <- gerarIndiceLetra
    y <- gerarIndiceLetra

    let novaVariavel = [chr x, chr y]

    if Map.member novaVariavel mapa
        then obterVariavelGeradaAleatoriamente mapa
        else return novaVariavel

gerarIndiceLetra :: IO Int
gerarIndiceLetra = do
    indice <- randomRIO (65, 122)

    if indice > 90 && indice < 97 then
        gerarIndiceLetra
    else
        return indice

trim :: String -> String
trim str
    | null str = error  "A string não pode ser vazia."
    | otherwise = dropWhileEnd isSpace (dropWhile isSpace str)

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
extrairOperadores str = concat matches
  where
    matches :: [[String]]
    matches = str =~ Utils.regexOperador

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