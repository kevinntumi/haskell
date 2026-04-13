import Utils (permitirCaracteresUTF8, ehExpressao)
import AvaliadorExpressao (avaliarExpressao)
import System.IO (hSetEncoding, stdin, stdout, stderr, utf8)
import Text.Regex.TDFA

main :: IO ()
main = do
    permitirCaracteresUTF8
    putStrLn "Digite a expressão aritmética (exemplo: a + b * d - c):"
    expressao <- getLine
    avaliarExpressao expressao