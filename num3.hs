import Control.Monad (when)

fibonacci :: Integer -> Integer
fibonacci n
    | n <= 0 = error "O número deve ser maior que zero."
    | n >= 1 && n <= 2 = 1
    | otherwise = fibonacci (n - 1) + fibonacci (n - 2)

imprimirTermosFibonacci :: Integer -> IO()
imprimirTermosFibonacci n
    | n <= 0 = error "O número deve ser maior que zero."
    | otherwise = do
        print(fibonacci n)
        when (n >= 2) $ imprimirTermosFibonacci (n - 1)

main :: IO ()
main = do
    imprimirTermosFibonacci 11