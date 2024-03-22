-- Ejercicio 1
-- null :: [a] -> Bool
-- determina si una lista es vacia

-- take :: []
-- Ejercicio 2
valorAbsoluto :: Float -> Float
valorAbsoluto x
    | x >= 0    = x
    | otherwise = -x

valorAbsoluto2 :: Float -> Float
valorAbsoluto2 x = if x<=0 then -x else x

bisiesto :: Int -> Bool
bisiesto n 
    | n `mod` 400 == 0  = True
    | n `mod` 100 == 0  = False
    | n `mod` 4   == 0  = True
    | otherwise         = False

factorial :: Int -> Int
factorial 0 = 1
factorial n = n * factorial (n-1)


cantDivisoresPrimos :: Int -> Int
cantDivisoresPrimos n = cantDivisoresPrimosAux n 1 0
    where 
        cantDivisoresPrimosAux :: Int -> Int -> Int -> Int
        cantDivisoresPrimosAux n i c
            | esPrimo n = 1
            | n == i    = c
            | esPrimo i && n `mod` i == 0 = cantDivisoresPrimosAux n (i+1) (c+1)
            | otherwise = cantDivisoresPrimosAux n (i+1) c

esPrimo :: Int -> Bool
esPrimo n = esPrimoAux n (n-1)
 where 
    esPrimoAux :: Int -> Int -> Bool
    esPrimoAux 1 _ = False
    esPrimoAux n 1 = True
    esPrimoAux n i = n `mod` i /= 0 && esPrimoAux n (i-1)
-- Ejercicio 3

inverso :: Float -> Maybe Float
inverso x = if x /= 0 then Just (1/x) else Nothing

aEntero :: Either Int Bool -> Int
aEntero (Left a) = a
aEntero (Right a) = if a then 1 else 0

-- Ejercicio 4

limpiar :: [Char] -> [Char] -> [Char]
limpiar _ [] = []
limpiar [] a = a -- No hace falta pero reduce el tiempo de ejec en este caso
limpiar borrar (p:ps)
    | p `elem` borrar = limpiar borrar ps
    | otherwise       = p:limpiar borrar ps

limpiar2 :: [Char] -> [Char] -> [Char]
limpiar2 _ [] = []
limpiar2 [] a = a
limpiar2 borrar palabra = filter (\c->not(c `elem` borrar)) palabra


difPromedio :: [Float] -> [Float]
difPromedio [] = [] -- Hara falta?
difPromedio notas = map (\y->y-p) notas
    where
        p :: Float
        p = (sum notas) / fromIntegral(length notas)

todosIguales :: [Int] -> Bool -- sabias que!
todosIguales [] = True
todosIguales xs = all (\x->x == xs!!0) xs -- Si todos son iguales, en particular lo son al primero

-- Ejercicio 5
data AB a = Nil | Bin (AB a) a (AB a)

-- Para poder mostrar los AB
toStringAB ::(Show a) => AB a -> String
toStringAB (Nil) = "Nil"
toStringAB (Bin i n d) = "<" ++ (show n) ++ ", {" ++ (toStringAB i) ++ "," ++ (toStringAB d) ++ "}>"

instance (Show a) => Show (AB a) where
    show (Nil) = show "Nil"
    show a = toStringAB a



vacioAB :: AB a -> Bool
vacioAB Nil = True
vacioAB _   = False

negacionAB :: AB Bool -> AB Bool
negacionAB (Nil) = Nil
negacionAB (Bin (i) b (d)) = Bin (negacionAB i) (not b) (negacionAB d)

productoAB :: AB Int -> Int
productoAB Nil = 1
productoAB (Bin i n d) = productoAB i * n * productoAB d