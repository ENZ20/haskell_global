import Data.Char

-- TP2 Ejercicio 1 y extras de las diapo
esPositivo :: Int -> Bool
esPositivo x = (x>0)

esNegativo :: Int -> Bool
esNegativo x = (x<0)

esRango :: Int -> Bool
esRango  x = x>=9 && x<=10

esMultiploDe3 :: Int -> Bool
esMultiploDe3 x = (mod x 3)==0

esMultiploDe :: Int -> Int -> Bool
esMultiploDe x y= (mod x y) == 0

fueraDeRango :: Int -> Int -> Int -> Bool
fueraDeRango a b c = (a >= b) && (a <= c)

celsius2Fahr :: Float -> Float
celsius2Fahr x = (x*(9/5)) + 32

-- TP2 Ejercicio 2 funciones Sobre Tuplas

ptoMedio :: (Float, Float) -> (Float, Float) -> (Float,Float)
ptoMedio (x,y) (z,t) = (((x+z)/2) , ((y+t)/2))

norma :: (Float,Float) -> Float
norma (x,y) = sqrt(x^2 + y^2)

segundo2Tiempo :: Int -> (Int,Int,Int)
segundo2Tiempo x = (x `div` 3600, (x `mod` 3600) `div` 60, (x `mod` 3600) `mod` 60)

-- TP2 Ejercicio 3 funciones sobre listas

pertenece :: String -> Char -> Bool --funcion Auxiliar
pertenece lista c
 |lista == [] = False
 |c == head lista = True
 |otherwise = pertenece (tail lista) c 
{-
limpiar :: String -> String -> String
limpiar frase1 frase2
 |frase1 == [] = frase2
 |limpiar (x:frase1) frase2 = limpiar frase1 (filter (/=x) frase2) 
-}
limpiar :: String -> String -> String
limpiar [] ys = ys                                  -- Caso base: si no hay más caracteres a eliminar ([]), devolvemos el texto original.
limpiar (x:xs) ys = limpiar xs (filter (/= x) ys)   -- Caso recursivo: tomamos un carácter x de la primera cadena, lo eliminamos de ys con filter (/= x) y seguimos con el resto (xs).

limpiar2 :: String -> String -> String
limpiar2 [] ys = ys                          -- si no hay nada que borrar, devolvemos el texto
limpiar2 (x:xs) [] = []                      -- si el texto está vacío, ya no queda nada
limpiar2 (x:xs) (y:ys)
    | x == y    = limpiar2 (x:xs) ys         -- si el char a borrar coincide, lo quitamos
    | otherwise = y : limpiar2 (x:xs) ys     -- si no, lo mantenemos

promedio :: [Float] -> Float
promedio xs = (sum xs) / fromIntegral (length xs)

dif :: [Float] -> [Float]            -- revisa todos elementos y los resta
dif xs = [x - promedio xs | x <- xs]

todosIguales :: [Int] -> Bool        --conpara toda la lista elemento con elemento
todosIguales [] = False
todosIguales [_] = True
todosIguales (x:y:xs)
 | x /= y    = False -- " /= significa si x es DISTINTO de y , en haskell"
 | otherwise = todosIguales (y:xs)


repLista :: [Int] -> Int -> [Int]   --agrega n elementos a la lista
repLista [] _ = []
repLista (x:xs) n = replicate n x ++ repLista xs n

listas2lista :: [[Int]] -> [Int]
listas2lista [] = []
listas2lista (x:xs) = x ++ listas2lista xs

{-
contadorDeCaracter :: String -> Char -> Int
contadorDeCaracter [] _ = 0
contadorDeCaracter (x:xs) c
 |x == c = 1 + contadorDeCaracter xs c
 |otherwise = contadorDeCaracter xs c-}

contadorDeCaracter :: String -> Char -> Int
contadorDeCaracter [] _ = 0  -- Caso base: lista vacía
contadorDeCaracter (x:xs) c
    | x == c    = 1 + contadorDeCaracter xs c
    | otherwise = contadorDeCaracter xs c

checkParentesis :: String -> Bool
checkParentesis xs = balanceado xs 0
 where
    balanceado [] 0 = True
    balanceado [] _ = False
    balanceado (x:xs) n
     |n < 0 = False
     |x == '(' = balanceado xs (n+1)
     |x == ')' = balanceado xs (n-1)
     |otherwise = balanceado xs n

finales :: Int -> [Int] -> [Int]
finales 0 _= []
finales _ [x]= [x]
finales n xs = reverse (take n (reverse xs) )  --take 3 [1,2,3,4,5]    -- [1,2,3]

extremos :: Int -> [Int] -> [Int]
extremos 0 xs = [] --devuelve nada porque seria "ninguno de los extremos"
extremos _ [x] = [x]
extremos n xs = take n xs ++ reverse (take n (reverse xs))

--Ejercicio 4 Listas por comprension

listaCuadrados :: Int -> [Int]
listaCuadrados n = [x^2 | x <- [1..n]] -- si n  = 5 entonces la salida es : [1,4,9,16,25] 

sumCuadrados :: Int -> Int
sumCuadrados n = sum (listaCuadrados n) -- 1² + 2² + 3² + ... + n²

replica :: Int -> a -> [a]
replica n x = [x | _ <- [1..n]]

pares :: [a] -> [b] -> [(a, b)]
pares xs ys = [(x, y) | x <- xs, y <- ys]

-- EJERCICIO de NLM
--lista :: [Int]
--lista = [1,2,3,4,5]

--listaDoble :: [Int] -> [Int]
--listaDoble xs = map (*2) xs
------------------------------

--ejercicios de LISTAS - profe

listar:: Int -> Int -> Int -> [Int]
listar x y z = [x,y,z]

rangoDePaso :: Int -> Int -> Int -> [Int]
rangoDePaso n1 n3 n2 = [n1,n3..n2]




--ejercicios de TUPLAS - profe


--t:: (Double,Double)
--t = (1,2)

norma1:: (Double,Double) -> Double
norma1 t = sqrt ( fst t ^ 2 + snd t ^ 2 )  

norma2:: (Double,Double) -> Double
norma2 (x, y) = sqrt (x^2 + y^2)

--Crea una función que tome dos tuplas representando coordenadas 2D (x, y) y devuelva: LA DISTANCIA Y PUNTO MEDIO

distanciaYPuntoMedio :: (Double, Double) -> (Double, Double) -> (Double, (Double, Double))
distanciaYPuntoMedio (x1,y1) (x2,y2) = ( sqrt ( (x2-x1)^2 + (y2-y1)^2 ) ,(  (x1 + x2) /2 ,(y1+y2)/2  ) )

--codigo cesar de alejo
codigoCesar::String->Int->String
codigoCesar [] _ = []
codigoCesar (c:cs) n = (desplazar c n) : codigoCesar cs n

desplazar::Char->Int->Char
desplazar c n
 |isUpper c = devElem aBC (mod ((devPos aBC c) + n-1) 26 +1)
 |isLower c = devElem abc (mod ((devPos abc c) + n-1) 26 +1)
 |otherwise = c
 where 
 abc = ['a','b'..'z']
 aBC = ['A','B'..'Z']


devPos::String->Char->Int
devPos [] _ = error "no esta en la lista"
devPos (x:xs) c
 |c==x = 1
 |otherwise = 1 + devPos xs c

devElem::String->Int->Char
devElem [] _ = error "no existe elemento en la pos"
devElem (x:xs) n
 |n==1 = x
 |otherwise = devElem xs (n-1)


----------------------------------------
codMurcielago::String->String
codMurcielago [] = []
codMurcielago (x:xs) 
 |toLower x == 'm' = '0':codMurcielago xs
 |toLower x == 'u' = '1':codMurcielago xs
 |toLower x == 'r' = '2':codMurcielago xs
 |toLower x == 'c' = '3':codMurcielago xs
 |toLower x == 'i' = '4':codMurcielago xs
 |toLower x == 'e' = '5':codMurcielago xs
 |toLower x == 'l' = '6':codMurcielago xs
 |toLower x == 'a' = '7':codMurcielago xs
 |toLower x == 'g' = '8':codMurcielago xs
 |toLower x == 'o' = '9':codMurcielago xs
 |otherwise = x:codMurcielago xs