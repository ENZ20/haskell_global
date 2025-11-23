import Data.Char (toLower)


siguiente:: Int -> Int --Pattern Matching
siguiente a = a + 1

analizarTemp :: Int -> String --Guardas (Logica)
analizarTemp temperatura
    |temperatura <= 0 = "Congelado"
    |temperatura > 0 && temperatura <= 20 = "Frio"
    |otherwise = "Calido" 

sumaVectores :: (Int, Int) -> (Int, Int) -> (Int, Int) --Pattern Matching (Tuplas)
sumaVectores (x1, y1) (x2, y2) = (x1+x2 , y1 + y2)

miOr :: Bool -> Bool -> Bool --mi OR logico casero
miOr True _ = True
miOr _ True = True
miOr False False = False

factorial :: Int -> Int
factorial 0 = 1                 -- 1. Caso Base: factorial de 0 es 1. ¡Aquí paramos!
factorial n = n * factorial (n - 1)  -- 2. Caso Recursivo: n * factorial del anterior

potencia :: Int -> Int -> Int
potencia _ 0 = 1
potencia base 1 = base
potencia base expo = base * potencia base (expo - 1) 

miLongitud :: [a] -> Int --cuenta los elementos de un vector de cosas
miLongitud [] = 0
miLongitud (_:xs) = 1 + miLongitud xs

sumatoria :: [Int] -> Int --suma los enteros de un vector de enteros
sumatoria [] = 0
sumatoria (x:xs) = x + sumatoria xs

soloPares :: [Int] -> [Int] --solo devuelve los pares de una lista de numeros enteros 
soloPares [] = []
soloPares (x:xs)
    | x `mod` 2 == 0 = x : soloPares xs
    | otherwise = soloPares xs

cuadrados :: [Int] -> [Int]
--cuadrados [] = []
cuadrados xs = map (^2) xs

mayoresA5 :: [Int] -> [Int]
--mayoresA5 [] = []
mayoresA5 xs = filter (\x -> x > 5) xs

sumaCuadradosPares :: [Int] -> Int
sumaCuadradosPares lista = 
    let pares = filter (\x -> mod x 2 == 0) lista
        cuad  = map (^2) pares
    in sum cuad

aplicarDosVeces :: (Int -> Int) -> Int -> Int
aplicarDosVeces f n = f (f n) --Ejemplo: aplicarDosVeces (+3) 10 debería dar 16 (10+3=13, 13+3=16).


{-
🎯 Foco 1: Recursividad con Acumuladores (Lo más difícil)
Ejercicio A: La Conjetura de Collatz
Este es muy probable que caiga porque es una secuencia matemática recursiva (parecido a tu fibo pero con lógica condicional).

Reglas:
Si el número es 1, terminamos.
Si es par, lo dividimos por 2.
Si es impar, lo multiplicamos por 3 y sumamos 1.
Misión: Crea una función collatz :: Int -> [Int] que devuelva la lista de todos los pasos hasta llegar a 1.

Ejemplo: collatz 10 → [10, 5, 16, 8, 4, 2, 1]
Pista: Es recursividad directa. collatz 1 = [1] (Caso base) collatz n = n : collatz (...) (Caso recursivo con guardas para par/impar).

-}

collatz :: Int -> [Int]
collatz 1 = [1]
collatz n 
    | mod n 2 == 0 = n : collatz (n `div` 2)
    | mod n 2 /= 0 = n : collatz (n*3 + 1)






{-
🎯Foco 2: Procesamiento de Dos Listas a la vez
Ejercicio B: Producto Punto
Calcula el producto punto de dos vectores (listas de enteros). Multiplica el primero con el primero, el segundo con el segundo, y suma todo.

Matemáticas: [1,2,3]⋅[4,5,6]=(1×4)+(2×5)+(3×6)=32

Misión: Crea productoPunto :: [Int] -> [Int] -> Int.

Caso base: Si alguna lista está vacía, la suma es 0.

Recursión: Multiplica las cabezas + producto punto de las colas.
-}

productoPunto :: [Int] -> [Int] -> Int
productoPunto [] [] = 0
productoPunto (x:xs) (y:ys) = x * y + productoPunto xs ys



{-
🎯Foco 3: Cifrados y Strings (Estilo codMurcielago)

Ejercicio C: Encriptador de Vocales Numérico
Crea una función encriptar :: String -> String.

Reemplaza las vocales por números: 'a'->'1', 'e'->'2', 'i'->'3', 'o'->'4', 'u'->'5'.

Las consonantes se quedan igual.

Extra: Usa toLower (importa Data.Char) para que funcione con mayúsculas también, o ignora las mayúsculas por ahora si prefieres.
-}

encriptar :: String -> String
encriptar [] = []
encriptar (x:xs)
    | toLower x == 'a' = '1' : encriptar xs
    | toLower x == 'e' = '2' : encriptar xs
    | toLower x == 'i' = '3' : encriptar xs
    | toLower x == 'o' = '4' : encriptar xs
    | toLower x == 'u' = '5' : encriptar xs
    |otherwise =  x  : encriptar xs

{-
🎯 Foco 4: Listas por Comprensión (Estilo qsort y pares)
Ejercicio D: Triángulos Rectángulos
Usando listas por comprensión, encuentra todas las tuplas (a,b,c) tal que:

Los tres lados sean menores o iguales a n (un número dado).

Cumplan el teorema de Pitágoras: a^2 + b^2= c^2.

Misión: pitagoras :: Int -> [(Int, Int, Int)]

Pista: [(a,b,c) | a <- [1..n], b <- [1..n], c <- [1..n], ...condicion...]
-}

pitagoras :: Int -> [(Int, Int, Int)]
pitagoras n = [ (a,b,c) | a <- [1..n], b <- [1..n], c <- [1..n], (a^2 + b^2) == c^2 ]

{-
🛡️ ¿Te animas a un "Simulacro de Examen"?
Para cerrar con broche de oro, te propongo un ejercicio que combina Tuplas + Listas + Lógica. Si te sale este, estás sobradísimo para aprobar.
Ejercicio: "Aprobados y Reprobados" Tienes una lista de tuplas con (Nombre_Alumno, Nota). Tienes que devolver una lista de Strings que diga "APROBO" o "REPROBO" para cada uno.

Nota >= 6: "APROBO"

Nota < 6: "REPROBO"

Firma: calificar :: [(String, Int)] -> [String]

Ejemplo: calificar [("Juan", 7), ("Ana", 4)] → ["APROBO", "REPROBO"]

Pista: Usa recursividad con pattern matching de tuplas en la cabeza: calificar ((nombre, nota):xs) = ...
-}

calificar :: [(String, Int)] -> [String]
calificar [] = []
calificar ((nom, nota):xs)
    | nota >= 6 = "APROBO"      : calificar xs
    | nota <  6 = "desaprobado" : calificar xs 


{-
Ejercicio 3 (40pts): Cifrado por Palabra Clave

Se desea implementar un sistema de cifrado por palabra clave en Haskell.
El cifrado sustituye cada letra del mensaje según un alfabeto cifrado construido a partir de una palabra clave.

Procedimiento:
1. Tomar la palabra clave y eliminar letras repetidas.
2. Construir el alfabeto cifrado: primero las letras de la clave, luego las letras restantes del alfabeto inglés en orden alfabético.
3. Cada letra del mensaje se reemplaza por la letra correspondiente en el alfabeto cifrado (misma posición).
4. Los caracteres que no son letras (espacios, signos) se mantienen sin cambios.

Ejemplo:
Clave: "HASKELL" -> procesada: "HASKEL"
Alfabeto cifrado: "HASKELBCDFGIJMNOPQRSTUVWXYZ"
Mensaje: "HOLA MUNDO" -> Cifrado: "CNIH JUMKN"
-}

-- 1. Elimina las letras repetidas de la palabra clave
procesarClave :: String -> String
procesarClave []     = []
procesarClave (x:xs) = x : procesarClave ( filter (/=x) xs )  

-- 2. Devuelve el alfabeto completo cifrado, agregando las letras que no están en la clave.
alfabetoIngles :: String
alfabetoIngles = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"

alfabetoCifrado :: String -> String
alfabetoCifrado clave = procesarClave (clave ++ alfabetoIngles)

-- 3. Devuelve el mensaje cifrado usando el alfabeto cifrado.
buscarPosicion :: Char -> String -> Int
buscarPosicion c [] = -1 -- Caso error (no debería pasar si validamos)
buscarPosicion c (x:xs)
   | c == x    = 0
   | otherwise = 1 + buscarPosicion c xs

cifrar :: String -> String -> String
cifrar clave mensaje = cifrarRecursivo mensaje
    where
    alfabetoIngles = "ABCDEFGHIJKLMNOPQKRSTUVWXYZ"
    alfabetoNuevo = alfabetoCifrado clave

    cifrarRecursivo :: String -> String
    cifrarRecursivo [] = []
    cifrarRecursivo (x:xs)
        |elem x alfabetoIngles = (alfabetoNuevo !! buscarPosicion x alfabetoIngles) : cifrarRecursivo xs
        |otherwise = x : cifrarRecursivo xs


-- 4. Devuelve el mensaje original.
--descifrar :: String -> String -> String
--descifrar = undefined

--CODIGO CESAR
alfabetoMin :: String
alfabetoMin = ['a'..'z']

alfabetoMayu :: String
alfabetoMayu = ['A'..'Z']

devuelvePos :: String -> Char -> Int -- "HOLA" L -> 2
devuelvePos (x:xs) letra
    | x == letra = 1
    | otherwise  = 1 + devuelvePos xs letra

desplazar2 :: Char -> Int -> Char
desplazar2 letra numDesplazamiento
    |elem letra alfabetoMin = alfabetoMin !! (mod (devuelvePos alfabetoMin  letra + numDesplazamiento) 26)
    |elem letra alfabetoMin = alfabetoMin !! (mod (devuelvePos alfabetoMayu letra + numDesplazamiento) 26)
    |otherwise = letra

codificacionCesarDerecha :: String -> Int -> String
codificacionCesarDerecha [] _ = []
codificacionCesarDerecha (x:xs) n = desplazar2 x n : codificacionCesarDerecha xs n

codificacionCesarIzquierda :: String -> Int -> String
codificacionCesarIzquierda [] _ = []
codificacionCesarIzquierda (x:xs) n = codificacionCesarDerecha xs (-n)