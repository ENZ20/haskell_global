import Data.Char

{-
Hola chicos, estos son los algoritmos que vimos en la clase de hoy
En el archivo de hoy (y especificamente arriba de este comentario),
podemos ver algo muy particular "import Data.Char" asi se importan
librerias en haskell, en este caso importe la libreria porque la 
funcion toLower se encuentra en la misma, caso de no importarla el
programa no compilara. Para el resto de funciones que se encuentran
en las firminas del profe son bastante parecidas.
Pata buscar librerias pueden usar la pagina "https://hoogle.haskell.org/"
Por otro lado, al ultimo les deje la misma funcion de divisores
escrita de una forma distinta, se las dejo para que curioseen e
investiguen
-}

sumarLista::[Int]->Int
sumarLista [] = 0
sumarLista (x:[]) = x
sumarLista (x:xs) = x + sumarLista xs

sumList::[Int]->Int
sumList [] = error "No tiene elementos"
sumList (x:xs)
 | xs == [] = x
 | otherwise = x + sumList xs
 
sumarListas::[[Int]]->Int
sumarListas [] = error "No hay listas"
sumarListas (x:[]) = sumarLista x
sumarListas (x:xs) = sumarLista x + sumarListas xs

isVocal::Char->Bool
isVocal x = elem (toLower x) "AEIOUaeiou"

contarVocales::String->Int
contarVocales [] = 0
contarVocales (x:xs)
 | isVocal x = 1+contarVocales xs
 | otherwise = contarVocales xs

dejarVocales::String->String
dejarVocales [] = []
dejarVocales (x:xs)
 | isVocal x = x:dejarVocales xs
 | otherwise = dejarVocales xs

divisoresDe::Int->[Int]
divisoresDe x = divisoresDer x 1

divisoresDer::Int->Int->[Int]
divisoresDer x i
 | i == x = [x]
 | i < x && (mod x i) == 0 = x:divisoresDer x (i+1)
 | otherwise = divisoresDer x (i+1)

divisores::Int->[Int]
divisores x = [j|j<-[1,2..x], (mod x j == 0)]