import Data.List
{-
Ejercicio: Programación Funcional: 
La criptografía es el conjunto de estrategias que permiten transformar la información para protegerla de accesos no autorizados. 
Un algoritmo conocido es el Cifrado por Transposición, que consiste en reorganizar las letras de un mensaje en una tabla con un 
número determinado de columnas. Al leer las letras de las columnas en lugar de las filas, se obtiene un mensaje cifrado. Por ejemplo, 
el mensaje “Haskell es genial” se puede organizar en una tabla de 4 columnas, obteniendo el mensaje cifrado: “Heeelalsn sl i k ga ”.

Implemente en Haskell las siguientes funciones:

CodificacionTransposicion :: String -> Int -> String, donde el primer parámetro es el mensaje a cifrar y el segundo parámetro 
indica el número de columnas en las que se debe organizar el texto.

DecodificacionTransposicion :: String -> Int -> String, donde el primer parámetro es el mensaje cifrado y el segundo parámetro 
indica el número de columnas utilizadas para realizar la transposición original.

tema A ->
-}
codifTransp::String->Int->String
codifTransp [] _ = []
codifTransp x n = codificar (matriz x n)

codificar::[String]->String
codificar [] = []
codificar ([]:_) = []
codificar x = primeros x ++ codificar (quitarPrimeros x)

primeros::[String]->String
primeros [] = []
primeros ([]:_) =[]
primeros ((x:_):xss) = x : primeros xss 

quitarPrimeros::[String]->[String]
quitarPrimeros [] = []
quitarPrimeros ([]:_) = []
quitarPrimeros [_:xs] =  [xs]
quitarPrimeros ((_:xs):xss) = xs : quitarPrimeros xss


matriz::String->Int->[String]
matriz [] _ = []
matriz x n 
 |n<=0 = error "Debe ser un valor mayor o igual 1"
 |n<=length x = take n x : matriz (drop n x) n
 |otherwise = [completar x n]

completar::String->Int->String
completar x n
 |length x < n = completar (x ++ [' ']) n
 |otherwise = x

{-
Tema A v2
-}
cTranpos::String->Int->String
cTranpos [] _ = []
cTranpos x n = cTranposAux (completarB x n) n 0 1

completarB::String->Int->String
completarB x n
 |n<=0 = error "No vailido"
 |mod (length x) n == 0 = x --aca tuve el error de contemplar length x - 1, era simplemente length x
 |otherwise = completarB (x++[' ']) n 

cTranposAux::String->Int->Int->Int->String --Pueden creer que lleva un año esta version sin ser completada, no sé que patron falta
cTranposAux [] _ _ _ = []
cTranposAux x n i j
 |n<=0 = error "Particion en columnas no valida"
 |i<0 = error "Indice no valido"
 |j>n = [devPos x (length x - 1)] --luego me di cuenta que no devolvia el ultimo elemento, pero con esto ya esta solucionado
 |i<=(length x - 1) && i>=(length x - 1)-n = devPos x i : cTranposAux x n j (j+1)
 |i<=(length x - 1) = devPos x i : cTranposAux x n (i+n) j

{-
Tema A v3 Hecha en 2025
-}
cTranpos2::String->Int->String
cTranpos2 s n = m2S (transpose (s2M (completarc s n) n))

completarc::String->Int->String
completarc s n = s ++ replicate tam ' '
 where tam = n*(div (length s) n +1) - length s

s2M::String->Int->[String]
s2M [] _ = []
s2M s n = (take n s) : s2M (drop n s) n

--transpose esta en la libreria Data.List

m2S::[String]->String
m2S [] = []
m2S (x:xs) = x ++ m2S xs

{-
Tema B ->
La criptografía es el conjunto de estrategias que permiten transformar la información para protegerla de accesos no autorizados. Uno de los métodos más conocidos es el cifrado de Vigenere, que utiliza una palabra clave para modificar el texto original de manera que se desplaza cada letra según la letra correspondiente de la clave. Por ejemplo, si usamos la palabra clave "clave" para cifrar el mensaje "HOLA MUNDO" el resultado del cifrado sería: "JZLV QWYDJ". Para ello se asignan valores numéricos (A=0, B=1, ..., Z=25) y se aplican los desplazamientos:
    • H (7) + c (2) = J (9)
    • O(14) + l (11) = Z (25)
    • L (11) + a (0) = L (11)
    • A (0) + v (21) = V (21)
    • (espacio) permanece igual
    • M (12) + e (4) = Q (16)
    • U (20) + c (2) = W (22)
    • N (13) + l (11) = Y (24)
    • D (3) + a (0) = D (3)
    • O(14) + v (21) = J (9)
Implemente en Haskell las siguientes fun
-}

cifrarVigene::String->String->String
cifrarVigene [] _ = []
cifrarVigene x y = cifrarVigeneAux x y 0

cifrarVigeneAux::String->String->Int->String
cifrarVigeneAux [] _ _ = []
cifrarVigeneAux _ [] _ = []
cifrarVigeneAux (x:xs) y i
 |elem x ['a'..'z'] =  devPos ['a'..'z'] (mod (pos ['a'..'z'] x + pos ['a'..'z'] (devPos y (mod i (length y)))) 26) : cifrarVigeneAux xs y (i+1)
 |otherwise = x : cifrarVigeneAux xs y i

devPos::String->Int->Char
devPos [] _ = error "Supero el tamaño de la lista"
devPos (x:xs) n
 |n == 0 = x
 |otherwise = devPos xs (n-1)

pos::String->Char->Int
pos [] _ = error "El caracter no esta en la lista"
pos (x:xs) c
 |x==c = 0
 |otherwise = pos xs c + 1
