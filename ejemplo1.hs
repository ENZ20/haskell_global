suma::Double->Double->Double
suma x y = x + y

--norma::Double->Double->Double
norma::Floating a => a -> a -> a
norma x y = sqrt (x*x+y*y)

{-norma2::(Double,Double)->Double
norma2 t = (a**2+b**2)**0.5
 where (a,b)=t-}

doble::Double->Double
doble x = 2*x 

esPositivo::Int->Bool
esPositivo x =  x > 0

entre::Double->Bool
entre x = (0 <= x && x <= 9)

esMultiploDe3::Int->Bool
esMultiploDe3 x =  mod x 3 == 0 

celsius2farh::Double->Double
celsius2farh x = x*(9/5)+32 

fact::Int->Int
fact 0 = 1
fact n
 |n>0 = n*fact (n-1)
 |otherwise = error "se necesita un numero positivo"

revertir::[a]->[a]
revertir [] = []
revertir (x:xs) = revertir xs ++ [x]

fibo::Int->Int
fibo 0 = 0
fibo 1 = 1
fibo n = fibo (n-2) + fibo (n-1)

{-
Por las dudas, para quien no sepan, los comentarios de mas de una linea se escriben 
entre corchetes seguidos de un menos "{-Comentario-}"

Los comentarios de una linea se escriben con "--Coemntario"

Recuerden que para ejecutar los archivos primero tienen que ejecutar en consola los
siguientes comandos:
    1: Abrir una consola y ejecutar el comando "ghci"
    2: Ejecutar el comando ":l ./(nombre del archivo).hs" (el comando es la abreviacion de :load)
    3: Si no presenta ningun error pueden testear el modulo que les interesa
        ejemplo:    esPositivo 10 
                    esPositivo (-10)
                    norma 4 5
    4: Si el programa no funciono o quieren seguir programando, trabajaen libremente en su editor
        de preferencia
    5: Si corrigieron el error o agregaron más codigo recuerden guardar el archivo
        (a mi varias veces me paso que agregue codigo o modifique varias veces codigo
        que si funcionaba por no guardar el archivo)
    6: Como la direccion del archivo ya se cargo con :l ahora pueden usar libremente el
        ":r" que recarga los codigos del .hs cargado (el comando es la abreviacion de :reload)
        aclaro, no hace falta colocar nada más, es literalmente solo eso ":r"

Sean libres de experimentar e intenten hacer diversos experimentos, los reto a que hagan
una funcion matematica de algun tipo ejemplo: f(x)= x³-x²+cos(X) o algo asi, para que
vean como funcionan las signaturs

Algo que no pude mencionar el profe y yo (y no sé si lo hara en la siguiente clase) es
ver las signaturs de funciones predefinidas como la funcion otherwise. es realmente simple
solo tienen que ingresar el comando ":t (funcion a buscar)". Ejemplo: ":t otherwise", ":t mod"7
":t div"

Ah, casi me olvido, animense a ver que sucede con la funcion "norma" al ver el ":t", le deje una
posible signatur comentada, pero vean con eso que dice el ghci, porque no es exactamente lo que
yo digo en ese prototipo

Si usan VisualStudioCode, les puedo recomendar las siguientes extensiones:
    1:  Haskell
    2:  Haskell Syntax Highlighting
    3:  haskell-linter
    4:  Haskell GHCi Debug Adapter Phoityne
    5:  Haskell (legacy)
    6:  Simple GHC (Haskell) Integration (esta es muy buena, si quieren ignoren las anteriores, esta no)

Escribiendo esto, me di cuenta que no me pagan lo suficiente, asi que disfruten y si llegaron hasta acá
muchas gracias por leer
-}
               