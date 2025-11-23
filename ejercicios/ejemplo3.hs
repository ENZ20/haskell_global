qsort::(Ord a)=>[a]->[a]
qsort [] = []
qsort (x:xs) = qsort ([y|y<-xs,y<=x]) ++ [x] ++ qsort ([y|y<-xs,y>x])

fibo::Int->Int
fibo 0 = 1
fibo 1 = 1
fibo x = fibo (x-1) + fibo (x-2)

limpiar::String->String->String
limpiar [] y = y
limpiar (x:xs) y = limpiar xs (limpiarelem x y)

limpiarelem::Char->String->String
limpiarelem _ [] = []
limpiarelem x (y:ys)
 |x==y = limpiarelem x ys
 |otherwise = y : limpiarelem x ys

pares::[a]->[b]->[(a,b)]
pares _ [] = []
pares [] _ = []
pares (x:xs) y = (minpares x y) ++ (pares xs y)
{-pares x y 
 |otherwise = (minpares cab y) ++ (pares col y)
 where
 cab = head x
 col = tail x -}

minpares::a->[b]->[(a,b)]
minpares _ [] = []
minpares x (y:ys) = (x,y) : minpares x ys 



{-
int fibo(int n){
    int i=0, ant=1 sig=1,aux #body1
    while(i<n){
        aux=ant
        ant=sig
        sig=aux+sig
        i++
    }                       #replazar
    return ant              #body2
}
-}

fiboo::Int->(Int,Int,Int)
fiboo n
 |i <= n = (antr,sigr,ir)
 |otherwise = (1,1,n)
 where
 i=0
 ant = 1
 sig = 1
 (antr,sigr,ir) = fibor n i ant sig

fibor::Int->Int->Int->Int->(Int,Int,Int)
fibor n i ant sig
 |i < n = fibor n (i+1) antr sigr
 |otherwise = (antr,sigr,i)
 where
 (antr,sigr)=(sig,ant+sig)

{-
bisctriz(float a, float b, float e, func f){
    float aux,c
    if(a>b){
        aux=a
        a=b
        b=a
        }
    if(f(a)*f(b)<0){
        c=(a+b)/2
        while(abs(f(c))>e){
            if(f(a)*f(c)<0){
                b=c
                }
            else{
                a=c
            }
            c=(a+b)/2
        }
    }
    return c
}
-}

bicectriz::Double->Double->Double->(Double->Double)->(Double,Int)
bicectriz a b e f
 |(a>b) && (f a) * (f b) < 0 = bicetrizr b a e 0 f
 |(f a) * (f b) < 0 = bicetrizr a b e 0 f
 |otherwise = error "No cumple condiciones"

bicetrizr::Double->Double->Double->Int->(Double->Double)->(Double,Int)
bicetrizr a b e i f  
 |abs (f c) > e && f a * f c < 0 = bicetrizr a c e (i+1) f
 |abs (f c) > e && f b * f c < 0 = bicetrizr c b e (i+1) f
 |otherwise= (c,i)
 where
 c = (a+b)/2

lineal::Double->Double
lineal x = x-3

