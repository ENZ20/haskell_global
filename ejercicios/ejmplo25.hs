f::Float->Float
f x = 3*x+2

g::Float->Float->Float
g 0 0 = 0
g x y = 1/(x*x+y*y)

g1 x y 
 |x*x+y*y==0 = 0
 |otherwise = 1/(x*x+y*y)

fact 0 = 1
fact x = fact (x-1) * x

fibo 0 = 1
fibo 1 = 1
fibo x = fibo (x-1) + fibo (x-2)

{-func :: (Integral a1, Num t, Num a2, Eq a2) =>((a1 -> a1 -> a1) -> t -> a2) -> IO ()
func x = if x rem 2 == 0
 then putStrLn "numero par"
 else putStrLn "numero impar"-}

sumDig1::Int->Int
sumDig1 0 = 0
sumDig1 x = (mod x 10) + sumDig1 (div x 10)

sumDig2::Int->Int
sumDig2 x
 |x<=9 = x
 |otherwise = (mod x 10) + sumDig2 (div x 10)
boomBang::[Int]->[String]
boomBang xs = [if x < 10 then "boom" else "bang" | x<-xs,odd x]

suma::Int->Int->Int
suma x y = x+y

swap::(Int,Int)->(Int,Int)
swap (a,b) = (b,a)
tup2List::(Int,Int)->[Int]
tup2List (a,b) = [a..b] ++ reverse [a..b]
palindromo::String->Bool
palindromo x = x == reverse x
insertTup::(Char,Char,String)->String
insertTup (a,b,s) = a : s ++ [b] 