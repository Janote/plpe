--                                              Guia 1

{- 
 Ejercicio 1 

max2 :: Ord a , Ord b => (a,b) -> a

normaVectorial :: Num a , Num b => a -> b -> Float

subtract :: Num a => a -> a -> a 

predecesor :: Num a => a -> a

evaluarEnCero :: Num b => (b-> a) -> a 

dosVeces :: (a -> a) -> a -> a 



Vamos con flipall, lo que hace esta funcion es que toma una lista de funciones
e invierte el orden de sus argumentos

flipall = map flip 

bien, yo se que map es :: (a -> b) -> [a] -> [b]

y flip es :: (a' -> b' -> c) -> (b' -> a' -> c)

Entonces :   
a = (a' -> b' -> c) 
b = (b' -> a' -> c)

Pero la funcion ya la toma , que es flip, por lo tanto queda [a] -> [b] o lo que es lo mismo....

map flip :: [a -> b -> c] -> [b -> a -> c]


flipRaro: flip flip

flip :: (a -> b -> c) -> b -> a -> c

        (a' -> b' -> c') -> b' -> a' -> c'

-}


-- filter :: (a -> Bool) -> [a] -> [a]
-- filter _ [] = []
-- filter _ [x] = [x]
-- filter p (x:xs) = if p x then x: filter p xs else filter p xs


-- Ejercicio 2 

curry :: ((a,b) -> c) -> a -> b -> c
curry f x y =  f (x,y)


uncurry :: (a -> b -> c) -> (a,b) -> c
uncurry f (x,y) = f x y


{- 
Se p odría definir una función curryN, que tome una función de un número arbitrario de argumentos y
devuelva su versión currificada?
Sugerencia: pensar cuál sería el tipo de la función


curryN :: ()
PREGUNTAR: Me parece que no, ya que nunca podriamos declarar la cantidad de argumentos en el tipo


-- Esta sería la idea, pero no se puede hacer en Haskell.
curryN :: ((a, bs...) -> c) -> a -> bs... -> c
curryN f a bs = f (a, bs...)


-}

{- 
Ejercicio 3 ⋆
i. Redefinir usando foldr las funciones sum, elem, (++), filter y map.
ii. Definir la función mejorSegún :: (a -> a -> Bool) -> [a] -> a, que devuelve el máximo elemento
de la lista según una función de comparación, utilizando foldr1. Por ejemplo, maximum = mejorSegún
(>).
iii. Definir la función sumasParciales :: Num a => [a] -> [a], que dada una lista de números devuelve
otra de la misma longitud, que tiene en cada posición la suma parcial de los elementos de la lista original
desde la cabeza hasta la posición actual. Por ejemplo, sumasParciales [1,4,-1,0,5] ; [1,5,4,4,9].
iv. Definir la función sumaAlt, que realiza la suma alternada de los elementos de una lista. Es decir, da como
resultado: el pimer elemento, menos el segundo, más el tercero, menos el cuarto, etc. Usar foldr.
v. Hacer lo mismo que en el punto anterior, pero en sentido inverso (el último elemento menos el anteúltimo,
etc.). Pensar qué esquema de recursión conviene usar en este caso.

-}

{- 
DEFINICION DE FOLDR

foldr :: (a -> b -> b) -> b -> [a] -> b 
foldr f z [] = z
foldr f z (x : xs) = f x (foldr f z xs)

foldl :: (b -> a -> b) -> b -> [a] -> b
foldl f ac [] = ac
foldl f ac (x : xs) = foldl f (f ac x) xs

-}

--3 ) i 

foldrsum :: Num a => [a] -> a
foldrsum = foldr (+) 0

foldrelem :: Eq a => a -> [a] -> Bool
foldrelem elem = foldr (\x rec -> if x == elem then True else rec) False

foldrfilter :: (a -> Bool) -> [a] -> [a]
foldrfilter p = foldr (\x rec -> if p x then x : rec else rec) []
{- 
foldrfilter even [1,4] = foldr (\x rec -> if even x then x: rec else rec) [] [1,4]
= if even 1 then 1: (foldr f [] [4]) else (foldr f [] [4])
= foldr f [] [4]
= if even 4 then 4 : (foldr f [] []) else (foldr f [] [])
= 4: foldr f [] [] = 4 : [] = [4]
-}

foldrmap :: (a -> b) -> [a] -> [b]
foldrmap f = foldr (\x rec -> f x : rec) []

-- ii) foldr1 usa el ultimo elemento como base

mejorSegun :: (a -> a -> Bool) ->  [a] -> a
mejorSegun criterio = foldr1 (\x rec -> if criterio x rec then x else rec)


-- iii )
{- 

foldr f [] 

-}
sumasParciales :: Num a => [a] -> [a]
sumasParciales  = foldr (\x rec -> x : map (+x) rec) []


sumaAlt :: Num a => [a] -> a
sumaAlt = foldr (-) 0


sumaAlt2 :: Num a => [a] -> a
sumaAlt2 = foldl (\ac x -> ac - x ) 0



-- Ejercicio 4 

partes :: [Integer] -> [[Integer]]
partes [] = [[]]
partes (x:xs) =  partes xs ++ map ([x] ++ ) (partes xs)

{- 
partes [1,2,3] = 
        map ([1] ++ ) (partes [2,3])
        map ([1] ++ ) (map ([2]++) (partes [3]))
        map ([1] ++ ) (map ([2]++) (map ([3]++) (partes [])))
        map ([1] ++ ) (map ([2]++) (map ([3]++) [[]]))

(map ([3]++) [[]]) = [[3],[]]

-}

prefijos :: [Integer] -> [[Integer]]
prefijos [] = [[]]
prefijos (x:xs) = [x] : map ([x]++) (prefijos xs)



sublistas :: [Integer] -> [[Integer]]
sublistas [] = [[]]
sublistas (x:xs) = map (x :) (prefijos xs) ++ sublistas xs


{- 
sublistas [5,1,2] = [5,1,2] ++ 

[1]}]}]

-}





{- 
sublistas []

-}