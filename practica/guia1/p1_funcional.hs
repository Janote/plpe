import Text.XHtml (alt)
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


uncurry' :: (a -> b -> c) -> (a,b) -> c
uncurry' f (x,y) = f x y


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
foldrsum = sum

foldrelem :: Eq a => a -> [a] -> Bool
foldrelem elem = foldr (\x rec -> x == elem || rec) False

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
foldrmap f = map f

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
sumaAlt2 = foldl (-) 0



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

-- Ejercicio 5 

-- a) no es recursion estructural porque se hace referencia a xs en el conditional del if
-- b) si es, la definicion es: 

entrelazar :: [a] -> ([a] -> [a])
entrelazar = foldr (\ x fr ys -> if null ys then x : fr ys else x : head ys : fr (tail ys)) id

{- 

entrelazar [1,2,3] [4,5,6] = 

        foldr (\x fr -> \ys -> if null ys then x : fr ys else x : head ys : fr (tail ys)) id [1,2,3] [4,5,6]
        (\ys -> ......)  [4,5,6]
        (if null [4,5,6] then 1:(foldr f id [2,3]) []) else 1: 4: (foldr f id [] [5,6])
         1 : 4 :  id [5,6] 

-}

-- Ejercicio 6: 

recr :: (a -> [a] -> b -> b) -> b -> [a] -> b
recr f z [] = z
recr f z (x : xs) = f x xs (recr f z xs)

sacarUna :: Eq a => a -> [a] -> [a]
sacarUna m = recr (\x xs rec -> if x == m then xs else x:rec) []

-- b 
{- 
El tema es que con foldr no puedo salir del bucle, ya que por mas que haga  foldr (\x rec -> if x == m then xs else x:rec) [] xs
entra con la copia original de xs, no es que se va actualizando el resto de la estructura, encambio con recr si.
-}


-- c 
insertarOrdenado :: Ord a => a -> [a] -> [a]
insertarOrdenado x = recr (\y ys rec -> if x < y then x:y:ys else y:rec) []


mapPares :: (a -> b -> c) -> [(a,b)] -> [c]
mapPares f = map (uncurry' f)


armarPares :: [a] -> [b] -> [(a,b)]
armarPares = foldr (\ x fr ys -> if null ys then [] else (x,head ys): fr (tail ys) ) (const [])


mapDoble :: (a -> b ->c) -> [a] -> [b] -> [c]
mapDoble f =  foldr (\ x fr ys -> if null ys then [] else f x (head ys):fr (tail ys)) (const [])

sumaMat :: [[Int]] -> [[Int]] -> [[Int]]
sumaMat = foldr (\ x fr ys -> if null ys then [] else zipWith (+) x (head ys) : fr (tail ys)) (const [])

-- transponer


-- Ejercicio 9

foldNat :: (Integer -> b -> b) -> b -> Integer -> b
foldNat f cBase 1 = cBase
foldNat f cBase n = f n (foldNat f  cBase (n-1))

potencia :: Integer -> Integer -> Integer
potencia base = foldNat (\x fNat -> base * fNat) base

-- Ejercicio 10


-- Ejercicio 11



data Polinomio a = X | Cte a | Suma (Polinomio a) (Polinomio a) | Prod (Polinomio a) (Polinomio a ) deriving Show


foldPoli :: b -> (a -> b) -> (b -> b -> b) -> (b -> b -> b) -> Polinomio a -> b
foldPoli fX fCte fSuma fProd X = fX
foldPoli fX fCte fSuma fProd (Cte n) = fCte n
foldPoli fX fCte fSuma fProd (Suma p q ) = fSuma (foldPoli fX fCte fSuma fProd p) (foldPoli fX fCte fSuma fProd q)
foldPoli fX fCte fSuma fProd (Prod p q ) = fProd (foldPoli fX fCte fSuma fProd p) (foldPoli fX fCte fSuma fProd q)


polino = Suma (Suma (Prod X X) X) (Cte 5)

evaluarPoli :: Num a => a -> Polinomio a -> a
evaluarPoli n = foldPoli n id (+) (*)

-- Ejercicio 12

data AB a = Nil | Bin (AB a) a (AB a) deriving Show



foldAB :: b -> (b -> a -> b -> b) -> AB a -> b
foldAB fNil _ Nil = fNil
foldAB fNil fBin (Bin i r d) = fBin (foldAB fNil fBin i ) r (foldAB fNil fBin d)


recAB :: b -> (b -> a -> b -> AB a ->AB a -> b) -> AB a -> b
recAB fNil fBin Nil = fNil
recAB fNil fBin (Bin i r d) = fBin (recAB fNil fBin i) r (recAB fNil fBin d) i d



esNil :: AB a -> Bool
esNil arbol = case arbol of
                Nil -> True
                _ -> False



altura :: AB a -> Integer
altura  = foldAB 0 (\i _ d-> 1 + max i d)


cantNodos :: AB a -> Integer
cantNodos = foldAB 0 (\i _ d -> 1 + i + d )

{- 
cantNodos arbolito = foldAB 0 (\i _ d -> 1 + i + d) (Bin (Bin (Bin Nil 4 Nil) 3 (Bin Nil 2 Nil)) 5 (Bin Nil 1 Nil))
= foldAB 0 fSuma3 (Bin (Bin Nil 4 Nil) 3 (Bin Nil 2 Nil) + 1 + foldAB 0 fSuma3 (Bin Nil 1 Nil)
= .........
-}

mejorSegunAB :: (a -> a -> Bool) -> AB a -> a
mejorSegunAB f (Bin i v d) = foldAB v (\ri r rd -> criterio ri (criterio r rd)) (Bin i v d)
                        where criterio x y = if f x y then x else y


{- 
         5
       3   1 
      4  2   nil
-}



arbolito = Bin (Bin (Bin Nil 4 Nil) 3 (Bin Nil 2 Nil)) 5 (Bin Nil 1 Nil)

esABB :: Ord a => AB a -> Bool
esABB  = recAB True (\ri r rd i d -> if esNil d then r > raiz i && ri else r < raiz d && rd && ri )


raiz :: AB a -> a
raiz (Bin _ r _) = r



-- Ejercicio 13
-- data AB a = Nil | Bin (AB a) a (AB a) deriving Show
ramasAB :: AB a -> [[a]]
ramasAB = foldAB [[]] (\ri r rd -> map(r:) ri ++ map(r:) rd )

raizAB :: AB a -> a 
raizAB (Bin _ r _ ) = r 

esNilAB :: AB a -> Bool 
esNilAB (Bin _ r _ ) = False
esNilAB _ = True

hijoIzquierdo :: AB a -> AB a
hijoIzquierdo (Bin i _ _  ) = i 


hijoDerecho :: AB a -> AB a
hijoDerecho (Bin _ _ d  ) = d 

mismaEstructura :: Eq a => AB a -> AB a -> Bool
mismaEstructura arbol = foldAB (\ab -> if esNil ab then True else False) (\ri r rd -> \arbol2 -> if esNil arbol2 then False else r == raizAB arbol2 && ri (hijoIzquierdo arbol2) && rd (hijoDerecho arbol2)) arbol 


{- 
ramass = foldAB [] f arbolito

[5: (foldAB [] f (Bin (Bin Nil 4 Nil) 3 (Bin Nil 2 Nil)))] ++ [5: foldAB [] f (Bin Nil 1 Nil)]
.... ++ [5: ([1 :foldAB [] f Nil] ++ [1 : foldAB [] f Nil])]
.... ++ [5: ([1:[]] ++ [1: []])]
.... ++ 
-}


-- RoseTree clase

data RoseTree a = Rose a [RoseTree a] deriving Show

listaMasLarga :: [[Integer]] -> [Integer]
listaMasLarga = foldr (\x rec -> if length x > length rec then x else rec ) []

{- 
                        1
                2               3       4
        5 
-}
arbolDeRosas  = Rose 1 [Rose 2 [Rose 5 []] , Rose 3 [] ,  Rose 4 []]

foldRoseTree :: (a -> [b] -> b) -> RoseTree a -> b 
foldRoseTree f (Rose v hijos) = f v (map (foldRoseTree  f) hijos)


hojasRT :: RoseTree a -> [a]
hojasRT = foldRoseTree (\head hijos -> if null hijos then [head] else concat hijos)

sumaNodosRT :: RoseTree Integer -> Integer
sumaNodosRT (Rose v hijos) = foldRoseTree (\x rec -> x+ sum rec ) (Rose v hijos)

alturaRT :: RoseTree a -> Int
alturaRT = foldRoseTree (\head hijos -> if null hijos then 1 else  1 + maximum hijos)


-- arreglar esta 
distanciasRT :: RoseTree a -> [[a]]
distanciasRT = foldRoseTree (\head hijos -> if null hijos then [[head]] else concatMap ([head]:) hijos)

{- 
distanciasRT = concatMap ([1]:) (foldRoseTree f (Rose 2 []))
             = concatap ([1]:) ([[2]])
-}



arbolitoDerosas :: RoseTree Integer
arbolitoDerosas = Rose 1 [Rose 2 []]


{- 
sumaNodosRT arbolDeRosas= foldRoseTree (+) arbolDeRosas
=  foldRoseTree (\x rec -> x + sum rec) (Rose 1 [Rose 2 []])
=  1 + sum (map (foldRoseTree (\x rec -> x + sum rec) [Rose 2 []]))
=  1 + foldroseTree (\x rec -> x + sum rec) Rose 2 [] + 0
= 1 + (2 + sum (map(foldroseTree (\x rec -> x + sum rec)) [])) + 0 
= 1 + (2 + sum []) + 0 
= 1 + 2 = 3 
-}

