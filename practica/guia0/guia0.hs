--                                                         Guia 0

-- Ejercicio 1 

-- null : Indica si una estructura es vacia, su tipo es : Foldable t => t a -> Bool

-- head : Devuelve el primer elemento de una estructura, su tipo es :  Foldable t => t a -> a 

-- tail : Dada una lista, devuelve la misma lista sin el primer elemento, su tipo es: Foldable t => t a -> a 

-- init : Dada una lista, devuelve la misma lista sin el ultimo elemento: [a] -> [a]

-- last : Dada una lista, devuelve el ultimo elemento : [a] -> a

-- take : Dado un numero n y una lista, devuelve los primeros n elementos de la lista (si n es mas grande, devuelve la lista completa): Int -> [a] -> [a] 

-- drop: Dado un numero n y una lista, devuelve una lista sin los primeros n elementos de la lista : Int -> [a] -> [a]

-- (++) : Dadas dos listas, se encarga de unir la primer lista pasada como argumento a la segunda : [a] -> [a] -> [a]

-- concat : Dado un elemento y una lista, inserta ese elemento en la cabeza de la lista : a -> [a] -> [a]

-- reverse: Devuelve una lista con el orden de sus elementos invertidos: [a] -> [a]

-- elem: Dado un elemento (que sea comparable)y una estructura, indica si el elemento pertenece a ella: (Foldable t ,Eq a) =>  a -> t a  -> Bool



-- Ejercicio 2:

-- a 

valorAbsoluto :: (Ord a, Num a) => a -> a
valorAbsoluto n 
  | n < 0     = -n
  | otherwise = n


-- b 
bisiesto :: Int -> Bool
bisiesto n = (n `mod` 400 == 0) || ((n `mod` 4 == 0) && (n `mod` 100 /= 0))


-- c 

factorial :: Int -> Int
factorial 0 = 1 
factorial n = n * factorial (n-1)

-- d 

esPrimo :: Int -> Bool
esPrimo n | n <= 0 = False
esPrimo n = esPrimoAux n (n-1)


-- se puede optimizar haciendo lo de que si es compuesto hay un primo que lo divide que es menor que la raiz cuadrada del compuesto. 
esPrimoAux :: Int -> Int -> Bool
esPrimoAux n i | n `mod` i == 0 && i /= 1  = False
               | i == 1 =  True
               | otherwise = esPrimoAux n (i-1) 


cantDivisoresPrimos :: Int -> Int
cantDivisoresPrimos n = cantDivisoresPrimosAux n (n-1)

cantDivisoresPrimosAux :: Int -> Int -> Int 
cantDivisoresPrimosAux n 0 = 0 
cantDivisoresPrimosAux n 1 = 0 
cantDivisoresPrimosAux n i | n `mod` i == 0 &&  esPrimo i = 1 + cantDivisoresPrimosAux n (i-1)
cantDivisoresPrimosAux n i =  cantDivisoresPrimosAux n (i-1)


-- Ejercicio 3

--data Maybe a = Nothing | Just a
-- data Either a b = Left a | Right b 


inverso :: Float -> Maybe Float 
inverso 0 = Nothing
inverso n = Just (1/n)


aEntero :: Either Int Bool -> Int
aEntero (Left x) = x 
aEntero (Right x) | x == True = 1 
                  | otherwise = 0

-- Ejercicio 4

-- a 
limpiar :: String -> String -> String
limpiar _ [] = []
limpiar ys (x:xs) | x `elem` ys = limpiar ys xs 
                  | otherwise = x : limpiar ys xs 


-- b 
difPromedio :: [Float] -> [Float] 
difPromedio xs = difPromedioAux xs  (sum xs / fromIntegral(length xs))
  
difPromedioAux :: [Float] -> Float -> [Float]
difPromedioAux [] _ = [] 
difPromedioAux (x:xs) i =  x - i : difPromedioAux xs i 

-- c
todosIguales :: [Int] -> Bool
todosIguales [] = True
todosIguales (x:xs) =  todosIgualesAux xs x 


todosIgualesAux :: [Int] -> Int -> Bool
todosIgualesAux [] _ = True
todosIgualesAux (y:ys) x = x == y && todosIgualesAux ys x 


-- Ejercicio 5 


data AB a = Nil | Bin (AB a) a (AB a)


vacioAB  :: AB a -> Bool
vacioAB Nil = True 
vacioAB _ = False


negacionAB  :: AB Bool  -> AB Bool
negacionAB Nil = Nil
negacionAB (Bin i r d)  = Bin (negacionAB d) (not r) (negacionAB i)
                       


productoAB :: AB Int -> Int
productoAB Nil = 1 
productoAB (Bin i r d) = r * (productoAB i) * (productoAB d)