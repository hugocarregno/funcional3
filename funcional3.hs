-- Realizado por Carreño Hugo
{- 1.1. Armar la función que me devuelve el n-ésimo número de la serie de Fibonacci, que se arma así:
fib(1) = 1
fib(2) = 1
fib(n) = fib(n-1) + fib(n-2) si n > 2
Hacerlo con guardas, y también con pattern matching. -}

fib :: Integer -> Integer
fib 1 = 1
fib 2 = 1
fib n = fib(n-1) + fib(n-2)

fib' :: Int -> Int
fib' n | n==1 = 1
       | n==2 = 1
       | n>2  = (fib' (n-1)) + (fib' (n-2))

{- 1.2. Armar la función pertenece/2, que dados una lista y un número vale True si el número está en la lista. Ayuda: Acá hay que pensar bien el caso base y variantes del caso recursivo. -}

pertenece :: [Integer] -> Integer -> Bool
pertenece [] n = False
pertenece (x:xs) n = (x == n) || pertenece (xs) n

-- otra solucion
pert :: Int -> [Int] -> Bool
pert n [] = False
pert n (x:xs) | n==x = True
              | otherwise = pert n xs 
{- 1.3. Armar una función intersección/2 que dadas dos listas me devuelve la lista de los elementos que están en las dos. -}

interseccion :: [Integer] -> [Integer] -> [Integer]
interseccion [] [] = []
interseccion (x:xs) [] = []
interseccion [] (x:xs) = []
interseccion (x:xs) (y:ys) = if(pertenece (y:ys) x) then x:interseccion xs ys else interseccion xs ys
-- otra solucion
inter :: [Int] -> [Int] -> [Int]
inter [] ys = []
inter (x:xs) ys | (pert x ys) =  x : (inter xs ys)
                | otherwise        =  inter xs ys

{- 1.4. Armar una función transformadaLoca que dada una lista de números, devuelva otra a partir de la original tal que a los elementos mayores a 19 los elimina a los menores a 19 pares les suma 2 a los menores a 19 impares les suma 1 -}

transformadaLoca :: [Integer] -> [Integer]
transformadaLoca [] = []
transformadaLoca (x:xs) = if(x<19 && even x) then x+2 : transformadaLoca xs else if(x<19 && odd x) then x+1 : transformadaLoca xs else [] ++ transformadaLoca xs

{- 1.5. Definir recursivamente la función productList/1, que me dada una lista me devuelva la productoria de los elementos de la misma. -}

productList :: [Int] -> Int
productList [] = 1
productList (x:xs) = x * productList xs 

{- 1.6. Definir recursivamente la función maximo/1, recibe una lista de números y devuelve el máximo número de la lista. -}

maximo :: [Int] -> Int
maximo [x] = x
maximo (x:xs) = max x (maximo xs)

{-1.7. Definir recursivamente la función menoresA/2, recibe un número n y una lista de números l, y devuelve la sublista de l de los menores a n. -}

menoresA :: Int -> [Int] -> [Int]
menoresA n [] = []
menoresA n (x:xs) = if(n>x) then [x]++menoresA n (xs) else []++menoresA n (xs)

{- 1.8.1 Armar una función promedios que dada una lista de listas me devuelve la lista de los promedios de cada
lista-elemento -}

promedios :: [[Float]] -> [Float]
promedios [] = []
promedios (x:xs) = [sum(x) / fromIntegral(length (x))]++promedios (xs)

{- 1.8.2. Armar una función promediosSinAplazos/1 que dada una lista de listas me devuelve la lista de los promedios de cada lista-elemento, excluyendo los que sean menores a 4 que no se cuentan.-}

promediosSinAplazos :: [[Float]] -> [Float]
promediosSinAplazos [] = []
promediosSinAplazos (x:xs) = if(sum(x) / fromIntegral(length (x))>4)  then [sum(x) / fromIntegral(length (x))]++promediosSinAplazos (xs) else []++promediosSinAplazos (xs)

{- 1.9. Definir la función diferencias/1 recursivamente, que recibe una lista de números devuelve la diferencia, en valor absoluto, de cada uno con el siguiente, excepto el último que no tiene siguiente. -}

diferencias :: [Int] -> [Int]
diferencias [] = []
diferencias [x] = []
diferencias (x:y:xs) = (abs(x - y)):(diferencias (y:xs))

{- 1.10. Definir la función sinRepetidos/1 -}

sinRepetidos :: [Int] -> [Int]
sinRepetidos [] = []
sinRepetidos (x:xs) = x : sinRepetidos(filter(/=x) xs)

{- 1.11.1 Definir la función alVesre, que recibe una lista y devuelve otra con los mismos elementos pero en el orden inverso. -}

alVerse :: [a] -> [a]
alVerse [] = []
alVerse (x:xs) = (alVerse xs)++[x] 

{- 1.11.2 Pensar primero y probar después la respuesta que va a dar Haskell ante estas consultas

alVesre ["la", "tarde", "se", "puso", "linda"]
alVesre ["la tarde se puso linda"] -}

{- 1.12.1 Definir la función sinExtremos, que recibe una lista de números y devuelve lo que resulta de sacar los números máximo y el mínimo. 
Nota: Si la lista tiene menos de dos elementos, sinExtremos debe devolver la lista vacía.-}

sinExtremos :: [Integer] -> [Integer]
sinExtremos (xs) | ((xs) == []) || (tail (xs) == []) || (drop 2 (xs) == []) = []
                     | (head (xs) /= foldl1 (max) (xs)) && (head (xs) /= foldl1 (min) (xs)) = (take 1 (xs)) ++ sinExtremos (tail (xs))
                     | (head (xs) == maximum (xs)) || (head (xs) == minimum (xs)) = sinExtremos (tail (xs) ++ take 1 (xs))

{- 1.12.2. Definir la función sinPuntas/2, que generaliza sinExtremos diciendo cuántos quiero sacar de cada punta. P.ej. se espera que
Main> sinPuntas 2 [38,3,86,341,29,42,35,9]
[38,29,42,35].
¿Cómo se puede definir sinExtremos usando sinPuntas?-}

sinPuntas :: Integer -> [Integer] -> [Integer]
sinPuntas n (xs) | n == 1 = sinExtremos xs
                  | otherwise = sinExtremos(sinPuntas (n-1) xs)
{- falta version sinExtremos usando sinPuntas -}

{- 1.12.3. Definir la función sonTodosIguales/1, que dada una lista devuelve True si todos sus elementos son iguales.-}

sonTodosIguales :: [Integer] -> Bool
sonTodosIguales (x:xs) | xs == [] = True
                       | (xs /= [] && x == head xs) =  sonTodosIguales xs
                       | otherwise = False

{- 1.13.1 Implementar la función esPar/1, que dado un número natural (entero >= 0) indique si es par o no, usando únicamente sumas y/o restas (o sea, no vale hacer mod, ni usar modr).-}

esPar :: Integer -> Bool
esPar n | n < 0 = False
        | n == 2 = True
        | otherwise = esPar (n-2)

{- 1.14.1 Implementar la función divr/2, que es la misma función que div (parte entera de la división, p.ej. div 14 3 es 4), de forma tal que se usen sólo sumas y/o restas.-}

divr x y | x > y = divr (x - y) y
         | otherwise = x + y

{-1.14.2. Idem para mod: implementar modr/2.-}

{- 1.14.3. Definir la función devolverParDivMod/2 que devuelve el par, el primer elemento de la tupla el divr/2 y el segundo el modr/2 . Por. Ej: 
Main> devolverParDivMod 14 3
(4,2) -}

{- 1.16.1. Definir la función mcd/2, que recibe dos números y devuelve su máximo común divisor. Se puede usar el algoritmo de Euclides, que se describe a continuación:

r resto de a entre b (dar a r el valor del resto de a por b)
si r es 0, entonces el mcd entre a y b es b
si r es distinto de 0 entonces el mcd entre a y b es el mismo que el mcd entre b y r.

P.ej. si a = 12 y b = 4, el mcd es 4, porque el resto de dividir a por b es 0. Si a = 30 y b = 9, el mcd entre a y b es el mismo que hay entre 9 y 3, o sea entre b y el resto de dividir a por b. -}

{-1.16.2. Definir la función mcm'/2 (sí, vale poner apóstrofe, es como decir "mcm prima") que devuelva el mínimo común múltiplo entre dos números calculándolo así:
si el mayor divide al menor, ya está, el mcm es el mayor.
si no pruebo con el mayor * 2.
si no pruebo con el mayor * 3. ... y así, en algún momento va a parar (¿por qué?).
P.ej. quiero calcular el mcm entre 10 y 6
10 no divide a 6
20 no divide a 6
30 sí divide a 6 ¡voilá! el mcm entre 10 y 6 es 30.-}

{-1.19. Implementar la función nroCapicua/1 que indica si un número es o no capicúa.-}

nroCapicua :: Integer -> Bool
nroCapicua n = (show n) == (reverse (show n))

{-1.22. Definir la función estaOrdenada/1, que recibe una lista de números e indica si está ordenada de menor a
mayor o no.-}



{-1.27.1. Ordenamiento Definir la función ordenar/1, que recibe una lista y devuelve otra con los mismos elementos de la anterior pero ordenados de menor a mayor.-}

main = do
  print("fib 10")
  print(fib 10)
  print("pertenece [1,2] 3")
  print(pertenece [1,2] 3)
  print("interseccion [1,2][4,1]")
  print(interseccion [1,2][4,1])
  print("transformadaLoca [8,15,22,9,101,13]")
  print(transformadaLoca [8,15,22,9,101,13])
  print("productList [1,2,3,4,5]")
  print(productList [1,2,3,4,5])
  print("maximo [1,2,3,4,5]")
  print(maximo [1,2,3,4,5])
  print("menoresA 20 [23,5,16,38,11,24]")
  print(menoresA 20 [23,5,16,38,11,24])
  print("promedios [[8,6],[7,9,4],[6,2,4],[9,6]]")
  print(promedios [[8,6],[7,9,4],[6,2,4],[9,6]])
  print("promediosSinAplazos [[8,6],[6,2,4]]")
  print(promediosSinAplazos [[8,6],[6,2,4]])
  print("diferencias [5,8,3,1,9]")
  print(diferencias [5,8,3,1,9])
  print("sinRepetidos [1,2,1,3,1,4,2,5,3]")
  print(sinRepetidos [1,2,1,3,1,4,2,5,3])
  print("alVerse [1,3,5,7,3,4]")
  print(alVerse [1,3,5,7,3,4])
  print("alVerse ['la', 'tarde', 'se', 'puso', 'linda']")
  print(alVerse ["la", "tarde", "se", "puso", "linda"])
  print("alVerse ['la tarde se puso linda']")
  print(alVerse ["la tarde se puso linda"])
  print("sinExtremos [38,3,86,341,29,42,35,9]")
  print(sinExtremos [38,3,86,341,29,42,35,9])
  print("sinPuntas 2 [38,3,86,341,29,42,35,9]")
  print(sinPuntas 2 [38,3,86,341,29,42,35,9])
  print("sonTodosIguales [1,2,3]")
  print(sonTodosIguales [1,2,3])
  print("esPar 6")
  print(esPar 6)
  --print("divr 13 3")
  --print(divr 13 3)
  print("nroCapicua 222")
  print(nroCapicua 222)