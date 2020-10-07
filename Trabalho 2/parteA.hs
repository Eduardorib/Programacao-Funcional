l1 :: [Integer]
l1 = [1 .. 1000]

l2 :: [Integer]
l2 = [1000, 999 .. 1]

l3 :: [Integer]
l3 = l1 ++ [0]

l4 :: [Integer]
l4 = [0] ++ l2

l5 :: [Integer]
l5 = l1 ++ [0] ++ l2

l6 :: [Integer]
l6 = l2 ++ [0] ++ l1

l7 :: [Integer]
l7 = l2 ++ [0] ++ l2

x1 :: [Integer]
x1 = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20]

x2 :: [Integer]
x2 = [20, 19, 18, 17, 16, 15, 14, 13, 12, 11, 10, 9, 8, 7, 6, 5, 4, 3, 2, 1]

x3 :: [Integer]
x3 = [11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10]

x4 :: [Integer]
x4 = [10, 9, 8, 7, 6, 5, 4, 3, 2, 1, 20, 19, 18, 17, 16, 15, 14, 13, 12, 11]

x5 :: [Integer]
x5 = [11, 12, 13, 14, 15, 5, 4, 3, 2, 1, 16, 17, 18, 19, 20, 10, 9, 8, 7, 6]

x6 :: [Integer]
x6 = [1, 12, 3, 14, 5, 15, 4, 13, 2, 11, 6, 17, 8, 19, 20, 10, 9, 18, 7, 16]

x7 :: [Integer]
x7 = [20, 8, 2, 11, 13, 3, 7, 18, 14, 4, 16, 10, 15, 1, 9, 17, 19, 12, 5, 6]

-- Ex 1


-- a) Refaça a implementação do algoritmo Seleção usando funções genéricas (foldr ou foldr1).

selecao :: Ord a => [a] -> [a]
selecao [] = []
selecao xs = [x] ++ selecao (remove x xs)
                    where x = foldr1 (min) xs

remove :: Eq t => t -> [t] -> [t]
remove a [] = []
remove a (x:xs)
                | a==x = xs
                | otherwise = (x:(remove a xs))
                
-- b) Refaça a implementação do algoritmo Inserção usando funções genéricas (foldr ou foldr1).

insercao :: (Ord a) => [a] -> [a]
insercao = foldr (insereOrd) []


insereOrd :: Ord t => t -> [t] -> [t]
insereOrd x [] = [x]
insereOrd x (y:ys)
                 | x < y     = x : y : ys
                 | otherwise = y : insereOrd x ys 

-- c) Refaça a implementação do algoritmo quicksort usando funções genéricas (filter):
        -- modifique a função principal do algoritmo (quicksort) para que seja utilizada a função
        -- de alta ordem (genérica) filter para a obtenção dos elementos maiores e menores do
        -- que o pivô a cada iteração.


quickSort :: Ord a => [a] -> [a]
quickSort [] = []
-- s eh o head e tmb eh o pivo 
quickSort (s:xs) = (quickSort menor) ++ [s] ++ (quickSort maior)
                   where menor = filter (<s) xs
                         maior = filter (>=s) xs


-- Ex 2

bubbleOrig :: (Ord a) => [a] -> [a]
bubbleOrig [] = []
bubbleOrig l = bubbleOrdOrig l (length l)

bubbleOrdOrig :: (Ord a) => [a] -> Int -> [a]
bubbleOrdOrig l 0 = l
bubbleOrdOrig l n = bubbleOrdOrig (trocaOrig l) (n -1)

trocaOrig :: (Ord a) => [a] -> [a]
trocaOrig [x] = [x]
trocaOrig (x : y : zs)
  | x > y = y : trocaOrig (x : zs)
  | otherwise = x : trocaOrig (y : zs)

bubbleSort :: (Ord a) => [a] -> [a]
bubbleSort [] = []
bubbleSort l = fst (bubbleOrd (l, -1) (length l))

bubbleOrd :: (Ord a) => ([a], Int) -> Int -> ([a], Int)
bubbleOrd (l, num) 0 = (l, num)
bubbleOrd (l, num) n
  | num == 0 = (l, num)
  | otherwise = bubbleOrd (troca (l, 0)) (n -1)

troca :: (Ord a) => ([a], Int) -> ([a], Int)
troca ([x], num) = ([x], num)
troca ((x : y : zs), num)
  | x > y = add (troca ((x : zs), 1)) y
  | otherwise = add (troca ((y : zs), num)) x
  where
    add (a, b) c = (c : a, b)

bubbleSort2 :: (Ord a) => [a] -> [a]
bubbleSort2 [] = []
bubbleSort2 l =
  let troca [x] = [x]
      troca (x : y : xs)
        | x > y = y : troca (x : xs)
        | otherwise = x : troca (y : xs)

      separa l = (take (length l - 1) l, drop (length l - 1) l)

      bubble [x] = [x]
      bubble l = (bubble lista) ++ elemfim
        where
          trocada = troca l
          (lista, elemfim) = separa trocada
   in bubble l

bubbleSort3 :: (Ord a) => [a] -> [a]
bubbleSort3 [] = []
bubbleSort3 l =
  let troca ([x], num) = ([x], num)
      troca ((x : y : zs), num)
        | x > y = add (troca ((x : zs), 1)) y
        | otherwise = add (troca ((y : zs), num)) x
        where
          add (a, b) c = (c : a, b)

      separa l = (take (length l -1) l, drop (length l - 1) l)

      bubble ([x], num) = ([x], num)
      bubble (l, num)
        | num1 == 0 = (l, num)
        | otherwise = (fst (bubble (lista, 0)) ++ elemfim, 0)
        where
          (lista, elemfim) = separa trocada
          (trocada, num1) = troca (l, num)
   in fst (bubble (l, -1))

-- Com contador
bubbleOrigCont :: (Ord a) => [a] -> ([a], Int)
bubbleOrigCont [] = ([], 0)
bubbleOrigCont l = (bubbleOrdOrigCont (l, 0) (length l))

bubbleOrdOrigCont :: (Ord a) => ([a], Int) -> Int -> ([a], Int)
bubbleOrdOrigCont (l, cont) 0 = (l, cont)
bubbleOrdOrigCont (l, cont) n = bubbleOrdOrigCont (trocaOrigCont (l, cont)) (n -1)

trocaOrigCont :: (Ord a) => ([a], Int) -> ([a], Int)
trocaOrigCont ([x], cont) = ([x], cont)
trocaOrigCont ((x : y : zs), cont)
  | x > y = add (trocaOrigCont ((x : zs), cont + 1)) y
  | otherwise = add (trocaOrigCont ((y : zs), cont + 1)) x
  where
    add (a, cont) c = (c : a, cont)

bubbleSortCont :: (Ord a) => [a] -> ([a], Int)
bubbleSortCont [] = ([], 0)
bubbleSortCont l = exp (bubbleOrdCont (l, -1, 0) (length l))
  where
    exp (l, _, cont) = (l, cont)

bubbleOrdCont :: (Ord a) => ([a], Int, Int) -> Int -> ([a], Int, Int)
bubbleOrdCont (l, num, cont) 0 = (l, num, cont)
bubbleOrdCont (l, num, cont) n
  | num == 0 = (l, num, cont)
  | otherwise = bubbleOrdCont (trocaCont (l, 0, cont)) (n -1)

trocaCont :: (Ord a) => ([a], Int, Int) -> ([a], Int, Int)
trocaCont ([x], num, cont) = ([x], num, cont)
trocaCont ((x : y : zs), num, cont)
  | x > y = add (trocaCont ((x : zs), 1, cont + 1)) y
  | otherwise = add (trocaCont ((y : zs), num, cont + 1)) x
  where
    add (a, b, cont) c = (c : a, b, cont)

bubbleSort2Cont :: (Ord a) => [a] -> ([a], Int)
bubbleSort2Cont [] = ([], 0)
bubbleSort2Cont l =
  let add (a, b) c = (c : a, b)

      trocaCont ([x], cont) = ([x], cont)
      trocaCont ((x : y : xs), cont)
        | x > y = add (trocaCont (x : xs, cont + 1)) y
        | otherwise = add (trocaCont (y : xs, cont + 1)) x

      separa l = (take (length l - 1) l, drop (length l - 1) l)

      bubble ([x], cont) = ([x], cont)
      bubble (l, cont) = (prox ++ elemfim, contprox)
        where
          (trocada, cont1) = trocaCont (l, cont)
          (lista, elemfim) = separa trocada
          (prox, contprox) = bubble (lista, cont1)
   in bubble (l, 0)

bubbleSort3Cont :: (Ord a) => [a] -> ([a], Int)
bubbleSort3Cont [] = ([], 0)
bubbleSort3Cont l =
  let add (a, b, c) d = (d : a, b, c)

      trocaCont ([x], num, cont) = ([x], num, cont)
      trocaCont ((x : y : zs), num, cont)
        | x > y = add (trocaCont ((x : zs), 1, cont + 1)) y
        | otherwise = add (trocaCont ((y : zs), num, cont + 1)) x

      exp (l, _, cont) = (l, cont)
      separa l = (take (length l -1) l, drop (length l - 1) l)

      bubble ([x], num, cont) = ([x], num, cont)
      bubble (l, num, cont)
        | num1 == 0 = (l, num, cont)
        | otherwise = (prox ++ elemfim, 0, contprox)
        where
          (trocada, num1, cont1) = trocaCont (l, num, cont)
          (lista, elemfim) = separa trocada
          (prox, _, contprox) = bubble (lista, 0, cont1)
   in exp (bubble (l, -1, 0))

-- Ex 3

-- 1) apenas modifique a função principal do algoritmo (seleção) para que não seja
--    utilizada uma concatenação de listas a cada iteração, no passo [x] ++ selecao (remove x xs). Ao
--    invés disso, utilize o operador de construção de listas “:”, como em a:b.

selecao1 :: Ord a => [a] -> [a]
selecao1 [] = []
selecao1 xs = (x:selecao1 (remove x xs))
                    where x = minimo xs

remove1 :: Eq t => t -> [t] -> [t]
remove1 a [] = []
remove1 a (x:xs)
                | a==x = xs
                | otherwise = (x:(remove1 a xs))
                
minimo :: Ord a => [a] -> a
minimo [] = undefined
minimo [x] = x
minimo (x:xs)
             | x <= (minimo xs) = x
             | otherwise = minimo xs


-- 2) a partir da Variação 1, refazer o código para que a busca pelo menor elemento
--   (função mínimo) e a eliminação desse menor elemento da lista a ser ordenada (função remove)
--   ocorra numa mesma função (remove_menor), sem a necessidade de se percorrer a lista duas
--   vezes a cada iteração (uma para remover e outra para remover o menor elemento).


selectionSort :: Ord a => [a] -> [a]
selectionSort [] = []
selectionSort xs = (menor:(selectionSort resto))
                    where 
                        menor = minimum xs
                        resto = remove menor xs

-- Avaliação

--  l1 -> 499500
--  l2 -> 499500
--  l3 -> 500500
--  l4 -> 500500
--  l5 -> 2001000
--  l6 -> 2001000
--  l7 -> 190
--  x1 -> 190
--  x2 -> 190
--  x3 -> 190
--  x4 -> 190
--  x5 -> 190
--  x6 -> 190
--  x7 -> 190


--  A variação é mais eficiente pois consegue encontrar o menor elemento e remove-lo percorrendo a lista apenas uma vez

-- Ex 4

divide :: (Ord a) => a -> [a] -> ([a], [a])
divide _ [] = ([], [])
divide x [y]
  | y < x = ([y], [])
  | otherwise = ([], [y])
divide x (y : ys)
  | y < x = add1 y (divide x ys)
  | otherwise = add2 y (divide x ys)
  where
    add1 a (b, c) = (a : b, c)
    add2 a (b, c) = (b, a : c)

quickSortDiv :: (Ord a) => [a] -> [a]
quickSortDiv [] = []
quickSortDiv (pivo : xs) =
  (quickSortDiv esq)
    ++ [pivo]
    ++ (quickSortDiv dir)
  where
    (esq, dir) = divide pivo xs

quickSortDiv2 :: (Ord a) => [a] -> [a]
quickSortDiv2 [] = []
quickSortDiv2 l =
  let organiza lst = (quickSortDiv lst)

      elementos = take 3 l
      pivoMed
        | length (elementos) < 3 = (elementos) !! 0 -- retorna o elem no indice 0
        | otherwise = (organiza (elementos)) !! 1 -- retorna o elem no "meio" da lista ordenada
      remove1 _ [] = []
      remove1 x (y : ys)
        | x == y = ys
        | otherwise = y : remove1 x ys

      (esq, dir) = divide pivoMed (remove1 pivoMed l)
   in (quickSortDiv2 esq) ++ [pivoMed] ++ (quickSortDiv2 dir)


-- Com Contador

divideCont :: (Ord a) => a -> [a] -> Int -> ([a], [a], Int)
divideCont _ [] cont = ([], [], cont)
divideCont x [y] cont
  | y < x = ([y], [], cont + 1)
  | otherwise = ([], [y], cont + 1)
divideCont x (y : ys) cont
  | y < x = add1 y (divideCont x ys (cont + 1))
  | otherwise = add2 y (divideCont x ys (cont + 1))
  where
    add1 a (b, c,cont) = (a : b, c, cont)
    add2 a (b, c,cont) = (b, a : c, cont)

quickSortDivCont :: (Ord a) => [a] -> ([a],Int)
quickSortDivCont [] = ([],0)
quickSortDivCont (pivo : xs) =
      ( esqCont ++ [pivo] ++ dirCont , cont + contEsq + contDir )
  where
    (esqCont, contEsq) = quickSortDivCont esq
    (dirCont, contDir) = quickSortDivCont dir
    (esq, dir, cont) = divideCont pivo xs 0


quickSortDiv2Cont :: (Ord a) => [a] -> ([a],Int)
quickSortDiv2Cont [] = ([],0)
quickSortDiv2Cont l =
  let organiza lst = (quickSortDiv lst)

      elementos = take 3 l
      pivoMed
        | length (elementos) < 3 = (elementos) !! 0 -- retorna o elem no indice 0
        | otherwise = (organiza (elementos)) !! 1 -- retorna o elem no "meio" da lista ordenada
   
      remove1 :: (Ord a) => a -> [a] -> Int -> ([a],Int)
      remove1 _ [] cont = ([],cont)
      remove1 x (y : ys) cont
        | x == y = (ys, cont + 1)
        | otherwise = add y (remove1 x ys (cont + 1))
        where
          add c (a,cont) = (c : a, cont)

      (novo, contRem) = remove1 pivoMed l 0

      (esqCont, contEsq) = quickSortDiv2Cont esq
      (dirCont, contDir) = quickSortDiv2Cont dir
      (esq, dir,cont) = divideCont pivoMed novo 0
   in (esqCont ++ [pivoMed] ++ dirCont, cont + contEsq + contDir + contRem)

quickSortOrig :: (Ord a) => [a] -> [a]
quickSortOrig [] = []
quickSortOrig (pivo : xs) = quickSortOrig [x | x <- xs , x < pivo]
                            ++ [pivo] ++
                            quickSortOrig [x | x <- xs , x >= pivo]

-- Ex 5

-- Pesquise e implemente sua própria versão em Haskell dos algoritmos mergesort e bucketsort

-- mergeSort

-- divide recursivamente a lista em duas pastes
        -- continua até cada parte ter um elemento
-- combinar duas lista de forma a obter uma maior ordenada
        -- combinacao é feita intercalando os elementos de acordo com o sentido de ordenacao
-- este processo se repete até que exista apenas uma lista

-- divide os dados em listas cada vez menores
msort :: Ord a => [a] -> [a]
msort [] = []
msort [a] = [a]
msort xs = merge (msort (metade1 xs)) (msort (metade2 xs))

-- combina intercalando os dados de forma ordenada  em uma lista lista maior
merge :: Ord a => [a] -> [a] -> [a]
merge xs [] = xs
merge [] ys = ys
merge (x:xs) (y:ys) | x <= y = x:merge xs (y:ys)
                    | otherwise = y:merge (x:xs) ys

metade1 :: [a] -> [a]
metade1  xs = let n = length xs 
                     in take (div n 2) xs
metade2 :: [a] -> [a]
metade2 xs = let n = length xs 
                     in drop (div n 2) xs

-- bucketSort
sortIntoBuckets :: Int -> Int -> Int -> Int -> [[Int]] -> [[Int]]
sortIntoBuckets num k m n [bucket] =
  if ((num * k) `div` m) <= n
    then [num : bucket]
    else [bucket]
sortIntoBuckets num k m n (bucket : buckets)
  | ((num * k) `div` m) <= n = (num : bucket) : buckets
  | otherwise = bucket : (sortIntoBuckets num k m (n + 1) buckets)

bucketSort :: [Int] -> [Int]
bucketSort [] = []
bucketSort [x] = [x]
bucketSort l1 =
  let k = length l1

      m = foldr1 (max) l1

      buckets = [[] | _ <- [1 .. k]]

      newBuckets = foldr (\x -> sortIntoBuckets x k m 1) buckets l1

      sortedBuckets = map (msort) newBuckets

      finalList = foldr1 (++) sortedBuckets
   in finalList

-- Bubble 
-- 999	-> l1
-- 499500 -> l2
-- 500500 -> l3
-- 500499 -> l4
-- 2000997 -> l5
-- 1500500 ->	l6
-- 2000997 ->	l7
-- 19 -> x1
-- 190 ->	x2
-- 145 ->	x3
-- 135 ->	x4
-- 175 ->	x5
-- 162 ->	x6
-- 175 ->	x7



-- Selection 
-- 499500	-> l1
-- 499500 -> l2
-- 500500 -> l3
-- 500500 -> l4
-- 2001000 -> l5
-- 2001000 ->	l6
-- 2001000 ->	l7
-- 190 -> x1
-- 190 ->	x2
-- 190 ->	x3
-- 190 ->	x4
-- 190 ->	x5
-- 190 ->	x6
-- 190 ->	x7



-- Quick

-- 503500	-> l1
-- 172165 -> l2
-- 503505 -> l3
-- 173169 -> l4
-- 1009007 -> l5
-- 347668 ->	l6
-- 346336 ->	l7
-- 270 -> x1
-- 175 ->	x2
-- 180 ->	x3
-- 170 ->	x4
-- 170 ->	x5
-- 196 ->	x6
-- 180 ->	x7

--  o QuickSort comparando mais vezes que alguns dos outros métodos possui um tempo de resposta menor.