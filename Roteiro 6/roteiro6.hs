import Data.Char
-- Ex 1

paridade :: [Int] -> [Bool]
paridade l = map even l

-- Ex 2

prefixos :: [String] -> [String]
prefixos l = map (take 3) l

-- Ex 3

saudacao :: [String] -> [String]
saudacao l = map ("Oi " ++) l

-- Ex 4

filtrar :: (a -> Bool) -> [a] -> [a]
filtrar p [] = []
filtrar p (x:xs)
            | p x = x : filter p xs
            | otherwise = filter p xs 

filtrar1 :: (a -> Bool) -> [a] -> [a]
filtrar1 p l = [x | x <- l, p x]

-- Ex 5

pares :: [Int] -> [Int]
pares l = filter (even) l

-- Ex 6

solucoes :: [Int] -> [Int]
solucoes l = filter (\x -> ((5 * x) +6) < (x * x) ) l

-- Ex 7

maior :: [Int] -> Int
maior l = foldr1 max l

-- Ex 8

menor_min10 :: [Int] -> Int
menor_min10 l = foldr min 10 l

-- Ex 9

junta_silabaplural :: [String] -> String
junta_silabaplural l = foldr (++) "s" l

-- Ex 10
lst1 :: [Integer]
lst1 = [1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20]
lst2 :: [Integer]
lst2 = [20,19,18,17,16,15,14,13,12,11,10,9,8,7,6,5,4,3,2,1]
lst3 :: [Integer]
lst3 = [11,12,13,14,15,16,17,18,19,20,1,2,3,4,5,6,7,8,9,10]
lst4 :: [Integer]
lst4 = [10,9,8,7,6,5,4,3,2,1,20,19,18,17,16,15,14,13,12,11]
lst5 :: [Integer]
lst5 = [11,12,13,14,15,5,4,3,2,1,16,17,18,19,20,10,9,8,7,6]
lst6 :: [Integer]
lst6 = [1,12,3,14,5,15,4,13,2,11,6,17,8,19,20,10,9,18,7,16]
lst7 :: [Integer]
lst7 = [1..1000]
lst8 :: [Integer]
lst8 = [1000,999..1]
lst9 :: [Integer]
lst9 = lst1++[0]
lst10 :: [Integer]
lst10 = [0]++lst3
lst11 :: [Integer]
lst11 = lst1++[0]++lst3
lst12 :: [Integer]
lst12 = lst3++[0]++lst1


bubblesort :: Ord a => [a] -> [a]
bubblesort [] = []
bubblesort l = bolhaOrd l (length l)


bolhaOrd :: (Num t, Ord a, Eq t) => [a] -> t -> [a]
bolhaOrd l 0 = l
bolhaOrd l n = bolhaOrd (troca l) (n-1)


troca :: Ord a => [a] -> [a]
troca [x] = [x]
troca (x:y:zs)
        | x > y  = y : troca (x:zs)
        | otherwise = x : troca (y:zs)


selectionSort :: (Ord a) => [a] -> [a]
selectionSort [] = []
selectionSort xs = [x] ++ selectionSort (remove x xs)
                                where x = minimum xs
remove :: (Ord a) => a -> [a] -> [a]
remove a [] = []
remove a (x:xs)
                | a == x = xs
                | otherwise = x : (remove a xs)


insertionSort :: (Ord a) => [a] -> [a]
insertionSort = foldr insereOrd []

insereOrd :: (Ord a) => a -> [a] -> [a]
insereOrd x [] = [x]
insereOrd x (y:ys)
                  | x <= y = (x:y:ys)
                  | otherwise = y: (insereOrd x ys)


quicksort :: Ord a => [a] -> [a]
quicksort [] = []
quicksort (s:xs) = quicksort [x | x <- xs,x < s]
                   ++ [s] ++
                   quicksort [x | x <- xs,x >= s]
-- Obs : Tive que pedir ajuda nos exercicios 11 e 12 , portanto talvez esteja parecido com os exs de outra pessoa,
-- não consegui manter a originalidade de alguns porque foi a implementação foi a única que fez sentido para mim :)

-- Ex 11

bubblesort2  :: Ord a => [a] -> ([a],Int)
bubblesort2 [] = ([],0)
bubblesort2 l = bolhaOrd2 (l,0) (length l)


bolhaOrd2 :: (Num t,Num b, Ord a, Eq t) => ([a],b) -> t -> ([a],b)
bolhaOrd2 (l,cont) 0 = (l,cont)
bolhaOrd2 (l,cont) n = bolhaOrd2 (troca2 (l,cont)) (n-1)

troca2 :: (Ord a,Num b) => ([a],b) -> ([a],b)
troca2 ([x],cont) = ([x],cont)
troca2 ( (x:y:zs),cont) 
                | x > y = add (troca2 ((x : zs), cont + 1)) y
                | otherwise = add (troca2 ((y : zs), cont + 1)) x
                where
                add (lista, cont) a = (a : lista, cont)

selectionsort2 :: Ord a => [a] -> ([a], Int)
selectionsort2 lista = selectionAUX lista 0

selectionAUX :: (Ord a) => [a] -> Int -> ([a], Int)
selectionAUX [] n = ([], n)
selectionAUX (x : xs) n =
  let (least, n_num) = minimo2 (x : xs) n

      remove2 _ [] = []
      remove2 n (h : t) =
        if (n == h)
          then t
          else h : (remove2 n t)

      add (lst, n) y = (y : lst, n)
   in add (selectionAUX (remove2 least (x : xs)) n_num) least

minimo2 :: (Ord a) => [a] -> Int -> (a, Int)
minimo2 [] _ = undefined
minimo2 [x] cont = (x, cont)
minimo2 (x : y : xs) cont
  | x > y = minimo2 (y : xs) (cont + 1)
  | otherwise = minimo2 (x : xs) (cont + 1)


insertionsort2 :: (Ord a) => [a] -> ([a], Int)
insertionsort2 [] = ([], 0)
insertionsort2 [x] = ([x], 0)
insertionsort2 (x : xs) =
  let (sorted_tail, n) = insertionsort2 xs

      (lst, n1) = insereOrd2 x sorted_tail n
   in (lst, n1)

insereOrd2 :: (Ord a) => a -> [a] -> Int -> ([a], Int)
insereOrd2 x [] n = ([x], n)
insereOrd2 x (h : t) n =
  if (x <= h)
    then ((x : h : t), n + 1)
    else add (insereOrd2 x t (n + 1)) h
  where
    add (list, n) y = (y : list, n)


quickAux :: [a] -> Int -> (a -> Bool) -> ([a], Int)
quickAux [] n _ = ([], n)
quickAux (x : xs) n cond =
  if (cond x)
    then add (quickAux xs (n + 1) cond) x
    else quickAux xs (n + 1) cond
  where
    add (list, n) y = (y : list, n)

quicksort2  :: (Ord a) => [a] -> ([a], Int)
quicksort2 [] = ([], 0)
quicksort2 (piv : xs) =
  let (left, n_L) = quickAux xs 0 (<= piv)
      (right, n_R) = quickAux xs 0 (> piv)
      (sorted_L, n1_L) = quicksort2 left
      (sorted_R, n1_R) = quicksort2 right
   in (sorted_L ++ [piv] ++ sorted_R, n_L + n_R + n1_L + n1_R)

-- Ex 12

bubblesort3 :: (Ord a) => [a] -> ([a], Int)
bubblesort3 [] = ([], 0)
bubblesort3 lista = bolhaOrd3 (lista, 0) (length lista)

bolhaOrd3 :: (Ord a, Num t, Num b, Eq t) => ([a], b) -> t -> ([a], b)
bolhaOrd3 (lista, count) 0 = (lista, count)
bolhaOrd3 (lista, count) n = bolhaOrd3 (troca3 (lista, count)) (n -1)

troca3 :: (Ord a, Num b) => ([a], b) -> ([a], b)
troca3 ([x], cont) = ([x], cont)
troca3 ((x : y : zs), cont) =
  if x > y
    then add (troca3 ((y : zs), cont + 1)) x
    else add (troca3 ((x : zs), cont + 1)) y
  where
    add (lista, count) a = (a : lista, count)


selectionsort3  :: Ord a => [a] -> ([a], Int)
selectionsort3 lista = selectionAUX2 lista 0

selectionAUX2 :: (Ord a) => [a] -> Int -> ([a], Int)
selectionAUX2 [] n = ([], n)
selectionAUX2 (x : xs) n =
  let (least, n_num) = minimo3 (x : xs) n

      remove3 _ [] = []
      remove3 n (h : t) =
        if (n == h)
          then t
          else h : (remove3 n t)

      add (lst, n) y = (y : lst, n)
   in add (selectionAUX2 (remove3 least (x : xs)) n_num) least

minimo3 :: (Ord a) => [a] -> Int -> (a, Int)
minimo3 [] _ = undefined
minimo3 [x] cont = (x, cont)
minimo3 (x : y : xs) cont
  | x > y = minimo3 (x : xs) (cont + 1)
  | otherwise = minimo3 (y : xs) (cont + 1)



insertionsort3 :: (Ord a) => [a] -> ([a], Int)
insertionsort3 [] = ([], 0)
insertionsort3 [x] = ([x], 0)
insertionsort3 (h : t) =
  let (sorted_tail, n) = insertionsort3 t

      (lst, n1) = insereOrd3 h sorted_tail n
   in (lst, n1)

insereOrd3 :: (Ord a) => a -> [a] -> Int -> ([a], Int)
insereOrd3 x [] n = ([x], n)
insereOrd3 x (h : t) n =
  if (x >= h)
    then ((x : h : t), n + 1)
    else add (insereOrd3 x t (n + 1)) h
  where
    add (list, n) y = (y : list, n)

quickAux2 :: [a] -> Int -> (a -> Bool) -> ([a], Int)
quickAux2 [] n _ = ([], n)
quickAux2 (x : xs) n cond =
  if (cond x)
    then quickAux2 xs (n + 1) cond
    else add (quickAux2 xs (n + 1) cond) x
  where
    add (list, n) y = (y : list, n)

quicksort3  :: (Ord a) => [a] -> ([a], Int)
quicksort3 [] = ([], 0)
quicksort3 (piv : xs) =
  let (left, n_L) = quickAux2 xs 0 (<= piv)
      (right, n_R) = quickAux2 xs 0 (> piv)
      (sorted_L, n1_L) = quicksort3 left
      (sorted_R, n1_R) = quicksort3 right
   in (sorted_L ++ [piv] ++ sorted_R, n_L + n_R + n1_L + n1_R)












