--Ex 1

-- A)
type Data = (Int, Int, Int)

valida :: Data -> Bool
valida (dia, mes, ano)
  | dia >= 1 && dia <= 31 && (mes == 1 || mes == 3 || mes == 5 || mes == 7 || mes == 8 || mes == 10 || mes == 12) = True
  | dia >= 1 && dia <= 30 && (mes == 4 || mes == 6 || mes == 9 || mes == 11) = True
  | dia >= 1 && dia <= 28 && mes == 2 && not (bissexto ano) = True
  | dia >= 1 && dia <= 29 && mes == 2 && (bissexto ano) = True
  | otherwise = False
  where
    bissexto ano
      | (mod ano 400 == 0) = True
      | (mod ano 4 == 0) && (mod ano 100 /= 0) = True
      | otherwise = False

-- B)
bissexto :: Int -> Bool
bissexto ano
      | (mod ano 400 == 0) = True
      | (mod ano 4 == 0) && (mod ano 100 /= 0) = True
      | otherwise = False

bissextos :: [Int] -> [Int]
bissextos li = popli
  where
    popli = [x | x <- li , bissexto x]


-- C)

type Data = (Int, Int, Int)

type Emprestimo = (String, String, Data, Data, String)

type Emprestimos = [Emprestimo]

bdEmprestimo :: Emprestimos
bdEmprestimo =
  [ ("H123C9", "BSI945", (12, 9, 2009), (20, 09, 2009), "aberto"),
    ("L433C5", "BCC021", (01, 9, 2009), (10, 09, 2009), "encerrado"),
    ("M654C3", "BCC008", (04, 9, 2009), (15, 09, 2009), "aberto")
  ]

em_dia :: Data -> Data -> Bool
em_dia (dia,mes,ano) (dia2,mes2,ano2) = exp1 && (exp2 || exp3 || exp4)
  where
    exp1 = (valida(dia,mes,ano)) && (valida(dia2,mes2,ano2))
    exp2 = ano < ano2
    exp3 = ano == ano2 && mes < mes2
    exp4 = ano == ano2 && mes < mes2 && dia < dia

emprestimoEmDia :: Data -> Emprestimo -> Bool
emprestimoEmDia dataAtual (codLivro, codAluno, dataEmprestimo, dataDevolucao, status) = exp
  where
    exp = em_dia dataAtual dataDevolucao

atrasados :: Emprestimos -> Data -> Emprestimos
atrasados li dataAtual = exp
  where
    exp = [x | x <- li, not (emprestimoEmDia dataAtual x)]
  
-- D)

passo :: (Int,Int) -> (Int,Int)
passo (x,y) = exp
  where
    exp = (y,x + y)

fibo2 :: Int -> (Int,Int)
fibo2 0 = (0,1)
fibo2 n = exp
  where
    exp = passo(fibo2 (n-1))

-- E)

prodIntervalo :: Int -> Int -> Int 
prodIntervalo m n
        |m >= n = exp1
        |otherwise = exp2
  where
    exp1 = n
    exp2 = (m * (prodIntervalo (m + 1) n))

fatorial :: Int -> Int
fatorial n = exp
  where 
    exp = prodIntervalo 1 n


-- Ex 2

bissexto :: Int -> Bool
bissexto ano =
  let exp1 = (mod ano 400 == 0)
      exp2 = (mod ano 4 == 0)
      exp3 = (mod ano 100 /= 0)
   in exp1 || (exp2 && exp3)

type Data = (Int, Int, Int)

valida :: Data -> Bool
valida (dia, mes, ano) =
  let exp1 = dia >= 1 && dia <= 31 && (mes == 1 || mes == 3 || mes == 5 || mes == 7 || mes == 8 || mes == 10 || mes == 12)
      exp2 = dia >= 1 && dia <= 30 && (mes == 4 || mes == 6 || mes == 9 || mes == 11)
      exp3 = dia >= 1 && dia <= 28 && mes == 2 && not (bissexto ano)
      exp4 = dia >= 1 && dia <= 29 && mes == 2 && (bissexto ano)
   in exp1 || exp2 || exp3 || exp4

-- B)

bissexto :: Int -> Bool
bissexto ano =
  let exp1 = (mod ano 400 == 0)
      exp2 = (mod ano 4 == 0)
      exp3 = (mod ano 100 /= 0)
   in  exp1 || (exp2 && exp3)

bissextos :: [Int] -> [Int]
bissextos li = 
  let  popli = [x | x <- li , bissexto x]
  
   in   popli


-- C)

type Data = (Int, Int, Int)

type Emprestimo = (String, String, Data, Data, String)

type Emprestimos = [Emprestimo]

bdEmprestimo :: Emprestimos
bdEmprestimo =
  [ ("H123C9", "BSI945", (12, 9, 2009), (20, 09, 2009), "aberto"),
    ("L433C5", "BCC021", (01, 9, 2009), (10, 09, 2009), "encerrado"),
    ("M654C3", "BCC008", (04, 9, 2009), (15, 09, 2009), "aberto")
  ]

em_dia :: Data -> Data -> Bool
em_dia (dia,mes,ano) (dia2,mes2,ano2) =
  
  let exp1 = (valida(dia,mes,ano)) && (valida(dia2,mes2,ano2))
      exp2 = ano < ano2
      exp3 = ano == ano2 && mes < mes2
      exp4 = ano == ano2 && mes < mes2 && dia < dia
    
   in  exp1 && (exp2 || exp3 || exp4)

   

emprestimoEmDia :: Data -> Emprestimo -> Bool
emprestimoEmDia dataAtual (codLivro, codAluno, dataEmprestimo, dataDevolucao, status) = 
  let exp = em_dia dataAtual dataDevolucao
  
   in exp 
    

atrasados :: Emprestimos -> Data -> Emprestimos
atrasados li dataAtual = 
 
  let exp = [x | x <- li, not (emprestimoEmDia dataAtual x)]
  
   in exp
    
  
-- D)

passo :: (Int,Int) -> (Int,Int)
passo (x,y) = 
  let exp = (y,x + y)
  
   in exp

fibo2 :: Int -> (Int,Int)
fibo2 0 = (0,1)
fibo2 n = 
  let exp = passo(fibo2 (n-1)) 
  
   in exp
    

-- E)

prodIntervalo :: Int -> Int -> Int 
prodIntervalo m n =
  let exp1 =
      if (m >= n)
        then n
        else (m * (prodIntervalo (m + 1) n))
   
   in = exp1
    

fatorial :: Int -> Int
fatorial n =
  let exp = prodIntervalo 1 n
   
   in exp


--  Ex 3

-- 1) 
(\x. 2*x + 1) 3
2*3 + 1
6 + 1
7

-- 2)
(\xy. x-y) 5 7
5-7
-2

-- 3)
(\yx. x-y) 5 7
7-5
2

-- 4)
(\xy. x-y) (\z. z/2)
(\xy. z/2 - y)

-- 5)
(\xy . x-y) ((\z. z/2) 6) 1
(\xy . x-y) (\z. 6/2) 1
(\xy . x-y) 3 1
(\xy . 3-1)
3 - 1
2

-- 6)
(\x .\y - x y) 9 4
(\x . - x 4) 9
(- 9 4)
- 9 4
5

-- 7)
(\x .xx) (\y .y)
(\x .xx) y
(yy)
yy




