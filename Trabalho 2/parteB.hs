-- Ex 6

data Exp a
  = Val a
  | Add (Exp a) (Exp a)
  | Sub (Exp a) (Exp a)
  | Prod (Exp a) (Exp a)
  | Pot (Exp a) (Exp a)

avalia :: Floating a => Exp a -> a
avalia (Val x) = x
avalia (Add exp1 exp2) = (avalia exp1) + (avalia exp2)
avalia (Sub exp1 exp2) = (avalia exp1) - (avalia exp2)
avalia (Prod exp1 exp2) = (avalia exp1) * (avalia exp2)
avalia (Pot exp1 exp2) = (avalia exp1) ** (avalia exp2)

expressao1 :: Exp Float
expressao1 = Prod (Add (Val 3) (Val 12)) (Pot (Sub (Val 15) (Val 5)) (Prod (Val 1) (Val 3)))

expressao2 :: Exp Float
expressao2 = (Sub (Val 0) (Prod (Add (Add (Val 6) (Val 8)) (Sub (Val 1) (Val 5))) (Add (Val 2) (Pot (Val 6) (Val 2)))))

-- Ex 7

data Hora = PM Int Int 
          | AM Int Int 
          deriving (Eq, Show, Ord)

validaHora :: Int -> Bool
validaHora h
            | h >= 0 && h <= 11 = True
            | otherwise = False

validaMin :: Int -> Bool
validaMin m
            | m >= 0 && m <= 59 = True
            | otherwise = False

horasDecorridas :: Hora -> Int

horasDecorridas (AM hora min)
                                | validaHora (hora) == True && validaMin (min) == True = hora
                                | otherwise = -1

horasDecorridas (PM hora min)
                                | validaHora (hora) == True && validaMin (min) == True = 12 + hora
                                | otherwise = -1

minutosDecorridos :: Hora -> Int
minutosDecorridos (AM hora min)
                                | validaHora (hora) == True && validaMin (min) == True = hora * 60 + min
                                | otherwise = -1

minutosDecorridos (PM hora min)
                                | validaHora (hora) == True && validaMin (min) == True = ((12 + hora) * 60) + min
                                | otherwise = -1

segundosDecorridos :: Hora -> Int
segundosDecorridos (AM hora min)
                                | validaHora (hora) == True && validaMin (min) == True = (hora * 60 + min) * 60
                                | otherwise = -1

segundosDecorridos (PM hora min)
                                | validaHora (hora) == True && validaMin (min) == True = (((12 + hora) * 60) + min) * 60
                                | otherwise = -1

-- Ex 8
data Contato
  = Nome String
  | Telefone String
  deriving (Eq, Show)

data Mensagem = Msg Contato Texto Data Hora App
  deriving (Show)

type Data = (Int, Int, Int)

type Texto = String

type App = String

-- B)

msgs :: [Mensagem]
msgs =
  [ (Msg (Nome "Nome1") "Msg 1" (03, 09, 2020) (AM 08 30) "Linkedin"),
    (Msg (Telefone "400") "Msg 2" (03, 09, 2020) (AM 08 40) "WhatsApp"),
    (Msg (Telefone "200") "Msg 3" (03, 09, 2020) (PM 13 40) "Facebook"),
    (Msg (Nome "Nome4") "Msg 4" (03, 09, 2020) (AM 08 50) "Facebook"),
    (Msg (Nome "Nome5") "Msg 5" (03, 09, 2020) (PM 14 00) "Linkedin"),
    (Msg (Nome "Nome6") "Msg 6" (03, 09, 2020) (AM 09 00) "WhatsApp"),
    (Msg (Telefone "540") "Msg 7" (03, 09, 2020) (AM 09 05) "Linkedin"),
    (Msg (Telefone "670") "Msg 8" (03, 09, 2020) (AM 07 00) "Facebook"),
    (Msg (Nome "Nome9") "Msg 9" (03, 09, 2020) (AM 09 13) "WhatsApp"),
    (Msg (Nome "Nome10") "Msg 10" (03, 09, 2020) (AM 09 15) "WhatsApp"),
    (Msg (Nome "Nome11") "Msg 11" (03, 09, 2020) (AM 09 25) "Facebook"),
    (Msg (Telefone "234") "Msg 12" (03, 09, 2020) (AM 09 50) "Linkedin"),
    (Msg (Nome "Nome13") "Msg 13" (03, 09, 2020) (AM 10 30) "WhatsApp"),
    (Msg (Nome "Nome14") "Msg 14" (03, 09, 2020) (PM 12 30) "Facebook"),
    (Msg (Nome "Nome15") "Msg 15" (03, 09, 2020) (PM 13 00) "WhatsApp"),
    (Msg (Telefone "453") "Msg 16" (04, 09, 2020) (AM 09 00) "Facebook"),
    (Msg (Nome "Nome17") "Msg 17" (04, 09, 2020) (AM 09 30) "Linkedin"),
    (Msg (Nome "Nome18") "Msg 18" (04, 09, 2020) (AM 10 00) "Linkedin"),
    (Msg (Nome "Nome19") "Msg 19" (04, 09, 2020) (AM 10 35) "WhatsApp"),
    (Msg (Nome "Nome20") "Msg 20" (04, 09, 2020) (AM 10 40) "Linkedin"),
    (Msg (Telefone "287") "Msg 21" (04, 09, 2020) (AM 10 45) "Facebook"),
    (Msg (Nome "Nome22") "Msg 22" (04, 09, 2020) (AM 11 00) "WhatsApp"),
    (Msg (Nome "Nome23") "Msg 23" (04, 09, 2020) (AM 11 30) "Facebook"),
    (Msg (Nome "Nome24") "Msg 24" (04, 09, 2020) (PM 12 00) "WhatsApp"),
    (Msg (Nome "Nome25") "Msg 25" (04, 09, 2020) (PM 13 00) "Linkedin"),
    (Msg (Telefone "874") "Msg 26" (04, 09, 2020) (PM 13 30) "WhatsApp"),
    (Msg (Nome "Nome27") "Msg 27" (04, 09, 2020) (AM 08 45) "Linkedin"),
    (Msg (Nome "Nome28") "Msg 28" (04, 09, 2020) (PM 13 55) "Facebook"),
    (Msg (Nome "Nome29") "Msg 29" (04, 09, 2020) (AM 08 55) "WhatsApp"),
    (Msg (Nome "Nome30") "Msg 30" (04, 09, 2020) (PM 14 10) "Facebook")
  ]

bolha :: [Mensagem] -> [Mensagem]
bolha [] = []
bolha l = bolhaOrd l (length l)

bolhaOrd :: [Mensagem] -> Int -> [Mensagem]
bolhaOrd l 0 = l
bolhaOrd l n = bolhaOrd (ordenaMsgs l) (n -1)

ordenaMsgs :: [Mensagem] -> [Mensagem]
ordenaMsgs [x] = [x]
ordenaMsgs (x : y : zs)
  | check x y = y : ordenaMsgs (x : zs)
  | otherwise = x : ordenaMsgs (y : zs)
  where
    check (Msg (Nome _) _ _ _ _) (Msg (Telefone _) _ _ _ _) = True
    check (Msg (Telefone _) _ _ _ _) (Msg (Nome _) _ _ _ _) = False
    check (Msg (Nome nome1) _ _ _ _) (Msg (Nome nome2) _ _ _ _) = nome1 > nome2
    check (Msg (Telefone tel1) _ _ _ _) (Msg (Telefone tel2) _ _ _ _) = tel1 > tel2

-- C)

quickMsgs :: [Mensagem] -> [Mensagem]
quickMsgs [] = []
quickMsgs (pivo : zs) =
  (quickMsgs [x | x <- zs, (precedeMsg x pivo) == True])
    ++ [pivo]
    ++ (quickMsgs [x | x <- zs, (precedeMsg x pivo) == False])

precedeData :: Data -> Data -> Bool
precedeData (d1, m1, a1) (d2, m2, a2)
  | a1 > a2 = False
  | a1 == a2 && m1 > m2 = False
  | a1 == a2 && m1 == m2 && d1 > d2 = False
  | otherwise = True

precedeHora :: Hora -> Hora -> Bool
precedeHora (AM _ _) (PM _ _) = True
precedeHora (PM _ _) (AM _ _) = False
precedeHora (AM hora1 min1) (AM hora2 min2)
  | hora1 > hora2 = False
  | hora1 == hora2 && min1 > min2 = False
  | otherwise = True
precedeHora (PM hora1 min1) (PM hora2 min2)
  | hora1 > hora2 = False
  | hora1 == hora2 && min1 > min2 = False
  | otherwise = True

precedeMsg :: Mensagem -> Mensagem -> Bool
precedeMsg (Msg _ _ data1 hora1 _) (Msg _ _ data2 hora2 _)
  | data1 == data2 = precedeHora hora1 hora2
  | otherwise = precedeData data1 data2

-- Ex 9

-- Arv Bin

data ArvBinInt = Nulo
                | No Int ArvBinInt ArvBinInt
                deriving (Show)

arvDados :: ArvBinInt


arvDados =
  No
    4
    (No 2 
        (No 1 Nulo Nulo)
        (No 10 Nulo Nulo)
    )
    
    ( No 10
        (No 5 Nulo Nulo)
        (No 15 Nulo Nulo)
    )

{-- ArvDados é essa
         4
      2     10
    1  10  5   15
--}


-- A)
internos :: ArvBinInt -> [Int]
internos Nulo = []
internos (No n Nulo Nulo) = []
internos (No n esq dir) = [n] ++ internos esq ++ internos dir

-- B)
somaNos :: ArvBinInt -> Int
somaNos Nulo = 0
somaNos (No n Nulo Nulo) = n --no folha
somaNos (No n esq dir) = n + somaNos esq + somaNos dir --soma n com a soma dos filhos da dir e da esq


-- C)
pertenceArv :: Int -> ArvBinInt -> Bool
pertenceArv x Nulo = False
pertenceArv x (No v esq dir) =
  x == v --compara o valor com o no
    || if x < v --escolhe pra qual lado da arvore vai
      then (pertenceArv x esq)
      else (pertenceArv x dir)

-- Ex 10

data ArvBinEA a
  = Vazia
  | Folha a
  | NoEA (Char, ArvBinEA a, ArvBinEA a)
  deriving (Show)

ea::ArvBinEA Float
ea = NoEA ('+', NoEA ('*', Folha 10, Folha 5), Folha 7)

arvEAexp :: Floating a => ArvBinEA a -> a
arvEAexp Vazia = 0
arvEAexp (Folha elem) = elem
arvEAexp (NoEA (operacao, esq, dir))
  | (operacao == '+') = (arvEAexp esq) + (arvEAexp dir)
  | (operacao == '-') = (arvEAexp esq) - (arvEAexp dir)
  | (operacao == '*') = (arvEAexp esq) * (arvEAexp dir)
  | (operacao == '/') = (arvEAexp esq) / (arvEAexp dir)
  | (operacao == '^') = (arvEAexp esq) ** (arvEAexp dir)
