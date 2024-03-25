---------------- Paradigmas de Linguagens de Programação -----------------
---------------------- Lista de Exercícios 1 ---------------------------
-- Aluno: Erik Samuel Viana Hsu
-- RA: 10403109
-- Turma: 05G

-- OBS: Exercício 8 anexado no Moodle
 
--- Exercício 1 -> tot3.hs
tot3 :: [Int] -> [Int]
tot3 [] = []
tot3 lista
    | length subLista == 3 = soma3 subLista : tot3 resto
    | otherwise = subLista ++ tot3 resto
    where
        subLista = take 3 lista
        resto = drop 3 lista
        soma3 :: [Int] -> Int
        soma3 = sum


--- Exercício 2 -> rev.hs
rev :: [int] -> [int]
rev [] = []
rev (x:xs) = rev(xs) ++ [x]


--- Exercício 3 -> seg.hs
seg :: String -> Char
seg [] = error "Informe uma string com pelo menos 2 caracteres"
seg (x:y:_) = y
seg _ = error "Informe uma string com pelo menos 2 caracteres"


--- Exercício 4 -> del_rep.hs

tem_repetido :: Eq a => [a] -> Bool
tem_repetido [] = False
tem_repetido (x:xs)
    | x `elem` xs = True
    | otherwise = tem_repetido xs

del_rep :: Eq a => [a] -> [a]
del_rep xs = remover xs []
    where
        remover [] _ = []  
        remover (x:xs) aux
            | x `elem` aux = remover xs aux  
            | otherwise = x : remover xs (x:aux)


--- Exercício 5 -> totk.hs
totk :: Int -> [Int] -> [Int]
totk _ [] = []
totk k lista = somaK (take k lista) : totk k (drop k lista)
    where
        somaK :: [Int] -> Int
        somaK = sum


--- Exercício 6 -> trok2.hs
trok2 :: [Int] -> [Int]
trok2 [] = []
trok2 [x] = [x]
trok2 (x:y:zs) = y : x : trok2 zs


--- Exercício 7 -> delk.hs
delk :: Int -> [a] -> [a]
delk _ [] = []
delk k xs = remover k xs 1
    where
        remover _ [] _ = []
        remover k (x:xs) n
            | n == k = remover k xs 1
            | otherwise = x : remover k xs (n+1)

--- Exercício 8 -> Anexado no Moodle



