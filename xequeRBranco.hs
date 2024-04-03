import Data.Char (toLower)
import Data.List (elemIndex)
import Data.Maybe (mapMaybe)

type PosPeca = (Int, Int)
type Tabuleiro = [[Char]]


xequeRBranco :: Tabuleiro -> Bool
xequeRBranco tab = any (\(x,y) -> ameacaRei (tab !! x !! y) posRei (x,y) && not (temPecaEntre tab posRei (x,y))) posPretas
    where
        posRei = buscaPosRei 'R' tab
        posPretas = buscaPosPeca 'b' tab

-- Função para verificar se uma peça pode atacar o rei branco
ameacaRei :: Char -> PosPeca -> PosPeca -> Bool
ameacaRei peca (linhaRei, colunaRei) (linha, coluna)
    | tipoPeca == 'b' && pecaBranca peca = abs (linha - linhaRei) == abs (coluna - colunaRei)
    | tipoPeca == 'r' = linha == linhaRei || coluna == colunaRei
    | tipoPeca == 'd' = linha == linhaRei || coluna == colunaRei || abs (linha - linhaRei) == abs (coluna - colunaRei)
    | tipoPeca == 'p' && pecaBranca peca = linha - linhaRei == 1 && (coluna - colunaRei == 1 || coluna - colunaRei == -1)
    | tipoPeca == 'c' && pecaBranca peca = (abs (linha - linhaRei) == 2 && abs (coluna - colunaRei) == 1) || (abs (linha - linhaRei) == 1 && abs (coluna - colunaRei) == 2)
    | otherwise = False
    where
        tipoPeca = toLower peca

pecaBranca :: Char -> Bool
pecaBranca peca = elem peca pecasBrancas
    where 
        pecasBrancas = "TCBDRBP"

-- Função para verificar se ha peças entre duas posiçoes 
temPecaEntre :: Tabuleiro -> PosPeca -> PosPeca -> Bool
temPecaEntre tabuleiro (linha1, coluna1) (linha2, coluna2)
  | linha1 == linha2 && coluna1 < coluna2 = any (\x -> tabuleiro !! linha1 !! x /= ' ') [coluna1 + 1 .. coluna2 - 1]
  | linha1 == linha2 && coluna1 > coluna2 = any (\x -> tabuleiro !! linha1 !! x /= ' ') [coluna2 + 1 .. coluna1 - 1]
  | coluna1 == coluna2 && linha1 < linha2 = any (\y -> tabuleiro !! y !! coluna1 /= ' ') [linha1 + 1 .. linha2 - 1]
  | coluna1 == coluna2 && linha1 > linha2 = any (\y -> tabuleiro !! y !! coluna1 /= ' ') [linha2 + 1 .. linha1 - 1]
  | otherwise = False


-- Função auxiliar para encontrar todas as posições das peças pretas
buscaPosPeca :: Char -> [String] -> [PosPeca]
buscaPosPeca tipoPeca tab = 
    concatMap (\(l, linha) -> mapMaybe (\(c, peca) -> if toLower peca == tipoPeca then Just (l, c) else Nothing) (zip [0..] linha)) (zip [0..] tab)

-- Função auxiliar para encontrar a posição do rei branco
buscaPosRei :: Char -> Tabuleiro -> PosPeca
buscaPosRei _ [] = (-1, -1)
buscaPosRei tipoPeca (fila:resto) =
    case elemIndex tipoPeca fila of
        Just coluna -> (8 - length resto, coluna)
        Nothing -> buscaPosRei tipoPeca resto

