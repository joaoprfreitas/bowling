-- João Pedro Rodrigues Freitas - 11316552
-- Paulo Henrique de Souza Soares - 11884713

-- ##### CALCULO DO SCORE #####
isStrike :: [Int] -> Bool
isStrike (x:_) = x == 10 -- Se x == 10, então é strike
isStrike _ = False -- Se não, não é strike

isSpare :: [Int] -> Bool
isSpare (x:y:_) = x + y == 10 -- Se x + y == 10, então é spare
isSpare _ = False -- Se não, não é spare

-- Retorna a soma das duas próximas jogadas
bonusStrike :: [Int] -> Int
bonusStrike (x:y:z:_) = y + z

-- Retorna a primeira jogada da lista de jogadas
bonusSpare :: [Int] -> Int
bonusSpare (x:y:_) = x
bonusSpare _ = 0

-- Retorna o score de uma partida
getScoreAux :: Int -> [Int] -> Int
getScoreAux _ [] = 0 -- Se a lista de jogadas estiver vazia, retorna 0
getScoreAux frame jogadas
    -- Se for o último frame e for strike, retorna a soma das jogadas
    | frame == 10 && isStrike jogadas = sum jogadas
    -- Se for a último frame e for spare, retorna a soma das jogadas
    | frame == 10 && isSpare jogadas  = sum jogadas
    -- Se for strike, retorna 10 + bonusStrike + score do próximo frame
    | isStrike jogadas = 10 + bonusStrike jogadas + getScoreAux (frame + 1) (drop 1 jogadas)
    -- Se for spare, retorna
    -- 10 + bonusSpare para as proximas duas posiçoes +
    -- score do próximo frame
    | isSpare jogadas = 10 + bonusSpare (drop 2 jogadas) + getScoreAux (frame + 1) (drop 2 jogadas)
    -- Se não for strike nem spare, retorna a soma das duas jogadas +
    -- score do próximo frame
    | otherwise = sum (take 2 jogadas) + getScoreAux (frame + 1) (drop 2 jogadas)

-- Retorna o score final
getScore :: [Int] -> Int
getScore = getScoreAux 1 -- equivale a getScore jogadas = getScoreAux 1 jogadas

-- ##### PARSE DAS JOGADAS #####

-- Recebe uma lista de jogadas e retorna uma string
parseJogadas :: [Int] -> String
parseJogadas jogadas = formatarJogadas $ inserirBarras jogadas 1

-- Insere as "/", "_" e espaços na lista de jogadas
inserirBarras :: [Int] -> Int -> [String]
inserirBarras [] _ = [] -- Se a lista de jogadas estiver vazia, retorna uma lista vazia
inserirBarras [x] _ = [mostrarJogada x] -- Se a lista de jogadas tiver apenas um elemento, retorna o elemento
inserirBarras (x:y:tail) frame -- Se a lista de jogadas tiver mais de um elemento
    -- Se for o último frame e for strike seguido de spare
    | frame == 10 && x == 10 && y + head tail == 10 = [mostrarJogada x, mostrarJogada y, "/"]
    -- Se for o último frame e for strike
    | frame == 10 && x == 10 = [mostrarJogada x, mostrarJogada y, mostrarJogada (head tail)]
    -- Se for o último frame e for spare
    | frame == 10 && x + y == 10 = [mostrarJogada x, "/", mostrarJogada (head tail)]
    -- Se for o último frame
    | frame == 10 = [mostrarJogada x, mostrarJogada y]
    -- Se for strike
    | x == 10 = "X _ |" : inserirBarras (y:tail) (frame + 1)
    -- Se for spare
    | x + y == 10 = [mostrarJogada x, "/ |"] ++ inserirBarras tail (frame + 1)
    -- Se não for strike nem spare
    | otherwise = [mostrarJogada x, mostrarJogada y, "|"] ++ inserirBarras tail (frame + 1)

-- Mostra a jogada de acordo com o valor
mostrarJogada :: Int -> String
mostrarJogada 10 = "X"
mostrarJogada x = show x

-- Formata a lista de jogadas para uma string,
-- separando por espaços
formatarJogadas :: [String] -> String
formatarJogadas = unwords

main :: IO()
main = do
    input <- getLine -- Recebe a entrada
    -- Converte a entrada para uma lista de inteiros
    let listaJogadas = map read (words input) :: [Int]
    -- Calcula o placar final
    let placarFinal = getScore listaJogadas
    -- Imprime no formato "PARSE | SCORE"
    putStrLn $ parseJogadas listaJogadas ++ " | " ++ show placarFinal