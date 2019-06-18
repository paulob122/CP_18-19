

-- Check

check :: (Eq a, Eq b) => FS a b -> Bool
check = cataFS geneCheck

geneCheck :: (Eq a, Eq b) => [(a,Either b Bool)] -> Bool
geneCheck [] = True
geneCheck ((a, Left b):cauda) = naoTemRepetidos(passaLista (((a, Left b):cauda))) && geneCheck cauda
geneCheck ((a, Right bool):cauda) = bool && naoTemRepetidos(passaLista (((a, Right bool):cauda))) && geneCheck cauda

-- \textit{Verifica se uma lista de pares não tem elementos repetidos.}

naoTemRepetidos :: (Eq a) =>  [a] -> Bool
naoTemRepetidos [x] = True
naoTemRepetidos (h:t) | elem h t = False
                      | otherwise = naoTemRepetidos t

-- \textit{Cria uma lista de pares no qual a primeira componente é o nome da Dir/File e a segunda componente é o conteúdo.}

passaLista  :: [(a,Either b bool)] -> [a]
passaLista [] = []
passaLista ((a, Left b):cauda) = [a] ++ passaLista cauda
passaLista ((a, Right bool):cauda) = [a] ++ passaLista cauda