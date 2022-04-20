import System.IO
import Data.Char

import Categories.Finite
import GraphFunctor

-- esse diagrama esta errado,
-- o erro acontece porque o graphiviz nao aponta erro se tentar trocar o label de um no ja existente

edgeF = GraphFunctor finiteCat F "a" "c"
edgeG = GraphFunctor finiteCat G "c" "d"
edgeH = GraphFunctor finiteCat H "d" "a"
edgeK = GraphFunctor finiteCat K "a" "b"

main = do  
    putStrLn $ catToDot [edgeF, edgeG, edgeH, edgeK]