import System.IO
import Data.Char
import Categories.Category
import Categories.Finite
import Categories.Graph

-- esse diagrama esta errado,
-- o erro acontece porque o graphiviz nao aponta erro se tentar trocar o label de um no ja existente

edgeF = finiteToArrow F "a" "c"
edgeG = finiteToArrow G "c" "d"
edgeH = finiteToArrow H "d" "a"
edgeK = finiteToArrow K "a" "b"

finiteToNode :: FiniteObject -> String -> Node
finiteToNode finObject name = Node name (show finObject)

finiteToArrow :: FiniteArrow -> String -> String -> Edge
finiteToArrow finArrow nameA nameB = Edge (show finArrow) (finiteToNode (source finiteCat finArrow) nameA) (finiteToNode (target finiteCat finArrow) nameB) 

example = constructGraph [edgeF, edgeG, edgeH, edgeK]

main = do  
    writeFile "girlfriendcaps.txt" example