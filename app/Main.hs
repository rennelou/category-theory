import System.IO
import Data.Char
import Categories.Category
import Categories.Finite
import Categories.Graph

edgeF = finiteToArrow F "a" "b"
edgeG = finiteToArrow G "c" "d"
edgeH = finiteToArrow H "e" "f"
edgeK = finiteToArrow K "h" "i"

finiteToNode :: FiniteObject -> String -> Node
finiteToNode finObject name = Node name (show finObject)

finiteToArrow :: FiniteArrow -> String -> String -> Edge
finiteToArrow finArrow nameA nameB = Edge (show finArrow) (finiteToNode (source finiteCat finArrow) nameA) (finiteToNode (target finiteCat finArrow) nameB) 

example = constructGraph [edgeF, edgeG, edgeH, edgeK]

main = do  
    writeFile "girlfriendcaps.txt" example