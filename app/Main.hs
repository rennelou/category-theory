import System.IO
import Data.Char

import Categories.Finite
import GraphFunctor

edgeF = GraphFunctor finiteCat F "2" "1"
edgeG = GraphFunctor finiteCat G "4" "3"
edgeH = GraphFunctor finiteCat H "1" "3"
edgeK = GraphFunctor finiteCat K "2" "3"

main = do  
    putStrLn $ catToDot [edgeF, edgeG, edgeH, edgeK]