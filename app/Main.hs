import System.IO
import Data.Char

import Categories.Finite
import GraphFunctor

edgeF = GraphFunctor F "2" "1"
edgeG = GraphFunctor G "1" "3"
edgeH = GraphFunctor H "1" "3"
edgeK = GraphFunctor K "2" "3"

main = do  
    putStrLn $ catToDot finiteCat [edgeF, edgeG, edgeH, edgeK]