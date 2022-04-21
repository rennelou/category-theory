import System.IO
import Data.Char

import Categories.Finite
import Categories.EmbGr

edgeF = createEmblishArrow F "2" "1"
edgeG = createEmblishArrow G "1" "3"
edgeH = createEmblishArrow H "1" "3"
edgeK = createEmblishArrow K "2" "3"

main = do  
    putStrLn $ catToDot finiteCat [edgeF, edgeG, edgeH, edgeK]