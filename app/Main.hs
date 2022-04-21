import System.IO
import Data.Char

import Categories.Finite
import Categories.EmbGr

edgeF = EmbGrArrow F (EmbGrObject "0") (EmbGrObject "1")
edgeG = EmbGrArrow G (EmbGrObject "1") (EmbGrObject "3")
edgeH = EmbGrArrow H (EmbGrObject "1") (EmbGrObject "3")
edgeK = EmbGrArrow K (EmbGrObject "2") (EmbGrObject "3")

main = do  
    putStrLn $ catToDot finiteCat [edgeF, edgeG, edgeH, edgeK]