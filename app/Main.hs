{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE TypeFamilies              #-}

import Categories.Finite

import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine
import Categories.Category (Cat(source, target))

node :: (Show o) => o -> Diagram B
node n = text (show n) # fontSizeL 0.2 # fc white <> circle 0.2 # fc green # named (show n)

customConnectOutside = connectOutside' arrawOpts
    where arrawOpts = with  & gaps .~ small 
                            & headLength  .~ local 0.15

diagramChasing :: (Show o) => Cat o a -> [o] -> [a] -> Diagram B
diagramChasing cat o a = atPoints (trailVertices (regPoly (length o) 1)) (map node o)
    # applyAll [customConnectOutside (show $ source cat arrow) (show $ target cat arrow) | arrow <- a]

example :: Diagram B
example = diagramChasing finiteCat  [C, A, B] [F, G, H, K]

finite = finiteCat

main = mainWith example