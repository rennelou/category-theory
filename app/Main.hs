{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE TypeFamilies              #-}

import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine

node :: Int -> Diagram B
node n = text (show n) # fontSizeL 0.2 # fc white <> circle 0.2 # fc green # named n

customConnectOutside = connectOutside' arrawOpts
    where arrawOpts = with  & gaps .~ small 
                            & headLength  .~ local 0.15


tournament :: Int -> Diagram B
tournament n = atPoints (trailVertices (regPoly n 1)) (map node [1..n])
    # applyAll [customConnectOutside j k | j <- [1..n-1], k <- [j+1..n]]

example :: Diagram B
example = tournament 6

main = mainWith example