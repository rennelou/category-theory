{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE TypeFamilies              #-}

import Categories.Finite
import DiagramChasing

import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine

example :: Diagram B
example = diagramChasing finiteCat [C, A, B] [F, G, H, K]

finite = finiteCat

main = mainWith example