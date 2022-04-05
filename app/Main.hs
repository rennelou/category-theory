{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE TypeFamilies              #-}

import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine
import DiagramChasing

import qualified Data.Set as Set
import Categories.FinSet

a = Set.fromList [0, 1, 2, 3]
b = Set.fromList [0, 1, 4, 9]
c = Set.fromList [0, 3, 6, 9]

f = FinSetArrow a (\x -> x*x) b
g = FinSetArrow b (floor . sqrt . fromIntegral) a
h = FinSetArrow a (*3) c

example :: Diagram B
example = diagramChasing finIntSetCat [a, b, c] [f, g, h]

main = mainWith example