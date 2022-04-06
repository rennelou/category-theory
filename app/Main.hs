{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE TypeFamilies              #-}

import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine
import DiagramChasing

import qualified Data.Set as Set
import Categories.FinSet

a = Set.fromList [0, 1, 2, 3]
f = constructFinSetArrow a (\x -> x*x) Set.empty

b = codomain f

g = constructFinSetArrow b (floor . sqrt . fromIntegral) (Set.fromList [0, 1])

h = constructFinSetArrow a (*3) (Set.fromList [5])
c = codomain h

tag :: Set.Set Int -> String 
tag x 
    | x == a = "A"
    | x == b = "B"
    | x == c = "C"
    | otherwise = "Void"


example :: Diagram B
example = diagramChasing finIntSetCat tag [a, b, c] [f, g, h]

main = mainWith example