import System.IO

import Categories.EmbGr
import Categories.Graph
import Categories.Finite
import Categories.FinSet
import qualified Data.Set as Set

finiteExample = graphToDot $ catToGraph finiteCat (show, show) [edgeF, edgeG, edgeH, edgeK]
    where edgeF = createEmblishArrow F "0" "1"
          edgeG = createEmblishArrow G "1" "3"
          edgeH = createEmblishArrow H "1" "3"
          edgeK = createEmblishArrow K "2" "3"

finIntSetExample = graphToDot $ catToGraph createFinSetCat (tagO, tagA) [edgeF, edgeG, edgeH]
    where a = Set.fromList [0, 1, 2, 3]
          f = constructFinSetArrow a (\x -> x*x) Set.empty
          b = codomain f
          g = constructFinSetArrow b (floor . sqrt . fromIntegral) (Set.fromList [0, 1])
          h = constructFinSetArrow a (*3) (Set.fromList [5])
          c = codomain h
          edgeF = createEmblishArrow f "1" "2"
          edgeG = createEmblishArrow g "2" "1"
          edgeH = createEmblishArrow h "1" "3"
          tagO x 
                | x == a = "a"
                | x == b = "b"
                | x == c = "c"
                | otherwise = "Untaged"
          tagA x 
                | compareFinSetArrow x f = "F"
                | compareFinSetArrow x g = "G"
                | compareFinSetArrow x h = "H"
                | otherwise = "Untaged"

main = do  
    putStrLn finIntSetExample