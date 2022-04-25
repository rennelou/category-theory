module Examples.FinPoset (
    finIntSetExample
) where
    import System.IO

    import Categories.EmbGraph
    import Categories.SimpleGraph
    import Categories.Finite
    import Categories.FinPoset
    import qualified Data.Set as Set

    finIntSetExample :: String
    finIntSetExample = simpleGraphToDot $ toGraph embFinPosetInt [edgeF, edgeG, edgeH]
        where 
            embFinPosetInt :: EmbGraph (FinPosetObjcet Int) (FinPosetArrow Int)
            embFinPosetInt = createEmbGraph createFinPosetCat (tagO, tagA)
          
            a = Set.fromList [0, 1, 2, 3]
            f = constructFinPosetArrow a (\x -> x*x) Set.empty
            b = codomain f
            g = constructFinPosetArrow b (floor . sqrt . fromIntegral) (Set.fromList [0, 1])
            h = constructFinPosetArrow a (*3) (Set.fromList [5])
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
                | compareFinPosetArrow x f = "F"
                | compareFinPosetArrow x g = "G"
                | compareFinPosetArrow x h = "H"
                | otherwise = "Untaged"