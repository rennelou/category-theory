module Examples.FinPoset (
    finIntSetExample
) where
    import System.IO

    import Categories.EmbGraph
    import Categories.SimpleGraph
    import Categories.FinPoset
    import qualified Data.Set as Set

    embFinPosetInt :: EmbGraph (FinPosetObjcet Int) (FinPosetArrow Int)
    embFinPosetInt = createEmbGraph createFinPosetCat (tagO, tagA)

    tagO x 
        | x == Set.empty = "Empty"
        | x == a = "a"
        | x == b = "b"
        | x == c = "c"
        | otherwise = "Untaged"
  
    tagA x 
        | compareFinPosetArrow x f = "F"
        | compareFinPosetArrow x g = "G"
        | compareFinPosetArrow x h = "H"
        | otherwise = "Untaged"

    a = Set.fromList [0, 1, 2, 3]
    b = Set.fromList [0, 1, 4, 9]
    c = Set.fromList [0, 3, 5, 6, 9]

    f = constructFinPosetArrow a (\x -> x*x) Set.empty
    g = constructFinPosetArrow b (floor . sqrt . fromIntegral) (Set.fromList [0, 1])
    h = constructFinPosetArrow a (*3) (Set.fromList [5])

    finIntSetExample :: String
    finIntSetExample = simpleGraphToDot $ toGraph embFinPosetInt [edgeF, edgeG, edgeH]
        where 
            edgeF = createEmblishArrow f "1" "2"
            edgeG = createEmblishArrow g "2" "1"
            edgeH = createEmblishArrow h "1" "3"