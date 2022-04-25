module Examples.FinPoset (
    finIntSetExample,
    initalObjectExample
) where
    import System.IO

    import Categories.EmbGraph
    import Categories.SimpleGraph
    import Categories.Finite
    import Categories.FinPoset
    import UniversalConstructions.InitialObject
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
        | compareFinPosetArrow x initToA = "Init2A"
        | compareFinPosetArrow x initToB = "Init2B"
        | compareFinPosetArrow x initToC = "Init2C"
        | otherwise = "Untaged"

    a = Set.fromList [0, 1, 2, 3]
    b = Set.fromList [0, 1, 4, 9]
    c = Set.fromList [0, 3, 5, 12, 27]

    f = constructFinPosetArrow a (\x -> x*x) Set.empty
    g = constructFinPosetArrow b (floor . sqrt . fromIntegral) (Set.fromList [0, 1])
    h = constructFinPosetArrow a (*3) (Set.fromList [5])
    
    initToA = universalProperty posetInitialObject a
    initToB = universalProperty posetInitialObject b
    initToC = universalProperty posetInitialObject c

    posetInitialObject :: InitialObject (Set.Set t) (FinPosetArrow t)
    posetInitialObject = InitialObject
                            Set.empty
                            (\ s -> FinPosetArrow Set.empty (error "empty domain function") s)

    finIntSetExample :: String
    finIntSetExample = simpleGraphToDot $ toGraph embFinPosetInt [edgeF, edgeG, edgeH]
        where 
            edgeF = createEmblishArrow f "1" "2"
            edgeG = createEmblishArrow g "2" "1"
            edgeH = createEmblishArrow h "1" "3"
          

    initalObjectExample :: String
    initalObjectExample = simpleGraphToDot $ toGraph embFinPosetInt [edgeF, edgeG, edgeH]
        where
            edgeF = createEmblishArrow (universalProperty posetInitialObject a) "0" "1"
            edgeG = createEmblishArrow (universalProperty posetInitialObject b) "0" "2"
            edgeH = createEmblishArrow (universalProperty posetInitialObject c) "0" "3"