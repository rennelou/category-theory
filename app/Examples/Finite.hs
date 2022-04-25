module Examples.Finite (
    finiteExample
) where 
    import System.IO

    import Categories.EmbGraph
    import Categories.SimpleGraph
    import Categories.Finite

    embFinitGraph :: EmbGraph FiniteObject  FiniteArrow 
    embFinitGraph = createEmbGraph finiteCat (show, show)

    finiteExample :: String
    finiteExample = simpleGraphToDot $ toGraph embFinitGraph [edgeF, edgeG, edgeH, edgeK]
        where edgeF = createEmblishArrow F "0" "1"
              edgeG = createEmblishArrow G "1" "3"
              edgeH = createEmblishArrow H "1" "3"
              edgeK = createEmblishArrow K "2" "3"