module Categories.Graph(
    Node(..),
    Edge(..),
    GraphCat
) where
    import Categories.Category

    data Node = Node { label :: String }
    data Edge = Edge { label:: String, graphSource :: Node, graphTarget :: Node }

    graphId :: Node -> Edge
    graphId a = Edge("id", a, a)

    graphComposition :: Edge -> Edge -> Edge
    graphComposition f, g = if graphTarget a == graphSource b
                                then Edge(label a ++ "." ++ label b, graphSource a, graphTarget b)
                                else error "these arrows not compose"

    GraphCat :: Cat Node Edge
    GraphCat = Cat graphSource graphTarget graphId graphComposition