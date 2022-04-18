module Categories.Graph(
    Node(..),
    Edge(..),
    graphCat
) where
    import Categories.Category

    data Node = Node { name :: String, labelNode :: String } deriving (Eq)
    data Edge = Edge { labelEdge :: String, graphSource :: Node, graphTarget :: Node } deriving (Eq)

    graphId :: Node -> Edge
    graphId a = Edge "id" a a

    graphComposition :: Edge -> Edge -> Edge
    graphComposition f g = if graphTarget f == graphSource g
                                then Edge (labelEdge f ++ "." ++ labelEdge g) (graphSource f) (graphTarget g)
                                else error "these arrows not compose"

    graphCat :: Cat Node Edge
    graphCat = Cat graphSource graphTarget graphId graphComposition