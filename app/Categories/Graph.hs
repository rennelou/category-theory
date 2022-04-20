module Categories.Graph(
    Node(..),
    Edge(..),
    graphCat,
    graphToDot
) where
    import Categories.Category

    data Node = Node { nodeId :: String, labelNode :: String }
    data Edge = Edge { labelEdge :: String, graphSource :: Node, graphTarget :: Node }

    graphId :: Node -> Edge
    graphId a = Edge "id" a a

    graphComposition :: Edge -> Edge -> Edge
    graphComposition f g = if (labelNode.graphTarget) f == (labelNode.graphSource) g
                                then Edge (labelEdge f ++ "." ++ labelEdge g) (graphSource f) (graphTarget g)
                                else error "these arrows not compose"

    graphCat :: Cat Node Edge
    graphCat = Cat graphSource graphTarget graphId graphComposition

    graphToDot :: [Node] -> [Edge] -> String
    graphToDot nodes edges =
        "digraph G {\n" ++
        concatMap nodeToDot nodes ++
        concatMap edgeToDot edges  ++
        "}\n"

    nodeToDot :: Node -> String
    nodeToDot n =
        "\t" ++ nodeId n ++
        "[label=\"" ++
        labelNode n ++
        "\"];\n"

    edgeToDot :: Edge -> String
    edgeToDot e =
        "\t" ++ (nodeId.source graphCat) e ++
        " -> " ++
        (nodeId.target graphCat) e ++
        "[label=\"" ++
        labelEdge e ++
        "\"];\n"