module Categories.Graph(
    Node(..),
    Edge(..),
    graphCat,
    constructGraph
) where
    import Categories.Category

    data Node = Node { nameNode :: String, labelNode :: String }
    data Edge = Edge { labelEdge :: String, graphSource :: Node, graphTarget :: Node }

    graphId :: Node -> Edge
    graphId a = Edge "id" a a

    graphComposition :: Edge -> Edge -> Edge
    graphComposition f g = if (labelNode.graphTarget) f == (labelNode.graphSource) g
                                then Edge (labelEdge f ++ "." ++ labelEdge g) (graphSource f) (graphTarget g)
                                else error "these arrows not compose"

    graphCat :: Cat Node Edge
    graphCat = Cat graphSource graphTarget graphId graphComposition

    constructGraph :: [Edge] -> String
    constructGraph edges =
        "digraph G {\n" ++
        concatMap printEdge edges  ++
        "}\n"

    defineNode :: Node -> String
    defineNode n =
        "\t" ++ nameNode n ++
        "[label=\"" ++
        labelNode n ++
        "\"];\n"

    printEdge :: Edge -> String
    printEdge e =
        defineNode (source graphCat e) ++
        
        defineNode (target graphCat e) ++

        "\t" ++ (nameNode.source graphCat) e ++
        " -> " ++
        (nameNode.target graphCat) e ++
        "[label=\"" ++
        labelEdge e ++
        "\"];\n"