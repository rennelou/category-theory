module Categories.Graph(
    Node(..),
    Edge(..),
    graphCat,
    graphToDot
) where
    import Categories.Category
    import Text.Printf

    data Node = Node { nodeId :: String, labelNode :: String }
    data Edge = Edge { labelEdge :: String, graphSource :: Node, graphTarget :: Node }

    graphCat :: Cat Node Edge
    graphCat = Cat graphSource graphTarget graphId graphComposition
        where
            graphId :: Node -> Edge
            graphId a = Edge "id" a a

            graphComposition :: Edge -> Edge -> Edge
            graphComposition f g = if (labelNode.graphTarget) f == (labelNode.graphSource) g then
                                        Edge 
                                        (printf "%s.%s" (labelEdge f) (labelEdge g))
                                        (graphSource f)
                                        (graphTarget g)
                                   else
                                        error "these arrows not compose"

    graphToDot :: ([Node], [Edge]) -> String
    graphToDot (nodes, edges) =
        printf "digraph G {\n%s\n%s}\n" (concatMap nodeToDot nodes) (concatMap edgeToDot edges)

    nodeToDot :: Node -> String
    nodeToDot n =
        printf "\t%s[label=\"%s\"];\n" (nodeId n) (labelNode n)

    edgeToDot :: Edge -> String
    edgeToDot e =
        printf
            "\t%s -> %s [label=\"%s\"];\n"
            (nodeId.source graphCat $ e)
            (nodeId.target graphCat $ e)
            (labelEdge e)