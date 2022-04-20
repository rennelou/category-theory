module GraphFunctor (
    GraphFunctor(..),
    catToDot,
) where
    import Categories.Category
    import Categories.Graph

    data GraphFunctor o a = GraphFunctor { 
        cat :: Cat o a,
        arrow :: a,
        sourceId :: String,
        targetId :: String
    }

    arrowToEdge :: (Show o, Show a) => GraphFunctor o a -> Edge
    arrowToEdge GraphFunctor { cat = cat, arrow = arrow, sourceId = sourceId, targetId = targetId} =
        Edge (show arrow) sourceNode targetNode
            where sourceNode = objectToNode (source cat arrow) sourceId
                  targetNode = objectToNode (target cat arrow) targetId

    objectToNode :: (Show o) => o -> String -> Node
    objectToNode object name = Node name (show object)

    catToGraph :: (Show o, Show a) => [GraphFunctor o a] -> ([Node], [Edge])
    catToGraph arrows = (nodes, edges)
        where edges = map arrowToEdge arrows
              nodes = concatMap (\Edge {labelEdge = _, graphSource = sourceNode, graphTarget = targetNode} -> [sourceNode, targetNode]) edges

    catToDot :: (Show o, Show a) => [GraphFunctor o a] -> String
    catToDot cat = uncurry graphToDot (catToGraph cat)