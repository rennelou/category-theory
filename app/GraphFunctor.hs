module GraphFunctor (
    GraphFunctor(..),
    catToDot,
) where
    import Categories.Category
    import Categories.Graph
    import qualified Data.Map as Map
    import Text.Printf
    
    data GraphFunctor o a = GraphFunctor { 
        cat :: Cat o a,
        arrow :: a,
        sourceId :: String,
        targetId :: String
    }

    catToDot :: (Show o, Show a) => [GraphFunctor o a] -> String
    catToDot cat = uncurry graphToDot (catToGraph cat)
    
    catToGraph :: (Show o, Show a) => [GraphFunctor o a] -> ([Node], [Edge])
    catToGraph arrows = (nodes, edges)
        where edges = map arrowToEdge arrows
              nodes = selectNodeFromEdges edges

    arrowToEdge :: (Show o, Show a) => GraphFunctor o a -> Edge
    arrowToEdge GraphFunctor { cat = cat, arrow = arrow, sourceId = sourceId, targetId = targetId} =
        Edge (show arrow) sourceNode targetNode
            where sourceNode = objectToNode (source cat arrow) sourceId
                  targetNode = objectToNode (target cat arrow) targetId

    selectNodeFromEdges :: [Edge] -> [Node]
    selectNodeFromEdges edges = Map.elems $ 
        foldr tryAddNodes Map.empty (concatMap (\e -> [graphSource e, graphTarget e]) edges)

    tryAddNodes :: Node -> Map.Map String Node -> Map.Map String Node
    tryAddNodes node = Map.insertWith 
        (\ a b ->
            if labelNode a == labelNode b then
                a 
            else
                error $ printf 
                            "Node %s can't rename label %s to %s"
                            (nodeId a)
                            (labelNode a)
                            (labelNode b)
        )
        (nodeId node)
        node

    objectToNode :: (Show o) => o -> String -> Node
    objectToNode object name = Node name (show object)