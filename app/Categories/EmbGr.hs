module Categories.EmbGr (
    EmbGrObject(..),
    EmbGrArrow(..),
    catToDot,
) where
    import Categories.Category
    import Categories.Graph
    import qualified Data.Map as Map
    import Text.Printf
    
    data EmbGrObject o = EmbGrObject {
        tag :: String,
        embeddedObject :: o
    }

    data EmbGrArrow o a = EmbGrArrow { 
        arrow :: a,
        tagSource :: o -> EmbGrObject o,
        tagTarget :: o -> EmbGrObject o
    }

    catToDot :: (Show o, Show a) => Cat o a -> [EmbGrArrow o a] -> String
    catToDot cat arrows = uncurry graphToDot (catToGraph cat arrows)
    
    catToGraph :: (Show o, Show a) => Cat o a -> [EmbGrArrow o a] -> ([Node], [Edge])
    catToGraph cat arrows = (nodes, edges)
        where edges = map (arrowToEdge cat) arrows
              nodes = selectNodeFromEdges edges

    arrowToEdge :: (Show o, Show a) => Cat o a -> EmbGrArrow o a -> Edge
    arrowToEdge cat EmbGrArrow {arrow=arr, tagSource=tagSource, tagTarget=tagTarget} =
        Edge (show arr) sourceNode targetNode
            where sourceNode = objectToNode (tagSource (source cat arr))
                  targetNode = objectToNode (tagTarget (target cat arr))

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

    objectToNode :: (Show o) =>  EmbGrObject o -> Node
    objectToNode embGr = Node (tag embGr) (show (embeddedObject embGr))