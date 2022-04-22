module Categories.EmbGr (
    EmbGrObject,
    EmbGrArrow,
    createEmbGr,
    createEmblishArrow,
    catToGraph
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

    createEmblishArrow :: a -> String -> String -> EmbGrArrow o a
    createEmblishArrow arr tagA tagB = EmbGrArrow arr (EmbGrObject tagA) (EmbGrObject tagB)

-- Categoria dos grafos enriquecidos
    createEmbGr :: Cat o a -> Cat (EmbGrObject o) (EmbGrArrow o a)
    createEmbGr cat = Cat (embGrSource cat) (embGrTarget cat) (embGrId cat) (embComp cat)

    embGrSource :: Cat o a -> EmbGrArrow o a -> EmbGrObject o
    embGrSource cat embArrow = tagSource embArrow $ source cat (arrow embArrow)

    embGrTarget :: Cat o a -> EmbGrArrow o a -> EmbGrObject o
    embGrTarget cat embArrow = tagTarget embArrow $ target cat (arrow embArrow)

    embGrId :: Cat o a -> EmbGrObject o -> EmbGrArrow o a
    embGrId cat obj = EmbGrArrow (identity cat (embeddedObject obj)) (const obj) (const obj)

    embComp :: Cat o a -> EmbGrArrow o a -> EmbGrArrow o a -> EmbGrArrow o a
    embComp cat a b = EmbGrArrow (camposition cat (arrow a) (arrow b)) (tagSource a) (tagTarget b)

-- Metodos
    catToGraph :: Cat o a -> (o -> String) -> (a -> String) -> [EmbGrArrow o a] -> ([Node], [Edge])
    catToGraph cat labelObject labelArrow arrows = (nodes, edges)
        where edges = map (arrowToEdge cat labelObject labelArrow) arrows
              nodes = selectNodeFromEdges edges

    arrowToEdge :: Cat o a -> (o -> String) -> (a -> String) -> EmbGrArrow o a -> Edge
    arrowToEdge cat labelObject labelArrow EmbGrArrow {arrow=arr, tagSource=tagSource, tagTarget=tagTarget} =
        Edge (labelArrow arr) sourceNode targetNode
            where sourceNode = objectToNode labelObject (tagSource (source cat arr))
                  targetNode = objectToNode labelObject (tagTarget (target cat arr))

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

    objectToNode :: (o -> String) -> EmbGrObject o -> Node
    objectToNode labelObject embGr = Node (tag embGr) (labelObject (embeddedObject embGr))