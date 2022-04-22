module Categories.EmbGraph (
    EmbGraph,
    createEmbGraph,
    catToGraph,
    createEmblishArrow
) where
    import Categories.Category
    import Categories.SimpleGraph
    
    type EmbGraph o a = Cat (EmbGraphObject o) (EmbGraphArrow o a)

    data EmbGraphObject o = EmbGraphObject {
        tag :: String,
        embeddedObject :: o
    }

    data EmbGraphArrow o a = EmbGraphArrow { 
        arrow :: a,
        tagSource :: o -> EmbGraphObject o,
        tagTarget :: o -> EmbGraphObject o
    }

    createEmblishArrow :: a -> String -> String -> EmbGraphArrow o a
    createEmblishArrow arr tagA tagB = EmbGraphArrow arr (EmbGraphObject tagA) (EmbGraphObject tagB)

    createEmbGraph :: Cat o a -> EmbGraph o a
    createEmbGraph cat = Cat (embGraphSource cat) (embGraphTarget cat) (embGraphId cat) (embGraphComp cat)
        where
            embGraphSource :: Cat o a -> EmbGraphArrow o a -> EmbGraphObject o
            embGraphSource cat embArrow = tagSource embArrow $ source cat (arrow embArrow)

            embGraphTarget :: Cat o a -> EmbGraphArrow o a -> EmbGraphObject o
            embGraphTarget cat embArrow = tagTarget embArrow $ target cat (arrow embArrow)

            embGraphId :: Cat o a -> EmbGraphObject o -> EmbGraphArrow o a
            embGraphId cat obj = EmbGraphArrow (identity cat (embeddedObject obj)) (const obj) (const obj)

            embGraphComp :: Cat o a -> EmbGraphArrow o a -> EmbGraphArrow o a -> EmbGraphArrow o a
            embGraphComp cat a b = EmbGraphArrow (camposition cat (arrow a) (arrow b)) (tagSource a) (tagTarget b)
    
    catToGraph :: Cat o a -> (o -> String, a -> String) -> [EmbGraphArrow o a] -> [Edge]
    catToGraph cat toStringFunctor = map arrowToEdge'
        where arrowToEdge' = arrowToEdge cat toStringFunctor

    arrowToEdge :: Cat o a -> (o -> String, a -> String) -> EmbGraphArrow o a -> Edge
    arrowToEdge cat (labelObject, labelArrow) EmbGraphArrow {arrow=arr, tagSource=tagSource, tagTarget=tagTarget} =
        Edge (labelArrow arr) sourceNode targetNode
            where sourceNode = objectToNode labelObject (tagSource (source cat arr))
                  targetNode = objectToNode labelObject (tagTarget (target cat arr))

    objectToNode :: (o -> String) -> EmbGraphObject o -> Node
    objectToNode labelObject embGraph = Node (tag embGraph) (labelObject (embeddedObject embGraph))