module Categories.EmbGr (
    EmbGr,
    EmbGrObject,
    EmbGrArrow,
    createEmbGr,
    createEmblishArrow,
    catToGraph
) where
    import Categories.Category
    import Categories.Graph
    
    type EmbGr o a = Cat (EmbGrObject o) (EmbGrArrow o a)

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

    createEmbGr :: Cat o a -> EmbGr o a
    createEmbGr cat = Cat (embGrSource cat) (embGrTarget cat) (embGrId cat) (embComp cat)
        where
            embGrSource :: Cat o a -> EmbGrArrow o a -> EmbGrObject o
            embGrSource cat embArrow = tagSource embArrow $ source cat (arrow embArrow)

            embGrTarget :: Cat o a -> EmbGrArrow o a -> EmbGrObject o
            embGrTarget cat embArrow = tagTarget embArrow $ target cat (arrow embArrow)

            embGrId :: Cat o a -> EmbGrObject o -> EmbGrArrow o a
            embGrId cat obj = EmbGrArrow (identity cat (embeddedObject obj)) (const obj) (const obj)

            embComp :: Cat o a -> EmbGrArrow o a -> EmbGrArrow o a -> EmbGrArrow o a
            embComp cat a b = EmbGrArrow (camposition cat (arrow a) (arrow b)) (tagSource a) (tagTarget b)

    catToGraph :: Cat o a -> (o -> String) -> (a -> String) -> [EmbGrArrow o a] -> [Edge]
    catToGraph cat labelObject labelArrow = map arrowToEdge'
        where arrowToEdge' = arrowToEdge cat labelObject labelArrow

    arrowToEdge :: Cat o a -> (o -> String) -> (a -> String) -> EmbGrArrow o a -> Edge
    arrowToEdge cat labelObject labelArrow EmbGrArrow {arrow=arr, tagSource=tagSource, tagTarget=tagTarget} =
        Edge (labelArrow arr) sourceNode targetNode
            where sourceNode = objectToNode labelObject (tagSource (source cat arr))
                  targetNode = objectToNode labelObject (tagTarget (target cat arr))

    objectToNode :: (o -> String) -> EmbGrObject o -> Node
    objectToNode labelObject embGr = Node (tag embGr) (labelObject (embeddedObject embGr))