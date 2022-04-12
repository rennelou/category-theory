module DiagramChasing (
    diagramChasing
) where
    import Categories.Category (Cat(source, target))
    import Diagrams.Prelude
    import Diagrams.Backend.SVG.CmdLine

    diagramChasing :: Cat o a -> (o -> String) -> [o] -> [a] -> Diagram B
    diagramChasing cat tag o a = applyAll [ 
            customConnectOutside (tag $ source cat arrow) (tag $ target cat arrow) | arrow <- a
        ] (getNodes cat tag o)

    getNodes :: Cat o a -> (o -> String) -> [o] -> Diagram B
    getNodes cat tag o = atPoints (trailVertices (regPoly (length o) 1)) (map (node.tag) o)

    node :: String -> Diagram B
    node s = text s # fontSizeL 0.2 # fc white 
                <> circle 0.2 # fc green # named s

    customConnectOutside = connectOutside' arrawOpts
        where arrawOpts = with  & gaps .~ small 
                            & headLength  .~ local 0.15