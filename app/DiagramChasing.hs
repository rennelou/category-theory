module DiagramChasing (
    diagramChasing
) where
    import Categories.Category (Cat(source, target))
    import Diagrams.Prelude
    import Diagrams.Backend.SVG.CmdLine

    node :: String -> Diagram B
    node s = text s # fontSizeL 0.2 # fc white 
                <> circle 0.2 # fc green # named s

    customConnectOutside = connectOutside' arrawOpts
        where arrawOpts = with  & gaps .~ small 
                            & headLength  .~ local 0.15

    diagramChasing :: Cat o a -> (o -> String) -> [o] -> [a] -> Diagram B
    diagramChasing cat tag o a = atPoints (trailVertices (regPoly (length o) 1)) (map (node.tag) o)
        # applyAll [ 
            customConnectOutside (tag $ source cat arrow) (tag $ target cat arrow) | arrow <- a
        ]