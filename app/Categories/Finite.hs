module Categories.Finite
(
    finiteCat,
    FiniteObject(..),
    FiniteArrow(..)
) where
    import Constructions.Category

    data FiniteObject = A | B | C deriving(Show)
    data FiniteArrow = F | G | H | K | Id FiniteObject deriving(Show)

    finiteCat = Cat finiteSource finiteTarget finiteIdent finiteComp
        where
            finiteSource :: FiniteArrow -> FiniteObject
            finiteSource F = B
            finiteSource G = A
            finiteSource H = A
            finiteSource K = B
            finiteSource (Id o) = o

            finiteTarget :: FiniteArrow -> FiniteObject
            finiteTarget F = A
            finiteTarget G = C
            finiteTarget H = C
            finiteTarget K = C
            finiteTarget (Id o) = o

            finiteIdent :: FiniteObject -> FiniteArrow
            finiteIdent = Id        

            finiteComp :: FiniteArrow -> FiniteArrow -> FiniteArrow
            finiteComp (Id _) b = b
            finiteComp b (Id _) = b
            finiteComp G F = K
            finiteComp H F = K
            finiteComp _ _ = error "these arrows not compose"