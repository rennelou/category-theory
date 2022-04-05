module Categories.FinSet (
    FinSetObject,
    FinSetArrow(..),
    finIntSetCat
) where
    import Categories.Category

    import qualified Data.Set as Set

    type FinSetObject t = Set.Set t

    data FinSetArrow t = FinSetArrow {
        domain :: Set.Set t,
        totalFunction :: t -> t,
        codomain :: Set.Set t
    }

    finSetSource :: FinSetArrow t -> FinSetObject t
    finSetSource FinSetArrow {domain = d, totalFunction = _, codomain = _} = d

    finSetTarget :: FinSetArrow t -> FinSetObject t
    finSetTarget FinSetArrow {domain = _, totalFunction = _, codomain = c} = c

    finSetId :: FinSetObject t -> FinSetArrow t
    finSetId object = FinSetArrow object id object

    finSetComp :: (Ord t) => FinSetArrow t -> FinSetArrow t -> FinSetArrow t
    finSetComp FinSetArrow {domain = a, totalFunction = f, codomain = b}
               FinSetArrow {domain = c, totalFunction = g, codomain = d} =
                   if b == c
                       then FinSetArrow a (g.f) d
                       else error "these arrows not compose"

    finIntSetCat :: Cat (FinSetObject Int) (FinSetArrow Int)
    finIntSetCat = Cat finSetSource finSetTarget finSetId finSetComp