module Categories.FinPoset (
    FinPosetCat,
    FinPosetObjcet,
    FinPosetArrow,
    domain,
    totalFunction,
    codomain,
    constructFinPosetArrow,
    createFinPosetCat,
    applyFinPosetArrow,
    compareFinPosetArrow
) where
    import Categories.Category
    import qualified Data.Set as Set

    type FinPosetCat t = Cat (FinPosetObjcet t) (FinPosetArrow t)

    type FinPosetObjcet t = Set.Set t

    data FinPosetArrow t = FinPosetArrow {
        domain :: Set.Set t,
        totalFunction :: t -> t,
        codomain :: Set.Set t
    }

    constructFinPosetArrow :: (Ord t) => Set.Set t -> (t -> t) -> Set.Set t -> FinPosetArrow t
    constructFinPosetArrow d f c = FinPosetArrow d f (Set.union c (Set.map f d))
    
    createFinPosetCat :: (Ord t) => FinPosetCat t
    createFinPosetCat = Cat finSetSource finSetTarget finSetId finSetComp
        where
            finSetSource :: FinPosetArrow t -> FinPosetObjcet t
            finSetSource FinPosetArrow {domain = d, totalFunction = _, codomain = _} = d

            finSetTarget :: FinPosetArrow t -> FinPosetObjcet t
            finSetTarget FinPosetArrow {domain = _, totalFunction = _, codomain = c} = c

            finSetId :: FinPosetObjcet t -> FinPosetArrow t
            finSetId object = FinPosetArrow object id object

            finSetComp :: (Ord t) => FinPosetArrow t -> FinPosetArrow t -> FinPosetArrow t
            finSetComp FinPosetArrow {domain = a, totalFunction = f, codomain = b}
                    FinPosetArrow {domain = c, totalFunction = g, codomain = d} =
                    if b == c
                        then FinPosetArrow a (g.f) d
                        else error "these arrows not compose"

    compareFinPosetArrow :: (Ord t) => FinPosetArrow t -> FinPosetArrow t -> Bool
    compareFinPosetArrow a b =
        (domain a == domain b) &&
        (codomain a == codomain b) &&
        (applyFinPosetArrow a == applyFinPosetArrow b)

    applyFinPosetArrow :: (Ord t) => FinPosetArrow t -> FinPosetObjcet t
    applyFinPosetArrow a = Set.map (totalFunction a) (domain a)