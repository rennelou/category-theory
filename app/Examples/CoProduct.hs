module Examples.CoProduct (
    
)
where

    import Constructions.CoProduct
    import Categories.FinPoset
    import qualified Data.Set as Set

    embFinPosetTagedInt :: EmbGraph (FinPosetObjcet (Eihter Int Int)) (FinPosetArrow (Eihter Int Int))
    embFinPosetTagedInt = createEmbGraph createFinPosetCat (tagO, tagA)

    coProductFinPosetTagedInt :: CoProduct (FinPosetObjcet (Eihter Int Int)) (FinPosetArrow (Eihter Int Int))
    coProductFinPosetTagedInt = createCoProductColimit

    createCoProductColimit :: o -> o -> CoProductColimit (FinPosetObjcet (Eihter Int Int)) (FinPosetArrow (Eihter Int Int))
    createCoProductColimit a b = CoProductColimit cocone univ
        where
            sumab :: Set.Set (Eihter Int Int)
            sumab =  Set.union a b
            
            cocone :: PreCoProduct (FinPosetObjcet (Eihter Int Int)) (FinPosetArrow (Eihter Int Int))
            cocone = PreCoProduct sumab (FinPosetArrow a id sumab) (FinPosetArrow b id sumab)

            univ :: PreCoProduct (FinPosetObjcet (Eihter Int Int)) (FinPosetArrow (Eihter Int Int)) -> (FinPosetArrow (Eihter Int Int))
            univ preCoProduct((Left n), f', g') = f'
            univ preCoProduct((Right n), f', g') = g'

    tagO x 
        | x == Set.empty = "Empty"
        | x == a = "a"
        | x == b = "b"
        | otherwise = "Untaged"
  
    tagA x 
        | compareFinPosetArrow x f = "F"
        | compareFinPosetArrow x g = "G"
        | otherwise = "Untaged"

    a = Set.fromList $ map Right [0, 1, 2, 3]
    b = Set.fromList $ map Left [0, 1, 4, 9]

    f = constructFinPosetArrow a (\x -> x*x) Set.empty
    g = constructFinPosetArrow b (floor . sqrt . fromIntegral) (Set.fromList [0, 1])