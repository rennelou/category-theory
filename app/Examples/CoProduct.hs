module Examples.CoProduct (
    
)
where

    import Constructions.CoProduct
    import Categories.FinPoset
    import Categories.EmbGraph
    import Categories.SimpleGraph
    import Data.Either
    import qualified Data.Set as Set

    embFinPosetTagedInt :: EmbGraph (FinPosetObjcet (Either Int Int)) (FinPosetArrow (Either Int Int))
    embFinPosetTagedInt = createEmbGraph createFinPosetCat (tagO, tagA)

    coProductFinPosetTagedInt :: CoProduct (FinPosetObjcet (Either Int Int)) (FinPosetArrow (Either Int Int))
    coProductFinPosetTagedInt = CoProduct createCoProductColimit

    createCoProductColimit :: FinPosetObjcet (Either Int Int) -> FinPosetObjcet (Either Int Int) -> CoProductColimit (FinPosetObjcet (Either Int Int)) (FinPosetArrow (Either Int Int))
    createCoProductColimit a b = CoProductColimit cocone univ
        where
            sumab :: Set.Set (Either Int Int)
            sumab =  Set.union a b
            
            cocone :: PreCoProduct (FinPosetObjcet (Either Int Int)) (FinPosetArrow (Either Int Int))
            cocone = PreCoProduct sumab (FinPosetArrow a id sumab) (FinPosetArrow b id sumab)

            univ :: PreCoProduct (FinPosetObjcet (Either Int Int)) (FinPosetArrow (Either Int Int)) -> FinPosetArrow (Either Int Int)
            univ (PreCoProduct p FinPosetArrow{domain=_, totalFunction=f', codomain=_} FinPosetArrow{domain=_, totalFunction=g', codomain=_}) =
                FinPosetArrow sumab (u' f' g') p

            u' :: (Either Int Int -> Either Int Int) -> (Either Int Int -> Either Int Int) -> (Either Int Int -> Either Int Int)
            u' f' g' = (\ x -> 
                case x of
                    Left _  -> f' x
                    Right _ -> g' x )

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