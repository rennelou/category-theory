module UniversalConstructions.InitialObject (
    InitialObject(..)
) where

    data InitialObject o a = InitialObject {
        initialObject :: o,
        universalProperty :: (o -> a)
    }