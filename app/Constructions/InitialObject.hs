module Constructions.InitialObject (
    InitialObject(..)
) where

    data InitialObject object arrow = InitialObject {
        initialObject :: object,
        universalProperty :: object -> arrow
    }