module Constructions.CoProduct (
    PreCoProduct(..),
    CoProductColimit(..),
    CoProduct(..)
) where

    data PreCoProduct object arrow = PreCoProduct object arrow arrow

    data CoProductColimit object arrow = CoProductColimit {
        getCoCone :: PreCoProduct object arrow,
        getUniversalProductProperty :: PreCoProduct object arrow -> arrow
    };

    newtype CoProduct object arrow = CoProduct (object -> object -> CoProductColimit object arrow)