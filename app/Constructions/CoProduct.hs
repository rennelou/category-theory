module Constructions.CoProduct (

) where

    data PreCoProduct object arrow = PreCoProduct object arrow arrow

    data CoProductCoCone object arrow = CoProductCoCone {
        coProduct :: PreCoProduct object arrow,
        universalProductProperty :: PreCoProduct object arrow -> arrow
    };

    newtype CoProduct object arrow = CoProduct (object -> object -> CoProductCoCone object arrow)