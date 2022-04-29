module Constructions.CoProduct (
    PreCoProduct(..),
    CoProductCoCone(..),
    CoProduct(..)
) where

    data PreCoProduct object arrow = PreCoProduct object arrow arrow

    data CoProductCoCone object arrow = CoProductCoCone {
        getCoCone :: PreCoProduct object arrow,
        getUniversalProductProperty :: PreCoProduct object arrow -> arrow
    };

    newtype CoProduct object arrow = CoProduct (object -> object -> CoProductCoCone object arrow)