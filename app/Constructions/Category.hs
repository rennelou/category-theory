module Constructions.Category
(
    Cat(..)
) where

    data Cat object arrow = Cat {
        source :: arrow -> object,
        target :: arrow -> object,
        identity :: object -> arrow,
        camposition :: arrow -> arrow -> arrow
    }