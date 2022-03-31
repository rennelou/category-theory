module Categories.Category
(
    Cat(..)
) where

    data Cat arrow object = Cat (arrow -> object) (arrow -> object) (object -> arrow) (arrow -> arrow -> arrow)