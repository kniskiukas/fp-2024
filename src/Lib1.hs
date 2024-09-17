module Lib1
    ( completions
    ) where

-- | This function returns a list of words
-- to be autocompleted in your program's repl.
completions :: [String]
completions = [
    -- actions
    "addCar",
    "removeCar",
    "listCars",
    "findCar",
    "updateCar",
    "quit",
    -- colors
    "red",
    "blue",
    "green",
    "yellow",
    "black",
    "white",
    -- fuel types
    "gasoline",
    "diesel",
    "electric",
    "hybrid",
    -- models
    "sedan",
    "coupe",
    "convertible",
    "hatchback",
    "suv",
    "truck",
    "van"
    ]
