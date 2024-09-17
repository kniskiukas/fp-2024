module Lib1
    ( completions
    ) where

-- | This function returns a list of words
-- to be autocompleted in your program's repl.
completions :: [String]
completions = [
    -- actions
    "addRequest",
    "removeRequest",
    "listRequests",
    "findRequest",
    "updateRequest",
    "quit",
    -- request type
    "drink",
    "appetizer",
    "main",
    "desert",
    -- request origin
    "table",
    "bar",
    "online",
    "waiter"
    ]
