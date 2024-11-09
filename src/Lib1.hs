module Lib1
    ( completions
    ) where

-- | This function returns a list of words
-- to be autocompleted in your program's repl.
completions :: [String]
completions = [
    -- actions
    "add_request",
    "remove_request",
    "list_requests",
    "find_request",
    "update_request",
    "remove_all_requests"
    ]
