module Lib1
    ( completions
    ) where

-- | This function returns a list of words
-- to be autocompleted in your program's repl.
completions :: [String]
completions = [
    -- actions
    "add_request id,type,origin,item1,item2,...",
    "remove_request id",
    "list_requests",
    "find_request id",
    "update_request id, id,type,origin,item1,item2,...",
    "remove_all_requests",
    "save",
    "load"
    ]
