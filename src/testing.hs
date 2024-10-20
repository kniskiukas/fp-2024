{-# OPTIONS_GHC -Wno-unused-top-binds -Wname-shadowing #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant lambda" #-}

import qualified Data.Char as C
import qualified Data.List as L

type Parser a = String -> Either String (a, String)

parseString :: String -> Parser String
parseString s [] = Left ("Cannot find " ++ s ++ " in an empty input")
parseString s input =
    if L.isPrefixOf s input
        then Right (s, L.drop (length s) input)
        else Left (s ++ " is not found in " ++ input)


