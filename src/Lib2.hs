
{-# OPTIONS_GHC -Wno-unused-top-binds -Wname-shadowing #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant lambda" #-}


module Lib2
    ( Query(..),
    parseQuery,
    State(..),
    Request(..),
    emptyState,
    stateTransition
    --parseAddRequest,
    --parseListRequests,
    --parseRemoveRequest,
    --parseUpdateRequest,
    --parseFindRequest,
    --parseAddRequestQuery,
    --parseRemoveRequestQuery,
    --parseUpdateRequestQuery,
    --parseFindRequestQuery,
    --parseListRequestsQuery,
    ) where



import qualified Data.Char as C
import qualified Data.List as L

type Parser a = String -> Either String (a, String)

-- parseItems :: Parser Items
-- parseItems = or2 (and3 parseManyLetters parseComma parseItems) parseManyLetters
parseItems :: Parser Items
parseItems = or2 (and3                                                                  parseManyLetters parseComma     parseItems)    parseManyLetters
--                                                                                       parser string   parser char   parser items    parser string
--               (a -> b -> c -> d) -> Parser a -> Parser b -> Parser c -> Parser d





parseRequest :: Parser Request
parseRequest = and7 parseNumber parseComma parseManyLetters parseComma parseManyLetters parseComma parseItems

-- >>> parseItems "item1,item2"
-- /workspaces/fp-2024/src/Lib2.hs:34:64: error:
--     • Couldn't match type ‘Char’ with ‘[Char]’
--       Expected: Parser Items
--         Actual: Parser String
--     • In the second argument of ‘or2’, namely ‘parseManyLetters’
--       In the expression:
--         or2 (and3 parseManyLetters parseComma parseItems) parseManyLetters
--       In an equation for ‘parseItems’:
--           parseItems
--             = or2
--                 (and3 parseManyLetters parseComma parseItems) parseManyLetters
-- (deferred type error)



data Query = 
  AddRequest Request
  | ListRequests
  | RemoveRequest Int
  | UpdateRequest Int Request
  | FindRequest String
  deriving (Eq, Show)

data Request = Request RequestId RequestType RequestOrigin Items
  deriving (Eq, Show)
type RequestId = Int
type RequestType = String
type RequestOrigin = String 
type Items = [String]

parseQuery :: String -> Either String Query
parseQuery s = case parseCommand s of
  Left err -> Left err
  Right (v, r) -> if null r then Right v else Left ("Unexpected '" ++ r ++ "' after parsing")

-- >>> parseAddRequest "add_request 1, type, origin, item1, item2"
-- /workspaces/fp-2024/src/Lib2.hs:77:50: error:
--     • Couldn't match type ‘Char’ with ‘[Char]’
--       Expected: Parser String
--         Actual: Parser Char
--     • In the third argument of ‘and3’, namely ‘parseSpace’
--       In the expression:
--         and3 parseString "add_request" parseSpace parseRequest
--       In an equation for ‘parseAddRequest’:
--           parseAddRequest
--             = and3 parseString "add_request" parseSpace parseRequest
-- (deferred type error)
parseCommand :: Parser Query
parseCommand = or2 parseAddRequest (or2 parseListRequests (or2 parseRemoveRequest (or2 parseUpdateRequest parseFindRequest)))

-- Assuming you have a type `Request` and a parser `parseRequest`
parseAddRequest :: Parser Query
parseAddRequest = and3 parseString "add_request" parseSpace parseRequest

parseListRequests :: Parser Query
parseListRequests = parseString "list_requests"

parseRemoveRequest :: Parser Query
parseRemoveRequest = and3 parseString "remove_request" parseSpace parseDigit

parseUpdateRequest :: Parser Query
parseUpdateRequest = and5 parseString "update_request" parseSpace parseDigit parseSpace parseRequest

parseFindRequest :: Parser Query
parseFindRequest = and3 parseString "find_request" parseSpace parseDigit

data State = State {
  requests :: [Request],
  requestIds :: [Int]
}
 deriving (Eq, Show)


 


emptyState :: State
emptyState = State [] []


stateTransition :: State -> Query -> Either String (Maybe String, State)
stateTransition s (AddRequest r) = 
  if requestExists (requests s) r
    then Left "Request already exists"
    else Right (Just "Request added", State (r : requests s) (requestIds s ++ [requestId r]))

stateTransition s ListRequests = Right (Just (show (requests s)), s)

stateTransition s (RemoveRequest i) =
  if not (requestIdExists (requestIds s) i)
    then Left "Request does not exist"
    else Right (Just "Request removed", State (filter (\r -> requestId r /= i) (requests s)) (filter (/= i) (requestIds s)))

stateTransition s (UpdateRequest i r) =
  if not (requestIdExists (requestIds s) i)
    then Left "Request does not exist"
    else Right (Just "Request updated", State (map (\r2 -> if requestId r2 == i then r else r2) (requests s)) (requestIds s))

stateTransition s (FindRequest t) = Right (Just (show (filter (\r -> requestType r == t) (requests s))), s)

stateTransition s _ = Right (Nothing, s)







--basic parsers

parseChar :: Char -> Parser Char
parseChar _ [] = Left "Cannot find character in an empty input"
parseChar c s@(h : t) = if c == h then Right (c, t) else Left ("Expected '" ++ [c] ++ "' but found '" ++ [h] ++ "' in " ++ s)

parseSpace :: Parser Char
parseSpace = parseChar ' '

parseComma :: Parser Char
parseComma = parseChar ','

parseDigit :: Parser Char
parseDigit [] = Left "Cannot find any digits in an empty input"
parseDigit s@(h : t) = if C.isDigit h then Right (h, t) else Left (s ++ " does not start with a digit")

parseNumber :: Parser Integer
parseNumber [] = Left "empty input, cannot parse a number"
parseNumber str =
    let
        digits = L.takeWhile C.isDigit str
        rest = drop (length digits) str
    in
        case digits of
            [] -> Left "not a number"
            _ -> Right (read digits, rest)

parseString :: String -> Parser String
parseString [] s = Right ([], s)  
parseString (c:cs) s = case parseChar c s of
  Left err -> Left err
  Right (_, rest) -> case parseString cs rest of
    Left err -> Left err
    Right (v2, r2) -> Right (c:v2, r2)

parseManyLetters :: Parser String
parseManyLetters = many1 parseLetter

parseLetter :: Parser Char
parseLetter [] = Left "Cannot find any letter in an empty input"

parseLetter s@(h:t) = if C.isLetter h then Right (h, t) else Left (s ++ " does not start with a letter")


--helper parsers

or2 :: Parser a -> Parser a -> Parser a
or2 a b = \input ->
    case a input of
        Right r1 -> Right r1
        Left e1 ->
            case b input of
                Right r2 -> Right r2
                Left e2 -> Left (e1 ++ ", " ++ e2)

and2 :: (a -> b -> c) -> Parser a -> Parser b -> Parser c
and2 f p1 p2 s = case p1 s of
  Left err -> Left err
  Right (v1, r1) -> case p2 r1 of
    Left err -> Left err
    Right (v2, r2) -> Right (f v1 v2, r2)

and3 :: (a -> b -> c -> d) -> Parser a -> Parser b -> Parser c -> Parser d
and3 f p1 p2 p3 s = case p1 s of
  Left err -> Left err
  Right (v1, r1) -> case p2 r1 of
    Left err -> Left err
    Right (v2, r2) -> case p3 r2 of
      Left err -> Left err
      Right (v3, r3) -> Right (f v1 v2 v3, r3)

and4 :: (a -> b -> c -> d -> e) -> Parser a -> Parser b -> Parser c -> Parser d -> Parser e
and4 f p1 p2 p3 p4 s = case p1 s of
  Left err -> Left err
  Right (v1, r1) -> case p2 r1 of
    Left err -> Left err
    Right (v2, r2) -> case p3 r2 of
      Left err -> Left err
      Right (v3, r3) -> case p4 r3 of
        Left err -> Left err
        Right (v4, r4) -> Right (f v1 v2 v3 v4, r4)

and7 :: (a -> b -> c -> d -> e -> f -> g -> h) -> Parser a -> Parser b -> Parser c -> Parser d -> Parser e -> Parser f -> Parser g -> Parser h
and7 f p1 p2 p3 p4 p5 p6 p7 s = case p1 s of
  Left err -> Left err
  Right (v1, r1) -> case p2 r1 of
    Left err -> Left err
    Right (v2, r2) -> case p3 r2 of
      Left err -> Left err
      Right (v3, r3) -> case p4 r3 of
        Left err -> Left err
        Right (v4, r4) -> case p5 r4 of
          Left err -> Left err
          Right (v5, r5) -> case p6 r5 of
            Left err -> Left err
            Right (v6, r6) -> case p7 r6 of
              Left err -> Left err
              Right (v7, r7) -> Right (f v1 v2 v3 v4 v5 v6 v7, r7)

many1 :: Parser a -> Parser [a]
many1 p s = case p s of
  Left err -> Left err
  Right (v1, r1) -> case many1' r1 of
    (v2, r2) -> Right (v1:v2, r2)
  where
    many1' s2 = case p s2 of
      Left _ -> ([], s2)
      Right (v2, r2) -> let (vs, r3) = many1' r2 in (v2 : vs, r3)


