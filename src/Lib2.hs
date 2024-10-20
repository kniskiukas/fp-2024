{-# LANGUAGE InstanceSigs #-}



module Lib2
    ( Query(..),
    parseQuery,
    State(..),
    emptyState,
    stateTransition
    parseAddRequest,
    parseListRequests,
    parseRemoveRequest,
    parseUpdateRequest,
    parseFindRequest,
    RequestInfo,

    ) where

-- | An entity which represets user input.
-- It should match the grammar from Laboratory work #1.
-- Currently it has no constructors but you can introduce
-- as many as needed.

import qualified Data.Char as C

type Parser a = String -> Either String (a, String)

--basic parsers

parseChar :: Char -> Parser Char
parseChar _ [] = Left "Cannot find character in an empty input"
parseChar c s@(h : t) = if c == h then Right (c, t) else Left ("Expected '" ++ [c] ++ "' but found '" ++ [h] ++ "' in " ++ s)

parseLetter :: Parser Char
parseLetter [] = Left "Cannot find any letter in an empty input"
parseLetter s@(h : t) = if C.isLetter h then Right (h, t) else Left (s ++ " does not start with a letter")

parseSpace :: Parser Char
parseSpace = parseChar ' '

parseDigit :: Parser Char
parseDigit [] = Left "Cannot find any digits in an empty input"
parseDigit s@(h : t) = if C.isDigit h then Right (h, t) else Left (s ++ " does not start with a digit")

parseString :: String -> Parser String
parseString [] s = Right ([], s)  
parseString (c:cs) s = case parseChar c s of
    Left err -> Left err
    Right (_, rest) -> case parseString cs rest of
        Left err -> Left err
        Right (v2, r2) -> Right (c:v2, r2)

--helper parsers

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

orX :: [Parser a] -> Parser a
orX [] _ = Left "No parser matched"
orX (p : ps) s = case p s of
  Left _ -> orX ps s
  Right res -> Right res

many1 :: Parser a -> Parser [a]
many1 p s = case p s of
  Left err -> Left err
  Right (v1, r1) -> case many1' r1 of
    (v2, r2) -> Right (v1:v2, r2)
  where
    many1' s2 = case p s2 of
      Left _ -> ([], s2)
      Right (v2, r2) -> let (vs, r3) = many1' r2 in (v2 : vs, r3)

parseWord :: Parser String
parseWord [] = Left "Cannot find any word in an empty input"
parseWord s = case parseLetter s of
    Left err -> Left err
    Right (v1, r1) -> case parseString v1 r1 of
        Left err -> Left err
        Right (v2, r2) -> Right (v1 ++ v2, r2)

--needed parsers






data Query = 
  AddRequest RequestInfo
  | ListRequests
  | RemoveRequest Int
  | UpdateRequest Int RequestInfo
  | FindRequest String
  deriving (Eq, Show)

  data RequestInfo = RequestInfo request_id request_type request_origin items 
  deriving (Eq, Show)
  type request_id = Int
  data request_type = Drink | Main | Dessert | Snack | Other
  deriving (Eq, Show)
  data request_origin = Table | Waiter | Bar | Kitchen | Delivery | Takeaway | Online
  deriving (Eq, Show)
  type items = [item]
  type item = String
  deriving (Eq, Show)

-- | The instances are needed basically for tests
instance Eq Query where
  (==) _ _= False




instance Show Query where
  show _ = ""



-- | Parses user's input.
-- The function must have tests.
parseQuery :: String -> Either String Query
parseQuery _ = Left "Not implemented 2"

-- <add_request> ::= "add_request " <request>
-- <list_requests> ::= "list_requests "
-- <find_request> ::= "find_request " <request_id>
-- <remove_request> ::= "remove_request " <request_id>
-- <update_request> ::= "update_request " <request>

parseRequestInfo :: Parser RequestInfo
parseRequestInfo input = 
  case parseRequestId input of
    Left err -> Left err
    Right (id, r1) -> case parseSpace r1 of
      Left err -> Left err
      Right (type, r2) -> case parseRequestType r2 of
        Left err -> Left err
        Right (origin, r3) -> case parseSpace r3 of
          Left err -> Left err
          Right (items, r4) -> case parseItems r4 of
            Left err -> Left err
            Right (items, r5) -> Right (RequestInfo id type origin items, r5)

parseRequestType :: Parser request_type
-- <request_type> ::= <string>
parseRequestType s = case parseWord s of
    Left err -> Left err
    Right ("Drink", r) -> Right (Drink, r)
    Right ("Main", r) -> Right (Main, r)
    Right ("Dessert", r) -> Right (Dessert, r)
    Right ("Snack", r) -> Right (Snack, r)
    Right ("Other", r) -> Right (Other, r)
    Right (v, _) -> Left (v ++ " is not a valid request type")

-- <request_id> ::= <int>
parseRequestId :: Parser request_id

parseRequestId s = case parseDigit s of
    Left err -> Left err
    Right (v, r) -> Right (read [v], r)

-- <request_origin> ::= <string>
parseRequestOrigin :: Parser request_origin
parseRequestOrigin s = case parseWord s of
    Left err -> Left err
    Right ("Table", r) -> Right (Table, r)
    Right ("Waiter", r) -> Right (Waiter, r)
    Right ("Bar", r) -> Right (Bar, r)
    Right ("Kitchen", r) -> Right (Kitchen, r)
    Right ("Delivery", r) -> Right (Delivery, r)
    Right ("Takeaway", r) -> Right (Takeaway, r)
    Right ("Online", r) -> Right (Online, r)
    Right (v, _) -> Left (v ++ " is not a valid request origin")

parseItem :: Parser item
parseItem s = case parseWord s of
    Left err -> Left err
    Right (v, r) -> Right (v, r)

parseItems :: Parser items
parseItems = many1 parseItem

-- | An entity which represents your program's state.
-- Currently it has no constructors but you can introduce
-- as many as needed.
data State
  deriving (Eq, Show)




-- | Creates an initial program's state.
-- It is called once when the program starts.
emptyState :: State
emptyState = error "Not implemented 1"



-- | Updates a state according to a query.
-- This allows your program to share the state
-- between repl iterations.
-- Right contains an optional message to print and
-- an updated program's state.
stateTransition :: State -> Query -> Either String (Maybe String, State)
stateTransition _ _ = Left "Not implemented 3"


