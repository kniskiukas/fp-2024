
{-# OPTIONS_GHC -Wno-unused-top-binds -Wname-shadowing #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant lambda" #-}


module Lib2
    ( Query(..),
    parseQuery,
    State(..),
    Request(..),
    emptyState,
    stateTransition,
    parseAddRequest,
    parseListRequests,
    parseRemoveRequest,
    parseUpdateRequest,
    parseFindRequest,
    Items(..)
    ) where

import qualified Data.Char as C
import qualified Data.List as L

type Parser a = String -> Either String (a, String)

parseItem :: Parser Items
parseItem s = case many parseAlphaNum s of
  Left e -> Left e
  Right (cs, r) -> Right (Items [cs], r)

parseItems :: Parser Items
parseItems = or2  (and3 (\(Items i) _ (Items it) -> Items (head i : it)) parseItem parseComma parseItems) parseItem

parseRequestId :: Parser Int
parseRequestId input =
  case many parseDigit input of
    Left err -> Left err
    Right (digits, rest) -> Right (read digits, rest)

parseRequest :: Parser Request
parseRequest = and7 (\n _ t _ o _ i -> Request n t o i) parseRequestId parseComma (many parseLetter) parseComma (many parseLetter) parseComma parseItems

data Query =
  AddRequest Request
  | ListRequests
  | RemoveRequest Int
  | UpdateRequest Int Request
  | FindRequest Int
  | RemoveAllRequests
  deriving (Eq, Show)

data Request = Request {
  requestId :: Int,
  requestType :: String,
  requestOrigin :: String,
  items :: Items
 }
 deriving (Eq, Show)
--type Items = [String]
newtype Items = Items [String]
  deriving (Eq, Show)

parseQuery :: String -> Either String Query
parseQuery s = case or2 parseAddRequest (or2 parseListRequests (or2 parseRemoveRequest (or2 parseUpdateRequest (or2 parseFindRequest parseRemoveAllRequests))))
  s of
    Left _ -> Left ("Unexpected command: " ++ s)
    Right (v, r) -> if null r then Right v else Left ("Unexpected '" ++ r ++ "' after parsing")

parseAddRequest :: Parser Query
parseAddRequest = and3 (\_ _ r -> AddRequest r) (parseString "add_request") parseSpace parseRequest

parseListRequests :: Parser Query
parseListRequests s = case parseString "list_requests" s of
  Left err -> Left err
  Right (_, r) -> Right (ListRequests, r)

parseRemoveRequest :: Parser Query
parseRemoveRequest = and3 (\_ _ n -> RemoveRequest n) (parseString "remove_request") parseSpace parseRequestId

parseUpdateRequest :: Parser Query
parseUpdateRequest = and5 (\_ _ n _ r -> UpdateRequest n r) (parseString "update_request") parseSpace parseRequestId parseSpace parseRequest

parseFindRequest :: Parser Query
parseFindRequest = and3 (\_ _ n -> FindRequest n) (parseString "find_request") parseSpace parseRequestId

parseRemoveAllRequests :: Parser Query
parseRemoveAllRequests s = case parseString "remove_all_requests" s of
  Left err -> Left err
  Right (_, r) -> Right (RemoveAllRequests, r)

newtype State =
  State {
    requests :: [Request]
  }
 deriving (Eq, Show)

emptyState :: State
emptyState = State []

stateTransition :: State -> Query -> Either String (Maybe String, State)
stateTransition s (AddRequest r) =
  if requestExists r s
    then Left "Request already exists"
    else Right (Just "Request added", State (r : requests s))

stateTransition s ListRequests = Right (Just (show (requests s)), s)

stateTransition s (RemoveRequest i) =
  if not (requestByIdExists i s)
    then Left "Request does not exist"
    else Right (Just "Request removed", State (filter (\r -> requestId r /= i) (requests s)))

stateTransition s (RemoveAllRequests) = 
  if null (requests s) 
    then Left "No requests to remove" 
    else Right (Just "All requests removed", State [])

stateTransition s (UpdateRequest i r) =
  if not (requestByIdExists i s)
    then Left "Request does not exist"
    else Right (Just "Request updated", State (map (\r2 -> if requestId r2 == i then r else r2) (requests s)))

stateTransition s (FindRequest i) = Right (Just (show (filter (\r -> requestId r == i) (requests s))), s)

requestByIdExists :: Int -> State -> Bool
requestByIdExists rid s = rid `elem` map requestId (requests s)

requestExists :: Request -> State -> Bool
requestExists r s = r `elem` requests s

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

parseLetter :: Parser Char
parseLetter [] = Left "Cannot find any letter in an empty input"
parseLetter s@(h:t) = if C.isLetter h then Right (h, t) else Left (s ++ " does not start with a letter")

parseAlphaNum :: Parser Char
parseAlphaNum [] = Left "Cannot find any alphanumeric character in an empty input"
parseAlphaNum s@(h:t) = if C.isAlphaNum h then Right (h, t) else Left (s ++ " does not start with an alphanumeric character")

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

many :: Parser a -> Parser [a]
many p = many' p []
    where
        many' p' acc = \input ->
            case p' input of
                Left _ -> Right (acc, input)
                Right (v, r) -> many' p' (acc ++ [v]) r

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

and5 :: (a -> b -> c -> d -> e -> f) -> Parser a -> Parser b -> Parser c -> Parser d -> Parser e -> Parser f
and5 f p1 p2 p3 p4 p5 s = case p1 s of
  Left err -> Left err
  Right (v1, r1) -> case p2 r1 of
    Left err -> Left err
    Right (v2, r2) -> case p3 r2 of
      Left err -> Left err
      Right (v3, r3) -> case p4 r3 of
        Left err -> Left err
        Right (v4, r4) -> case p5 r4 of
          Left err -> Left err
          Right (v5, r5) -> Right (f v1 v2 v3 v4 v5, r5)

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


