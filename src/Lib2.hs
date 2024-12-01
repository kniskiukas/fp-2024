
{-# OPTIONS_GHC -Wno-unused-top-binds -Wname-shadowing #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant lambda" #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}

--TODO
--Make initialization
--rework state transitions
--rework the main parser and divide the parsers into different files


module Lib2
    ( Query(..),
    parseQuery,
    Parser(..),
    State(..),
    Request(..),
    emptyState,
    stateTransition,
    parseAddRequest,
    parseListRequests,
    parseRemoveRequest,
    parseUpdateRequest,
    parseFindRequest,
    parseRemoveAllRequests,
    Items(..),
    parseString,
    query,
    parseLiteral
    ) where

import qualified Data.Char as C
import Control.Applicative

-- newtype Parser a = Parser {
--     runParser :: String -> Either String (a, String)
-- }
newtype Parser a where
  P :: { parse :: String -> Either String (a, String) } -> Parser a



data Query =
  AddRequest Request
  | ListRequests
  | RemoveRequest Int
  | UpdateRequest Int Request
  | FindRequest Int
  | RemoveAllRequests
  | Operation [Query]
  deriving (Eq, Show)

query :: Parser Query
query = parseAddRequest <|> parseListRequests <|> parseRemoveRequest <|> parseUpdateRequest <|> parseFindRequest <|> parseRemoveAllRequests

parseQuery :: String -> Either String Query
parseQuery input = case parse parseTaskList input of
  Right (qs, r) ->
      if null r
        then case qs of
          [q] -> Right q
          _ -> Right (Operation qs)
        else Left ("Unrecognized characters: " ++ r)
  _ -> Left "Failed to parse query: Unknown command"

parseAddRequest :: Parser Query

parseAddRequest = do
  _ <- parseString "add_request"
  _ <- parseSpace
  AddRequest <$> parseRequest

parseListRequests :: Parser Query
parseListRequests = do
  _ <- parseString "list_requests"
  return ListRequests

parseRemoveRequest :: Parser Query
parseRemoveRequest = do
  _ <- parseString "remove_request"
  _ <- parseSpace
  RemoveRequest <$> parseRequestId

parseUpdateRequest :: Parser Query
parseUpdateRequest = do
  _ <- parseString "update_request"
  _ <- parseSpace
  i <- parseRequestId
  _ <- parseComma
  UpdateRequest i <$> parseRequest

parseFindRequest :: Parser Query
parseFindRequest = do
  _ <- parseString "find_request"
  _ <- parseSpace
  FindRequest <$> parseRequestId

parseRemoveAllRequests :: Parser Query
parseRemoveAllRequests = do
  _ <- parseString "remove_all_requests"
-- Variable not in scope:
--   testParseAddRequest :: String -> t_a2nzn[sk:1]
-- ProgressCancelledException
  return RemoveAllRequests

parseTask :: Parser Query
parseTask = parseAddRequest <|> parseListRequests <|> parseRemoveRequest <|> parseUpdateRequest <|> parseFindRequest <|> parseRemoveAllRequests

parseTaskList :: Parser [Query]
parseTaskList = do
  firstQuery <- parseTask
  rest <- optional (parseChar ';' >> parseTaskList)
  return $ case rest of
    Just otherQueries -> firstQuery : otherQueries
    Nothing -> [firstQuery]

parseOperation :: Parser Query
parseOperation = do
  Operation <$> parseTaskList


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

parseSingleItem :: Parser Items
-- parseItem s = case many parseAlphaNum s of
--   Left e -> Left e
--   Right (cs, r) -> Right (Items [cs], r)
parseSingleItem = Items . (:[]) <$> parseAlphaNumString

parseMultipleItems :: Parser Items
parseMultipleItems = do
  i <- parseSingleItem
  _ <- parseComma
  is <- parseMultipleItems
  case (i, is) of
    (Items i', Items is') -> return $ Items (i' ++ is')



parseItems :: Parser Items
parseItems = parseMultipleItems <|> parseSingleItem

parseRequestId :: Parser Int
parseRequestId = read <$> many parseDigit

parseRequest :: Parser Request
parseRequest = Request <$> (parseRequestId <* parseComma) <*> (many parseLetter <* parseComma) <*> (many parseLetter <* parseComma) <*> parseItems

requestByIdExists :: Int -> State -> Bool
requestByIdExists rid s = rid `elem` map requestId (requests s)

requestExists :: Request -> State -> Bool
requestExists r s = r `elem` requests s




-- state stuff

newtype State =
  State {
    requests :: [Request]
  }
 deriving (Eq, Show)

emptyState :: State
emptyState = State []

stateTransition :: State -> Query -> Either String (Maybe String, State)
stateTransition state q = case q of
  AddRequest r -> if requestExists r state
    then Left "Request already exists"
    else Right (Just "Request added", State (r : requests state))
  ListRequests -> Right (Just (show (requests state)), state)
  RemoveRequest i -> if not (requestByIdExists i state)
    then Left "Request does not exist"
    else Right (Just "Request removed", State (filter (\r -> requestId r /= i) (requests state)))
  RemoveAllRequests -> if null (requests state)
    then Left "No requests to remove"
    else Right (Just "All requests removed", State [])
  UpdateRequest i r -> if not (requestByIdExists i state)
    then Left "Request does not exist"
    else Right (Just "Request updated", State (map (\r2 -> if requestId r2 == i then r else r2) (requests state)))
  FindRequest i -> Right (Just (show (filter (\r -> requestId r == i) (requests state))), state)
  Operation qs -> foldl (\acc q' -> acc >>= \(_, st) -> stateTransition st q') (Right (Nothing, state)) qs



parseChar :: Char -> Parser Char
parseChar c = P $ \input -> case input of
  [] -> Left ("Cannot find " ++ [c] ++ " in an empty input")
  (h : t) -> if h == c then Right (c, t) else Left (input ++ " does not start with " ++ [c])

parseSpace :: Parser Char
parseSpace = parseChar ' '

parseComma :: Parser Char
parseComma = parseChar ','

parseDigit :: Parser Char
parseDigit = P $ \input -> case input of
  [] -> Left "Cannot find any digits in an empty input"
  (h : t) -> if C.isDigit h then Right (h, t) else Left (input ++ " does not start with a digit")

parseLetter :: Parser Char
parseLetter = P $ \input -> case input of
  [] -> Left "Cannot find any letter in an empty input"
  (h : t) -> if C.isLetter h then Right (h, t) else Left (input ++ " does not start with a letter")

parseAlphaNum :: Parser Char
parseAlphaNum = parseLetter <|> parseDigit

parseString :: String -> Parser String
parseString = foldr (\ h -> (<*>) ((:) <$> parseChar h)) (pure [])


parseAlphaNumString :: Parser String
parseAlphaNumString = many parseAlphaNum



parseManyLetters :: Parser String
parseManyLetters = many parseLetter

sat :: (Char -> Bool) -> Parser Char
sat p = P $ \case
  [] -> Left "Empty String"
  s@(x : xs) -> if p x then Right (x, xs) else Left $ "Could not recognize: " ++ s

char :: Char -> Parser Char
char c = sat (== c)

parseChar' :: Char -> Parser Char
parseChar' = char

parseLiteral :: String -> Parser String
parseLiteral [] = return []
parseLiteral (x : xs) = do
  _ <- parseChar' x
  parseLiteral xs

-- needed helpers
-- class Functor f where
--   fmap :: (a -> b) -> f a -> f b
--   pure :: a -> f a
--   (<*>) :: f (a -> b) -> f a -> f b
--   (>>=) :: f a -> (a -> f b) -> f b
--   (>>) :: f a -> f b -> f b

instance Functor Parser where
  fmap :: (a -> b) -> Parser a -> Parser b
  fmap f p = do
    f <$> p

instance Applicative Parser where
  pure :: a -> Parser a
  pure x = P $ \str -> Right (x, str)
  (<*>) :: Parser (a -> b) -> Parser a -> Parser b
  pf <*> pa = do
    f <- pf
    f <$> pa

instance Alternative Parser where
  empty :: Parser a
  empty = P $ \_ -> Left "Failed to parse"
  (<|>) :: Parser a -> Parser a -> Parser a
  (<|>) p1 p2 = P $ \str -> case parse p1 str of
    Right (v, r) -> Right (v, r)
    Left _ -> parse p2 str

instance Monad Parser where
  (>>=) :: Parser a -> (a -> Parser b) -> Parser b
  (>>=) pa f = P $ \str -> case parse pa str of
    Left e -> Left e
    Right (a, r) -> parse (f a) r





