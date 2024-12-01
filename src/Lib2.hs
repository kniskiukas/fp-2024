
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
    Items(..)
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
  deriving (Eq, Show)

parseQuery :: String -> Either String Query
parseQuery s = case parse (parseAddRequest <|> parseListRequests <|> parseRemoveRequest <|> parseUpdateRequest <|> parseFindRequest <|> parseRemoveAllRequests) s of
  Left err -> Left err
  Right (result, rest) -> if null rest then Right result else Left ("Unexpected '" ++ rest ++ "' after parsing")

parseAddRequest :: Parser Query
parseAddRequest =  AddRequest <$> (parseString "add_request" *> parseSpace *> parseRequest)

parseListRequests :: Parser Query
parseListRequests = ListRequests <$ parseString "list_requests"

parseRemoveRequest :: Parser Query
parseRemoveRequest = RemoveRequest <$> (parseString "remove_request" *> parseSpace *> parseRequestId)


parseUpdateRequest :: Parser Query
parseUpdateRequest = UpdateRequest <$> (parseString "update_request" *> parseSpace *> parseRequestId) <*> (parseSpace *> parseRequest)

parseFindRequest :: Parser Query
parseFindRequest = FindRequest <$> (parseString "find_request" *> parseSpace *> parseRequestId)

parseRemoveAllRequests :: Parser Query
parseRemoveAllRequests = RemoveAllRequests <$ parseString "remove_all_requests"

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
stateTransition s (AddRequest r) =
  if requestExists r s
    then Left "Request already exists"
    else Right (Just "Request added", State (r : requests s))

stateTransition s ListRequests = Right (Just (show (requests s)), s)

stateTransition s (RemoveRequest i) =
  if not (requestByIdExists i s)
    then Left "Request does not exist"
    else Right (Just "Request removed", State (filter (\r -> requestId r /= i) (requests s)))

stateTransition s RemoveAllRequests =
  if null (requests s)
    then Left "No requests to remove"
    else Right (Just "All requests removed", State [])

stateTransition s (UpdateRequest i r) =
  if not (requestByIdExists i s)
    then Left "Request does not exist"
    else Right (Just "Request updated", State (map (\r2 -> if requestId r2 == i then r else r2) (requests s)))

stateTransition s (FindRequest i) = Right (Just (show (filter (\r -> requestId r == i) (requests s))), s)



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





