
{-# OPTIONS_GHC -Wno-unused-top-binds -Wname-shadowing #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant lambda" #-}
{-# LANGUAGE InstanceSigs #-}


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
import Control.Applicative

newtype Parser a = Parser {
    runParser :: String -> Either String (a, String)
}

parseItem :: Parser Items
parseItem = Items . (:[]) <$> parseAlphaNumString


parseItems :: Parser Items
parseItems = Items <$> many (parseItem <* parseComma)

parseRequestId :: Parser Int
-- parseRequestId input =
--   case many parseDigit input of
--     Left err -> Left err
--     Right (digits, rest) -> Right (read digits, rest)
parseRequestId = read <$> many parseDigit

parseRequest :: Parser Request
-- parseRequest = and7 (\n _ t _ o _ i -> Request n t o i) parseRequestId parseComma (many parseLetter) parseComma (many parseLetter) parseComma parseItems
parseRequest = Request <$> (parseRequestId <* parseComma) <*> (many parseLetter <* parseComma) <*> (many parseLetter <* parseComma) <*> (parseItems <|> parseItem)

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

parseQuery s = case runParser (parseAddRequest <|> parseListRequests <|> parseRemoveRequest <|> parseUpdateRequest <|> parseFindRequest <|> parseRemoveAllRequests) s of
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

requestByIdExists :: Int -> State -> Bool
requestByIdExists rid s = rid `elem` map requestId (requests s)

requestExists :: Request -> State -> Bool
requestExists r s = r `elem` requests s

--basic parsers

parseChar :: Char -> Parser Char
parseChar c = Parser $ \input -> case input of
  [] -> Left ("Cannot find " ++ [c] ++ " in an empty input")
  (h : t) -> if h == c then Right (c, t) else Left (input ++ " does not start with " ++ [c])

parseSpace :: Parser Char
parseSpace = parseChar ' '

parseComma :: Parser Char
parseComma = parseChar ','

parseDigit :: Parser Char
parseDigit = Parser $ \input -> case input of
  [] -> Left "Cannot find any digits in an empty input"
  (h : t) -> if C.isDigit h then Right (h, t) else Left (input ++ " does not start with a digit")

parseLetter :: Parser Char
parseLetter = Parser $ \input -> case input of
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

-- needed helpers
-- class Functor f where
--   fmap :: (a -> b) -> f a -> f b
--   pure :: a -> f a
--   (<*>) :: f (a -> b) -> f a -> f b
--   (>>=) :: f a -> (a -> f b) -> f b
--   (>>) :: f a -> f b -> f b

instance Functor Parser where
  fmap :: (a -> b) -> Parser a -> Parser b
  fmap f p = Parser $ \s -> case runParser p s of
    Left err -> Left err
    Right (result, rest) -> Right (f result, rest)

instance Applicative Parser where
  pure :: a -> Parser a
  pure x = Parser $ \s -> Right (x, s)
  (<*>) :: Parser (a -> b) -> Parser a -> Parser b
  pf <*> px = Parser $ \s -> case runParser pf s of
    Left err -> Left err
    Right (f, rest) -> case runParser px rest of
      Left err -> Left err
      Right (x, rest') -> Right (f x, rest')

instance Alternative Parser where
  empty :: Parser a
  empty = Parser $ \_ -> Left "empty"
  (<|>) :: Parser a -> Parser a -> Parser a
  a <|> b = Parser $ \s -> case runParser a s of
    Left _ -> runParser b s
    Right r -> Right r
  -- many :: Parser a -> Parser [a]
  -- many p = many' p []
  --   where
  --     many' p' acc = \input ->
  --       case p' input of
  --         Left _ -> Right (acc, input)
  --         Right (v, r) -> many' p' (acc ++ [v]) r



