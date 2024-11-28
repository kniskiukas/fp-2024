
{-# OPTIONS_GHC -Wno-unused-top-binds -Wname-shadowing #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant lambda" #-}
{-# LANGUAGE InstanceSigs #-}

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
    parseString,
    Items(..)
    ) where

import qualified Data.Char as C
import Control.Applicative



-- type Parser a = String -> Either String (a, String)

newtype Parser a = Parser {
    runParser :: String -> Either String (a, String)
}

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



-- parseItems :: Parser Items
-- parseItems = or2  (and3 (\(Items i) _ (Items it) -> Items (head i : it)) parseItem parseComma parseItems) parseItem

-- parseItems = Items <$> many (parseAlphaNumString <* parseComma)

-- parseItems = do (
--  item <- parseItem
--  parseComma
--  items <- parseItems
--  return item:items
-- )
-- <|>
-- do (
--  item <- parseItem
--  return item
-- )
-- parseItems :: Parser Items
-- parseItems = do (
--   i <- parseItem
--   it <- parseItems
--   return (Items (unItems item ++ unItems items))
--   )
--   <|>
--   do (
--     item <- parseItem
--     return item
--   )
--   where (
--     unItems (Items xs) = xs
-- )

-- parseItems :: Parser Items
-- parseItems = parseMultipleItems <|> parseItem
--   where
--     parseMultipleItems = parseItem >>= \firstItem ->
--                          parseComma *> 
--                          parseItems >>= \restItems ->
--                          return $ Items (unItems firstItem ++ unItems restItems)
--     unItems (Items xs) = xs

parseRequestId :: Parser Int
-- parseRequestId input =
--   case many parseDigit input of
--     Left err -> Left err
--     Right (digits, rest) -> Right (read digits, rest)
parseRequestId = read <$> many parseDigit

parseRequest :: Parser Request
-- parseRequest = and7 (\n _ t _ o _ i -> Request n t o i) parseRequestId parseComma (many parseLetter) parseComma (many parseLetter) parseComma parseItems
parseRequest = Request <$> (parseRequestId <* parseComma) <*> (many parseLetter <* parseComma) <*> (many parseLetter <* parseComma) <*> parseItems

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






-- query parsers

parseQuery :: String -> Either String Query
-- parseQuery s = case or2 parseAddRequest (or2 parseListRequests (or2 parseRemoveRequest (or2 parseUpdateRequest (or2 parseFindRequest parseRemoveAllRequests))))
--   s of
--     Left _ -> Left ("Unexpected command: " ++ s)
--     Right (v, r) -> if null r then Right v else Left ("Unexpected '" ++ r ++ "' after parsing")
parseQuery s = case runParser (parseAddRequest <|> parseListRequests <|> parseRemoveRequest <|> parseUpdateRequest <|> parseFindRequest <|> parseRemoveAllRequests) s of
  Left err -> Left err
  Right (result, rest) -> if null rest then Right result else Left ("Unexpected '" ++ rest ++ "' after parsing")

parseAddRequest :: Parser Query
-- parseAddRequest = and3 (\_ _ r -> AddRequest r) (parseString "add_request") parseSpace parseRequest

parseAddRequest =  AddRequest <$> (parseString "add_request" *> parseSpace *> parseRequest)

-- >>> runParser parseRequest "1,abc,def,ghi,jkl,mno,pqr"
-- Right (Request {requestId = 1, requestType = "abc", requestOrigin = "def", items = Items ["ghi","jkl","mno","pqr"]},"")
-- >>> runParser parseRequest "1,abc,def,ghi"
-- Right (Request {requestId = 1, requestType = "abc", requestOrigin = "def", items = Items ["ghi"]},"")

parseListRequests :: Parser Query
-- parseListRequests s = case parseString "list_requests" s of
--   Left err -> Left err
--   Right (_, r) -> Right (ListRequests, r)

parseListRequests = ListRequests <$ parseString "list_requests"

parseRemoveRequest :: Parser Query
parseRemoveRequest = RemoveRequest <$> (parseString "remove_request" *> parseSpace *> parseRequestId)


parseUpdateRequest :: Parser Query
-- parseUpdateRequest = and5 (\_ _ n _ r -> UpdateRequest n r) (parseString "update_request") parseSpace parseRequestId parseSpace parseRequest
parseUpdateRequest = UpdateRequest <$> (parseString "update_request" *> parseSpace *> parseRequestId) <*> (parseSpace *> parseRequest)

parseFindRequest :: Parser Query
-- parseFindRequest = and3 (\_ _ n -> FindRequest n) (parseString "find_request") parseSpace parseRequestId
parseFindRequest = FindRequest <$> (parseString "find_request" *> parseSpace *> parseRequestId)

parseRemoveAllRequests :: Parser Query
-- parseRemoveAllRequests s = case parseString "remove_all_requests" s of
--   Left err -> Left err
--   Right (_, r) -> Right (RemoveAllRequests, r)

parseRemoveAllRequests = RemoveAllRequests <$ parseString "remove_all_requests"







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
-- parseDigit [] = Left "Cannot find any digits in an empty input"
-- parseDigit s@(h : t) = if C.isDigit h then Right (h, t) else Left (s ++ " does not start with a digit")
parseDigit = Parser $ \input -> case input of
  [] -> Left "Cannot find any digits in an empty input"
  (h : t) -> if C.isDigit h then Right (h, t) else Left (input ++ " does not start with a digit")

parseLetter :: Parser Char
parseLetter = Parser $ \input -> case input of
  [] -> Left "Cannot find any letter in an empty input"
  (h : t) -> if C.isLetter h then Right (h, t) else Left (input ++ " does not start with a letter")

parseAlphaNum :: Parser Char
-- parseAlphaNum [] = Left "Cannot find any alphanumeric character in an empty input"
-- parseAlphaNum s@(h:t) = if C.isAlphaNum h then Right (h, t) else Left (s ++ " does not start with an alphanumeric character")
parseAlphaNum = parseLetter <|> parseDigit

parseString :: String -> Parser String
parseString = foldr (\ h -> (<*>) ((:) <$> parseChar h)) (pure [])
-- parseString [] = pure []
-- parseString (h : t) = (:) <$> parseChar h <*> parseString t

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
instance Monad Parser where
    (>>=) :: Parser a -> (a -> Parser b) -> Parser b
    ma >>= mf = Parser $ \input ->
        case runParser ma input of
            Left e1 -> Left e1
            Right (a, r1) -> case runParser (mf a) r1 of
                                Left e2 -> Left e2
                                Right (b, r2) -> Right (b, r2)
