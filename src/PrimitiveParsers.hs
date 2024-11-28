

{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# HLINT ignore "Use <$>" #-}
{-# HLINT ignore "Use lambda-case" #-}
{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

import Control.Applicative (Alternative (empty), (<|>))
import qualified Data.Char as C

-- newtype Parser a = Parser { runParser :: String -> Either String (a, String) }

newtype Parser a where
  P :: { parse :: String -> Either String (a, String) } -> Parser a

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


string :: String -> Parser String
string [] = return []
string (c : cs) = do
  c' <- char c
  cs' <- string cs
  return (c' : cs')

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
  fmap f p = P $ \s -> case parse p s of
    Left err -> Left err
    Right (result, rest) -> Right (f result, rest)

instance Applicative Parser where
  pure :: a -> Parser a
  pure x = P $ \s -> Right (x, s)
  (<*>) :: Parser (a -> b) -> Parser a -> Parser b
  pf <*> px = P $ \s -> case parse pf s of
    Left err -> Left err
    Right (f, rest) -> case parse px rest of
      Left err -> Left err
      Right (x, rest') -> Right (f x, rest')

instance Alternative Parser where
  empty :: Parser a
  empty = P $ \_ -> Left "empty"
  (<|>) :: Parser a -> Parser a -> Parser a
  a <|> b = P $ \s -> case parse a s of
    Left _ -> parse b s
    Right r -> Right r




