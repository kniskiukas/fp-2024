{-# LANGUAGE InstanceSigs #-}
module Lib3
    ( stateTransition,
    StorageOp (..),
    storageOpLoop,
    parseCommand,
    parseStatements,
    marshallState,
    renderStatements
    ) where

import Control.Concurrent (Chan, newChan, readChan, writeChan)
import Control.Concurrent.STM (STM, TVar, atomically, readTVar, readTVarIO, writeTVar)
import qualified Lib2
import Lib2 (Parser, parseString, runParser)
import System.Directory (doesFileExist)
import Control.Applicative ((<|>), many, (<*), (*>), (<$>), (<*>))

data StorageOp = Save String (Chan ()) | Load (Chan String)

-- | This function is started from main
-- in a dedicated thread. It must be used to control
-- file access in a synchronized manner: read requests
-- from chan, do the IO operations needed and respond
-- to a channel provided in a request.
-- Modify as needed.
storageOpLoop :: Chan StorageOp -> IO ()
storageOpLoop storageOpChannel = do
  op <- readChan storageOpChannel
  case storageOp of
    Save string responseChannel -> writeFile fileName string >> writeChan responseChannel ()
    Load responseChannel -> do
      fileExists <- doesFileExist fileName
      if fileExists
        then Just <$> readFile fileName >>= writeChan responseChannel
        else writeChan responseChannel Nothing

fileName :: String
fileName = "state.txt"

data Statements = Batch [Lib2.Query] |
               Single Lib2.Query
               deriving (Show, Eq)

instance Show Statements where
  show :: Statements -> String
  show (Single q) = renderQuery q
  show (Batch qs) = unlines $ map renderQuery qs


renderQuery :: Lib2.Query -> String
renderQuery (Lib2.AddRequest (Lib2.Request n t o i)) = show n ++ "," ++ t ++ "," ++ o ++ "," ++ i
renderQuery Lib2.ListRequests = "list"
renderQuery (Lib2.RemoveRequest i) = "remove " ++ show i
renderQuery (Lib2.UpdateRequest i (Lib2.Request n t o it)) = "update " ++ show i ++ ": " ++ n ++ "," ++ t ++ "," ++ o ++ "," ++ it
renderQuery (Lib2.FindRequest i) = "find " ++ show i
renderQuery Lib2.RemoveAllRequests = "removeall"


data Command = StatementCommand Statements |
               LoadCommand |
               SaveCommand
               deriving (Show, Eq)

-- | Parses user's input.
parseCommand :: String -> Either String (Command, String)
parseCommand = runParser command

-- | Parses Statement.
-- Must be used in parseCommand.
-- Reuse Lib2 as much as you can.
-- You can change Lib2.parseQuery signature if needed.
parseStatements :: String -> Either String (Statements, String)
parseStatements  = runParser statements

-- | Converts program's state into Statements
-- (probably a batch, but might be a single query)
marshallState :: Lib2.State -> Statements
marshallState (Lib2.State requests) = Batch (map Lib2.AddRequest requests)

-- | Renders Statements into a String which
-- can be parsed back into Statements by parseStatements
-- function. The String returned by this function must be used
-- as persist program's state in a file. 
-- Must have a property test
-- for all s: parseStatements (renderStatements s) == Right(s, "")
renderStatements :: Statements -> String
renderStatements = show

-- | Updates a state according to a command.
-- Performs file IO via ioChan if needed.
-- This allows your program to share the state
-- between repl iterations, save the state to a file,
-- load the state from the file so the state is preserved
-- between program restarts.
-- Keep IO as small as possible.
-- State update must be executed atomically (STM).
-- Right contains an optional message to print, updated state
-- is stored in transactinal variable
stateTransition :: TVar Lib2.State -> Command -> Chan StorageOp ->
                   IO (Either String (Maybe String))
stateTransition state SaveCommand ioChan = do
  currentState <- readTVarIO state
  responseChan <- newChan
  writeChan ioChan (Save (renderStatements $ marshallState currentState) responseChan)
  readChan responseChan
  return $ Right (Just "State saved")

stateTransition state LoadCommand ioChan = do
  atomically $ writeTVar state Lib2.emptyState
  responseChan <- newChan
  writeChan ioChan (Load responseChan)
  maybeContent <- readChan responseChan
  case maybeContent of 
    Nothing -> return $ Right (Just "No state to load")
    Just content -> case parseStatements content of
      Left err -> return $ Left $ "Error loading state: \n" ++ err
      Right (statements, _) -> stateTransition state (StatementCommand statements) ioChan

stateTransition state (StatementCommand statementList) _ =
  atomically $ atomicStatements state statementList

transitionThroughList :: Lib2.State -> [Lib2.Query] -> Either String (Maybe String, Lib2.State)
transitionThroughList _ [] = Left "No queries to execute"
transitionThroughList state (q:qs) = 
  case Lib2.stateTransition state q of
    Left err -> Left err
    Right (msg, newState) -> 
      if null qs
        then Right (msg, newState)
        else case transitionThroughList newState qs of
          Left err -> Left err
          Right (msg2, finalState) -> Right (combineMessages msg msg2, finalState)

combineMessages :: Maybe String -> Maybe String -> Maybe String
combineMessages Nothing Nothing = Nothing
combineMessages (Just msg1) Nothing = Just msg1
combineMessages Nothing (Just msg2) = Just msg2
combineMessages (Just msg1) (Just msg2) = Just (msg1 ++ "\n" ++ msg2)

atomicStatements :: TVar Lib2.State -> Statements -> STM (Either String (Maybe String))
atomicStatements state statement = do
  currentState <- readTVar state
  case statement of
    Single q -> 
      case Lib2.stateTransition currentState q of
        Left err -> return $ Left err
        Right (msg, newState) -> writeTVar state newState >> return (Right (msg))
    Batch qs -> 
      case transitionThroughList currentState qs of
        Left err -> return $ Left err
        Right (msg, newState) -> writeTVar state newState >> return (Right msg)
    

statements :: Parser Statements
statements =
  ( do
      _ <- parseString "begin\n"
      batch <-
        many
          ( do
              query <- Lib2.query
              _ <- parseString ";\n"
              return query
          )
      _ <- parseString "end\n"
      return $ Batch batch
  )
    <|> (Single <$> Lib2.query)

loadParser :: Parser Command
loadParser = do
  _ <- parseString "load"
  return LoadCommand

saveParser :: Parser Command
saveParser = do
  _ <- parseString "save"
  return SaveCommand

command :: Parser Command
command = StatementCommand <$> statements <|> loadParser <|> saveParser