{-# LANGUAGE InstanceSigs #-}

module Lib3
    ( stateTransition,
    StorageOp (..),
    storageOpLoop,
    parseCommand,
    parseStatements,
    marshallState,
    renderStatements,
    Statements(..)
    ) where

import Control.Concurrent (Chan, newChan, readChan, writeChan)
import Control.Concurrent.STM (TVar, readTVar, readTVarIO, writeTVar)
import Control.Monad.STM
import qualified Lib2
import Lib2 (Parser, parse, State (State))
-- import Data.Maybe
import Control.Monad (forever)
import System.Directory (doesFileExist)
import Control.Applicative ((<|>), many)
-- (<*), (*>), (<$>), (<*>)

data StorageOp = Save String (Chan ()) | Load (Chan String)

-- | This function is started from main
-- in a dedicated thread. It must be used to control
-- file access in a synchronized manner: read requests
-- from chan, do the IO operations needed and respond
-- to a channel provided in a request.
-- Modify as needed.
storageOpLoop :: Chan StorageOp -> IO ()
storageOpLoop opChan = forever $ do
  op <- readChan opChan
  case op of
    Save s chan -> do
      writeFile fileName s
      writeChan chan ()
    Load chan -> do
      exists <- doesFileExist fileName
      if exists
        then do
          s' <- readFile fileName
          writeChan chan s'
        else writeChan chan ""

fileName :: String
fileName = "state.txt"

data Statements = Batch [Lib2.Query] |
               Single Lib2.Query
               deriving (Eq)

instance Show Statements where
  show :: Statements -> String
  show (Single q) = renderQuery q
  show (Batch qs) = "BEGIN\n" ++ concatMap ((++ ";\n") . show) qs ++ "END\n"





data Command = StatementCommand Statements |
               LoadCommand |
               SaveCommand
               deriving (Eq)

-- | Parses user's input.
parseCommand :: String -> Either String (Command, String)
parseCommand = parse (StatementCommand <$> statements <|> parseLoad <|> parseSave)

-- | Parses Statement.
-- Must be used in parseCommand.
-- Reuse Lib2 as much as you can.
-- You can change Lib2.parseQuery signature if needed.
parseLoad :: Parser Command
parseLoad = do
  _ <- Lib2.parseWord "load"
  return LoadCommand

parseSave :: Parser Command
parseSave = do
  _ <- Lib2.parseWord "save"
  return SaveCommand

parseStatements :: String -> Either String (Statements, String)
parseStatements = parse statements

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


renderQuery :: Lib2.Query -> String
renderQuery (Lib2.AddRequest (Lib2.Request n t o (Lib2.Items i))) = "add, " ++ show n ++ "," ++ t ++ "," ++ o ++ "," ++ concat i
renderQuery Lib2.ListRequests = "list"
renderQuery (Lib2.RemoveRequest i) = "remove," ++ show i
renderQuery (Lib2.UpdateRequest i (Lib2.Request n t o (Lib2.Items it))) = "update," ++ show i ++ "," ++ show n ++ "," ++ t ++ "," ++ o ++ "," ++ concat it
renderQuery (Lib2.FindRequest i) = "find," ++ show i
renderQuery Lib2.RemoveAllRequests = "removeall"
renderQuery (Lib2.Operation _) = "operation"

renderStatements :: Statements -> String
renderStatements (Single q) = renderQuery q
renderStatements (Batch qs) = "BEGIN\n" ++ concatMap ((++ ";\n") . renderQuery) qs ++ "END\n"

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
stateTransition stateVar SaveCommand ioChan = do
  stateVar' <- readTVarIO stateVar
  chan <- newChan :: IO (Chan ())
  writeChan ioChan (Save (renderStatements $ marshallState stateVar') chan)
  readChan chan
  return $ Right $ Just "State saved"

stateTransition s LoadCommand ioChan = do
  chan <- newChan :: IO (Chan String)
  writeChan ioChan (Load chan)
  qs <- readChan chan
  if null qs
    then return (Left "No state file found")
    else case parseStatements qs of
      Left e -> do
        return $ Left $ "Failed to load from file:\n" ++ e
      Right (qs', _) -> stateTransition s (StatementCommand qs') ioChan
stateTransition stateVar (StatementCommand s) _ = atomically $ atomicStatements stateVar s


transitionThroughList :: Lib2.State -> [Lib2.Query] -> Either String (Maybe String, Lib2.State)
transitionThroughList _ [] = Left "Empty list of queries"
transitionThroughList state (q:qs) = case Lib2.stateTransition state q of
  Left e -> Left e
  Right (msg, newState) ->
    if null qs
      then Right (msg, newState)
      else case transitionThroughList newState qs of
        Left e -> Left e
        Right (msg', newState') -> Right ((\x y -> x ++ "\n" ++ y) <$> msg <*> msg', newState')


atomicStatements :: TVar Lib2.State -> Statements -> STM (Either String (Maybe String))
atomicStatements s (Batch qs) = do
  s' <- readTVar s
  case transitionThroughList s' qs of
    Left e -> return $ Left e
    Right (msg, ns) -> do
      writeTVar s ns
      return $ Right msg
atomicStatements s (Single q) = do
  s' <- readTVar s
  case Lib2.stateTransition s' q of
    Left e -> return $ Left e
    Right (msg, ns) -> do
      writeTVar s ns
      return $ Right msg

statements :: Parser Statements
statements =
  ( do
      _ <- Lib2.parseLiteral "BEGIN\n"
      q <-
        many
          ( do
              q <- Lib2.query
              _ <- Lib2.parseLiteral ";\n"
              return q
          )
      _ <- Lib2.parseLiteral "END\n"
      return $ Batch q
  )
    <|> (Single <$> Lib2.query)