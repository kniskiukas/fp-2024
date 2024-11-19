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

import Control.Concurrent ( Chan )
import Control.Concurrent.STM(STM, TVar)
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
renderQuery (Lib2.RemoveRequest i) = "remove," ++ show i
renderQuery (Lib2.UpdateRequest i (Lib2.Request n t o it)) = "update," ++ show i ++ "," ++ n ++ "," ++ t ++ "," ++ o ++ "," ++ it
renderQuery (Lib2.FindRequest i) = "find," ++ show i
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
stateTransition _ _ ioChan = return $ Left "Not implemented 6"


statements :: Parser Statements
statements = 
  ( do
    batch <- 
      many
        (do
          q <- Lib2.parseQuery
          _ <- parseString ";\n"
          return q
        )
      _ <- parseString "end\n"
      return $ Batch batch
) <|> (Single <$> Lib2.parseQuery)

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