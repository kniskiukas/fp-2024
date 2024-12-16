module Main (main) where

import Control.Concurrent (Chan, forkIO, killThread, newChan)
import Control.Lens
import Control.Monad.Free (Free (..), liftF)
import Data.Maybe (fromMaybe)
import Data.String.Conversions
import GHC.Conc (TVar, newTVar, newTVarIO, readTVar, readTVarIO)
import Network.Wreq
import qualified Lib2
import qualified Control.Applicative as Lib2
import Data.ByteString (putStr)
import qualified Lib3

-- data Query =
--   AddRequest Request
--   | ListRequests
--   | RemoveRequest Int
--   | UpdateRequest Int Request
--   | FindRequest Int
--   | RemoveAllRequests
--   | Operation [Query]
--   deriving (Eq, Show)

data MyDomainAlgebra a =
  AddRequest Request a
  | ListRequests ([Request] -> a)
  | RemoveRequest Int a
  | UpdateRequest Int Request a
  | FindRequest Int (Maybe Request -> a)
  | RemoveAllRequests a
  | Save a
  | Load a
  deriving (Functor)

type RestaurantProgram = Free MyDomainAlgebra

addRequest :: Request -> RestaurantProgram ()
addRequest req = liftF $ AddRequest req ()

listRequests :: RestaurantProgram [Request]
listRequests = liftF $ ListRequests

removeRequest :: Int -> RestaurantProgram ()
removeRequest id = liftF $ RemoveRequest id ()

updateRequest :: Int -> Request -> RestaurantProgram ()
updateRequest id req = liftF $ UpdateRequest id req ()

findRequest :: Int -> RestaurantProgram (Maybe Request)
findRequest id = liftF $ FindRequest id

removeAllRequests :: RestaurantProgram ()
removeAllRequests = liftF $ RemoveAllRequests ()

save :: RestaurantProgram ()
save = liftF $ Save ()

load :: RestaurantProgram ()
load = liftF $ Load ()

interpretorSingleRequest :: VHSRentalProgram a -> IO a
interpretorSingleRequest (Pure a) = return a
interpretorSingleRequest (Free step) = do
  next <- runStep step
  interpretorSingleRequest next
    where
    runStep :: MyDomainAlgebra a -> IO a
    runStep (AddRequest req next) = sendSingleStatement (Lib2.AddRequest req) >> return next
    runStep (ListRequests next) = sendSingleStatement Lib2.ListRequests >>= return . next
    runStep (RemoveRequest id next) = sendSingleStatement (Lib2.RemoveRequest id) >> return next
    runStep (UpdateRequest id req next) = sendSingleStatement (Lib2.UpdateRequest id req) >> return next
    runStep (FindRequest id next) = sendSingleStatement (Lib2.FindRequest id) >>= return . next
    runStep (RemoveAllRequests next) = sendSingleStatement Lib2.RemoveAllRequests >> return next
    runStep (Operation queries next) = sendMultipleStatements queries >> return next
    runStep (Save next) = postAsString "save" >> return next
    runStep (Load next) = postAsString "load" >> return next

interpretWithBatching' :: RestaurantProgram a -> [Lib2.Query] -> IO a
interpretWithBatching' (Pure a) batch = dumpBatch batch >> return a
interpretWithBatching' (Free step) batch = do
  case step of
    AddRequest req next -> interpretWithBatching' next $ batch ++ [Lib2.AddRequest req]
    ListRequests next -> interpretWithBatching' next $ batch ++ [Lib2.ListRequests]
    RemoveRequest id next -> interpretWithBatching' next $ batch ++ [Lib2.RemoveRequest id]
    UpdateRequest id req next -> interpretWithBatching' next $ batch ++ [Lib2.UpdateRequest id req]
    FindRequest id next -> interpretWithBatching' next $ batch ++ [Lib2.FindRequest id]
    RemoveAllRequests next -> interpretWithBatching' next $ batch ++ [Lib2.RemoveAllRequests]
    Save next -> dumpBatch batch >> postAsString "save" >> interpretWithBatching' next []
    Load next -> dumpBatch batch >> postAsString "load" >> interpretWithBatching' next []

interpretWithBatching :: RestaurantProgram a -> IO a
interpretWithBatching prog = interpretWithBatching' prog []

dumpBatch :: [Lib2.Query] -> IO (Maybe String)
dumpBatch [] = return Nothing
dumpBatch [single] = Just <$> sendSingleStatement single
dumpBatch batch = Just <$> sendAsBatch batch

sendAsBatch :: [Lib2.Query] -> IO String
sendAsBatch = postAsString . Lib3.renderStatements . Lib3.Batch

sendSingleStatement :: Lib2.Query -> IO String
sendSingleStatement = postAsString . Lib3.renderStatements . Lib3.Single

postAsString :: String -> IO String
postAsString s = do
  let rawRequest = cs s :: ByteString
  putStrLn $ "Sending request:\n" ++ cs rawRequest
  resp <- post "http://localhost:3000" rawRequest
  return $ cs $ resp ^. responseBody

testInterpretator :: RestaurantProgram a -> IO a
testInterpretator p = do
  state <- newTVarIO Lib2.emptyState
  chan <- newChan :: IO (Chan Lib3.StorageOp)
  initialState <- readTVarIO state
  putStrLn $ "Initial state:\n" ++ show initialState ++ "\n"
  _ <- forkIO $ Lib3.storageOpLoop chan
  testInterpretator' state chan p
  where
    testInterpretator' :: TVar Lib2.State -> Chan Lib3.StorageOp -> RestaurantProgram a -> IO a
    testInterpretator' _ _ (Pure a) = return a
    testInterpretator' state chan (Free step) = do
      next <- runStep state chan step
      putStrLn "State after:"
      newState <- readTVarIO state
      putStrLn $ show newState ++ "\n"
      testInterpretator' state chan next
    runStep :: TVar Lib2.State -> Chan Lib3.StorageOp -> MyDomainAlgebra a -> IO a
    runStep state chan (AddRequest req next) = transitionAndPrint state (Lib3.StatementCommand $ Lib3.Single $ Lib2.AddRequest req) >> return next
    runStep state chan (ListRequests next) = transitionAndPrint state (Lib3.StatementCommand $ Lib3.Single Lib2.ListRequests) >> return next
    runStep state chan (RemoveRequest id next) = transitionAndPrint state (Lib3.StatementCommand $ Lib3.Single $ Lib2.RemoveRequest id) >> return next
    runStep state chan (UpdateRequest id req next) = transitionAndPrint state (Lib3.StatementCommand $ Lib3.Single $ Lib2.UpdateRequest id req) >> return next
    runStep state chan (FindRequest id next) = transitionAndPrint state (Lib3.StatementCommand $ Lib3.Single $ Lib2.FindRequest id) >> return next
    runStep state chan (RemoveAllRequests next) = transitionAndPrint state (Lib3.StatementCommand $ Lib3.Single Lib2.RemoveAllRequests) >> return next
    runStep state chan (Save next) = transitionAndPrint state (Lib3.SaveCommand $ Lib3.Single Lib2.Save) >> return next
    runStep state chan (Load next) = transitionAndPrint state (Lib3.LoadCommand $ Lib3.Single Lib2.Load) >> return next

transitionAndPrint :: TVar Lib2.State -> Lib3.Command -> Chan Lib3.StorageOp -> IO String
transitionAndPrint state cmd chan = do
    putStrLn $ "Command:\n" ++ show cmd
    res <- Lib3.stateTransition state cmd chan
    let str = either id (fromMaybe "Success") res
    putStrLn "Result:"
    putStrLn str
    return str

process :: TVar Lib2.State -> Chan Lib3.StorageOp -> String -> IO String
process state storageChan input = case Lib3.parseCommand input of
  Left e -> return e
  Right (cmd, "") -> do
    info <- Lib3.stateTransition state cmd storageChan
    case info of
      Left e -> return e
      Right mb -> return $ fromMaybe "Success" mb
  Right (_, str) -> return $ "Could not parse: " ++ str

program :: RestaurantProgram ()
program = do
    AddRequest (Lib2.Request 1 "t1" "o1" (Lib2.Items ["i1", "i2"]))
    AddRequest (Lib2.Request 2 "t2" "o2" (Lib2.Items ["i3", "i4"]))
    save
    a <- listRequests
    UpdateRequest 1 (Lib2.Request 1 "t1" "o1" (Lib2.Items ["i1", "i2", "i3"]))
    RemoveRequest 2
    load
    FindRequest 1
    RemoveAllRequests
    b <- listRequests
    return (a, b)
    


main :: IO ()
main = do
  str <- interpretorSingleRequest program
  -- str <- interpretWithBatching program
  -- str <- testInterpretator program
  print str