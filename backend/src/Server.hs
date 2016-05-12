{-# LANGUAGE LambdaCase, OverloadedStrings #-}
module Server where

import Control.Concurrent.STM
import Network.WebSockets

import Control.Concurrent
import Control.Concurrent.STM
import Control.Concurrent.Async

import Control.Monad (void, forM_)

import Data.Attoparsec.ByteString.Char8

import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.ByteString as B
import Control.Exception

import Data.Maybe
import Data.List (deleteBy)
import Data.Function (on)
import Control.Monad

import ServerPlay hiding (US)

-- Requirements:
-- 1. I need every player thread to be able to message other threads including unrelated player threads of same side CHECK
-- 2. I need to know when there is no one watching at all on a particular side so I can inform the other players of a hopefully temporary disconnect CHECK
-- 3. Messages sent by a player should not be sent back to him unless he needs the result.

data State = State {
  gameList :: !(TVar (M.Map GameID Game)),
  nextGameID :: !(TVar GameID),
  gameLoadLocks :: !(TVar (S.Set GameID)) -- | To prevent a specific game from being loaded/saved by diff threads.
}

data ClientType = US | USSR | Observer deriving Eq
data Client = Client {
  clientType :: !ClientType,
  clientConnection :: !Connection,
  clientChan :: !(TChan TWMessage) -- | Read from chan to get alerts from other parties actions in game
}


data TWMessage = TWMessage1 | TWMessage2 | USPlayerDisconnected | USSRPlayerDisconnected

data Game = Game {
  gameId :: !GameID,
  gameState :: !(TVar (GameState Server)),  -- | Current state of the game, so far as the server knows
  gameChan :: !(TChan TWMessage),  -- | Messages can be written here to broadcast to all connected clients
  -- FIXME gameClients should not be a list
  gameClients :: !(TVar [Client])  -- | List of parties who are watching the game
}
newtype GameID = GameID Integer deriving (Eq, Ord, Show)



server :: IO ()
server = do
  let firstid = GameID 1
  state <- atomically $ State <$> newTVar M.empty <*> newTVar firstid <*> newTVar mempty
  runServer "127.0.0.1" 5715 (routeRequest state)

routeRequest :: State -> PendingConnection -> IO ()
routeRequest state pconn = do
  case getGameFromRequest (pendingRequest pconn) of
    Nothing -> rejectRequest pconn "Unrecognized game"
    Just (gameid, clienttype) -> do
      game <- getGame state gameid
      conn <- acceptRequest pconn
      client <- atomically $ newClient (gameChan game) clienttype conn
      void $ withAsync (clientThread client game) waitCatch

getGameFromRequest :: RequestHead -> Maybe (GameID, ClientType)
getGameFromRequest req = do
  gameid <- either (const Nothing) Just $ parseOnly gameIDPat path
  clienttype <- either (const Nothing) Just $ parseOnly rolePat path
  return $ (gameid, clienttype)
  where

    gameIDPat :: Parser GameID
    gameIDPat = GameID <$> (string "game/" *> decimal)

    rolePat :: Parser ClientType
    rolePat = return (US) -- TODO decode from a jwt passed in url
 
    path = requestPath req
    
gameToDisk :: State -> GameID -> IO ()
gameToDisk state@(State _ _ locks) gameid = do
  f <- atomically $ lookupGame state gameid >>= \case
    Nothing -> return (return ())
    Just game -> do
      gs <- readTVar $ gameState game
      closeGame state gameid
      l <- readTVar locks -- Check lock to see if we can get it from disk
      if S.member gameid l
        then return (return ()) -- Some other thread is in the process of saving it
        else do
          modifyTVar (locks) $ S.insert gameid
          return $ do
            void $ withAsync (storeGame gameid gs) waitCatch -- TODO I should log failures to save a game
            atomically $ modifyTVar (locks) $ S.delete gameid
  f
  where
    storeGame :: GameID -> GameState Server -> IO ()
    storeGame gameid game = undefined -- write file onto disk somewhere

getGame :: State -> GameID -> IO Game
getGame state@(State _ _ locks) gameid = do
  f <- atomically $ do
    lookupGame state gameid >>= \case
      Just game -> return (return game) -- Game was stored in memory, great
      Nothing -> do
         l <- readTVar locks -- Check lock to see if we can get it from disk
         if S.member gameid l
           then retry -- Some other thread must be reading / writing to disk so start over.
                      -- That's fine. Wait until it is in memory, then use it.
           else do
              -- Prevent more than one person from passing this point
              modifyTVar (locks) $ S.insert gameid

              -- create a closure that attempts to fetch from disk
              return $ do
                fetchGameStateFromDisk gameid >>= \case

                  -- There was no game on disk, or there was error.  Create a new one, register it and unlock
                  Nothing -> atomically $ do
                      newgame <- openGame state (initialGameState defaultInfluence)
                      modifyTVar locks $ S.delete gameid
                      return newgame
                    
                  -- There was a game on disk, create a new one, register it set its state then unlock
                  Just gs -> do
                    atomically $ do
                      newgame <- openGame state (initialGameState defaultInfluence)
                      writeTVar (gameState newgame) $ gs
                      modifyTVar locks $ S.delete gameid
                      return newgame

  f
  where
    -- try to read a file from disk somewhere
    fetchGameStateFromDisk :: GameID -> IO (Maybe (GameState Server))
    fetchGameStateFromDisk gameid = either (const Nothing) Just <$>
      withAsync undefined waitCatch


clientThread :: Client -> Game  -> IO ()
clientThread client game = do

  atomically $ addClientToGame client game
  forkPingThread (clientConnection client) 30 -- Should kill thread if client goes away.
  runclient `finally` atomically (removeClientFromGame client game)

  where

    runclient = race_ recthread sendthread

    -- player sends commands to update game state / inform players of his status
    recthread = async $ receiveData (clientConnection client) >>= sendToGame

    -- player receives events from any other players that have affected the game
    sendthread = async $ receiveFromGame >>= sendBinaryData (clientConnection client)

    receiveFromGame :: IO B.ByteString
    receiveFromGame = undefined $ atomically $ readTChan (clientChan client)

    sendToGame :: B.ByteString -> IO ()
    sendToGame = atomically . writeTChan (gameChan game) . undefined

  

addClientToGame :: Client -> Game -> STM ()
addClientToGame client game = modifyTVar' (gameClients game) $ (client:)

removeClientFromGame :: Client -> Game -> STM ()
removeClientFromGame client game = do
  clientsBefore <- readTVar (gameClients game)
  modifyTVar' (gameClients game) $ deleteBy ((==) `on` clientChan) client
  clientsAfter <- readTVar (gameClients game)
  when False undefined
--  when $ (length (filter (\c -> clientType c == US) clientsBefore) == 0) $
--    undefined
  return ()
  

newClient :: TChan TWMessage -> ClientType -> Connection -> STM Client
newClient gameChan clienttype conn =
  Client clienttype conn <$> dupTChan gameChan -- TODO cloneTChan?

clientJoinGame' :: Client -> Game -> STM ()
clientJoinGame' client game = modifyTVar' (gameClients game) $ (client:)
  
openGame :: State -> GameState Server -> STM Game
openGame state init = do
  gameid <- readTVar (nextGameID state)
  newgame <- Game gameid <$> newTVar init <*> newBroadcastTChan <*> newTVar []

  modifyTVar' (gameList state) $ M.insert gameid newgame
  modifyTVar' (nextGameID state) $ (\(GameID i) -> GameID (i+1))

  return newgame

closeGame :: State -> GameID -> STM ()
closeGame state gameid = do
  lookupGame state gameid >>= \case
    Just _ -> modifyTVar' (gameList state) $ M.delete gameid
    Nothing -> return ()

lookupGame :: State -> GameID -> STM (Maybe Game)
lookupGame (State games _ _) gameid = M.lookup gameid <$> readTVar games

gameIsOpen :: State -> GameID -> STM Bool
gameIsOpen (State games _ _) gameid = M.member gameid <$> readTVar games


removeGame :: GameID -> State -> STM ()
removeGame gameid (State games _ _) = modifyTVar' games $ M.delete gameid

renderGame :: Game -> String
renderGame (Game (GameID id) _ _ _) = "Game: "++show id
