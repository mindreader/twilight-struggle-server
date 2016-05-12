{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds, TypeOperators #-}
module API where

import qualified Data.ByteString as BS
import Control.Monad.Trans
import Control.Monad.Trans.Either
import Servant
import Servant.HTML.Lucid
import Data.Monoid

import ServerPlay hiding (Server)

newtype GameID = GameID Integer

newtype SessionID = SessionID String

data LobbyState = LobbyState


type TSApi = Game :<|> Raw

type Sessioned = Header "Cookie" SessionID

type Game = Sessioned :> "game"  :> Capture "gameid" GameID :> Get '[HTML] (Headers '[Header "Cookie" String] GameState)
type Lobby = Sessioned :> "lobby" :> Get '[HTML] LobbyState



server :: Server TSApi
server = game :<|> static "adsf" -- game :<|> lobby :<|> static (error "TODO where should the static directory be?")

-- game :: Maybe SessionID -> GameID -> EitherT ServantErr IO (Headers '[Header "Cookie" String] GameState)
-- game :: Maybe SessionID -> GameID -> EitherT ServantErr IO (Headers '[Header h String] GameState)
game :: Maybe SessionID -> GameID -> EitherT ServantErr IO (Headers '[Header "Cookie" String] GameState)
game Nothing gid = fmap (addHeader "asdf") $ game'

  where game' = return (undefined :: GameState) -- addHeader "asdf" $ do


-- setCookie :: EitherT ServantErr IO (Headers '[Header "Cookie" String] String)
-- setCookie = addHeader ("Cookie" :: String) ("asdf" :: String)


lobby :: Maybe SessionID -> EitherT ServantErr IO LobbyState
lobby msess = undefined

static :: FilePath -> Server Raw
static = serveDirectory
