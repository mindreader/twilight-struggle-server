{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE LambdaCase #-}
module ServerPlay (
  module Play,
  module ServerPlay,
) where

import Control.Monad.Trans

import qualified Data.Map as M
import qualified Data.Set as S

import Play
import Cards
import Rules


data PlayerAction = PlayerAction

data ServerActionType = DealCards (S.Set Card) (S.Set Card)

data InfluenceRestriction = ByRegion [Region] | ByCountry [Country]

nextServerAction :: MonadIO m => GameState Server -> m (Maybe ServerActionType)
nextServerAction (GameState {gsGamePhase}) | curTurn gsGamePhase <= (Turn 3) && curActionRound gsGamePhase > (ActionRound 6) = Just <$> dealCards
nextServerAction (GameState {gsGamePhase}) | curActionRound gsGamePhase > (ActionRound 7) = Just <$> dealCards
nextServerAction (GameState {gsGameLog}) = case curEvent gsGameLog of
  _ -> undefined 
nextServerAction gs  = return Nothing

dealCards :: MonadIO m => m (ServerActionType)
dealCards = DealCards <$> undefined <*> undefined

  --  usHandSize = length (psHand (gsUSState gs))
  --  ussrHandSize = length (psHand (gsUSSRState gs))

{-
availableActions :: GameState -> AvailableActions
availableActions gs = case gsGameMode gs of
  GPPlaceInitialInfluence US -> PlayerActions[PlaceInfluence 7 [ByRegion [WesternEurope]]] []
  GPPlaceInitialInfluence USSR -> PlayerActions[] [PlaceInfluence 5 [ByRegion [EasternEurope]]]
  GPPlayHeadlineSimul -> PlayerActions[PlayHeadline] [PlayHeadline]
  GPPlayHeadlineFirst US -> PlayerActions[PlayHeadline] []
  GPPlayHeadlineFirst USSR -> PlayerActions[] [PlayHeadline]
  GPPlayActionRound n US -> PlayerActions (maybeChina US ++ playFromHand) []
  GPPlayActionRound n USSR -> PlayerActions[] (maybeChina USSR ++ playFromHand)

  where
    playFromHand = missileEnvyOrDefault
    missileEnvyOrDefault = if S.member EfMissileEnvy (gsCurrentEffects gs)
      then [PlayMissileEnvy]
      else defaultPlayFromHand
    defaultPlayFromHand = [PlayFromHand]
    maybeChina p = if gsChinaCard gs  == CFaceUp p then [PlayChina] else []

-}
validatePlayerAction :: GameState Server -> PlayerAction -> Bool -- TODO not boolean
validatePlayerAction = undefined

evalPlayerAction :: GameState Server -> PlayerAction-> GameState Server
evalPlayerAction gs act = undefined

evalServerAction :: MonadIO m => GameState Server -> ServerActionType -> m (GameState Server)
evalServerAction = undefined

-- Default influence at beginning of game, based on rules
defaultInfluence :: M.Map Country Influence
defaultInfluence = undefined


initialGameState :: M.Map Country Influence -> GameState Server
initialGameState placedInfluence =
  GameState (GPPlaceInitialInfluence US) initialInfluence (DefCon 5) earlyWarCards S.empty S.empty S.empty newplayer newplayer (CFaceUp USSR) (Points 0) (GameLog mempty)
  where
    newplayer = PlayerState S.empty (MilOps 0) (SpacePos 0)

    initialInfluence :: M.Map Country (Influence, Influence)
    initialInfluence = undefined -- the board staring influence

