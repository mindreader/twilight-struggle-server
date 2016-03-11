{-# LANGUAGE NamedFieldPuns #-}
module Play where

import qualified Data.Set as S
import qualified Data.Map as M
import Rules

data GameMode = InitialInfluence | Headline | ActionRounds | CleanupPhase deriving Show


data PlayerAction = Action Player ActionType
data ActionType = PlaceInfluence Int [InfluenceRestriction] | PlayHeadline | PlayFromHand | PlayMissileEnvy | PlayChina | PlayCard Card

data InfluenceRestriction = ByRegion [Region] | ByCountry [Country]

data GamePhase =
  GPPlaceInitialInfluence Player |
  GPPlayHeadlineSimul |
  GPPlayHeadlineFirst Player |
  GPPlayActionRound ActionRound Player deriving Show


data GameState = GameState {
  gsGameMode :: GamePhase,
  gsTurn :: Turn,

  gsMapInfluence :: M.Map Country Influence,

  gsDefCon :: DefCon,

  gsDeck :: S.Set Card, -- Cards yet to be drawn
  gsDiscard :: S.Set Card, -- Cards that have been discarded
  gsRemovedFromGame :: S.Set Card, -- Cards that happen once, then go away forever

  gsCurrentEffects :: S.Set Effect, -- Cards that have been played and have lasting effects

  gsUSState :: PlayerState,
  gsUSSRState :: PlayerState,

  gsChinaCard :: ChinaCardState,

  gsPoints :: Points 

} deriving Show

data ChinaCardState = CFaceDown Player | CFaceUp Player deriving (Show, Eq)

gsPhase :: GameState -> Player -- Play alternates each action round starting with USSR
gsPhase = undefined

data PlayerState = PlayerState {
  psHand :: Either (S.Set Card) Int, -- Either your hand or the number of cards in your opponent's hand
  psMilOps :: MilOps,
  psSpacePos :: SpacePos
} deriving Show

data ActionError = ActionError

data AvailablePlayerActions = APA {
  usAvailActions :: [ActionType],
  ussrAvailActions :: [ActionType]
}

-- | Actions that are possible to be performed by each player based on the current game state.
availableActions :: GameState -> AvailablePlayerActions
availableActions gs = case gsGameMode gs of
  GPPlaceInitialInfluence US -> APA [PlaceInfluence 7 [ByRegion [WesternEurope]]] []
  GPPlaceInitialInfluence USSR -> APA [] [PlaceInfluence 5 [ByRegion [EasternEurope]]]
  GPPlayHeadlineSimul -> APA [PlayHeadline] [PlayHeadline]
  GPPlayHeadlineFirst US -> APA [PlayHeadline] []
  GPPlayHeadlineFirst USSR -> APA [] [PlayHeadline]
  GPPlayActionRound n US -> APA (maybeChina US ++ playFromHand) []
  GPPlayActionRound n USSR -> APA [] (maybeChina USSR ++ playFromHand)

  where
    playFromHand = missileEnvyOrDefault
    missileEnvyOrDefault = if S.member EfMissileEnvy (gsCurrentEffects gs)
      then [PlayMissileEnvy]
      else defaultPlayFromHand
    defaultPlayFromHand = [PlayFromHand]
    maybeChina p = if gsChinaCard gs  == CFaceUp p then [PlayChina] else []
    

performAction :: GameState -> PlayerAction -> Either ActionError (Player, GameState)
performAction gs act = undefined


initialGameState :: M.Map Country Influence -> GameState
initialGameState placedInfluence =
  GameState (GPPlaceInitialInfluence US) (Turn 1) initialInfluence DFFive earlyWarCards S.empty S.empty S.empty newplayer newplayer (CFaceUp USSR) (Points 0)
  where
    newplayer = PlayerState (Left S.empty) (MilOps 0) (SpacePos 0)

    initialInfluence :: M.Map Country Influence
    initialInfluence = undefined -- the board staring influence
