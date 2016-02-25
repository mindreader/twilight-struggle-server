module Play where

import qualified Data.Set as S
import qualified Data.Map as M
import Rules

data GameMode = InLobby | GameCreated | PlaceInitialInfluence Player | InGame GameState | FinalScoring

data GameState = GameState {
  gsHeadLine :: Bool, -- True at start of turn, both players play headlines
  gsTurn :: Turn,
  gsActionRound :: ActionRound,
  gsPhase :: Player, -- Play alternates each action round starting with USSR

  gsMapInfluence :: M.Map Country Influence,

  gsDefCon :: DefCon,

  gsDeck :: S.Set Card, -- Cards yet to be drawn
  gsDiscard :: S.Set Card, -- Cards that have been discarded
  gsRemovedFromGame :: S.Set Card, -- Cards that happen once, then go away forever

  gsCurrentEffects :: S.Set Effect, -- Cards that have been played and have lasting effects

  gsUSState :: PlayerState,
  gsUSSRState :: PlayerState,

  gsPoints :: Points 

} deriving Show

data PlayerState = PlayerState {
  psHand :: S.Set Card,
  psMilOps :: MilOps,
  psSpacePos :: SpacePos
} deriving Show


initialGameState :: M.Map Country Influence -> GameState
initialGameState placedInfluence =
  GameState True (Turn 1) (ActionRound 1) USSR initialInfluence DFFive earlyWarCards S.empty S.empty S.empty newplayer newplayer (Points 0)
  where
    newplayer = PlayerState S.empty (MilOps 0) (SpacePos 0)

    initialInfluence :: M.Map Country Influence
    initialInfluence = undefined


data ThingsAPlayerCanDo = PlayCardFromHand | PlayChinaCard | PlayMissileEnvy | Concede

data CardPlay = PlayOwnCardForOps | PlayOwnCardForEvent | PlayOpponentCard
