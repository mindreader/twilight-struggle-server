{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall #-}
module Play (
  module Types,
  module Play
) where

import Data.Proxy

import qualified Data.Set as S
import qualified Data.Map as M
import Data.Sequence as Seq
import Data.Tuple

import Control.Monad.Fix

import Types
import Countries
import Cards

import Control.Lens hiding ((:<), (<|), to, from)



curTurn :: GamePhase -> Turn
curTurn (GPPlayHeadlineSimul t) = t
curTurn (GPPlayHeadlineFirst t _) = t
curTurn (GPPlayActionRound t _ _) = t
curTurn (GPPlaceInitialInfluence _) = Turn 1

curActionRound :: GamePhase -> ActionRound
curActionRound (GPPlayHeadlineSimul _) = ActionRound 1
curActionRound (GPPlayHeadlineFirst _ _) = ActionRound 1
curActionRound (GPPlayActionRound _ r _) = r
curActionRound (GPPlaceInitialInfluence _) = ActionRound 1

curPlayer :: GamePhase -> Either Both Player
curPlayer (GPPlayHeadlineSimul _) = Left Both
curPlayer (GPPlayHeadlineFirst _ p) = Right p
curPlayer (GPPlayActionRound _ _ p) = Right p
curPlayer (GPPlaceInitialInfluence _) = Right USSR

{-
curEvent :: GameLog a -> Event a
curEvent (GameLog evs) = case viewl evs of
  EmptyL -> GameStart
  (ev :< _) -> ev
-}

replayLog :: forall a. HandType a => GameLog a -> GameState a
replayLog gl@(GameLog evs) = fix interpret evs (initialGS & gsGameLog .~ gl)
  where

    initialGS :: HandType a => GameState a
    initialGS = GameState (GPPlaceInitialInfluence US) initialInfluence (DefCon 5) earlyWarCards S.empty S.empty S.empty
      (newplayer (toUS (Proxy :: Proxy a) S.empty)) (newplayer (toUSSR (Proxy :: Proxy a) S.empty))
      (CFaceUp USSR) (Points 0) (GameLog mempty)

    newplayer hand = PlayerState hand  (MilOps 0) (SpacePos 0)

    initialInfluence :: M.Map Country (Influence, Influence)
    initialInfluence = undefined -- the board staring influence

interpret :: HandType a => (Seq (Event a) -> GameState a -> GameState a) -> Seq (Event a) -> GameState a -> GameState a
interpret = interpret'
  where

    interpret' _ evs gs | Seq.null evs = gs

    interpret' loop (viewl -> ChangeInfluence _ [] :< xs) gs = loop xs gs
    interpret' loop (viewl -> ChangeInfluence p ((c,chg):restinf) :< xs) gs =
      loop (ChangeInfluence p restinf <| xs)
           (gs & gsMapInfluence %~ updateInfluence chg p c)

    interpret' loop (viewl -> RelocateInfluence _ [] :< xs) gs = loop xs gs
    interpret' loop (viewl -> RelocateInfluence p ((from,to,chg):restchg)  :< xs) gs =
      loop (RelocateInfluence p restchg <| xs) $ gs &
        gsMapInfluence %~ updateInfluence (-chg) p from & gsMapInfluence %~ updateInfluence chg p to

    interpret' loop (viewl -> PlayChina _ [] :< xs) gs = loop xs gs
    interpret' loop (viewl -> PlayChina p ((c,chg):restinf) :< xs) gs =
      loop (PlayChina p restinf <| xs) $ gs &
        gsMapInfluence %~ updateInfluence chg p c

    interpret' loop (viewl -> PlayScoreCard p CentralAmericanScoring :< xs) gs =
      loop xs $ gs &
        gsPoints %~ score CentralAmerica (gs ^. gsMapInfluence) &
        id %~ discardCard p CentralAmericanScoring
--    interpret' loop (viewl -> PlayScoreCard p SouthAmericanScoring :< xs) gs = undefined
--    interpret' loop (viewl -> PlayScoreCard p EuropeScoring :< xs) gs = undefined
--    interpret' loop (viewl -> PlayScoreCard p AsiaScoring :< xs) gs = undefined
--    interpret' loop (viewl -> PlayScoreCard p SouthEastAsiaScoring  :< xs) gs = undefined
--    interpret' loop (viewl -> PlayScoreCard p MiddleEastScoring :< xs) gs = undefined
--    interpret' loop (viewl -> PlayScoreCard p AfricaScoring :< xs) gs = undefined

discardCard :: forall a. HandType a => Player -> Card -> GameState a -> GameState a
discardCard p c gs = gs & gsDiscard %~ S.insert c & removeFromHand p
  where
    removeFromHand US   = gsUSState   . psHand %~ addCardUS   (Proxy :: Proxy a) c
    removeFromHand USSR = gsUSSRState . psHand %~ addCardUSSR (Proxy :: Proxy a) c

score :: Region -> M.Map Country (Influence, Influence) -> Points -> Points
score region inf  = undefined

updateInfluence :: Influence -> Player -> Country -> M.Map Country (Influence,Influence) -> M.Map Country (Influence,Influence)
updateInfluence c = M.alter . \case
  US -> alterinf c 
  USSR -> fmap swap . alterinf c . fmap swap
  where
    alterinf :: Influence -> Maybe (Influence, Influence) -> Maybe (Influence, Influence)
    alterinf chg = \case
      Nothing | chg <= 0           -> Nothing
      Nothing                      -> Just (Influence 0, max (Influence 0) chg)
      Just (0, p2) | p2 + chg <= 0 -> Nothing
      Just (p1, p2)                -> Just (p1, max (Influence 0) (p2 + chg))


-- Every action to be recorded requires that it be numbered in order, starting from the gamestart
-- Without this, there is a chance that an out of sync client could remain out of sync and no know it.
{-
recordAction :: GameLog a -> (Int, Event a) -> GameLog a
recordAction gl@(GameLog acts) (i, newev) | i == Seq.length acts + 1 = GameLog (newev <| acts)
recordAction gl _                                                    = gl -- TODO a way to detect this error?
-}

data ActionError = ActionError



