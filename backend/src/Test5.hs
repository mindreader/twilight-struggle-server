{-# LANGUAGE  TypeFamilies, ScopedTypeVariables #-}
{-# LANGUAGE  OverloadedLists #-}

module Test where

import qualified Data.Set as S
import Data.Proxy

data Card = Card deriving (Eq, Ord)
data BoardState = BoardState
data GamePhase = GamePhase
data Country
data Player = PUS | PUSSR

data US
data USSR
data Observer
data Server


class HandType a where
  type USHand   a :: *
  type USSRHand a :: *
  toUS :: Proxy a -> S.Set Card -> USHand a
  toUSSR :: Proxy a -> S.Set Card -> USSRHand a

instance HandType Server where
  type USHand Server = S.Set Card
  type USSRHand Server = S.Set Card
  toUS _ = id
  toUSSR _ = id

instance HandType US where
  type USHand US = S.Set Card
  type USSRHand US = Int
  toUS _ = id
  toUSSR _ cs = length cs

instance HandType USSR where
  type USHand USSR = Int
  type USSRHand USSR = S.Set Card
  toUS _ cs = length cs
  toUSSR _ = id

instance HandType Observer where
  type USHand Observer = Int
  type USSRHand Observer = Int
  toUS _ cs = length cs
  toUSSR _ cs = length cs



data GameState a = GameState {
  gameTurn :: Int,
  gamePhase :: GamePhase,
  boardState :: BoardState,

  usHand :: USHand a,
  ussrHand :: USSRHand a
}

data Event a =
    PlaceInfluence Player Int Country -- | Most plays don't affect
  | PlayCard Player Card              -- | either hand
  | DealCards (USHand a) (USSRHand a) -- | This one does

obsEvents :: [Event US]
obsEvents = [PlayCard PUS Card, PlayCard PUSSR Card, DealCards [Card] (3)]

serverEvents :: [Event Server]
serverEvents = [PlayCard PUS Card, PlayCard PUSSR Card, DealCards [Card, Card] [Card]]

serverToPlayerGS :: forall a. HandType a => GameState Server -> GameState a
serverToPlayerGS (GameState turn phase bs us ussr) =
  GameState turn phase bs (toUS (Proxy :: Proxy a) us) (toUSSR (Proxy :: Proxy a) ussr)

serverToPlayerEvent :: forall a. HandType a => Event Server -> Event a
serverToPlayerEvent (PlaceInfluence p amt c) = PlaceInfluence p amt c
serverToPlayerEvent (PlayCard p c) = PlayCard p c
serverToPlayerEvent (DealCards us ussr) = DealCards (toUS (Proxy :: Proxy a) us) (toUSSR (Proxy :: Proxy a) ussr)
