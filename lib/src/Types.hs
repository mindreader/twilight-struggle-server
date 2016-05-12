{-# LANGUAGE  TypeFamilies, ScopedTypeVariables #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

module Types where

import Data.Proxy
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Sequence as Seq

import Control.Lens

-- This is not quite a functor, because you can't go from GameState Observer to GameState (US|USSR|Server)
data GameState pov = GameState {
  _gsGamePhase :: GamePhase,

  _gsMapInfluence :: M.Map Country (Influence,Influence),

  _gsDefCon :: DefCon,

  _gsDeck :: S.Set Card, -- Cards yet to be drawn
  _gsDiscard :: S.Set Card, -- Cards that have been discarded
  _gsRemovedFromGame :: S.Set Card, -- Cards that happen once, then go away forever

  _gsCurrentEffects :: S.Set OngoingEffect, -- Cards that have been played and have lasting effects

  _gsUSState :: PlayerState (USHand pov),
  _gsUSSRState :: PlayerState (USSRHand pov),

  _gsChinaCard :: ChinaCardState,

  _gsPoints :: Points,

  _gsGameLog :: GameLog pov 
}


data PlayerState a = PlayerState {
  _psHand :: a,
  _psMilOps :: MilOps,
  _psSpacePos :: SpacePos
} deriving Show



-- | Lingering effects that persist for some time (often the reset of the game)
data OngoingEffect = OgEffect Card | OgEffectOnPlayer Card Player
  deriving (Eq, Ord, Show)

data GamePhase =
  GPPlaceInitialInfluence Player |
  GPPlayHeadlineSimul Turn |
  GPPlayHeadlineFirst Turn Player |
  GPPlayActionRound Turn ActionRound Player deriving Show

-- For times when the player targetted depends on who played the card
data GameDependentPlayer = PhasingPlayer | Opponent

data ChinaCardState = CFaceDown Player | CFaceUp Player deriving (Show, Eq)

data DefCon = DefCon Int deriving Show -- 5-1, 1 means nuclear war

newtype MilOps = MilOps Int deriving Show            -- required military ops 0..5
newtype Turn = Turn Int deriving (Show, Eq, Ord)     -- There are 10 turns in this game
newtype ActionRound = ActionRound Int deriving (Show, Eq, Ord)  -- There are between 6 and 8 action rounds per turn


newtype Points = Points Int deriving (Eq, Ord, Show) -- Ranges from -20 to 20 (-20 is USSR win, 20 is US win)
newtype SpacePos = SpacePos Int deriving (Eq, Ord, Show) -- TODO Ranges from 0 to something.


data US
data USSR
data Observer
data Server

class HandType a where
  type USHand   a :: *
  type USSRHand a :: *
  toUS :: Proxy a -> S.Set Card -> USHand a
  addCardUS :: Proxy a -> Card -> USHand a -> USHand a
  toUSSR :: Proxy a -> S.Set Card -> USSRHand a
  addCardUSSR :: Proxy a -> Card -> USSRHand a -> USSRHand a

instance HandType Server where
  type USHand Server = S.Set Card
  type USSRHand Server = S.Set Card
  toUS _ = id
  addCardUS _ c = S.insert c
  toUSSR _ = id
  addCardUSSR _ c = S.insert c

instance HandType US where
  type USHand US = S.Set Card
  type USSRHand US = Int
  toUS _ = id
  addCardUS _ c = S.insert c
  toUSSR _ cs = length cs
  addCardUSSR _ _ i = i + 1

instance HandType USSR where
  type USHand USSR = Int
  type USSRHand USSR = S.Set Card
  toUS _ cs = length cs
  addCardUS _ _ i = i + 1
  toUSSR _ = id
  addCardUSSR _ c = S.insert c

instance HandType Observer where
  type USHand Observer = Int
  type USSRHand Observer = Int
  toUS _ cs = length cs
  addCardUS _ _ i = i + 1
  toUSSR _ cs = length cs
  addCardUSSR _ _ i = i + 1



data Player = US | USSR deriving (Eq, Ord, Show)
data Both = Both

data WarResult = WarVictory | WarFailure deriving Show

data Ops =
    PlaceInfluence Country Int
  | CoupSuccess Country
  | CoupFailure Country
  | RealignSuccess Country
  | RealignFailure Country
  deriving Show

-- duck cover -> all: degrade defcon, award vp
-- five year -> player: discard card all: maybe play as event
-- china card -> player: ops 4 or 5
-- soc gov -> player: remove influence
-- fidel -> all: remove inf, control country
-- vietnam -> all: add influence, add effect
-- blockade -> player: choice to discard, all: effects happen
-- korean war -> server: korean invasion
-- romainan abd -> all: remove influence, control country
-- israel war -> all: add milops, server: israel invasion
-- comecon -> player: choose countries: all add influence
-- nasser -> all: add remove influence from egypt
-- warsawp -> player: choose (remove|add), either: (all: remove influence or player: add influence to countries) all: add eff
-- degaulle -> all: remove and add inf, add eff
-- nazisci -> all: space race ahead
-- truman -> player: choose country, all: remove infl
-- olympic -> opponent: accept/decline, all: change vpn and defcon
-- nato -> all: add effect
-- indie reds -> player: choose country, all: add influence
-- marshall -> player: choose countries, all: add effect
-- indpak -> player: pick country, all: resolve indopak, add milops
-- contain -> all: add effect
-- cia -> all: reveal or add effect reveal, opponent: conduct ops
-- usjapan -> all: us control japan, add eff
-- suez -> player: choose countries and influence to remove
-- eastunrest -> player: choosecountries, all: remove influence
-- decol -> player: choose countries, all: add influence
-- redscare -> all: add eff
-- unint -> player: choose card, choose op with cardval
-- dstal -> player: relocate inf
-- ncban -> all: changevp
-- formosan -> all: addeff
-- defect -> (HEADLINE ONLY) all: cancel other headline
-- brush -> player: choose country, server: resolve brush war on country, all: addmilops
-- armsrace -> all: changevp
-- cubanm -> all: change defcon, add eff (new option for players for rest of turn)
-- nukesubs -> all: addeff
-- quag -> all: addef
--   quageffect -> player: maybe discard card, server: roll for quagmire, remeff
-- salt -> all: improve defccon, addeff, player: pick card from discard
-- bear -> all: addeff
--   beartrapeffect -> same as quag
-- summit -> server: resolve summit
-- howilearned -> player: change defcon, all: add milops
-- junta -> player: choose country for inf, choose country to coup or reallign
-- kitchen -> all: changevp
-- missenvy -> opponent: maybe choose card, based on card: play event or resolve event, all: addeff (opponent gets me)
-- willbury -> all: changedefcon, addeff
-- brezhnev -> all: addeff
-- portg -> all: addinf
-- saunrest -> player: choose 2 to sa or 1 to sa and 2 to adjacent
-- allende -> all: add infl
-- willbrandt -> all: addvp, addeff
-- muslimrev -> all: reminfluence from countries
-- abmtreaty -> all: changedefcon, player: conductops
-- cultrev -> all: changevp
-- flowpowe -> all: addeff
-- u2inc -> all: changevp, addeff
-- opec -> all: maybe changevp
-- lonegunman -> ussr: confirm seen hand?, conduct operations
-- rearguards -> us: choose countries
-- pancanal -> all: add inf
-- campdavid -> all: changevp, addinf, addeff

data Event a =
    GameStart
  | PlayHeadline Player Card

  | PlayCardOps Player Card Ops
  | PlayCardOpsEventFirst Player Card Ops

  | PlayCardEvent Player Card
  | PlayScoreCard Player Card

  | PlayChina Player [(Country, Influence)]


  | ChangeInfluence Player [(Country, Influence)]
  | RelocateInfluence Player [(Country,Country, Influence)] -- | from to

  | ChooseCountries Player [Country]
  | ChooseCards Player [Card]

  -- Server events
  | CardsDealt (USHand a) (USSRHand a)
  | Roll Int

-- A set of actions that should replicate the current game.  Also used to display a history of what has
-- occurred so far to the player.
data GameLog a = GameLog {
  _glEvents :: Seq.Seq (Event a)
}


data CountryScore = Presence | Control | Dominate deriving (Show, Eq, Ord)
data ScoreResult = ScoreResult Int | Win Player deriving (Show, Eq, Ord)

data RegionTarget = Region Region | NotRegion Region | TheseRegions [Region] | Anywhere
data Region = WesternEurope | EasternEurope | Europe | Asia | SouthEastAsia | Africa | CentralAmerica | SouthAmerica | MiddleEast
  deriving Show

newtype OpsValue = OpsValue Int deriving (Show, Num, Eq)

newtype Influence = Influence Int deriving (Show, Num, Eq, Ord)

data Country =
  Mexico | Guatemala | ElSalvador | Honduras | Cuba | CostaRica | Nicaragua | Panama | Haiti | DominicanRepublic | -- Central America

  Venezuela | Columbia | Ecuador | Peru | Bolivia | Brazil | Chile | Paraguay | Argentina | Uruguay | -- South America

  Canada | UnitedKingdom | Norway | Denmark | Benelux | France | SpainPortugal | Sweden | WestGermany | Italy | Greece | Turkey | -- West Europe

  Finland | Austria | -- Both East and West Europe

  EastGermany | Poland | Czech | Hungary | Yugoslavia | Romania | Bulgaria | -- East Europe

  Lebanon | Syria | Israel | Iraq | Iran | Libya | Egypt | Jordan | GulfStates | SaudiArabia | -- Middle East

  Morocco | Algeria | Tunisia | WestAfricanStates | SaharanStates | IvoryCoast | Nigeria | Cameroon | Zaire | Angola | SouthAfrica |
  Botswana | Zimbabwe | SEAfricanStates | Kenya | Somalia | Ethiopia | Sudan | -- Africa

  Afghanistan | Pakistan | India | NorthKorea | SouthKorea | Japan | Taiwan | Australia | -- Asia

  Burma | LaosCambodia | Vietnam | Thailand | Malaysia | Phillippines | Indonesia -- SE Asia
  deriving (Show, Eq, Ord, Enum, Bounded)

data CardAffil = AffUS | AffUSSR | AffNeutral

data Card =
    AsiaScoring | EuropeScoring | MiddleEastScoring | DuckAndCover | FiveYearPlan | TheChinaCard |
    SocialistGovernments | Fidel | VietnamRevolts | Blockade | KoreanWar | RomanianAbdication |
    ArabIsraeliWar | Comecon | Nasser | WarsawPactFormed | DeGaulleLeadsFrance | CapturedNaziScientist |   -- Early War
    TrumanDoctrine | OlympicGames | NATO | IndependentReds | MarshallPlan | IndoPakistaniWar |
    Containment | CIACreated | USJapanMutualDefencePact | SuezCrisis | EastEuropeanUnrest | Decolonization |
    RedScarePurge | UNIntervention | DeStalinization | NuclearTestBan | FormosanResolution | Defectors |
    TheCambridgeFive | SpecialRelationship | NORAD | -- Optional early war cards

    BrushWar | CentralAmericanScoring | SouthEastAsiaScoring | ArmsRace | CubanMissileCrisis | NuclearSubs |
    Quagmire | SaltNegotiations | BearTrap | Summit | HowILearnedToStopWorrying | Junta | KitchenDebates |
    MissileEnvy | WeWillBuryYou | BrezhnevDoctrine | PortugueseEmpireCrumbles | SouthAfricanUnrest | Allende |
    WillyBrandt | MuslimRevolution | ABMTreaty | CulturalRevolution | FlowerPower | U2Incident | OPEC |    -- Mid War
    LoneGunman | ColonialRearGuards | PanamaCanalReturned | CampDavidAccords | PuppetGovernments |
    GrainSalesToSoviets | JohnPaulIIElectedPope | LatinAmericanDeathSquads | OASFounded |
    NixonPlaysTheChinaCard | SadatExpelsSoviets | ShuttleDiplomacy | TheVoiceOfAmerica | LiberationTheology |
    UssuriRiverSkirmish | AskNotWhatYourCountry | AllianceForProgress | AfricaScoring | OneSmallStep |
    SouthAmericanScoring |
    Che | OurManInTehran | -- Optional mid war cards

    IranianHostageCrisis | TheIronLady | ReaganBombsLibya | StarWars | NorthSeaOil | TheReformer | MarineBarracksBombing |
    SovietsShootDownKAL007 | Glasnost | OrtegaElectedInNicaragua | Terrorism | IranContraScandal | Chernobyl |
    LatinAmericanDebtCrisis | TearDownThisWall | AnEvilEmpire | AldrichAmesRemix | PershingIIDeployed | -- Late War
    Wargames | Solidarity | IranIraqWar |
    YuriAndSamatha | AWACSSaleToSaudies -- Optional late war cards

  deriving (Eq, Ord, Enum, Bounded, Show)


makeLenses ''GameState
makeLenses ''GameLog
makeLenses ''PlayerState
