{-# LANGUAGE OverloadedLists #-}
module Rules where

import qualified Data.Set as S
import Data.Monoid

data Country =
  Mexico | Guatemala | ElSalvador | Honduras | Cuba | CostaRica | Nicaragua | Panama | Haiti | DominicanRepublic | -- Central America
  Venezuela | Columbia | Ecuador | Peru | Bolivia | Brazil | Chile | Paraguay | Argentina | Uruguay | -- South America
  Canada | UnitedKingdom | Norway | Denmark | Benelex | France | SpainPortugal | Sweden | WestGermany | Italy | Greece | Turkey | -- West Europe
  Finland | Austria | -- Both East and West Europe
  EastGermany | Poland | Czech | Hungary | Yugoslavaia | Romania | Bulgaria | -- East Europe
  Lebanon | Syria | Israel | Iraq | Iran | Libya | Egypt | Jordan | GulfStates | SaudiArabia | -- Middle East
  Morocco | Algeria | Tunisia | WestAfricanStates | SaharanStates | IvoryCoast | Nigeria | Camaroon | Zaire | Angola | SouthAfrica | Botswana | Zimbabwe | SEAfricanStatse | Kenya | Somalia | Ethiopia | Sudan | -- Africa
  Afghanistan | Pakistan | India | NorthKorea | SouthKorea | Japan | Taiwan | Australia | -- Asia
  Burma | LaosCambodia | Vietnam | Thailand | Malaysia | Phillippines | Indonesia -- SE Asia
  deriving (Show, Eq, Ord, Enum, Bounded)

centralAmerica, southAmerica, eastEurope, westEurope, middleEast, africa, asia, seAsia :: S.Set Country
centralAmerica = [Mexico .. DominicanRepublic]
southAmerica = [Venezuela .. Uruguay]
westEurope = [Finland, Austria] <> [Canada .. Turkey]
eastEurope = [Finland, Austria] <> [EastGermany .. Bulgaria]
middleEast = [Lebanon .. SaudiArabia]
asia = [Afghanistan .. Australia] <> seAsia
seAsia = [Burma .. Indonesia]
africa = [Morocco .. Sudan]

allCountries :: S.Set Country
allCountries = [minBound .. maxBound]

countriesOfCost :: Int -> S.Set Country
countriesOfCost 1 = [Guatemala, ElSalvador, Nicaragua, Haiti, DominicanRepublic, Columbia, Lebanon, SaharanStates, Nigeria, Camaroon, Zaire, Zimbabwe, SEAfricanStatse, Ethiopia, Sudan, LaosCambodia, Vietnam, Indonesia, Angola]
countriesOfCost 2 = [Mexico, Panama, Venezuela, Ecuador, Peru, Bolivia, Brazil, Paraguay, Uruguay, Argentina, Algeria, Tunisia, WestAfricanStates, IvoryCoast, Kenya, Botswana, Libya, Egypt, Iran, Jordan, Syria, Greece, Turkey, SpainPortugal, Italy, Afghanistan, Pakistan, Burma, Thailand, Malaysia, Phillippines, Honduras, Somalia]
countriesOfCost 3 = [Cuba, CostaRica, Chile, Morocco, SouthAfrica, SaudiArabia, Iraq, Bulgaria, Romania, Yugoslavaia, Hungary, Czech, France, Benelex, EastGermany, Poland, Denmark, India, SouthKorea, NorthKorea, Taiwan, GulfStates]
countriesOfCost 4 = [Canada, Norway, Sweden, Finland, Austria, Israel, Japan, WestGermany, Australia]
countriesOfCost 5 = [UnitedKingdom]
countriesOfCost _ = []

-- TODO, sometimes Taiwan is a battleground when [Rules in play] contains Formosan Resolution
battleGrounds :: S.Set Effect -> S.Set Country
battleGrounds ef = [Mexico, Cuba, Panama, Venezuela, Brazil, Chile, Argentina, Algeria, Nigeria, Angola, Zaire, SouthAfrica, France, WestGermany, EastGermany, Italy, Poland, Libya, Egypt, Israel, Iraq, Iran, Pakistan, India, Thailand, Japan, SouthKorea, NorthKorea]

-- TODO, usually Taiwan isn't a battleground
nonbattleGrounds :: S.Set Effect -> S.Set Country
nonbattleGrounds ef = allCountries S.\\ battleGrounds ef

data Card =
    AsiaScoring | EuropeScoring | MiddleEastScoring | DuckAndCover | FiveYearPlan | TheChinaCard |
    SocialistGovernments | Fidel | VietnamRevolts | Blockade | KoreanWar | RomanianAbdication |
    ArabIsraeliWar | Comecon | Nasser | WarsawPactFormed | DeGaulleLeadsFrance | CapturedNaziScientist |   -- Early War
    TrumanDoctrine | OlympicGames | NATO | IndependentReds | MarshalPlan | IndoPakistaniWar |
    Containment | CIACreated | USJapanMutualDefencePact | SuezCrisis | EastEuropeanUnrest | Decolonization |
    RedScarePurge | UNIntervention | DeStalinization | NuclearTestBan | FormosanResolution | Defectors |
    TheCambridgeFive | SpecialRelationship | NORAD | -- Optional early war cards

    BrushWar | CentralAmericanScoring | SouthEastAsiaScoring | ArmsRace | CubanMissileCrisis | NuclearSubs |
    Quagmire | SaltNegotiations | BearTrap | Summit | HowILearnedToStopWorrying | Junta | KitchenDebates |
    MissileEnvy | WeWillBuryYou | BrezhnevDoctrine | PortugueseEmpireCrumbles | SouthAfricanUnrest | Allende |
    WillyBrandt | MuslimRevolution | AMBTreaty | CulturalRevolution | FlowerPower | U2Incident | OPEC |    -- Mid War
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

earlyWarCards, midWarCards, lateWarCards :: S.Set Card
earlyWarCards = [AsiaScoring .. NORAD]
midWarCards = [BrushWar .. OurManInTehran]
lateWarCards = [IranianHostageCrisis .. AWACSSaleToSaudies]

data CardAffiliation = AffUS | AffUSSR | AffNeutral

allCards :: S.Set Card
allCards = [minBound .. maxBound]

neutralCards, usCards, ussrCards :: S.Set Card
neutralCards =
  [AsiaScoring, EuropeScoring, MiddleEastScoring, TheChinaCard, CapturedNaziScientist, OlympicGames, IndoPakistaniWar, UNIntervention, RedScarePurge, NuclearTestBan, BrushWar, CentralAmericanScoring,
  SouthEastAsiaScoring, ArmsRace, CubanMissileCrisis, SaltNegotiations, Summit, HowILearnedToStopWorrying, Junta, MissileEnvy, AMBTreaty, LatinAmericanDeathSquads,
  AfricaScoring, OneSmallStep, SouthAmericanScoring, Terrorism, IranIraqWar, Wargames]
usCards =
  [DuckAndCover, FiveYearPlan, TrumanDoctrine, NATO, IndependentReds, MarshalPlan, Containment, CIACreated, USJapanMutualDefencePact, EastEuropeanUnrest, FormosanResolution, Defectors,
  NuclearSubs, BearTrap, KitchenDebates, ColonialRearGuards, PanamaCanalReturned, CampDavidAccords, PuppetGovernments, GrainSalesToSoviets, JohnPaulIIElectedPope, OASFounded, NixonPlaysTheChinaCard, SadatExpelsSoviets, ShuttleDiplomacy, TheVoiceOfAmerica, UssuriRiverSkirmish, AskNotWhatYourCountry, AllianceForProgress, TheIronLady, ReaganBombsLibya, StarWars, NorthSeaOil, SovietsShootDownKAL007, Chernobyl, TearDownThisWall, AnEvilEmpire, Solidarity, SpecialRelationship, NORAD, OurManInTehran, AWACSSaleToSaudies]
ussrCards =
  [SocialistGovernments, Fidel, VietnamRevolts, Blockade, KoreanWar, RomanianAbdication, ArabIsraeliWar, Comecon, Nasser, WarsawPactFormed, DeGaulleLeadsFrance, SuezCrisis, Decolonization, DeStalinization, Quagmire, WeWillBuryYou, BrezhnevDoctrine, PortugueseEmpireCrumbles, SouthAfricanUnrest, Allende, WillyBrandt, MuslimRevolution, CulturalRevolution, FlowerPower, U2Incident, OPEC, LoneGunman, LiberationTheology, IranianHostageCrisis, TheReformer, Glasnost, MarineBarracksBombing, OrtegaElectedInNicaragua, IranContraScandal, LatinAmericanDebtCrisis, AldrichAmesRemix, PershingIIDeployed, TheCambridgeFive, Che, YuriAndSamatha]

scoringCards, opsCards :: S.Set Card
scoringCards = [AsiaScoring, EuropeScoring, AfricaScoring, CentralAmericanScoring, SouthEastAsiaScoring, SouthAmericanScoring]
opsCards = allCards S.\\ scoringCards

-- TODO the cost of cards change depending on effects, and who owns the card :: [Effect] -> Player -> Int -> [Card]
cardsWithCost :: S.Set Effect -> Player -> Int -> S.Set Card
cardsWithCost ef x = undefined
  where
    cardsWithCost' 0 = scoringCards
    cardsWithCost' 1 = []
    cardsWithCost' 2 = []
    cardsWithCost' 3 = []
    cardsWithCost' 4 = []

data Effect = EfFormosanResolution | EfRedScare Player | EfNATO -- TODO a bunch more
  deriving Show


data DefCon = DFFive | DFFour | DFThree | DFTwo | NuclearWar deriving Show

newtype MilOps = MilOps Int deriving Show            -- required military ops 0..5
newtype Turn = Turn Int deriving Show                -- There are 10 turns in this game
newtype ActionRound = ActionRound Int deriving Show  -- There are between 6 and 8 action rounds per turn

data GameState = GameState {
  gsTurn :: Turn,
  gsActionRound :: ActionRound,
  gsPhase :: Player, -- Play alternates each action round starting with USSR

  gsDefCon :: DefCon,

  gsDeck :: S.Set Card, -- Cards yet to be drawn
  gsDiscard :: S.Set Card, -- Cards that have been discarded
  gsRemovedFromGame :: S.Set Card, -- Cards that happen once, then go away forever

  gsCurrentEffects :: S.Set Effect, -- Cards that have been played and have lasting effects

  gsUSState :: PlayerState,
  gsUSSRState :: PlayerState

} deriving Show


data PlayerState = PlayerState {
  psHand :: S.Set Card,
  psMilOps :: MilOps
} deriving Show

data Player = US | USSR deriving Show

data Action = PlaceInfluence Int | Coup Country | Realign Country deriving Show

someFunc :: IO ()
someFunc = putStrLn "someFunc"
