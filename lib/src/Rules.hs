{-# LANGUAGE OverloadedLists, GeneralizedNewtypeDeriving #-}
module Rules where

import qualified Data.Set as S
import Data.Monoid

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

countriesOfCost 1 =
  [Guatemala, ElSalvador, Nicaragua, Haiti, DominicanRepublic, Columbia, Lebanon, SaharanStates, Nigeria,
  Cameroon, Zaire, Zimbabwe, SEAfricanStates, Ethiopia, Sudan, LaosCambodia, Vietnam, Indonesia, Angola]

countriesOfCost 2 =
  [Mexico, Panama, Venezuela, Ecuador, Peru, Bolivia, Brazil, Paraguay, Uruguay, Argentina, Algeria,
  Tunisia, WestAfricanStates, IvoryCoast, Kenya, Botswana, Libya, Egypt, Iran, Jordan, Syria, Greece,
  Turkey, SpainPortugal, Italy, Afghanistan, Pakistan, Burma, Thailand, Malaysia, Phillippines, Honduras, Somalia]

countriesOfCost 3 =
  [Cuba, CostaRica, Chile, Morocco, SouthAfrica, SaudiArabia, Iraq, Bulgaria, Romania, Yugoslavia,
  Hungary, Czech, France, Benelux, EastGermany, Poland, Denmark, India, SouthKorea, NorthKorea, Taiwan, GulfStates]

countriesOfCost 4 = [Canada, Norway, Sweden, Finland, Austria, Israel, Japan, WestGermany, Australia]
countriesOfCost 5 = [UnitedKingdom]
countriesOfCost _ = []


standardBattleGrounds :: S.Set Country
standardBattleGrounds =
  [Mexico, Cuba, Panama, Venezuela, Brazil, Chile, Argentina, Algeria, Nigeria, Angola, Zaire,
  SouthAfrica, France, WestGermany, EastGermany, Italy, Poland, Libya, Egypt, Israel, Iraq, Iran,
  Pakistan, India, Thailand, Japan, SouthKorea, NorthKorea]

-- For scoring in asia, for the US, if it controls taiwan while formosan resolution is in effect, taiwan is considered a battleground
battleGrounds :: scoreasia -> Player -> S.Set Effect -> S.Set Country
battleGrounds _ US effs | S.member EfFormosanResolution effs = standardBattleGrounds <> [Taiwan]
                                   | otherwise                          = standardBattleGrounds
battleGrounds _ _ _ = standardBattleGrounds

nonbattleGrounds :: a -> Player -> S.Set Effect -> S.Set Country
nonbattleGrounds act ef pl = allCountries S.\\ battleGrounds act ef pl

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

earlyWarCards, midWarCards, lateWarCards :: S.Set Card
earlyWarCards = [AsiaScoring .. NORAD]
midWarCards = [BrushWar .. OurManInTehran]
lateWarCards = [IranianHostageCrisis .. AWACSSaleToSaudies]

data CardAffiliation = AffUS | AffUSSR | AffNeutral

allCards :: S.Set Card
allCards = [minBound .. maxBound]

neutralCards, usCards, ussrCards :: S.Set Card
neutralCards =
  [AsiaScoring, EuropeScoring, MiddleEastScoring, TheChinaCard, CapturedNaziScientist, OlympicGames,
  IndoPakistaniWar, UNIntervention, RedScarePurge, NuclearTestBan, BrushWar, CentralAmericanScoring,
  SouthEastAsiaScoring, ArmsRace, CubanMissileCrisis, SaltNegotiations, Summit, HowILearnedToStopWorrying,
  Junta, MissileEnvy, ABMTreaty, LatinAmericanDeathSquads, AfricaScoring, OneSmallStep, SouthAmericanScoring,
  Terrorism, IranIraqWar, Wargames]

usCards =
  [DuckAndCover, FiveYearPlan, TrumanDoctrine, NATO, IndependentReds, MarshallPlan, Containment, CIACreated,
  USJapanMutualDefencePact, EastEuropeanUnrest, FormosanResolution, Defectors, NuclearSubs, BearTrap, KitchenDebates,
  ColonialRearGuards, PanamaCanalReturned, CampDavidAccords, PuppetGovernments, GrainSalesToSoviets, JohnPaulIIElectedPope,
  OASFounded, NixonPlaysTheChinaCard, SadatExpelsSoviets, ShuttleDiplomacy, TheVoiceOfAmerica, UssuriRiverSkirmish,
  AskNotWhatYourCountry, AllianceForProgress, TheIronLady, ReaganBombsLibya, StarWars, NorthSeaOil,
  SovietsShootDownKAL007, Chernobyl, TearDownThisWall, AnEvilEmpire, Solidarity, SpecialRelationship, NORAD, OurManInTehran,
  AWACSSaleToSaudies]

ussrCards =
  [SocialistGovernments, Fidel, VietnamRevolts, Blockade, KoreanWar, RomanianAbdication, ArabIsraeliWar, Comecon,
  Nasser, WarsawPactFormed, DeGaulleLeadsFrance, SuezCrisis, Decolonization, DeStalinization, Quagmire,
  WeWillBuryYou, BrezhnevDoctrine, PortugueseEmpireCrumbles, SouthAfricanUnrest, Allende, WillyBrandt,
  MuslimRevolution, CulturalRevolution, FlowerPower, U2Incident, OPEC, LoneGunman, LiberationTheology,
  IranianHostageCrisis, TheReformer, Glasnost, MarineBarracksBombing, OrtegaElectedInNicaragua, IranContraScandal,
  LatinAmericanDebtCrisis, AldrichAmesRemix, PershingIIDeployed, TheCambridgeFive, Che, YuriAndSamatha]

scoringCards, opsCards :: S.Set Card
scoringCards = [AsiaScoring, EuropeScoring, MiddleEastScoring, AfricaScoring, CentralAmericanScoring, SouthEastAsiaScoring, SouthAmericanScoring]
opsCards = allCards S.\\ scoringCards

cardsWithCostDef :: OpsValue -> S.Set Card

cardsWithCost :: S.Set Effect -> Player -> OpsValue -> S.Set Card
cardsWithCost effs pl cost = cardsWithCostDef (cost + costMod)
  where
    costMod = 0 + redscare + containment
    redscare = if S.member (EfRedScare pl) effs then (-1) else 0
    containment = if S.member EfContainment effs && pl == US then (1) else 0

cardsWithCostDef 0 = scoringCards
cardsWithCostDef 1 =
  [Blockade, RomanianAbdication, Nasser, CapturedNaziScientist, TrumanDoctrine, CIACreated, UNIntervention,
  Summit, KitchenDebates, Allende, LoneGunman, PanamaCanalReturned, OASFounded, SadatExpelsSoviets]
cardsWithCostDef 2 =
  [VietnamRevolts, Fidel, KoreanWar, ArabIsraeliWar, OlympicGames, IndependentReds, IndoPakistaniWar, Decolonization,
  FormosanResolution, Defectors, NuclearSubs, HowILearnedToStopWorrying, Junta, MissileEnvy,
  PortugueseEmpireCrumbles, SouthAfricanUnrest, WillyBrandt, ColonialRearGuards, CampDavidAccords,
  PuppetGovernments, GrainSalesToSoviets, JohnPaulIIElectedPope, NixonPlaysTheChinaCard, TheVoiceOfAmerica, LiberationTheology,
  OneSmallStep, ReaganBombsLibya, MarineBarracksBombing, OrtegaElectedInNicaragua, Terrorism, IranContraScandal,
  LatinAmericanDebtCrisis, Solidarity, IranIraqWar, TheCambridgeFive, SpecialRelationship, OurManInTehran, YuriAndSamatha, LatinAmericanDeathSquads, StarWars]
cardsWithCostDef 3 =
  [DuckAndCover, FiveYearPlan, SocialistGovernments, Comecon, WarsawPactFormed, DeGaulleLeadsFrance, Containment,
  SuezCrisis, EastEuropeanUnrest, DeStalinization, BrushWar, ArmsRace, CubanMissileCrisis, Quagmire, SaltNegotiations,
  BearTrap, BrezhnevDoctrine, CulturalRevolution, U2Incident, OPEC, ShuttleDiplomacy, UssuriRiverSkirmish,
  AskNotWhatYourCountry, AllianceForProgress, IranianHostageCrisis, TheIronLady, TheReformer, Chernobyl, TearDownThisWall,
  AnEvilEmpire, AldrichAmesRemix, PershingIIDeployed, NORAD, Che, AWACSSaleToSaudies, NorthSeaOil]
cardsWithCostDef 4 =
  [TheChinaCard, NATO, MarshallPlan, USJapanMutualDefencePact, RedScarePurge, NuclearTestBan, MuslimRevolution, ABMTreaty,
  FlowerPower, SovietsShootDownKAL007, Glasnost, Wargames, WeWillBuryYou]
cardsWithCostDef _ = []


data Effect = EfFormosanResolution | EfRedScare Player | EfContainment | EfNATO | EfMissileEnvy -- TODO a bunch more
  deriving (Eq, Ord, Show)


data DefCon = DFFive | DFFour | DFThree | DFTwo | NuclearWar deriving Show

newtype MilOps = MilOps Int deriving Show            -- required military ops 0..5
newtype Turn = Turn Int deriving Show                -- There are 10 turns in this game
newtype ActionRound = ActionRound Int deriving Show  -- There are between 6 and 8 action rounds per turn


newtype Points = Points Int deriving (Eq, Ord, Show) -- Ranges from -20 to 20 (-20 is USSR win, 20 is US win)
newtype SpacePos = SpacePos Int deriving (Eq, Ord, Show) -- TODO Ranges from 0 to something.

data Player = US | USSR deriving (Eq, Ord, Show)

data Region = WesternEurope | EasternEurope | Europe | Asia | SouthEastAsia | Africa | CentralAmerica | SouthAmerica | MiddleEast deriving Show

newtype OpsValue = OpsValue Int deriving (Show, Num, Eq)

newtype Influence = Influence Int deriving (Show, Num, Eq)

-- Effect of something the player has done.
-- data PlayerAction = PlayCard Card | PlaceInfluence [Country] OpsValue | Coup Country OpsValue | Realign Country OpsValue | Score Region | SpaceAttempt Card deriving Show
