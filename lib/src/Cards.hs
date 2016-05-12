{-# LANGUAGE OverloadedLists #-}
module Cards where

import qualified Data.Set as S
import Data.Monoid

import Types

earlyWarCards, midWarCards, lateWarCards :: S.Set Card
earlyWarCards = [AsiaScoring .. NORAD]
midWarCards = [BrushWar .. OurManInTehran]
lateWarCards = [IranianHostageCrisis .. AWACSSaleToSaudies]

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

cardsWithCost :: S.Set OngoingEffect -> Player -> OpsValue -> S.Set Card
cardsWithCost effs pl cost = cardsWithCostDef (cost + costMod)
  where
    costMod = 0 + redscare + containment
    redscare = if S.member (OgEffectOnPlayer RedScarePurge pl) effs then (-1) else 0
    containment = if S.member (OgEffect Containment) effs && pl == US then (1) else 0

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



-- | Actions that are possible to be performed by each player based on the current game state.
{-
 -
 - TODO this needs to be on the frontend and server, doing slightly different things.
 - Once I write them both I can find the similarities and put the common stuff here.
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
-}

{-
-- When you play this card, this is a list of effects that may occur, in order
cardEffect :: Card -> [Effect]
cardEffect CentralAmericanScoring = [Score CentralAmerica]
cardEffect AsiaScoring = [Score Asia]
cardEffect EuropeScoring = [Score Europe]
cardEffect SouthEastAsiaScoring = [Score SouthEastAsia]
cardEffect AfricaScoring = [Score Africa]
cardEffect MiddleEastScoring = [Score MiddleEast]

cardEffect TheChinaCard = [ConductOps 5 (Region Asia) :|: ConductOps 4 (NotRegion Asia)]

cardEffect DuckAndCover = [ChangeDefcon (-1), AdjustVPsByDefcon US (\(DefCon d) -> 5 - d)]

 -- play the card discarded if it belongs to US or neutral (but not scoring)
cardEffect FiveYearPlan = [DiscardCardRandomly USSR (not . flip S.member (ussrCards <> scoringCards))]

cardEffect SocialistGovernments = [RemoveInfluenceFromRegion WesternEurope 3 2]
cardEffect Fidel = [RemoveAllInfluence US Cuba, GainControlOf USSR Cuba]
cardEffect VietnamRevolts = [AddInfluence USSR Vietnam 2, CauseOngoingEffect VietnamRevolts]
cardEffect Blockade = [WhenUnsuccessful (DiscardCard US (CardOpsGreaterThanOrEqual 3))
  [RemoveAllInfluence US WestGermany]
  ]
cardEffect KoreanWar = [WhenSuccessful InvadeSouthKorea [FlipInfluenceTo USSR SouthKorea], AddMilOps USSR 2, FlowerPowerCheck]
-}
