module FrontendPlay where


availableUSActions :: GameState -> [PlayerActionType]
availableUSActions gs = case gsGameMode gs of
  GPPlaceInitialInfluence US -> PlayerAction [PlaceInfluence 7 [ByRegion [WesternEurope]]]
  _ -> []

availableUSSRActions :: GameState -> [PlayerActionType]
availableUSSRActions = undefined

