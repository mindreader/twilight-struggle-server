{-# LANGUAGE OverloadedLists #-}
module Countries where

import qualified Data.Set as S
import qualified Data.List as L
import Data.Monoid

import Types


centralAmerica, southAmerica, eastEurope, westEurope, middleEast, africa, asia, seAsia :: S.Set Country
centralAmerica = [Mexico .. DominicanRepublic]
southAmerica = [Venezuela .. Uruguay]
westEurope = [Finland, Austria] <> [Canada .. Turkey]
eastEurope = [Finland, Austria] <> [EastGermany .. Bulgaria]
middleEast = [Lebanon .. SaudiArabia]
asia = [Afghanistan .. Australia] <> seAsia
seAsia = [Burma .. Indonesia]
africa = [Morocco .. Sudan]

countriesByRegion :: Region -> S.Set Country
countriesByRegion CentralAmerica = centralAmerica
countriesByRegion SouthAmerica = southAmerica
countriesByRegion Europe = westEurope <> eastEurope
countriesByRegion WesternEurope = westEurope
countriesByRegion EasternEurope = eastEurope
countriesByRegion Africa = africa
countriesByRegion MiddleEast = middleEast
countriesByRegion Asia = asia
countriesByRegion SouthEastAsia = seAsia

allCountries :: S.Set Country
allCountries = [minBound .. maxBound]


countriesOfStability :: Int -> S.Set Country

countriesOfStability 1 =
  [Guatemala, ElSalvador, Nicaragua, Haiti, DominicanRepublic, Columbia, Lebanon, SaharanStates, Nigeria,
  Cameroon, Zaire, Zimbabwe, SEAfricanStates, Ethiopia, Sudan, LaosCambodia, Vietnam, Indonesia, Angola]

countriesOfStability 2 =
  [Mexico, Panama, Venezuela, Ecuador, Peru, Bolivia, Brazil, Paraguay, Uruguay, Argentina, Algeria,
  Tunisia, WestAfricanStates, IvoryCoast, Kenya, Botswana, Libya, Egypt, Iran, Jordan, Syria, Greece,
  Turkey, SpainPortugal, Italy, Afghanistan, Pakistan, Burma, Thailand, Malaysia, Phillippines, Honduras, Somalia]

countriesOfStability 3 =
  [Cuba, CostaRica, Chile, Morocco, SouthAfrica, SaudiArabia, Iraq, Bulgaria, Romania, Yugoslavia,
  Hungary, Czech, France, Benelux, EastGermany, Poland, Denmark, India, SouthKorea, NorthKorea, Taiwan, GulfStates]

countriesOfStability 4 = [Canada, Norway, Sweden, Finland, Austria, Israel, Japan, WestGermany, Australia]
countriesOfStability 5 = [UnitedKingdom]
countriesOfStability _ = []

countryStability :: Country -> Int
countryStability c =
  if (S.member c (countriesOfStability 1)) then 1 else
    if (S.member c (countriesOfStability 3)) then 3 else
      if (S.member c (countriesOfStability 4)) then 4 else
        if (S.member c (countriesOfStability 5)) then 5 else
          2

standardBattleGrounds :: S.Set Country
standardBattleGrounds =
  [Mexico, Cuba, Panama, Venezuela, Brazil, Chile, Argentina, Algeria, Nigeria, Angola, Zaire,
  SouthAfrica, France, WestGermany, EastGermany, Italy, Poland, Libya, Egypt, Israel, Iraq, Iran,
  Pakistan, India, Thailand, Japan, SouthKorea, NorthKorea]

-- For scoring in asia, for the US, if it controls taiwan while formosan resolution is in effect, taiwan is considered a battleground
battleGrounds :: scoreasia -> Player -> S.Set OngoingEffect -> S.Set Country
battleGrounds _ US effs | S.member (OgEffect FormosanResolution) effs = standardBattleGrounds <> [Taiwan]
                                   | otherwise                          = standardBattleGrounds
battleGrounds _ _ _ = standardBattleGrounds

nonbattleGrounds :: a -> Player -> S.Set OngoingEffect -> S.Set Country
nonbattleGrounds act ef pl = allCountries S.\\ battleGrounds act ef pl


