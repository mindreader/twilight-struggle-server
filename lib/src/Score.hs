{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ViewPatterns #-}
module Score where

import qualified Data.Map as M
import qualified Data.Set as S
import Data.Monoid
import Data.Tuple

import Types
import Countries


filterRegion :: Region -> M.Map Country (Influence, Influence) -> M.Map Country (Influence, Influence)
filterRegion region = M.filterWithKey (\c _ -> S.member c (countriesByRegion region))

regionControl :: Region -> M.Map Country (Influence, Influence) -> (RegionScore, RegionScore)
regionControl region (filterRegion region -> inf) = undefined
  where

    domination couns bgs oppcouns = bgs >= regionbgs && couns >= regionbgs + 1 && couns > oppcouns
    control couns bgs oppcouns = bgs >= regionbgs && couns >= regionbgs + 1 && couns > oppcouns
    presence couns = couns >= 1

    regionbgs = length $ standardBattleGrounds S.\\ countriesByRegion region

    uscouns, ussrcouns :: Int
    uscouns = length $ countriesBy US inf
    ussrcouns = length $ countriesBy USSR inf

    usbgs, ussrbgs :: Int
    usbgs = length $ bgCountriesBy US inf
    ussrbgs = length $ bgCountriesBy USSR inf


--    presencep = 1
--    dominationp = 3
--    contolp = 5

--presence :: M.Map Country (Influence, Influence) -> (Bool, Bool)
--presence inf = (not $ null uscoun, not $ null ussrcoun)
--  where


cur :: M.Map Country (Influence, Influence)
cur = [
  (France, (1,4)),
  (UnitedKingdom, (5,0)),
  (SpainPortugal, (2, 0)),
  (WestGermany, (5,0)),
  (Italy, (6,3))]

{-
domination :: M.Map Country (Influence, Influence) -> (Bool, Bool)
domination inf =
  (usbgs > ussrbgs && uscoun > ussrcoun,
   ussrbgs > usbgs && ussrcoun > uscoun)
  where
    usbgs, ussrbgs :: Int
    -- | battlegrounds

    -- | normal countries
    uscoun, ussrcoun :: Int
    uscoun = length $ countriesBy US inf
    ussrcoun = length $ countriesBy USSR inf
-}
{-
control :: Region -> M.Map Country (Influence, Influence) -> (Bool, Bool)
control region inf = 
  (usbgs >= bgs && uscouns >= bgs + 1 && uscouns > ussrcouns, -- has all bgs and has at least one non bg (implied by having bgs + 1)
   ussrbgs >= bgs && ussrcouns >= bgs + 1 && ussrcouns > uscouns) -- and you have more countries overall than your opponent
  where
    regionbgs :: S.Set Country
    regionbgs = standardBattleGrounds S.\\ countriesByRegion region

    bgs :: Int
    bgs = length regionbgs

    uscouns, ussrcouns :: Int
    uscouns = length $ countriesBy US inf
    ussrcouns = length $ countriesBy USSR inf

    usbgs = length $ bgCountriesBy US inf
    ussrbgs = length $ bgCountriesBy USSR inf
-}

-- TODO, deal with temporary battlegrounds
countriesByAndWho by who =
  let op = case who of US -> id; USSR -> swap
  in S.fromList . M.keys . M.filterWithKey (\c inf -> S.member c standardBattleGrounds && fst inf - snd inf >= countryStability c) . fmap op

bgCountriesBy, countriesBy :: Player -> M.Map Country (Influence, Influence) -> S.Set Country
bgCountriesBy = countriesByAndWho $ \c inf -> S.member c standardBattleGrounds && fst inf - snd inf >= countryStability c
countriesBy = countriesByAndWho $ \c inf -> fst inf - snd inf >= countryStability c


