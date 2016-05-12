{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ViewPatterns #-}
module Score where

import qualified Data.Map as M
import qualified Data.Set as S
import Data.Monoid

import Types
import Countries


filterRegion :: Region -> M.Map Country (Influence, Influence) -> M.Map Country (Influence, Influence)
filterRegion region = M.filterWithKey (\c _ -> S.member c (countriesByRegion region))

scoreRegion :: Region -> M.Map Country (Influence, Influence) -> ScoreResult
scoreRegion CentralAmerica inf =
  let
      uspresence = undefined
      ussrpresence = undefined
  in ScoreResult $ undefined

  where
    infByRegion = filterRegion CentralAmerica inf

    usbg = undefined
    ussrgs = undefined

    usnonbg = undefined
    ussrnonbg = undefined

    presencep = 1
    dominationp = 3
    contolp = 5

presence :: M.Map Country (Influence, Influence) -> (Bool, Bool)
presence inf = (M.null $ fmap fst inf, M.null $ fmap snd inf)

domination :: M.Map Country Influence -> (Bool, Bool)
domination = undefined
control :: M.Map Country Influence -> Bool
control = undefined

