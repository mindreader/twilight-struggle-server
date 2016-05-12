{-# LANGUAGE OverloadedLists #-}
import Rules
import Test.Tasty
import Test.Tasty.HUnit
import qualified Data.Set as S
import Data.Monoid

main :: IO ()
main = defaultMain $ testGroup "Twilight Struggle Tests" [unitTests]

unitTests = testGroup "Rules Unit Tests"
  [ testCase "Country values are correct" $
      assertEqual "all - (cost [1..4]) == cost 5"
        (allCountries S.\\ S.unions [countriesOfCost 1, countriesOfCost 2, countriesOfCost 3, countriesOfCost 4])
        (countriesOfCost 5)

  
    , testCase "Country costs add up to total" $
      assertEqual "cost [1..5] == all"
        (S.unions [countriesOfCost 1, countriesOfCost 2, countriesOfCost 3, countriesOfCost 4, countriesOfCost 5])
        (allCountries)


    , testCase "All cards minus early and mid should equal late war cards" $
      assertEqual "all - early - late == mid"
        ((allCards S.\\ earlyWarCards) S.\\ lateWarCards)
        (midWarCards)

    , testCase "All cards minus early and late should equal mid war cards" $
      assertEqual "all - early - mid == late"
        ((allCards S.\\ earlyWarCards) S.\\ midWarCards)
        (lateWarCards)

    , testCase "All cards minus mid and late should equal early war cards" $
      assertEqual "all - mid - late == early"
        ((allCards S.\\ midWarCards) S.\\ lateWarCards)
        (earlyWarCards)

    , testCase "All cards minus neutral should equal us and ussr cards" $
      assertEqual "all - neutral == us ++ ussr"
        (allCards S.\\ neutralCards)
        (usCards <> ussrCards)

    , testCase "All cards minus us should equal neutral and ussr cards" $
      assertEqual "all - us == neutral ++ ussr"
        (allCards S.\\ usCards)
        (neutralCards <> ussrCards)

    , testCase "All cards minus ussr should equal neutral and us cards" $
      assertEqual "all - ussr = neutral ++ us"
        (allCards S.\\ ussrCards)
        (neutralCards <> usCards)


    , testCase "Card totals by side add up" $
      assertEqual "neutral ++ us ++ ussr == all "
        (S.unions [neutralCards, usCards, ussrCards])
        (allCards)

    , testCase "Card totals by type (op / scoring) add up" $
      assertEqual "scoring ++ op == all"
        (scoringCards <> opsCards)
        (allCards)

    , testCase "Card cost adds up" $
      assertEqual "cards 0 ++ cards 1 ++ cards 2 ++ cards 3 ++ card 4 ++ cards 5 == all"
        (cardsWithCostDef 0 <> cardsWithCostDef 1 <> cardsWithCostDef 2 <> cardsWithCostDef 3 <> cardsWithCostDef 4 <> cardsWithCostDef 5)
        (allCards)

    , testCase "Ensure all cards have a cost" $
      assertEqual "all cards - cardswithcost [0..5] == []"
        (allCards S.\\ cardsWithCostDef 0 S.\\ cardsWithCostDef 1 S.\\ cardsWithCostDef 2 S.\\ cardsWithCostDef 3 S.\\cardsWithCostDef 4 S.\\ cardsWithCostDef 5)
        (S.empty)


   ]
