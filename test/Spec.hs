{-# LANGUAGE OverloadedLists #-}
import Rules
import Test.Tasty
import Test.Tasty.HUnit
import qualified Data.Set as S

main :: IO ()
main = defaultMain $ testGroup "RulesTest" [unitTests]

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
        (usCards `S.union` ussrCards)

    , testCase "All cards minus us should equal neutral and ussr cards" $
      assertEqual "all - us == neutral ++ ussr"
        (allCards S.\\ usCards)
        (neutralCards `S.union` ussrCards)

    , testCase "All cards minus ussr should equal neutral and us cards" $
      assertEqual "all - ussr = neutral ++ us"
        (allCards S.\\ ussrCards)
        (neutralCards `S.union` usCards)


    , testCase "Card totals by side add up" $
      assertEqual "neutral ++ us ++ ussr == all "
        (S.unions [neutralCards, usCards, ussrCards])
        (allCards)

    , testCase "Card totals by type (op / scoring) add up" $
      assertEqual "scoring ++ op == all"
        (scoringCards `S.union` opsCards)
        (allCards)
   ]
