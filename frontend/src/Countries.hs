{-# LANGUAGE OverloadedLists #-}
module Countries (
  module Play,
  module Rules,
  module Countries
)
where

import Play
import Rules
import qualified Data.Map.Strict as M

data Offset = Offset !Int !Int deriving Show

influenceTest :: [(Country, (Int,Int))]
influenceTest = [(UnitedKingdom, (5,0)), (France, (3,2))]

countryLocs :: M.Map Country Offset
countryLocs = M.fromList $ zip [minBound .. maxBound] offsets
  where
    offsets =
     --  pixels from left, pixels from top
     [Offset 118 914,
      Offset 245 1018,
      Offset 198 1127,
      Offset 346 1118,
      Offset 502 988,
      Offset 332 1221,
      Offset 493 1119,
      Offset 496 1219,
      Offset 647 1081,
      Offset 791 1080,
      Offset 671 1233,
      Offset 590 1341,
      Offset 436 1386,
      Offset 520 1498,
      Offset 673 1591,
      Offset 927 1489,
      Offset 592 1713,
      Offset 758 1701,
      Offset 642 1908,
      Offset 804 1827,
      Offset 561 502,
      Offset 1126 382,
      Offset 1288 185,
      Offset 1321 287,
      Offset 1239 485,
      Offset 1213 604,
      Offset 1107 745,
      Offset 1489 273,
      Offset 1385 485,
      Offset 1409 691,
      Offset 1595 799,
      Offset 1859 704,
      Offset 1682 192,
      Offset 1448 587,
      Offset 1438 386,
      Offset 1590 386,
      Offset 1563 485,
      Offset 1595 587,
      Offset 1561 691,
      Offset 1742 587,
      Offset 1713 691,
      Offset 1774 805,
      Offset 1918 802,
      Offset 1749 904,
      Offset 1917 904,
      Offset 2060 905,
      Offset 1529 993,
      Offset 1679 1007,
      Offset 1844 1006,
      Offset 2010 1003,
      Offset 1969 1103,
      Offset 1144 933,
      Offset 1292 889,
      Offset 1444 876,
      Offset 1131 1064,
      Offset 1352 1100,
      Offset 1226 1257,
      Offset 1409 1242,
      Offset 1476 1357,
      Offset 1650 1407,
      Offset 1521 1527,
      Offset 1583 1793,
      Offset 1652 1682,
      Offset 1700 1577,
      Offset 1845 1501,
      Offset 1825 1364,
      Offset 1971 1276,
      Offset 1809 1232,
      Offset 1705 1127,
      Offset 2231 838,
      Offset 2235 968,
      Offset 2395 1036,
      Offset 2991 701,
      Offset 3021 803,
      Offset 3137 899,
      Offset 2957 1019,
      Offset 2970 1629,
      Offset 2575 1056,
      Offset 2717 1070,
      Offset 2804 1175,
      Offset 2657 1174,
      Offset 2725 1330,
      Offset 3021 1171,
      Offset 2967 1452]
