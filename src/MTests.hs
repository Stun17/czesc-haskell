module MTests where

import Test.HUnit
import Decisions
import Game

f1 = [(1,9),(1,2),(3,2),(1,6),(1,12),(3,6),(1,8)]  -- exactly 5 elems
f2 = [(1,9),(2,5),(3,2),(4,2),(3,12),(3,6),(1,2)]  -- less than 5 elems
f3 = [(1,9),(1,2),(3,2),(1,6),(1,12),(1,7),(1,8)]  -- more than 5 elems

testflush :: IO Counts
testflush = runTestTT $ TestLabel "testing unit flush" $ TestList
  [ TestCase $ assertBool "exactly 5 elems"     (isFlush f1)
  , TestCase $ assertBool "less than 5 elems"   (not $ isFlush f2)
  , TestCase $ assertBool "more than 5 elems"   (isFlush f3)
  ]

s9, s8, s7 :: [Card]
s9 = [(1,10),(1,3),(3,9),(1,4),(1,6),(3,8),(1,7)]  -- exactly 5 elems
s8 = [(1,9),(2,5),(3,2),(4,2),(3,12),(3,6),(1,2)]  -- less than 5 elems
s7 = [(1,9),(1,8),(3,7),(1,6),(1,5),(1,4),(3,3)]   -- more than 5 elems

tstraight :: IO Counts
tstraight = runTestTT $ TestLabel "testing unit straight" $ TestList
  [ TestCase $ assertEqual "test straight exactly   5"  True  (isStraight s9)
  , TestCase $ assertEqual "test straight less than 5"  False (isStraight s8)
  , TestCase $ assertBool  "test straight more than 5"  (isStraight s7)
  ]

tc1 = [(1,9),(2,2),(3,2),(4,2),(3,12),(3,6),(1,2)]
tc2 = [(1,9),(2,5),(3,2),(4,2),(3,12),(3,6),(1,2)]

tcare :: IO Counts
tcare = runTestTT $ TestLabel "testing unit care" $ TestList
  [ TestCase $ assertEqual "test care with"     True  (isCare tc1)
  , TestCase $ assertEqual "test care without"  False (isCare tc2)
  ]

main = tstraight >>= print >> testflush >>= print >> tcare >>= print
