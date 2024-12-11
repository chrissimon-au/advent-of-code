import Lib
import Test.HUnit

tests :: Test
tests = test [ (numberOfStonesAfter [0] 1) @?= 1,
               (numberOfStonesAfter [0, 0] 1) @?= 2
             ]

main :: IO Counts
main = 
    runTestTT tests