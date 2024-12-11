import Lib
import Test.HUnit

tests :: Test
tests = test [ (numberOfStonesAfter [0] 1) @?= 1,
               (numberOfStonesAfter [0, 0] 1) @?= 2,
               (numberOfStonesAfter [10] 1) @?= 2,
               (numberOfStonesAfter [1] 1) @?= 1,
               (numberOfStonesAfter [1000] 2) @?= 3
             ]

main :: IO Counts
main = 
    runTestTT tests