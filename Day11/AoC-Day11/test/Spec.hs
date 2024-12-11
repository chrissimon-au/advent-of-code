import Lib
import Test.HUnit

tests :: Test
tests = test [ (numberOfStonesAfter [0] 1) @?= 1               
             ]

main :: IO Counts
main = 
    runTestTT tests