import Lib
import Test.HUnit

tests = test [ "Checking Test Infra" ~: assertBool "Should be true" check
             ]

main :: IO Counts
main = 
    runTestTT tests