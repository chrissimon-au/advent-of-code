import Lib
import Test.HUnit

tests :: Test
tests = test [ (getValue "asdf") @? "Simple Bool Check",
               (getValue "asd") @?= True,
               (getValue "") @?= False
             ]

main :: IO Counts
main = 
    runTestTT tests