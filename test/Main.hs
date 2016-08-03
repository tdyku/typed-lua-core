import Test.Hspec

import Test.Parser
import Test.Typechecker


main :: IO ()
main = hspec $ do 
    describe "Testing parser" parserTest
    describe "Testing typechecker" tpTest
