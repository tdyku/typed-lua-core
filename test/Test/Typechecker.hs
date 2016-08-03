module Test.Typechecker where

import Test.Hspec

tpTest :: Spec
tpTest = do
        it "number * integer should be number" $ do
            True