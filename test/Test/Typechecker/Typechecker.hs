module Test.Typechecker.Typechecker where


import Test.Hspec

import Test.Typechecker.Subtyping

typecheckerTest :: Spec
typecheckerTest = do 
	describe "Subtyping" $ do
		literalsTest
		basicTest