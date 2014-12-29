
module Main where

import Data.Maybe
import Data.Either
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.Ingredients.Rerun

import Traduisons.Resources
import Traduisons.Util


main :: IO ()
main = defaultMainWithIngredients [rerunningTests defaultIngredients] tests

tests :: TestTree
tests = testGroup "Tests" [unitTests]

assertJust :: Maybe a -> Assertion
assertJust = assert . isJust

assertRight :: Either a b -> Assertion
assertRight = assert . isRight

unitTests :: TestTree
unitTests = testGroup "Unit tests" [apiTests, utilTests]

apiTests :: TestTree
apiTests = testGroup "API tests"
  [ testCase "Valid translation API URL" $
      assertRight (mkReq translationURL)
  , testCase "Valid language detection API URL" $
      assertRight (mkReq detectionURL)
  ]

utilTests :: TestTree
utilTests = testGroup "Util tests"
  [ testCase "Strip unicode BOM" $
    stripBOM "\65279CHECKCHECK" @?= "CHECKCHECK"
  , testCase "Strip no unicode BOM" $
    stripBOM "NO BOM HERE!" @?= "NO BOM HERE!"
  ]
