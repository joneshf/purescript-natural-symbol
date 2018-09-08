module Test.Main
  ( main
  ) where

import Prelude

import Data.Symbol (SProxy(..))
import Effect (Effect)
import Test.Spec (describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Reporter.Spec as Test.Spec.Reporter.Spec
import Test.Spec.Runner (Reporter)
import Test.Spec.Runner as Test.Spec.Runner
import Type.Natural.Symbol (reflectNatural)

main ::
  Effect Unit
main = Test.Spec.Runner.run reporters do
  describe "reflectNatural" do
    it "should work for single digits" do
      reflectNatural (SProxy :: SProxy "0") `shouldEqual` 0
      reflectNatural (SProxy :: SProxy "1") `shouldEqual` 1
      reflectNatural (SProxy :: SProxy "2") `shouldEqual` 2
      reflectNatural (SProxy :: SProxy "3") `shouldEqual` 3
      reflectNatural (SProxy :: SProxy "4") `shouldEqual` 4
      reflectNatural (SProxy :: SProxy "5") `shouldEqual` 5
      reflectNatural (SProxy :: SProxy "6") `shouldEqual` 6
      reflectNatural (SProxy :: SProxy "7") `shouldEqual` 7
      reflectNatural (SProxy :: SProxy "8") `shouldEqual` 8
      reflectNatural (SProxy :: SProxy "9") `shouldEqual` 9
    it "should work for multiple digits" do
      reflectNatural (SProxy :: SProxy "10") `shouldEqual` 10
      reflectNatural (SProxy :: SProxy "321") `shouldEqual` 321

reporters ::
  Array Reporter
reporters = [ Test.Spec.Reporter.Spec.specReporter
            ]
