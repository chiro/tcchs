module SymbolSpec where

import Test.Hspec
import Symbol

spec :: Spec
spec = do
  describe "sizeOfType" $ do
    context "when passed SInt" $ do
      it "returns 4" $ do
        sizeOfType SInt `shouldBe` 4

    context "when passed SUndefined" $ do
      it "returns -1" $ do
        sizeOfType SUndefined `shouldBe` -1
