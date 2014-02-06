module SymbolSpec where

import Test.Hspec
import Symbol

spec :: Spec
spec =
  describe "sizeOfType" $ do
    context "when passed SInt" $
      it "returns 4" $
        sizeOfType SInt `shouldBe` 4

    context "when passed SUndefined" $
      it "returns -1" $
        sizeOfType SUndefined `shouldBe` -1
