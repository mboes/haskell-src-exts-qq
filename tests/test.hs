import Language.Haskell.Exts.QQ

import Test.Hspec

main :: IO ()
main = hspec $ do
    describe "Term splices" $ do
      it "Substitutes for splices in atomic terms" $ do
        (\x -> [hs| $x |]) [hs| 10 |] `shouldBe` [hs| 10 |]
      it "Substitutes for splices in composite terms" $ do
        (\x -> [hs| $x + $x |]) [hs| 10 |] `shouldBe` [hs| 10 + 10 |]

    describe "type splices" $ do
      it "Substitutes for splices in atomic types" $ do
        (\x -> [hs| () :: ((x)) |]) [ty| () |] `shouldBe` [hs| () :: () |]
      it "Substitutes for splices in composite types" $ do
        (\x -> [hs| ((), ()) :: (((x)), ((x))) |]) [ty| () |]
            `shouldBe` [hs| ((), ()) :: ((), ()) |]
