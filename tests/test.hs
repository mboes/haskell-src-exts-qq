import Language.Haskell.Exts.QQ

import qualified Language.Haskell.Exts as Hs
import Test.Hspec

main :: IO ()
main = hspec $ do
    describe "Term splices" $ do
      it "Substitutes for splices in atomic terms" $ do
        (\x -> [hs| $x |]) [hs| 10 |] `shouldBe` [hs| 10 |]
      it "Substitutes for splices in composite terms" $ do
        (\x -> [hs| $x + $x |]) [hs| 10 |] `shouldBe` [hs| 10 + 10 |]

    describe "Pattern name splices" $ do
      it "Substitutes for splices in atomic patterns" $ do
        (\x -> [hs| case 10 of ((x)) -> y |]) (Hs.name "y")
            `shouldBe` [hs| case 10 of y -> y |]
      it "Substitutes for splices in composite patterns" $ do
        (\x -> [hs| let (Just ((x)), x) = (Just 10, 10) in y |]) (Hs.name "y")
            `shouldBe` [hs| let (Just y, x) = (Just 10, 10) in y |]

    describe "type splices" $ do
      it "Substitutes for splices in atomic types" $ do
        (\x -> [hs| () :: ((x)) |]) [ty| () |] `shouldBe` [hs| () :: () |]
      it "Substitutes for splices in composite types" $ do
        (\x -> [hs| ((), ()) :: (((x)), ((x))) |]) [ty| () |]
            `shouldBe` [hs| ((), ()) :: ((), ()) |]
