import Test.Hspec
-- http://hspec.github.io
import ATPPFHS


main :: IO ()
main = hspec $ do
    describe "ATPPFHS.eval" $ do
        it "evaluates truth value of a formula under a valuation" $ do
            eval (makeV [False,False]) (Nega (Fvar 1 :~> Nega (Fvar 2))) `shouldBe` False
            eval (makeV [False,True ]) (Nega (Fvar 1 :~> Nega (Fvar 2))) `shouldBe` False
            eval (makeV [True ,False]) (Nega (Fvar 1 :~> Nega (Fvar 2))) `shouldBe` False
            eval (makeV [True ,True ]) (Nega (Fvar 1 :~> Nega (Fvar 2))) `shouldBe` True
        it "evaluates a tautology to True under all valuations" $ do
            eval (makeV [False,False]) ax3 `shouldBe` True
            eval (makeV [False,True ]) ax3 `shouldBe` True
            eval (makeV [True ,False]) ax3 `shouldBe` True
            eval (makeV [True ,True ]) ax3 `shouldBe` True
    describe "ATPPFHS.FormulaS(show)(←how to express?)" $ do
        it "shows formulas in ``normal'' looks" $ do
            show ax1 `shouldBe` "(f₁ → (f₂ → f₁))"
            show ax2 `shouldBe` "((f₁ → (f₂ → f₃)) → ((f₁ → f₂) → (f₁ → f₃)))"
            show ax3 `shouldBe` "((¬f₂ → ¬f₁) → (f₁ → f₂))"
    describe "ATPPFHS.normalize :: FormulaS -> FormulaS" $ do
        it "对公式内的变元做换名，使得各个变元序号递增地第一次出现" $ do
            normalize ax3 `shouldBe` ( (Nega (Fvar 1) :~> Nega (Fvar 2)) :~> Fvar 2 :~> Fvar 1 )