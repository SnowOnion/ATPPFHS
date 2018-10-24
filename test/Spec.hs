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



-- 用这两个代换试一试 substituteManyWithFunction。
-- sTest1 把 v1 替换成 v1 → v3，其他命题变号不管；sTest2替换得更多些
sTest1 :: Id -> Maybe FormulaS
sTest1 1 = Just (Fvar 1 :~> Fvar 3)
sTest1 _ = Nothing

sTest2 :: Id -> Maybe FormulaS
sTest2 1 = Just (Fvar 1 :~> Fvar 2 :~> Fvar 3)
sTest2 2 = Just (Fvar 3 :~> Fvar 3)
sTest2 _ = Nothing

sTest3 :: Id -> Maybe FormulaS
sTest3 1 = Just (Fvar 1 :~> Fvar 2 :~> Fvar 1)
sTest3 3 = Just (Fvar 2 :~> Nega (Fvar 4))
sTest3 _ = Nothing

test_substituteManyWithFunction 1 = substituteManyWithFunction sTest1 ax1
test_substituteManyWithFunction 2 = substituteManyWithFunction sTest2 ax1
test_substituteManyWithFunction 3 = substituteManyWithFunction sTest3 ax2