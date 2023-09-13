module Chapter1Spec (spec) where

import Chapter1
import Data.Text
import RunM
import Snail
import Test.HUnit
import Test.Hspec

spec :: Spec
spec = do
  describe "partial evaluation followed by interpreter" do
    it "first" do
      ast <- snailToAst "(program () (+ 10 (- (+ 5 3))))"
      ast `evaluationShouldBe` 2
    it "second" do
      ast <- snailToAst "(program () (+ 1 (+ 3 1)))"
      ast `evaluationShouldBe` 5
    it "third" do
      ast <- snailToAst "(program () (- (+ 3 (- 5))))"
      ast `evaluationShouldBe` 2

snailToAst :: Text -> IO Ast
snailToAst input = do
  snail <-
    case parseSnail input of
      Right [snail] -> pure snail
      Right _ -> assertFailure "More than one s-expression"
      _ -> assertFailure "Unable to parse snail program"
  runM (fromSnail snail) >>= \case
    Right ast -> pure ast
    Left _ -> assertFailure "Unable to convert snail to L(int)"

evaluationShouldBe :: Ast -> Integer -> IO ()
evaluationShouldBe lInt result = do
  let partial = partialEvaluation lInt
  withFull <- runM $ interpreter lInt
  withPartial <- runM $ interpreter partial
  withFull `shouldBe` Right result
  withFull `shouldBe` withPartial
