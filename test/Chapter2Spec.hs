module Chapter2Spec (spec) where

import Chapter2
import Data.Text
import RunM
import Snail
import Test.HUnit
import Test.Hspec

spec :: Spec
spec = do
  describe "Parse LVar" do
    it "handles singly nested let" do
      ast <- snailToLVar "(program () (+ (let (x 10) x) 10))"
      ast
        `shouldBe` Right
          ( LProgram
              ()
              (LPlus (LLet "x" (LInt 10) (LVar "x")) (LInt 10))
          )
    it "handles doubly nested let" do
      ast <- snailToLVar "(program () (let (x 32) (+ (let (x 10) x) x)))"
      ast
        `shouldBe` Right
          ( LProgram
              ()
              (LLet "x" (LInt 32) (LPlus (LLet "x" (LInt 10) (LVar "x")) (LVar "x")))
          )
    it "is unable to parse an invalid program" do
      ast <- snailToLVar "(let (x 32) (+ (let (x 10) x) x))"
      ast
        `shouldBe` Left InvalidLVarExpression

snailToLVar :: Text -> IO (Either LangError LVar)
snailToLVar input = do
  snail <-
    case parseSnail input of
      Right [snail] -> pure snail
      Right _ -> assertFailure "More than one s-expression"
      _ -> assertFailure "Unable to parse snail program"
  runM (parseLVar snail)
