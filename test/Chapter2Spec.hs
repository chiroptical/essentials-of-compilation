module Chapter2Spec (spec) where

import Chapter2
import Data.Text
import RunM
import Snail
import Test.HUnit
import Test.Hspec

spec :: Spec
spec = do
  describe "AST" $ do
    it "handles singly nested let" $ do
      ast <- snailToAst "(program () (+ (let (x 10) x) 10))"
      ast
        `shouldBe` Program
          (Info $ SExpression Nothing Round [])
          (Operation "+" [Let "x" (AstInt 10) (Var "x"), AstInt 10])
    it "handles doubly nested let" $ do
      ast <- snailToAst "(program () (let (x 32) (+ (let (x 10) x) x)))"
      ast
        `shouldBe` Program
          (Info $ SExpression Nothing Round [])
          (Let "x" (AstInt 32) (Operation "+" [Let "x" (AstInt 10) (Var "x"), Var "x"]))

snailToAst :: Text -> IO Ast
snailToAst input = do
  snail <-
    case parseSnail input of
      Right [snail] -> pure snail
      Right _ -> assertFailure "More than one s-expression"
      _ -> assertFailure "Unable to parse snail program"
  runM (fromSnail snail) >>= \case
    Right ast -> pure ast
    Left _ -> assertFailure "Unable to convert snail to L(var)"
