module Chapter2Spec (spec) where

import Chapter2
import Control.Monad.Random
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
  describe "uniquify" $ do
    it "handles singly nested let" $ do
      ast <- snailToAst "(let (x 10) x)"
      ast `shouldBe` Let "x" (AstInt 10) (Var "x")
      (x, y) <-
        runUniquify ast >>= \case
          Let x (AstInt 10) (Var y) -> pure (x, y)
          _ -> assertFailure "Unable to parse resulting AST"
      x `shouldNotBe` "x"
      x `shouldBe` y
    it "handles doubly nested let" $ do
      ast <- snailToAst "(let (x 10) (+ (let (x 10) x) x))"
      ast `shouldBe` Let "x" (AstInt 10) (Operation "+" [Let "x" (AstInt 10) (Var "x"), Var "x"])
      (a, b, c, d) <-
        runUniquify ast >>= \case
          Let a (AstInt 10) (Operation "+" [Let b (AstInt 10) (Var c), Var d]) -> pure (a, b, c, d)
          _ -> assertFailure "Unable to parse resulting AST"
      -- The variables should be new names
      a `shouldNotBe` "x"
      b `shouldNotBe` "x"
      -- The variables should be named different from one-another
      a `shouldNotBe` b
      -- The outer variables should match
      a `shouldBe` d
      -- The inner variables should match
      b `shouldBe` c

    it "handles let nested in body" $ do
      ast <- snailToAst "(let (x 10) (let (x 5) (+ x x)))"
      ast `shouldBe` Let "x" (AstInt 10) (Let "x" (AstInt 5) (Operation "+" [Var "x", Var "x"]))

      (a, b, c, d) <-
        runUniquify ast >>= \case
          Let a (AstInt 10) (Let b (AstInt 5) (Operation "+" [Var c, Var d])) -> pure (a, b, c, d)
          _ -> assertFailure "Unable to parse resulting AST"

      a `shouldNotBe` "x"
      b `shouldNotBe` "x"
      a `shouldNotBe` b
      b `shouldBe` c
      b `shouldBe` d

    it "handles reassigned variable in body" $ do
      ast <- snailToAst "(let (x 10) (let (y x) (+ y y))) "
      ast `shouldBe` Let "x" (AstInt 10) (Let "y" (Var "x") (Operation "+" [Var "y", Var "y"]))

      (a, b, c, d, e) <-
        runUniquify ast >>= \case
          Let a (AstInt 10) (Let b (Var c) (Operation "+" [Var d, Var e])) -> pure (a, b, c, d, e)
          _ -> assertFailure "Unable to parse resulting AST"

      a `shouldNotBe` "x"
      b `shouldNotBe` "y"
      a `shouldBe` c
      b `shouldBe` d
      b `shouldBe` e
      a `shouldNotBe` b

    it "handles nested let variable in variable definition" $ do
      ast <- snailToAst "(let (x (let (x 10) x)) x)"
      ast `shouldBe` Let "x" (Let "x" (AstInt 10) (Var "x")) (Var "x")

      (a, b, c, d) <-
        runUniquify ast >>= \case
          Let a (Let b (AstInt 10) (Var c)) (Var d) -> pure (a, b, c, d)
          _ -> assertFailure "Unable to parse resulting AST"

      a `shouldNotBe` "x"
      b `shouldNotBe` "x"
      a `shouldBe` d
      b `shouldBe` c
      b `shouldNotBe` a

    it "handles unused let variable in variable definition" $ do
      ast <- snailToAst "(let (x (let (y 10) x)) x)"
      ast `shouldBe` Let "x" (Let "y" (AstInt 10) (Var "x")) (Var "x")

      (a, b, c, d) <-
        runUniquify ast >>= \case
          Let a (Let b (AstInt 10) (Var c)) (Var d) -> pure (a, b, c, d)
          _ -> assertFailure "Unable to parse resulting AST"

      a `shouldNotBe` "x"
      b `shouldNotBe` "y"
      c `shouldNotBe` "x"
      c `shouldNotBe` a
      a `shouldBe` d

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

runUniquify :: Ast -> IO Ast
runUniquify ast = do
  let program = evalRandT (uniquify ast) $ mkStdGen 2023
  runMWith mempty program >>= \case
    Right result -> pure result
    Left _ -> assertFailure "Unable to run uniquify"
