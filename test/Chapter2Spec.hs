{-# OPTIONS_GHC -fno-warn-incomplete-uni-patterns #-}

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
  describe "AST" do
    it "handles singly nested let" do
      ast <- snailToAst "(program () (+ (let (x 10) x) 10))"
      ast
        `shouldBe` Program
          (Info $ SExpression Nothing Round [])
          (Plus (Let "x" (AstInt 10) (Var "x")) (AstInt 10))
    it "handles doubly nested let" do
      ast <- snailToAst "(program () (let (x 32) (+ (let (x 10) x) x)))"
      ast
        `shouldBe` Program
          (Info $ SExpression Nothing Round [])
          (Let "x" (AstInt 32) (Plus (Let "x" (AstInt 10) (Var "x")) (Var "x")))
  describe "Exercise 2.2" do
    it "handles singly nested let" do
      ast <- snailToAst "(let (x 10) x)"
      ast `shouldBe` Let "x" (AstInt 10) (Var "x")
      (x, y) <-
        runUniquify ast >>= \case
          Let x (AstInt 10) (Var y) -> pure (x, y)
          _ -> assertFailure "Unable to parse resulting AST"
      x `shouldNotBe` "x"
      x `shouldBe` y

    it "handles doubly nested let" do
      ast <- snailToAst "(let (x 10) (+ (let (x 10) x) x))"
      ast `shouldBe` Let "x" (AstInt 10) (Plus (Let "x" (AstInt 10) (Var "x")) (Var "x"))
      (a, b, c, d) <-
        runUniquify ast >>= \case
          Let a (AstInt 10) (Plus (Let b (AstInt 10) (Var c)) (Var d)) -> pure (a, b, c, d)
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

    it "handles let nested in body" do
      ast <- snailToAst "(let (x 10) (let (x 5) (+ x x)))"
      ast `shouldBe` Let "x" (AstInt 10) (Let "x" (AstInt 5) (Plus (Var "x") (Var "x")))

      (a, b, c, d) <-
        runUniquify ast >>= \case
          Let a (AstInt 10) (Let b (AstInt 5) (Plus (Var c) (Var d))) -> pure (a, b, c, d)
          _ -> assertFailure "Unable to parse resulting AST"

      -- The variables should have new names
      a `shouldNotBe` "x"
      b `shouldNotBe` "x"
      -- They should be named differently
      a `shouldNotBe` b
      -- The latter variable is used in the body
      b `shouldBe` c
      b `shouldBe` d

    it "handles reassigned variable in body" do
      ast <- snailToAst "(let (x 10) (let (y x) (+ y y))) "
      ast `shouldBe` Let "x" (AstInt 10) (Let "y" (Var "x") (Plus (Var "y") (Var "y")))

      (a, b, c, d, e) <-
        runUniquify ast >>= \case
          Let a (AstInt 10) (Let b (Var c) (Plus (Var d) (Var e))) -> pure (a, b, c, d, e)
          _ -> assertFailure "Unable to parse resulting AST"

      -- The variables should have new names
      a `shouldNotBe` "x"
      b `shouldNotBe` "y"
      -- They should be named differently
      a `shouldNotBe` b
      -- The variables match their bodies
      a `shouldBe` c
      b `shouldBe` d
      b `shouldBe` e

    it "handles nested let variable in variable definition" do
      ast <- snailToAst "(let (x (let (x 10) x)) x)"
      ast `shouldBe` Let "x" (Let "x" (AstInt 10) (Var "x")) (Var "x")

      (a, b, c, d) <-
        runUniquify ast >>= \case
          Let a (Let b (AstInt 10) (Var c)) (Var d) -> pure (a, b, c, d)
          _ -> assertFailure "Unable to parse resulting AST"

      -- The variables should have new names
      a `shouldNotBe` "x"
      b `shouldNotBe` "x"
      -- They should be named differently
      b `shouldNotBe` a
      -- They should match their bodies
      a `shouldBe` d
      b `shouldBe` c

    it "handles unused let variable in variable definition" do
      ast <- snailToAst "(let (x (let (y 10) x)) x)"
      ast `shouldBe` Let "x" (Let "y" (AstInt 10) (Var "x")) (Var "x")

      (a, b, c, d) <-
        runUniquify ast >>= \case
          Let a (Let b (AstInt 10) (Var c)) (Var d) -> pure (a, b, c, d)
          _ -> assertFailure "Unable to parse resulting AST"

      -- The variables should have new names
      a `shouldNotBe` "x"
      b `shouldNotBe` "y"
      c `shouldNotBe` "x"
      -- The x in the definition should be different
      c `shouldNotBe` a
      -- The correct variable used in the body
      a `shouldBe` d

  describe "Exercise 2.3" do
    it "handles read as a primitive expression" do
      ast <- snailToAst "(read)"
      ast `shouldBe` Read
      let program = evalRandT (removeComplexOperands ast) $ mkStdGen 2023
      runM program >>= \case
        Right expr -> expr `shouldBe` ast
        Left failure -> assertFailure failure

    it "makes the expression non-complex (page 28)" do
      ast <- snailToAst "(+ 42 (- 10))"
      ast `shouldBe` Plus (AstInt 42) (UnaryMinus (AstInt 10))
      let program = evalRandT (removeComplexOperands ast) $ mkStdGen 2023
      runM program >>= \case
        Left failure -> assertFailure failure
        Right expr -> do
          let (Let v0 (AstInt 10) (Plus (AstInt 42) (Var v1))) = expr
          v0 `shouldBe` v1

    it "doesn't need to change the program (page 28)" do
      ast <- snailToAst "(let (a 42) (let (b a) b))"
      ast `shouldBe` Let "a" (AstInt 42) (Let "b" (Var "a") (Var "b"))
      let program = evalRandT (removeComplexOperands ast) $ mkStdGen 2023
      runM program >>= \case
        Left failure -> assertFailure failure
        Right expr -> expr `shouldBe` ast

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
