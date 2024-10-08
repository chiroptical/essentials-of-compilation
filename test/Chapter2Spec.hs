{-# OPTIONS_GHC -fno-warn-incomplete-uni-patterns #-}

module Chapter2Spec (spec) where

import Chapter2
import Control.Monad.Random
import Control.Monad.State
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
    describe "makeAtomic" do
      it "handles makeAtomic with simple input" do
        ast <- snailToAst "(+ 42 (- 10))"
        ast `shouldBe` Plus (AstInt 42) (UnaryMinus (AstInt 10))
        let st = flip runStateT []
            program = st $ evalRandT (makeAtomic ast) $ mkStdGen 2023
        runM program >>= \case
          Left failure -> assertFailure failure
          Right (Plus (AstInt 42) (Var v), definitions) -> do
            print definitions
            definitions `shouldBe` [(v, UnaryMinus (AstInt 10))]
          Right result -> do
            print result
            assertFailure "Unable to match pattern"

      it "handles makeAtomic with nested input" do
        ast <- snailToAst "(- (- (- 10)))"
        ast `shouldBe` UnaryMinus (UnaryMinus (UnaryMinus (AstInt 10)))
        let st = flip runStateT []
            program = st $ evalRandT (makeAtomic ast) $ mkStdGen 2023
        runM program >>= \case
          Left failure -> assertFailure failure
          Right (UnaryMinus (Var x), definitions) -> do
            print definitions
            let [(matchX, UnaryMinus (Var y)), (matchY, body)] = definitions
            x `shouldBe` matchX
            y `shouldBe` matchY
            body `shouldBe` UnaryMinus (AstInt 10)
          Right x -> do
            print x
            assertFailure "Unable to match pattern"

      -- TODO: Ideally, this only creates one variable
      it "handles makeAtomic with duplicate expressions" do
        ast <- snailToAst "(+ (- 10) (- 10))"
        ast `shouldBe` Plus (UnaryMinus (AstInt 10)) (UnaryMinus (AstInt 10))
        let st = flip runStateT []
            program = st $ evalRandT (makeAtomic ast) $ mkStdGen 2023
        runM program >>= \case
          Left failure -> assertFailure failure
          Right (Plus (Var y) (Var x), definitions) -> do
            print definitions
            let [(matchX, eX), (matchY, eY)] = definitions
            x `shouldBe` matchX
            y `shouldBe` matchY
            eX `shouldBe` UnaryMinus (AstInt 10)
            eY `shouldBe` UnaryMinus (AstInt 10)
          Right expr -> do
            print expr
            assertFailure "Unable to match pattern"

    describe "removeComplexOperands" do
      it "handles makeAtomic with simple input" do
        ast <- snailToAst "(+ 42 (- 10))"
        ast `shouldBe` Plus (AstInt 42) (UnaryMinus (AstInt 10))
        nonComplexAst <- runLogM $ removeComplexOperands ast
        case nonComplexAst of
          Let var (UnaryMinus (AstInt 10)) (Plus (AstInt 42) (Var v)) ->
            var `shouldBe` v
          expr -> do
            print expr
            assertFailure "Unable to match pattern"

      it "handles makeAtomic with nested input" do
        ast <- snailToAst "(- (- (- 10)))"
        ast `shouldBe` UnaryMinus (UnaryMinus (UnaryMinus (AstInt 10)))
        nonComplexAst <- runLogM $ removeComplexOperands ast
        case nonComplexAst of
          Let
            x
            (UnaryMinus (AstInt 10))
            ( Let
                y
                (UnaryMinus (Var a))
                (UnaryMinus (Var b))
              ) -> do
              x `shouldBe` a
              y `shouldBe` b
          expr -> do
            print expr
            assertFailure "Unable to match pattern"

      it "handles makeAtomic with duplicate expressions" do
        ast <- snailToAst "(+ (- 10) (- 10))"
        ast `shouldBe` Plus (UnaryMinus (AstInt 10)) (UnaryMinus (AstInt 10))
        nonComplexAst <- runLogM $ removeComplexOperands ast
        case nonComplexAst of
          Let
            x
            (UnaryMinus (AstInt 10))
            ( Let
                y
                (UnaryMinus (AstInt 10))
                (Plus (Var a) (Var b))
              ) -> do
              x `shouldBe` a
              y `shouldBe` b
          Let var (UnaryMinus (AstInt 10)) (Plus (AstInt 42) (Var v)) ->
            var `shouldBe` v
          expr -> do
            print expr
            assertFailure "Unable to match pattern"

      it "doesn't modify ast with non-complex operations" do
        ast <- snailToAst "(let (a 42) (let (b a) b))"
        ast `shouldBe` Let "a" (AstInt 42) (Let "b" (Var "a") (Var "b"))
        nonComplexAst <- runLogM $ removeComplexOperands ast
        nonComplexAst `shouldBe` ast

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
