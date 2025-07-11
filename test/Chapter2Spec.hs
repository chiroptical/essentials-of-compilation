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

  describe "Exercise 2.2" do
    it "handles singly nested let" do
      lexprE <- snailToLExpr "(let (x 10) x)"
      lexprE `shouldBe` Right (LLet "x" (LInt 10) (LVar "x"))
      let lexpr = unsafeFromRight lexprE
      (x, y) <-
        runUniquify lexpr >>= \case
          LLet x (LInt 10) (LVar y) -> pure (x, y)
          _ -> assertFailure "Unable to parse resulting LVar"
      x `shouldNotBe` "x"
      x `shouldBe` y
    it "handles doubly nested let" do
      lexprE <- snailToLExpr "(let (x 10) (+ (let (x 10) x) x))"
      lexprE `shouldBe` Right (LLet "x" (LInt 10) (LPlus (LLet "x" (LInt 10) (LVar "x")) (LVar "x")))
      let lexpr = unsafeFromRight lexprE
      (a, b, c, d) <-
        runUniquify lexpr >>= \case
          LLet a (LInt 10) (LPlus (LLet b (LInt 10) (LVar c)) (LVar d)) -> pure (a, b, c, d)
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
      lexprE <- snailToLExpr "(let (x 10) (let (x 5) (+ x x)))"
      lexprE `shouldBe` Right (LLet "x" (LInt 10) (LLet "x" (LInt 5) (LPlus (LVar "x") (LVar "x"))))
      let lexpr = unsafeFromRight lexprE
      (a, b, c, d) <-
        runUniquify lexpr >>= \case
          LLet a (LInt 10) (LLet b (LInt 5) (LPlus (LVar c) (LVar d))) -> pure (a, b, c, d)
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
      lexprE <- snailToLExpr "(let (x 10) (let (y x) (+ y y))) "
      lexprE `shouldBe` Right (LLet "x" (LInt 10) (LLet "y" (LVar "x") (LPlus (LVar "y") (LVar "y"))))
      let lexpr = unsafeFromRight lexprE
      (a, b, c, d, e) <-
        runUniquify lexpr >>= \case
          LLet a (LInt 10) (LLet b (LVar c) (LPlus (LVar d) (LVar e))) -> pure (a, b, c, d, e)
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
      lexprE <- snailToLExpr "(let (x (let (x 10) x)) x)"
      lexprE `shouldBe` Right (LLet "x" (LLet "x" (LInt 10) (LVar "x")) (LVar "x"))
      let lexpr = unsafeFromRight lexprE
      (a, b, c, d) <-
        runUniquify lexpr >>= \case
          LLet a (LLet b (LInt 10) (LVar c)) (LVar d) -> pure (a, b, c, d)
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
      lexprE <- snailToLExpr "(let (x (let (y 10) x)) x)"
      lexprE `shouldBe` Right (LLet "x" (LLet "y" (LInt 10) (LVar "x")) (LVar "x"))
      let lexpr = unsafeFromRight lexprE
      (a, b, c, d) <-
        runUniquify lexpr >>= \case
          LLet a (LLet b (LInt 10) (LVar c)) (LVar d) -> pure (a, b, c, d)
          _ -> assertFailure "Unable to parse resulting AST"
      -- The variables should have new names
      a `shouldNotBe` "x"
      b `shouldNotBe` "y"
      c `shouldNotBe` "x"
      -- The x in the definition should be different
      c `shouldNotBe` a
      -- The correct variable used in the body
      a `shouldBe` d

    describe "removeComplexOperands" do
      it "handles makeAtomic with simple input" do
        lexprE <- snailToLExpr "(+ 42 (- 10))"
        lexprE `shouldBe` Right (LPlus (LInt 42) (LUnaryMinus (LInt 10)))
        let lexpr = unsafeFromRight lexprE
        LProgram () nonComplexAst <- runLogM $ removeComplexOperands $ LProgram () lexpr
        case nonComplexAst of
          LLet var (LUnaryMinus (LInt 10)) (LPlus (LInt 42) (LVar v)) ->
            var `shouldBe` v
          expr -> do
            print expr
            assertFailure "Unable to match pattern"

      it "handles makeAtomic with nested input" do
        lexprE <- snailToLExpr "(- (- (- 10)))"
        lexprE `shouldBe` Right (LUnaryMinus (LUnaryMinus (LUnaryMinus (LInt 10))))
        let lexpr = unsafeFromRight lexprE
        LProgram () nonComplexAst <- runLogM $ removeComplexOperands $ LProgram () lexpr
        case nonComplexAst of
          LLet
            x
            (LUnaryMinus (LInt 10))
            ( LLet
                y
                (LUnaryMinus (LVar a))
                (LUnaryMinus (LVar b))
              ) -> do
              x `shouldBe` a
              y `shouldBe` b
          expr -> do
            print expr
            assertFailure "Unable to match pattern"

      it "handles makeAtomic with duplicate expressions" do
        lexprE <- snailToLExpr "(+ (- 10) (- 10))"
        lexprE `shouldBe` Right (LPlus (LUnaryMinus (LInt 10)) (LUnaryMinus (LInt 10)))
        let lexpr = unsafeFromRight lexprE
        LProgram () nonComplexAst <- runLogM $ removeComplexOperands $ LProgram () lexpr
        case nonComplexAst of
          LLet
            x
            (LUnaryMinus (LInt 10))
            ( LLet
                y
                (LUnaryMinus (LInt 10))
                (LPlus (LVar a) (LVar b))
              ) -> do
              x `shouldBe` a
              y `shouldBe` b
          LLet var (LUnaryMinus (LInt 10)) (LPlus (LInt 42) (LVar v)) ->
            var `shouldBe` v
          expr -> do
            print expr
            assertFailure "Unable to match pattern"

      it "doesn't modify ast with non-complex operations" do
        lexprE <- snailToLExpr "(let (a 42) (let (b a) b))"
        lexprE `shouldBe` Right (LLet "a" (LInt 42) (LLet "b" (LVar "a") (LVar "b")))
        let lexpr = unsafeFromRight lexprE
        LProgram () nonComplexAst <- runLogM $ removeComplexOperands $ LProgram () lexpr
        nonComplexAst `shouldBe` lexpr

snailToLVar :: Text -> IO (Either LangError LVar)
snailToLVar input = do
  snail <-
    case parseSnail input of
      Right [snail] -> pure snail
      Right _ -> assertFailure "More than one s-expression"
      _ -> assertFailure "Unable to parse snail program"
  runM (parseLVar snail)

snailToLExpr :: Text -> IO (Either LangError LExpr)
snailToLExpr input = do
  snail <-
    case parseSnail input of
      Right [snail] -> pure snail
      Right _ -> assertFailure "More than one s-expression"
      _ -> assertFailure "Unable to parse snail program"
  runM (parseLExpr snail)

runUniquify :: LExpr -> IO LExpr
runUniquify lexpr = do
  let program = evalRandT (uniquifyLExpr lexpr) $ mkStdGen 2023
  runMWith (RenameMap {renameMap = mempty}) program >>= \case
    Right result -> pure result
    Left _ -> assertFailure "Unable to run uniquify"

unsafeFromRight :: Either a b -> b
unsafeFromRight = \case
  Left _ -> error "unsafeFromRight encountered Left"
  Right x -> x
