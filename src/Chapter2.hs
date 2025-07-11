{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Chapter2 where

import Control.Monad.Except
import Control.Monad.Log
import Control.Monad.Random
import Control.Monad.Reader
import Control.Monad.State
import Data.Foldable
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Text (Text)
import Data.Text qualified as Text
import Prettyprinter
import Snail
import Text.Read (readMaybe)

data LangError
  = TextLiteralUnsupported
  | EmptyExpression
  | InvalidLVarExpression
  deriving stock (Eq, Show)

-- L_var
data LExpr
  = LInt Integer
  | LRead
  | LUnaryMinus LExpr
  | LPlus LExpr LExpr
  | LMinus LExpr LExpr
  | LVar Text
  | LLet Text LExpr LExpr
  deriving stock (Eq, Show)

data LVar = LProgram () LExpr
  deriving stock (Eq, Show)

parseLeaf :: (MonadError LangError m) => Text -> m LExpr
parseLeaf = \case
  "read" -> pure LRead
  txt ->
    case readMaybe @Integer $ Text.unpack txt of
      Nothing -> pure $ LVar txt
      Just int -> pure $ LInt int

logSnailAst :: (MonadLog (WithSeverity (Doc ann)) m) => Text -> SnailAst -> m ()
logSnailAst msg expr = logInfo . pretty $ msg <> ": " <> toText expr

parseLExpr :: (MonadLog (WithSeverity (Doc ann)) m, MonadError LangError m) => SnailAst -> m LExpr
parseLExpr = \case
  -- `X` where `X` is a leaf in 'Ast'
  Lexeme (_, leaf) -> parseLeaf leaf
  -- no text literals are supported in this language
  TextLiteral _ -> throwError TextLiteralUnsupported
  -- `(- X)` where X is an integer or an S-expression
  SExpression _ _ [Lexeme (_, "-"), arg] -> do
    logSnailAst "Op -" arg
    operand <- parseLExpr arg
    pure $ LUnaryMinus operand
  -- `(- X)` where X is an integer or an S-expression
  SExpression _ _ [Lexeme (_, "-"), leftOp, rightOp] -> do
    logSnailAst "Op - Left" leftOp
    left <- parseLExpr leftOp
    logSnailAst "Op - Right" rightOp
    right <- parseLExpr rightOp
    pure $ LMinus left right
  -- `(+ X Y)` where X and Y are an integer or an S-expression
  SExpression _ _ [Lexeme (_, "+"), leftOp, rightOp] -> do
    logSnailAst "Op + Left" leftOp
    left <- parseLExpr leftOp
    logSnailAst "Op + Right" rightOp
    right <- parseLExpr rightOp
    pure $ LPlus left right
  SExpression _ _ [Lexeme (_, "let"), SExpression _ _ [Lexeme (_, name), binding], expr] -> do
    logSnailAst "Let binding" binding
    bin <- parseLExpr binding
    logSnailAst "Let expr" expr
    ex <- parseLExpr expr
    pure $ LLet name bin ex
  -- empty expressions are invalid
  SExpression _ _ [] -> throwError EmptyExpression
  -- expression of expressions, e.g. `((X))` -> `(X)`
  expr@(SExpression c b exprs) -> do
    logSnailAst "Expression of expression" expr
    parseLExpr . unwrap . SExpression c b $ unwrap <$> exprs

parseLVar :: (MonadLog (WithSeverity (Doc ann)) m, MonadError LangError m) => SnailAst -> m LVar
parseLVar = \case
  SExpression _ _ [Lexeme (_, "program"), info, body] -> do
    logSnailAst "Program info" info
    logSnailAst "Program body" body
    LProgram () <$> parseLExpr body
  ast -> do
    logSnailAst "Invalid LVar expression" ast
    throwError InvalidLVarExpression

newtype RenameMap = RenameMap {renameMap :: Map Text Text}

-- | Returns random characters in the alphabet either upper or lower case
randomChar :: (MonadRandom m) => m Char
randomChar = uniform $ ['a' .. 'z'] <> ['A' .. 'Z']

-- | Generate a 10 digit string of random characters
uniqueName :: (MonadRandom m) => m Text
uniqueName = Text.pack <$> replicateM 10 randomChar

-- | Exercise 2.1 (pg 26):  Make all variable names unique
uniquifyLExpr :: (RandomGen g, MonadReader RenameMap m) => LExpr -> RandT g m LExpr
uniquifyLExpr = \case
  -- Nothing to do with literals
  x@(LInt _) -> pure x
  x@LRead -> pure x
  -- recursive cases, but no uniqueness to deal with
  LPlus x y -> LPlus <$> uniquifyLExpr x <*> uniquifyLExpr y
  LUnaryMinus x -> LUnaryMinus <$> uniquifyLExpr x
  LMinus x y -> LMinus <$> uniquifyLExpr x <*> uniquifyLExpr y
  -- recursive cases, uniqueness matters
  LVar x -> do
    RenameMap {renameMap} <- ask
    -- we only need to generate a new name, if it doesn't already have a binding
    case Map.lookup x renameMap of
      Nothing ->
        LVar <$> uniqueName
      Just existingBinding ->
        pure $ LVar existingBinding
  LLet x expr body -> do
    uniqueX <- uniqueName
    uniqueExpr <- uniquifyLExpr expr -- can't use variable in definition
    uniqueBody <- local (\RenameMap {renameMap} -> RenameMap {renameMap = Map.insert x uniqueX renameMap}) $ uniquifyLExpr body
    pure $ LLet uniqueX uniqueExpr uniqueBody

uniquifyLVar :: (RandomGen g, MonadReader RenameMap m) => LVar -> RandT g m LVar
uniquifyLVar = \case
  LProgram () lexpr -> LProgram () <$> uniquifyLExpr lexpr

-- | 'LInt', 'LRead', 'LVar' are all simple atomic expressions
isSimpleAtomicExpression :: LExpr -> Bool
isSimpleAtomicExpression = \case
  LInt {} -> True
  LRead -> True
  LVar {} -> True
  LUnaryMinus {} -> False
  LPlus {} -> False
  LMinus {} -> False
  LLet {} -> False

{- | An expression is atomic when each component is atomic. 'LInt', 'Read',
'LVar' are all atomic expressions. 'LUnaryMinus' is atomic when it's argument
is atomic.
-}
isAtomic :: LExpr -> Bool
isAtomic = \case
  LInt {} -> True
  LRead -> True
  LVar {} -> True
  LUnaryMinus x -> isSimpleAtomicExpression x
  LPlus x y -> isSimpleAtomicExpression x && isSimpleAtomicExpression y
  LMinus x y -> isSimpleAtomicExpression x && isSimpleAtomicExpression y
  LLet _x expr body ->
    -- If the expression bound to the variable is atomic and the entire body is
    -- built of atomic expressions, the whole expression is atomic
    isSimpleAtomicExpression expr && isAtomic body

{- | If the 'LExpr' is a simple atomic expression, return it, otherwise you need
to make it atomic
-}
single ::
  (MonadLog (WithSeverity (Doc ann)) m, MonadState [(Text, LExpr)] m, RandomGen g) =>
  LExpr ->
  RandT g m LExpr
single x = do
  if isSimpleAtomicExpression x
    then pure x
    else do
      name <- uniqueName
      newX <- makeAtomic x
      modify $ \s -> [(name, newX)] <> s
      pure $ LVar name

lexprToText :: LExpr -> Text
lexprToText = \case
  LInt int -> Text.pack $ show int
  LRead -> "read"
  LUnaryMinus lexpr -> "(- " <> lexprToText lexpr <> ")"
  LPlus x y -> "(+ " <> lexprToText x <> " " <> lexprToText y <> ")"
  LMinus x y -> "(- " <> lexprToText x <> " " <> lexprToText y <> ")"
  LVar var -> "\"" <> var <> "\""
  LLet var x y -> "(let (" <> var <> lexprToText x <> ")" <> lexprToText y <> ")"

logLExpr :: forall ann m. (MonadLog (WithSeverity (Doc ann)) m) => Text -> LExpr -> m ()
logLExpr msg lexpr = logInfo . pretty $ msg <> ": " <> lexprToText lexpr

makeAtomic ::
  forall m g ann.
  (MonadLog (WithSeverity (Doc ann)) m, MonadState [(Text, LExpr)] m, RandomGen g) =>
  LExpr ->
  RandT g m LExpr
makeAtomic = \case
  -- No state changes needed
  x@(LInt _) -> pure x
  x@LRead -> pure x
  x@(LVar _) -> pure x
  -- May require state changes if not atomic
  expr@(LPlus x y) ->
    if isAtomic expr
      then pure expr
      else do
        newX <- single x
        newY <- single y
        pure $ LPlus newX newY
  expr@(LUnaryMinus x) ->
    if isAtomic expr
      then pure expr
      else do
        newX <- single x
        pure $ LUnaryMinus newX
  LMinus x y -> do
    newX <- single x
    newY <- single y
    pure $ LMinus newX newY
  lexpr@(LLet x expr body) -> do
    if isAtomic lexpr
      then do
        lift $ logLExpr "is atomic: " lexpr
        pure lexpr
      else do
        lift $ logLExpr "is not atomic: " lexpr
        newExpr <- single expr
        newBody <- single body
        pure $ LLet x newExpr newBody

buildLExprFromNonComplexOperands :: (LExpr, [(Text, LExpr)]) -> LExpr
buildLExprFromNonComplexOperands (ast, definitions) =
  let f :: LExpr -> (Text, LExpr) -> LExpr
      f nonComplex (name, expr) = LLet name expr nonComplex
   in foldl' f ast definitions

{- | Exercise 2.3 (pg 28)

This function forces 'LPlus' or 'LMinus's to act only on 'LInt' or 'LVar'

(let (x (+ 42 (- 10))) (+ x 10))
        ^^^^^^^^^^^^^
This is not allowed because (- 10) is an operation.
It needs to be (let (tmp (- 10)) (+ 42 tmp))
-}
removeComplexOperands ::
  (MonadLog (WithSeverity (Doc ann)) m) => LVar -> m LVar
removeComplexOperands (LProgram () lexpr) = do
  let st = flip runStateT []
      program = st $ evalRandT (makeAtomic lexpr) $ mkStdGen 2023
  results :: (LExpr, [(Text, LExpr)]) <- program
  pure $ LProgram () $ buildLExprFromNonComplexOperands results

-- C_var
data CAtom
  = CInt Integer
  | CVar Text

data CExpr
  = CAtomic CAtom
  | CRead
  | CUnaryMinus CAtom
  | CPlus CAtom CAtom
  | CMinus CAtom CAtom

data CStatement = CAssign Text CExpr

data CTail
  = CReturn CExpr
  | CSeq CStatement CTail

newtype CLabel = CLabel Text

data CVar = CProgram () CLabel CTail
