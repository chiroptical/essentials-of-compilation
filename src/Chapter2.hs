{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Chapter2 where

import Control.Monad.Except
import Control.Monad.Log
import Control.Monad.Random
import Control.Monad.Reader
import Control.Monad.State
import Data.Foldable (foldl')
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as Text
import Debug.Trace (traceM)
import Prettyprinter
import Snail
import Text.Read (readMaybe)

{- | The AST for $\mathcal{L}_\mathrm{var}$

Both 'AstInt' and 'Read' are leaves of 'Ast', they don't take 'Ast' as an
argument.
-}
data Ast
  = AstInt Integer
  | Read
  | Program Info Ast
  | Plus Ast Ast
  | UnaryMinus Ast
  | BinaryMinus Ast Ast
  | Let Text Ast Ast
  | Var Text
  deriving stock (Eq, Show)

-- | TODO: Unsure what this is used for yet
newtype Info = Info SnailAst
  deriving stock (Eq, Show)

data LangError
  = TextLiteralUnsupported
  | EmptyExpression
  deriving stock (Show)

parseLeaf :: (MonadError LangError m) => Text -> m Ast
parseLeaf = \case
  "read" -> pure Read
  txt ->
    case readMaybe @Integer $ Text.unpack txt of
      Nothing -> pure $ Var txt
      Just int -> pure $ AstInt int

logSnailAst :: (MonadLog (WithSeverity (Doc ann)) m) => Text -> SnailAst -> m ()
logSnailAst msg expr = logInfo . pretty $ msg <> ": " <> toText expr

logAst :: (MonadLog (WithSeverity (Doc ann)) m) => Text -> Ast -> m ()
logAst msg expr = logInfo . pretty $ msg <> ": " <> Text.pack (show expr)

fromSnail :: (MonadLog (WithSeverity (Doc ann)) m, MonadError LangError m) => SnailAst -> m Ast
fromSnail = \case
  -- `X` where `X` is a leaf in 'Ast'
  Lexeme (_, leaf) -> parseLeaf leaf
  -- no text literals are supported in this language
  TextLiteral _ -> throwError TextLiteralUnsupported
  -- `(- X)` where X is an integer or an S-expression
  SExpression _ _ [Lexeme (_, "-"), arg] -> do
    logSnailAst "Op -" arg
    operand <- fromSnail arg
    pure $ UnaryMinus operand
  -- `(- X)` where X is an integer or an S-expression
  SExpression _ _ [Lexeme (_, "-"), leftOp, rightOp] -> do
    logSnailAst "Op - Left" leftOp
    left <- fromSnail leftOp
    logSnailAst "Op - Right" rightOp
    right <- fromSnail rightOp
    pure $ BinaryMinus left right
  -- `(+ X Y)` where X and Y are an integer or an S-expression
  SExpression _ _ [Lexeme (_, "+"), leftOp, rightOp] -> do
    logSnailAst "Op + Left" leftOp
    left <- fromSnail leftOp
    logSnailAst "Op + Right" rightOp
    right <- fromSnail rightOp
    pure $ Plus left right
  SExpression _ _ [Lexeme (_, "let"), SExpression _ _ [Lexeme (_, name), binding], expr] -> do
    logSnailAst "Let binding" binding
    bin <- fromSnail binding
    logSnailAst "Let expr" expr
    ex <- fromSnail expr
    pure $ Let name bin ex
  -- `(program X Y)` where `X` is some information, `Y` is an expression
  SExpression _ _ [Lexeme (_, "program"), info, body] -> do
    logSnailAst "Program info" info
    logSnailAst "Program body" body
    Program (Info info) <$> fromSnail body
  -- empty expressions are invalid
  SExpression _ _ [] -> throwError EmptyExpression
  -- expression of expressions, e.g. `((X))` -> `(X)`
  expr@(SExpression c b exprs) -> do
    logSnailAst "Expression of expression" expr
    fromSnail . unwrap . SExpression c b $ unwrap <$> exprs

-- | Returns random characters in the alphabet either upper or lower case
randomChar :: (MonadRandom m) => m Char
randomChar = uniform $ ['a' .. 'z'] <> ['A' .. 'Z']

uniqueName :: (MonadRandom m) => m Text
uniqueName = Text.pack <$> replicateM 10 randomChar

type RenameMap = Map Text Text

-- | Exercise 2.1: Make all variable names unique
uniquify :: (RandomGen g, MonadReader RenameMap m) => Ast -> RandT g m Ast
uniquify = \case
  -- Nothing to do with literals
  x@(AstInt _) -> pure x
  x@Read -> pure x
  -- recursive cases, but no uniqueness to deal with
  Program info ast -> Program info <$> uniquify ast
  Plus x y -> Plus <$> uniquify x <*> uniquify y
  UnaryMinus x -> UnaryMinus <$> uniquify x
  BinaryMinus x y -> BinaryMinus <$> uniquify x <*> uniquify y
  -- recursive cases, uniqueness matters
  Var x -> do
    renameMap <- ask
    name <- uniqueName -- if unable to find binding, generate a new unique name
    pure . Var $ Map.findWithDefault name x renameMap
  Let x expr body -> do
    uniqueX <- uniqueName
    uniqueExpr <- uniquify expr -- can't use variable in definition
    uniqueBody <- local (Map.insert x uniqueX) $ uniquify body
    pure $ Let uniqueX uniqueExpr uniqueBody

-- | 'AstInt', 'Read', 'Var' are all simple atomic expressions
isSimpleAtomicExpression :: Ast -> Bool
isSimpleAtomicExpression = \case
  AstInt {} -> True
  Read -> True
  Var {} -> True
  UnaryMinus {} -> False
  Plus {} -> False
  BinaryMinus {} -> False
  Let {} -> False
  Program {} -> False

{- | An expression is atomic when each component is atomic. 'AstInt', 'Read',
'Var' are all atomic expressions. 'UnaryMinus' is atomic when it's argument
is atomic.
-}
isAtomic :: Ast -> Bool
isAtomic = \case
  AstInt {} -> True
  Read -> True
  Var {} -> True
  UnaryMinus x -> isSimpleAtomicExpression x
  Plus x y -> isSimpleAtomicExpression x && isSimpleAtomicExpression y
  BinaryMinus x y -> isSimpleAtomicExpression x && isSimpleAtomicExpression y
  Let _x expr body ->
    -- If the expression bound to the variable is atomic and the entire body is
    -- built of atomic expressions, the whole expression is atomic
    isSimpleAtomicExpression expr && isAtomic body
  Program _info ast -> isAtomic ast

single ::
  (MonadLog (WithSeverity (Doc ann)) m, MonadState [(Text, Ast)] m, RandomGen g) =>
  Ast ->
  RandT g m Ast
single x = do
  if isSimpleAtomicExpression x
    then pure x
    else do
      name <- uniqueName
      newX <- makeAtomic x
      modify $ \s -> [(name, newX)] <> s
      pure $ Var name

makeAtomic ::
  forall m g ann.
  (MonadLog (WithSeverity (Doc ann)) m, MonadState [(Text, Ast)] m, RandomGen g) =>
  Ast ->
  RandT g m Ast
makeAtomic = \case
  -- No state changes needed
  x@(AstInt _) -> pure x
  x@Read -> pure x
  x@(Var _) -> pure x
  -- May require state changes if not atomic
  expr@(Plus x y) ->
    if isAtomic expr
      then pure expr
      else do
        newX <- single x
        newY <- single y
        pure $ Plus newX newY
  expr@(UnaryMinus x) ->
    if isAtomic expr
      then pure expr
      else do
        newX <- single x
        pure $ UnaryMinus newX
  BinaryMinus x y -> do
    newX <- single x
    newY <- single y
    pure $ BinaryMinus newX newY

  -- May require state changes if not atomic
  ast@(Let x expr body) -> do
    if isAtomic ast
      then do
        traceM $ "isAtomic: " <> show ast
        pure ast
      else do
        traceM $ "notAtomic: " <> show ast
        newExpr <- single expr
        newBody <- single body
        pure $ Let x newExpr newBody
  Program info ast -> do
    newAst <- single ast
    pure $ Program info newAst

buildAstFromNonComplexOperands :: (Ast, [(Text, Ast)]) -> Ast
buildAstFromNonComplexOperands (ast, definitions) =
  let f :: Ast -> (Text, Ast) -> Ast
      f nonComplex (name, expr) = Let name expr nonComplex
   in foldl' f ast definitions

{- | This function forces 'Plus' or 'Minus's to act only on 'AstInt' or 'Var'

(let (x (+ 42 (- 10))) (+ x 10))
        ^^^^^^^^^^^^^
This is not allowed because (- 10) is an operation.
It needs to be (let (tmp (- 10)) (+ 42 tmp))
-}
removeComplexOperands ::
  (MonadLog (WithSeverity (Doc ann)) m) => Ast -> m Ast
removeComplexOperands ast = do
  let st = flip runStateT []
      program = st $ evalRandT (makeAtomic ast) $ mkStdGen 2023
  results :: (Ast, [(Text, Ast)]) <- program
  pure $ buildAstFromNonComplexOperands results

requestInteger :: (MonadIO m) => m Integer
requestInteger = do
  liftIO $ putStrLn "Enter an integer"
  value <- liftIO getLine
  maybe requestInteger pure $ readMaybe @Integer value

data InterpreterError
  = InvalidOperation Ast
  | InvalidProgram Ast
  | NotImplementedYet
  deriving stock (Eq, Show)

interpreter ::
  ( MonadLog (WithSeverity (Doc ann)) m
  , MonadIO m
  , MonadError InterpreterError m
  ) =>
  Ast ->
  m Integer
interpreter = \case
  AstInt int -> pure int
  Read -> requestInteger
  Program _ program -> interpreter program
  Let {} -> throwError NotImplementedYet
  Var {} -> throwError NotImplementedYet
  Plus x y -> (+) <$> interpreter x <*> interpreter y
  UnaryMinus x -> negate <$> interpreter x
  BinaryMinus x y -> (-) <$> interpreter x <*> interpreter y
