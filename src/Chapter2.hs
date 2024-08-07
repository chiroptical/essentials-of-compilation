{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Chapter2 where

import Control.Monad.Except
import Control.Monad.Log
import Control.Monad.Random
import Control.Monad.Reader
import Control.Monad.State
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as Text
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

isAtomic :: Ast -> Bool
isAtomic =
  \case
    AstInt _ -> True
    Read -> True
    Var _ -> True
    UnaryMinus _x -> False
    Plus _x _y -> False
    BinaryMinus _x _y -> False
    Let _x expr body -> isAtomic expr && isAtomic body
    Program _info ast -> isAtomic ast

single ::
  (MonadLog (WithSeverity (Doc ann)) m, MonadState (Map Text Ast) m, RandomGen g) =>
  Ast ->
  RandT g m Ast
single x =
  if isAtomic x
    then pure x
    else do
      m <- get
      name <- uniqueName
      put $ Map.insert name x m
      pure $ Var name

makeAtomic ::
  forall m g ann.
  (MonadLog (WithSeverity (Doc ann)) m, MonadState (Map Text Ast) m, RandomGen g) =>
  Ast ->
  RandT g m Ast
makeAtomic = \case
  -- No state changes needed
  x@(AstInt _) -> pure x
  x@Read -> pure x
  x@(Var _) -> pure x
  -- May require state changes if not atomic
  Plus x y -> do
    newX <- single x
    newY <- single y
    pure $ Plus newX newY
  UnaryMinus x -> UnaryMinus <$> single x
  BinaryMinus x y -> do
    newX <- single x
    newY <- single y
    pure $ BinaryMinus newX newY

  -- May require state changes if not atomic
  Let _x _expr _body -> error "..."
  Program _info _ast -> error "..."

makeExpression ::
  (MonadLog (WithSeverity (Doc ann)) m, RandomGen g) => Ast -> RandT g m Ast
makeExpression = error "..."

{- | This function forces 'Plus' or 'Minus's to act only on 'AstInt' or 'Var'

(let (x (+ 42 (- 10))) (+ x 10))
        ^^^^^^^^^^^^^
This is not allowed because (- 10) is an operation.
It needs to be (let (tmp (- 10)) (+ 42 tmp))
-}
removeComplexOperands ::
  (MonadLog (WithSeverity (Doc ann)) m, RandomGen g) => Ast -> RandT g m Ast
removeComplexOperands = \case
  -- atomic
  x@(AstInt _) -> pure x
  x@(Var _) -> pure x
  -- atomic expression
  Read -> pure Read
  -- A 'Let' expresssion may be complex, but we don't need to adjust the outer expression
  Let v definition body ->
    Let v
      <$> removeComplexOperands definition
      <*> removeComplexOperands body
  -- An 'Operation' expression may be complex and may need to be converted into
  -- a 'Let' expression
  Plus x y -> do
    x' <- removeComplexOperands x
    y' <- removeComplexOperands y
    pure $ Plus x' y'
  UnaryMinus x -> do
    x' <- removeComplexOperands x
    pure $ UnaryMinus x'
  BinaryMinus x y -> do
    x' <- removeComplexOperands x
    y' <- removeComplexOperands y
    pure $ BinaryMinus x' y'

  -- Simple recursive case
  Program info ast -> Program info <$> removeComplexOperands ast

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
