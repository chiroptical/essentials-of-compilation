{-# LANGUAGE FlexibleContexts #-}

module Chapter2 where

import Control.Monad.Except
import Control.Monad.Log
import Control.Monad.Random
import Control.Monad.Reader
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
  | Operation Text [Ast]
  | Let Text Ast Ast
  | Var Text
  deriving stock (Eq, Show)

-- | TODO: Unsure what this is used for yet
newtype Info = Info SnailAst
  deriving stock (Eq, Show)

data LangError
  = TextLiteralUnsupported
  | EmptyExpression
  | UnknownLexeme Text
  | InvalidLetName Text
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
  SExpression _ _ [Lexeme (_, op@"-"), arg] -> do
    logSnailAst "Op -" arg
    operand <- fromSnail arg
    pure $ Operation op [operand]
  -- `(+ X Y)` where X and Y are an integer or an S-expression
  SExpression _ _ [Lexeme (_, op@"+"), leftOp, rightOp] -> do
    logSnailAst "Op + Left" leftOp
    left <- fromSnail leftOp
    logSnailAst "Op + Right" rightOp
    right <- fromSnail rightOp
    pure $ Operation op [left, right]
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

randomChar :: (MonadRandom m) => m Char
randomChar = getRandomR ('a', 'z')

uniqueVariable :: (MonadRandom m) => m Text
uniqueVariable = Text.pack <$> replicateM 10 randomChar

-- | Exercise 2.1: Make all variable names unique
uniquify :: (RandomGen g, MonadReader (Text, Text) m) => Ast -> RandT g m Ast
uniquify = \case
  -- Nothing to do with literals
  x@(AstInt _) -> pure x
  x@Read -> pure x
  -- recursive cases, but no uniqueness to deal with
  Program info ast -> Program info <$> uniquify ast
  Operation op asts -> Operation op <$> traverse uniquify asts
  -- recursive cases, uniqueness matters
  Var x -> do
    (toReplace, with) <- ask
    pure . Var $
      if x == toReplace
        then with
        else x
  Let x expr body -> do
    uniqueX <- uniqueVariable
    let localState = (x, uniqueX)
    uniqueBody <- local (const localState) $ uniquify body
    uniqueExpr <- uniquify expr
    pure $ Let uniqueX uniqueExpr uniqueBody

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
  ast@(Operation op operands) ->
    case (op, operands) of
      ("+", [x, y]) -> (+) <$> interpreter x <*> interpreter y
      ("-", [x]) -> negate <$> interpreter x
      _ -> throwError $ InvalidOperation ast
