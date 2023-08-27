{-# LANGUAGE FlexibleContexts #-}

module Chapter2 where

import Control.Monad.Except
import Control.Monad.Log
import Data.Functor (($>))
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

unwrap :: SnailAst -> SnailAst
unwrap = \case
  SExpression _ [x] -> x
  x -> x

parseLeaf :: (MonadError LangError m) => Text -> m Ast
parseLeaf = \case
  "read" -> pure Read
  txt ->
    case readMaybe @Integer $ Text.unpack txt of
      Nothing -> throwError $ UnknownLexeme txt
      Just int -> pure $ AstInt int

-- | TODO: should this only include [a-zA-Z]?
isValidName :: (MonadError LangError m) => Text -> m Bool
isValidName txt =
  catchError
    ( -- a valid leaf is not a valid let binding
      parseLeaf txt $> False
    )
    ( -- if it throws, we have a valid name
      const $ pure True
    )

logSnailAst :: (MonadLog (WithSeverity (Doc ann)) m) => Text -> SnailAst -> m ()
logSnailAst msg expr = logInfo . pretty $ msg <> ": " <> toText expr

fromSnail :: (MonadLog (WithSeverity (Doc ann)) m, MonadError LangError m) => SnailAst -> m Ast
fromSnail = \case
  -- `X` where `X` is a leaf in 'Ast'
  Lexeme (_, leaf) -> parseLeaf leaf
  -- no text literals are supported in this language
  TextLiteral _ -> throwError TextLiteralUnsupported
  -- `(- X)` where X is an integer or an S-expression
  SExpression _ [Lexeme (_, op@"-"), arg] -> do
    logSnailAst "Op -" arg
    operand <- fromSnail arg
    pure $ Operation op [operand]
  -- `(+ X Y)` where X and Y are an integer or an S-expression
  SExpression _ [Lexeme (_, op@"+"), leftOp, rightOp] -> do
    logSnailAst "Op + Left" leftOp
    left <- fromSnail leftOp
    logSnailAst "Op + Right" rightOp
    right <- fromSnail rightOp
    pure $ Operation op [left, right]
  SExpression _ [Lexeme (_, "let"), SExpression _ [Lexeme (_, name), binding], expr] -> do
    isValidName name >>= \case
      False -> throwError $ InvalidLetName name
      True -> do
        logSnailAst "Let binding" binding
        bin <- fromSnail binding
        logSnailAst "Let expr" expr
        ex <- fromSnail expr
        pure $ Let name bin ex
  -- `(program X Y)` where `X` is some information, `Y` is an expression
  SExpression _ [Lexeme (_, "program"), info, body] -> do
    logSnailAst "Program info" info
    logSnailAst "Program body" body
    Program (Info info) <$> fromSnail body
  -- empty expressions are invalid
  SExpression _ [] -> throwError EmptyExpression
  -- expression of expressions, e.g. `((X))` -> `(X)`
  expr@(SExpression c exprs) -> do
    logSnailAst "Expression of expression" expr
    fromSnail . unwrap . SExpression c $ unwrap <$> exprs

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
  ast@(Operation op operands) ->
    case (op, operands) of
      ("+", [x, y]) -> (+) <$> interpreter x <*> interpreter y
      ("-", [x]) -> negate <$> interpreter x
      _ -> throwError $ InvalidOperation ast
