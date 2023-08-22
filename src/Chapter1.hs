{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}

module Chapter1 where

import Control.Monad.Catch
import Control.Monad.Except
import Control.Monad.Log
import Data.Text (Text)
import Data.Text qualified as Text
import Prettyprinter
import Snail.Shell
import System.IO (stdout)
import Text.Read (readMaybe)

-- | TODO: Unsure what this is used for yet
newtype Info = Info SExpression
  deriving stock (Show)

data LangError
  = TextLiteralUnsupported
  | EmptyExpression
  | UnknownLexeme Text
  deriving stock (Show)

-- | The AST for $\mathcal{L}_\mathrm{int}$
data Ast
  = Program Info Ast
  | AstInt Integer
  | Operation Text [Ast]
  | Negate Ast
  | Read
  deriving stock (Show)

flatten :: [SExpression] -> [SExpression]
flatten = \case
  [] -> []
  x@[Lexeme {}] -> x
  x@[TextLiteral {}] -> x
  [SExpression _ x] -> x
  (SExpression _ x : rest) -> flatten x <> flatten rest
  other -> error $ show other

parseLexeme :: (MonadError LangError m) => Text -> m Ast
parseLexeme = \case
  "read" -> pure Read
  txt ->
    case readMaybe @Integer $ Text.unpack txt of
      Nothing -> throwError $ UnknownLexeme txt
      Just int -> pure $ AstInt int

logSExpression :: (MonadLog (WithSeverity (Doc ann)) m) => Text -> SExpression -> m ()
logSExpression msg expr = logInfo . pretty $ msg <> ": " <> toText expr

fromSnail :: (MonadLog (WithSeverity (Doc ann)) m, MonadError LangError m) => SExpression -> m Ast
fromSnail = \case
  -- no text literals
  TextLiteral _ -> throwError TextLiteralUnsupported
  SExpression _ ((TextLiteral _) : _ : _) -> throwError TextLiteralUnsupported
  -- `X` could potentially be an 'AstInt' or 'Read'
  Lexeme (_, lexeme) -> parseLexeme lexeme
  -- `(read)`
  SExpression _ [Lexeme (_, "read")] -> pure Read
  -- `(- X)` where X is an integer or an S-expression
  SExpression _ [Lexeme (_, op@"-"), arg] -> do
    logSExpression "Op -" arg
    operand <- fromSnail arg
    pure $ Operation op [Negate operand]
  -- `(+ X Y)` where X and Y are an integer or an S-expression
  SExpression _ [Lexeme (_, op@"+"), leftOp, rightOp] -> do
    logSExpression "Op + Left" leftOp
    left <- fromSnail leftOp
    logSExpression "Op + Right" rightOp
    right <- fromSnail rightOp
    pure $ Operation op [left, right]
  -- `(program X Y)` where `X` is some information, `Y` is an expression
  SExpression _ [Lexeme (_, "program"), info, body] -> do
    logSExpression "Program info" info
    logSExpression "Program body" body
    Program (Info info) <$> fromSnail body
  -- Empty expressions are invalid
  SExpression _ [] -> throwError EmptyExpression
  -- Any other lexemes are unknown
  expr@(SExpression _ [Lexeme (_, unknown), _]) -> do
    logSExpression "Unknown expression of lexeme" expr
    throwError $ UnknownLexeme unknown
  -- expression of expressions
  expr@(SExpression c exprs) -> do
    logSExpression "Expression of expressions: " expr
    fromSnail . SExpression c $ flatten exprs

requestInteger :: (MonadIO m) => m Integer
requestInteger = do
  liftIO $ putStrLn "Enter an integer"
  value <- liftIO getLine
  maybe requestInteger pure $ readMaybe @Integer value

data InterpreterError
  = InvalidOperation Ast
  | InvalidProgram Ast
  deriving stock (Show)

interpreter ::
  ( MonadLog (WithSeverity (Doc ann)) m
  , MonadIO m
  , MonadError InterpreterError m
  ) =>
  Ast ->
  m Integer
interpreter = \case
  Negate (AstInt int) -> pure int
  AstInt int -> pure int
  Read -> requestInteger
  Program _ program -> interpreter program
  ast@(Operation op operands) ->
    case (op, operands) of
      ("+", [x, y]) -> (+) <$> interpreter x <*> interpreter y
      ("-", [x]) -> negate <$> interpreter x
      _ -> throwError $ InvalidOperation ast
  ast -> throwError $ InvalidProgram ast

runM ::
  ( MonadIO m
  , MonadMask m
  ) =>
  ExceptT e (LoggingT (WithSeverity (Doc ann)) m) a ->
  m (Either e a)
runM program =
  withFDHandler defaultBatchingOptions stdout 0.4 80 $ \logToStdout ->
    flip runLoggingT (logToStdout . renderWithSeverity id) $
      runExceptT program

main :: IO ()
main = do
  eSExpressions <- readSnailFile "./programs/chapter1.snail"
  case eSExpressions of
    Left err -> print err
    Right [snailAst] -> do
      runM (fromSnail snailAst) >>= \case
        Left err -> putStrLn $ "Unable to read snail as L(int): " <> show err
        Right lInt -> do
          putStrLn $ Text.unpack (toText snailAst) <> " ==> " <> show lInt
          runM (interpreter lInt) >>= \case
            Right int -> putStrLn $ "Interpreter result: " <> show int
            Left err -> putStrLn $ "Interpreter error: " <> show err
    Right _ ->
      putStrLn "More than one S-expression is not allows in L(int)"
