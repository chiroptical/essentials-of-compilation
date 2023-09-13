module Chapter1 where

import Control.Monad.Except
import Control.Monad.Log
import Data.Text (Text)
import Data.Text qualified as Text
import Prettyprinter
import RunM
import Snail
import Text.Read (readMaybe)

-- | TODO: Unsure what this is used for yet
newtype Info = Info SnailAst
  deriving stock (Eq, Show)

data LangError
  = TextLiteralUnsupported
  | EmptyExpression
  | UnknownLexeme Text
  deriving stock (Show)

{- | The AST for $\mathcal{L}_\mathrm{int}$

Both 'AstInt' and 'Read' are leaves of 'Ast', they don't take 'Ast' as an
argument.
-}
data Ast
  = AstInt Integer
  | Read
  | Program Info Ast
  | Operation Text [Ast]
  deriving stock (Eq, Show)

parseLeaf :: (MonadError LangError m) => Text -> m Ast
parseLeaf = \case
  "read" -> pure Read
  txt ->
    case readMaybe @Integer $ Text.unpack txt of
      Nothing -> throwError $ UnknownLexeme txt
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

requestInteger :: (MonadIO m) => m Integer
requestInteger = do
  liftIO $ putStrLn "Enter an integer"
  value <- liftIO getLine
  maybe requestInteger pure $ readMaybe @Integer value

data InterpreterError
  = InvalidOperation Ast
  | InvalidProgram Ast
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
  ast@(Operation op operands) ->
    case (op, operands) of
      ("+", [x, y]) -> (+) <$> interpreter x <*> interpreter y
      ("-", [x]) -> negate <$> interpreter x
      _ -> throwError $ InvalidOperation ast

partialEvaluation :: Ast -> Ast
partialEvaluation = \case
  Operation "+" [AstInt x, AstInt y] -> AstInt $ x + y
  Operation "-" [AstInt x] -> AstInt $ negate x
  x -> x

main :: IO ()
main = do
  eSExpressions <- readSnailFile "./programs/chapter1.snail"
  case eSExpressions of
    Left err -> print err
    Right [ast] -> do
      runM (fromSnail ast) >>= \case
        Left err -> putStrLn $ "Unable to read snail as L(int): " <> show err
        Right lInt -> do
          putStrLn $ Text.unpack (toText ast) <> " ==> " <> show lInt
          runM (interpreter lInt) >>= \case
            Right int -> putStrLn $ "Interpreter result: " <> show int
            Left err -> putStrLn $ "Interpreter error: " <> show err
    Right _ ->
      putStrLn "More than one S-expression is not allows in L(int)"
