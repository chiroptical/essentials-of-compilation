module Chapter1 where

import Control.Monad (forM, forM_)
import Control.Monad.Trans.Except
import Data.Text (Text)
import Data.Text qualified as Text
import Debug.Trace (trace)
import Snail.Shell
import Text.Read (readMaybe)

newtype Info = Info ()
  deriving stock (Show)

data LangError
  = TextLiteralUnsupported
  | EmptyExpression
  | UnknownLexeme Text
  deriving stock (Show)

data Ast
  = Program SExpression Ast
  | AstInt Integer
  | Magma Text Ast Ast
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

parseLexeme :: Text -> Except LangError Ast
parseLexeme = \case
  "read" -> pure Read
  txt ->
    case readMaybe @Integer $ Text.unpack txt of
      Nothing -> throwE $ UnknownLexeme txt
      Just int -> pure $ AstInt int

fromSnail :: SExpression -> Except LangError Ast
fromSnail = \case
  -- no text literals
  TextLiteral _ -> throwE TextLiteralUnsupported
  SExpression _ ((TextLiteral _) : _ : _) -> throwE TextLiteralUnsupported
  -- `read` or `(read)`
  Lexeme (_, "read") -> pure Read
  SExpression _ [Lexeme (_, "read")] -> pure Read
  -- `X` could potentially be an 'AstInt'
  Lexeme (_, lexeme) -> parseLexeme lexeme
  -- `(- X)` where X is an integer or an S-expression
  SExpression _ [Lexeme (_, op@"-"), Lexeme (_, lexeme)] ->
    Negate <$> parseLexeme lexeme
  SExpression _ [Lexeme (_, op@"-"), opExpr@SExpression {}] ->
    let expr = trace ("left " <> show opExpr) $ fromSnail opExpr
     in Negate <$> expr
  -- `(+ X Y)` where X, Y is an integer or an S-expression
  SExpression _ [Lexeme (_, op@"+"), Lexeme (_, leftOp), Lexeme (_, rightOp)] ->
    Magma op <$> parseLexeme leftOp <*> parseLexeme rightOp
  SExpression _ [Lexeme (_, op@"+"), Lexeme (_, leftOp), rightOp@SExpression {}] ->
    Magma op <$> parseLexeme leftOp <*> fromSnail rightOp
  SExpression _ [Lexeme (_, op@"+"), leftOp@SExpression {}, Lexeme (_, rightOp)] ->
    Magma op <$> fromSnail leftOp <*> parseLexeme rightOp
  SExpression _ [Lexeme (_, op@"+"), leftOp@SExpression {}, rightOp@SExpression {}] ->
    Magma op <$> fromSnail leftOp <*> fromSnail rightOp
  -- empty expression is not valid arith
  SExpression _ [] -> throwE EmptyExpression
  -- program
  SExpression _ [Lexeme (_, "program"), infoExpr@SExpression {}, programExpr@SExpression {}] ->
    let program = trace ("program " <> show programExpr) $ fromSnail programExpr
     in Program infoExpr <$> fromSnail programExpr
  expr@(SExpression _ [Lexeme (_, unknown), _]) ->
    let err = trace ("unknown " <> show expr) $ UnknownLexeme unknown
     in throwE err
  -- expression of expressions
  SExpression c exprs ->
    let out = trace ("expr " <> show exprs) $ flatten exprs
     in fromSnail $ SExpression c out

main :: IO ()
main = do
  eSExpressions <- readSnailFile "./programs/chapter1.snail"
  case eSExpressions of
    Left err -> print err
    Right snailAsts -> forM_ snailAsts $ \snail -> do
      let arith = runExcept $ fromSnail snail
      putStrLn $ Text.unpack (toText snail) <> " ==> " <> show arith
