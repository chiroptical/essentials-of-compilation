module RunM where

import Control.Monad.Catch
import Control.Monad.Except
import Control.Monad.Log
import Control.Monad.Reader
import Prettyprinter
import System.IO (stdout)

runM ::
  (MonadIO m, MonadMask m) =>
  ExceptT e (LoggingT (WithSeverity (Doc ann)) m) a ->
  m (Either e a)
runM program =
  withFDHandler defaultBatchingOptions stdout 0.4 80 $ \logToStdout ->
    flip runLoggingT (logToStdout . renderWithSeverity id) $
      runExceptT program

runMWith ::
  (MonadIO m, MonadMask m) =>
  r ->
  ReaderT r (ExceptT e (LoggingT (WithSeverity (Doc ann)) m)) a ->
  m (Either e a)
runMWith input program =
  withFDHandler defaultBatchingOptions stdout 0.4 80 $ \logToStdout ->
    flip runLoggingT (logToStdout . renderWithSeverity id) $
      runExceptT $
        runReaderT program input
