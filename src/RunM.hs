module RunM where

import Control.Monad.Catch
import Control.Monad.Except
import Control.Monad.Log
import Prettyprinter
import System.IO (stdout)

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
