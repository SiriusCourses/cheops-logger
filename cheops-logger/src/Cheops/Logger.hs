-- |
-- @cheops-logger@ is a highly opinionated logger,
-- it is a tiny interface over [co-log-concurrent](https://hackage.haskell.org/package/co-log-concurrent)
-- that provides a functionality to store strucutred messages. It's used for some internal projects
-- but should be sufficient for the wide use as well. The library itself does not provide
-- much flexibility and choice and concentrated on the features that project needs. However
-- it does not restrict ability to add those.
--
--
-- = Basic usage:
--
-- @
-- run :: LoggerConfig -> IO ()
-- run config = do
--   'withLogger' config $ \initial_logger -> do
--     let ?logger = initial_logger
--     'logSay' ?logger message1"
--     'logDebug' ?logger "message2"
--     let ?logger  = 'addContext' ('sl' "host" "127.0.0.1")
--                  & 'addNamespace' "subprogram" ?logger
--     'logDebug' ?logger "Host info will be added to this message"
-- @
--
module Cheops.Logger
  ( -- * Initialization
    withLogger
  , defCapacity
  , LoggerConfig(..)
  , module Colog.Json
    -- * Metrics support
    -- $metrics-support
  -- $setup
  ) where

import Cheops.Logger.Metrics (flushLog, submitLog)
import Colog.Json
import Colog.Json.Action
import Colog.Concurrent
import Colog.Concurrent.Internal
import Colog.Core hiding (Severity, Info)
import Data.Aeson
import System.IO

-- | Logger configuration
data LoggerConfig = LoggerConfig
  { loggerConfigMessagesInFlight :: !Capacity
    -- ^ Number of messages that can be waiting to be written
    -- before logger will wait.
  , loggerConfigDisabled :: !(Maybe Bool)
    -- ^ If logger is completely disabled.
  }

instance Show LoggerConfig where
  show (LoggerConfig _a b) = "LoggerConfig <capacity> " <> show b

-- | Logger config file.
instance FromJSON LoggerConfig where
  parseJSON = withObject "LoggerConfig" $ \o -> do
    n <- o .:? "disabled"
    pure $ LoggerConfig defCapacity n


-- | Create logger and initialize it with defaults.
withLogger
  :: LoggerConfig -- ^ Logger configuration.
  -> (LoggerEnv -> IO a) -- ^ Continuation where the program run
  -> IO a
withLogger LoggerConfig{..} f
  | Just True <- loggerConfigDisabled = f emptyLogger
  | otherwise =
      withBackgroundLogger
       loggerConfigMessagesInFlight
       (LogAction $ flushLog . feed)
       (hFlush stdout)
       $ \(LogAction log_action) ->
         f $ mkLogger (LogAction $ submitLog . log_action)
  where
    LogAction feed = logToHandle stdout




-- $metrics-support
--
-- Package provides metrics:
--
-- +----------------------------+--------------------------------------------------------------------------------+
-- | metrics name               | value                                                                          |
-- +============================+================================================================================+
-- | ghc_logger_submitted_total | Total amount of submitted messages                                             |
-- +----------------------------+--------------------------------------------------------------------------------+
-- | ghc_logger_written_total   | Total amount of messages flushed to the output                                 |
-- +----------------------------+--------------------------------------------------------------------------------+
-- | ghc_logger_max_in_flight   | Maximum number of messages in a window that were submitted but not yet flushed |
-- +----------------------------+--------------------------------------------------------------------------------+
--
-- These metrics could tell if configuration is good and if the package is health. For example if
-- @ghc_logger_max_in_flight@ is close to default limit then you may want to extend buffer size, give more CPU
-- units to the application, or improve internal structures in the package.
