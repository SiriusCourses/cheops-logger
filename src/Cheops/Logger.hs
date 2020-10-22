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
--   'withLogger' config $ \logger -> do
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
  , emptyLogger
  , defCapacity
  , LoggerConfig(..)
  , LoggerEnv
    -- * High level
    -- ** Writing logs
    -- $writing-logs
  , ls
  , showLS
  , LogStr(..)
    -- $writing-helpers
  , logDebug
  , logNotice
  , logInfo
  , logErr
  , logWarn
  , logAlert
  , logCrit
  , logEmergency
  , Severity(..)
    -- ** Adding context
    -- $adding-context
  , addContext
  , addNamespace
  , sl
    -- * Low level
  , PushContext
  , logSay
    -- * Metrics support
    -- $metrics-support
  -- $setup
  ) where

import Cheops.Logger.Internal.Metric
import Cheops.Logger.Internal.Structured
import Cheops.Logger.Internal.Writer
import Colog.Concurrent
import Colog.Concurrent.Internal
import Colog.Core hiding (Severity, Info)
import Control.Concurrent
import Data.Aeson
import System.IO
import qualified Data.Sequence as Seq
import qualified Data.Text as T

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

-- | Logger environment, keeps internal Katip environment,
-- current contexts (metadata), namespaces and minimal interesting
-- severity and verbosity.
data LoggerEnv = LoggerEnv
  { action :: LogAction IO Message
  , context :: Seq.Seq Structured
  }

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
         f $ LoggerEnv
              (LogAction $ submitLog . log_action)
              Seq.Empty
  where
    LogAction feed = feedHandle stdout

-- | Internal logger function.
logSay :: LoggerEnv -- ^ Logger handle.
       -> Severity -- ^ Message severity.
       -> LogStr -- ^ Message itself.
       -> IO ()
logSay (LoggerEnv action context) lvl msg = do
  tid <- myThreadId
  unLogAction action $ Message lvl (mkThreadId tid) context msg

-- | Logger that does nothing. Useful for the testing purpose.
emptyLogger :: LoggerEnv
emptyLogger = LoggerEnv (LogAction $ \_ -> pure ()) Seq.empty

-- $adding-context
--
-- Messages in the library forms a stack, and you can attach 2 kinds of data to it:
--
--   1. @namespace@ - a list of locations, that allows to tell that the component
--        is it.
--   2. @context@ - context is a list of key-value pairs, where key is a text and
--        a value is any JSON value.
--
-- When you attach that information to the 'LoggerContext' it will be added to
-- each message that is written in that context. Allowing to analyze data in the
-- external systems.
--

-- | Helper to update context, by appending another item to the log.
-- 
-- @
-- local ('addContext' ('sl' "key" "value")) $ do
--   ...
-- @
addContext
  :: PushContext -- ^ New data to store
  -> LoggerEnv   -- ^ Old context.
  -> LoggerEnv
addContext (PushContext f) LoggerEnv{..} = LoggerEnv{context = f context, ..}

-- | Helper to extend current namespace by appending sub-namespace.
--
-- @
-- local ('addNamespace' "subcomponent") $ do
--   ...
-- @
addNamespace :: T.Text -> LoggerEnv -> LoggerEnv
addNamespace ns LoggerEnv{..} = LoggerEnv{context=context Seq.|> Segment ns, ..}

-- $writing-logs
-- Logs can we written using one of the following helpers. The general pattern is
--
-- @
-- 'logDebug' logger "message"
-- @
--
-- Messages has type 'LogStr'. This is an abstraction over a data-type for efficient
-- log concatenation. Currently it uses 'Data.Text.Lazy.Builder' but it's an implementation
-- detail and may change in the future.
--
-- 'LogStr' implements 'Data.String.IsString' class, so you can write constant strings
-- without any boilerplate. For other types you should use 'ls' or 'showLS' names are
-- taken from 'Katip' interface.


-- $writing-helpers
-- Library provides helper for each 'Severity' level.

logDebug, logNotice, logInfo, logWarn,
  logErr, logAlert, logCrit, logEmergency
  :: LoggerEnv -> LogStr -> IO ()
logDebug  x = logSay x DebugS
logNotice x = logSay x InfoS
logInfo x = logSay x InfoS
logWarn x = logSay x WarningS
logErr x = logSay x ErrorS
logCrit x = logSay x CriticalS
logAlert x = logSay x AlertS
logEmergency x = logSay x EmergencyS


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
