-- |
-- High level interface for the structured logging.
module Cheops.Logger.Structured
  ( LoggerEnv
  , mkLogger
  , emptyLogger
  , unLogger
    -- * Writing logs
    -- $writing-logs
  , ls
  , showLS
  , LogStr
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
  
    -- * Adding context
    -- $adding-context
  , addContext
  , sl
  , addNamespace
  ) where

import Cheops.Logger.Internal.Structured
import Colog.Core hiding (Severity)
import Control.Concurrent
import Data.Sequence qualified as Seq
import Data.Text qualified as T

-- | Logger environment, keeps internal Katip environment,
-- current contexts (metadata), namespaces and minimal interesting
-- severity and verbosity.
data LoggerEnv = LoggerEnv
  { action :: LogAction IO Message
  , context :: Seq.Seq Structured
  }

-- | Logger that does nothing. Useful for the testing purpose.
emptyLogger :: LoggerEnv
emptyLogger = LoggerEnv (LogAction $ \_ -> pure ()) Seq.empty

mkLogger :: LogAction IO Message -> LoggerEnv
mkLogger action = LoggerEnv action Seq.empty

unLogger :: LoggerEnv -> LogAction IO (Severity, LogStr)
unLogger (LoggerEnv action st) = LogAction $ \(lvl, msg) -> do
  tid <- myThreadId
  unLogAction action $ Message lvl (mkThreadId tid) st msg

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

-- | Internal logger function.
logSay :: LoggerEnv -- ^ Logger handle.
       -> Severity -- ^ Message severity.
       -> LogStr -- ^ Message itself.
       -> IO ()
logSay (LoggerEnv action context) lvl msg = do
  tid <- myThreadId
  unLogAction action $ Message lvl (mkThreadId tid) context msg
