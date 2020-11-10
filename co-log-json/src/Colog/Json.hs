-- |
-- Provide a code that allows to use structured logging. Here
-- by structured logging we mean a way to add a additional structured
-- data (context) to the log messages, and a tooling that allows keep 
-- track of the current context and attach it to all the messages.
--
-- Short example:
--
-- @
-- logic :: LoggerEnv -> Int -> IO ()
-- logic ctx' entity_id = do                   -- (1)
--    'logInfo' ctx "start processing entity"    -- (2)
--    internal ctx entity_id                   -- (3)
--    'logInfo' ctx "finish processing
--   where
--     ctx = 'addNamespace' "logic"              -- (4)
--         . 'addContext' (sl "id" entity_id)    -- (5)
--         $ ctx'
-- @
-- Here we:
--
--   * @(1)@ pass initial context (@ctx'@ :: 'LoggerEnv')
--   * @(2)@ log message on 'InfoS' severtity level with new context attached
--   * @(3)@ start internal fuction with new context
--   * @(4)@ extend initial context with a new namespace
--   * @(5)@ extend initial context with a new user data
--
-- __NOTE__ You may notice a bit of an extra boilerplate code here. It can be removed
-- by using any effect handling approach, like ReaderT, mtl, various effects
-- system or service pattern. However this library does not commit to any of
-- those approaches and provide simple @IO@ interface, so there can be a light
-- wrapper with the system of your choice. E.g. The library author prefer to use 
-- @ImplicitParams@ extension, as you can see in cheops-logger package.
--
-- 
module Colog.Json
  ( -- $logger-env
    LoggerEnv
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

import Colog.Core hiding (Severity)
import Colog.Json.Internal.Structured
import Control.Concurrent
import Control.Monad.IO.Class
import Data.Sequence qualified as Seq
import Data.Text qualified as T

-- $logger-env
--
-- For each message we may want to attach an additional information
--
--   * @thread id@ — it can be useful to group messages by the
--       thread when debugging, espeacially in a case if thread
--       can be associated with the request processing.
--
--   * @namespace@ — '.' delimited text that describes the componet
--       that the log was emited from. It allows simple logs
--       filtering in external system, or in the logger action.
--
--   * @severity@ — information how urgent the message is.
--
--   * @user_data@ - any user data in a key-value form, key is a
--       text value, and value is an json-encoded value.
--
-- In order to keep track of that information we introduce 'LoggerEnv' handle,
-- we can emit messages using that hadle, see writing logs section,
-- and modify current context, see adding context section.

-- | Logger environment, is used to keep information about
-- the current context and modify it. When any log message
-- is emitted the current cotext is added to the message.
data LoggerEnv = LoggerEnv
  { action :: LogAction IO Message -- ^ Internal log action.
  , context :: Seq.Seq Structured -- ^ Current context.
  }

-- | Logger that does nothing. Useful for the testing purpose.
emptyLogger :: LoggerEnv
emptyLogger = LoggerEnv (LogAction $ \_ -> pure ()) Seq.empty

-- | Covert ordinary co-log action into 'LoggerEnv' this way
-- we can keep track of the current context and modify it.
mkLogger :: LogAction IO Message -> LoggerEnv
mkLogger action = LoggerEnv action Seq.empty

-- | Covert 'LoggerEnv' back into colog 'LogAction', so we can
-- combie it with the rest of the colog ecosystem.
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
-- external systems. User data is pushed as using 'PushContext' wrapper, that
-- can be created using 'sl'.
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
  :: MonadIO m => LoggerEnv -> LogStr -> m ()
logDebug  x = logSay x DebugS
logNotice x = logSay x InfoS
logInfo x = logSay x InfoS
logWarn x = logSay x WarningS
logErr x = logSay x ErrorS
logCrit x = logSay x CriticalS
logAlert x = logSay x AlertS
logEmergency x = logSay x EmergencyS

-- | Internal logger function.
logSay :: MonadIO m
       => LoggerEnv -- ^ Logger handle.
       -> Severity -- ^ Message severity.
       -> LogStr -- ^ Message itself.
       -> m ()
logSay (LoggerEnv action context) lvl msg = liftIO $ do
  tid <- myThreadId
  unLogAction action $ Message lvl (mkThreadId tid) context msg
{-# SPECIALIZE logSay :: LoggerEnv -> Severity -> LogStr -> IO () #-}
