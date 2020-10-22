-- | 
-- Metrics that do not log anything.
module Cheops.Logger.Internal.Metric.Noop
  ( submitLog
  , flushLog
  ) where

-- | Write metrics when message is submitted.
submitLog :: IO () -> IO ()
{-# INLINE submitLog #-}
submitLog = id

-- | Write metrics when message is flushed.
flushLog :: IO () -> IO ()
{-# INLINE flushLog #-}
flushLog = id
