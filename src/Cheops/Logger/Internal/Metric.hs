-- | 
-- Metrics support.
module Cheops.Logger.Internal.Metric
  ( submitLog
  , flushLog
  ) where

import Prometheus
import Prometheus.Metric.WindowGauge as Window

-- | Write metrics when message is submitted.
submitLog :: IO () -> IO ()
submitLog f = do
  incCounter metric_submitted_total
  f
  Window.incGauge metric_in_flight

-- | Write metrics when message is flushed.
flushLog :: IO () -> IO ()
flushLog f = do
  f
  incCounter metric_written_total
  Window.decGauge metric_in_flight

-- | Metrics.
metric_submitted_total :: Counter
metric_submitted_total = unsafeRegister $
  counter $ Info "ghc_logger_submitted_total" "Total amount of submitted messages"

-- | Metrics.
metric_written_total :: Counter
metric_written_total = unsafeRegister $
  counter $ Info "ghc_logger_written_total" "Total amount of written messages"

-- | Metrics.
metric_in_flight :: WinGauge
metric_in_flight = unsafeRegister $
  Window.gauge $ Info "ghc_logger_max_in_flight" "Maximum number of in flight messages"
