module Types where

import System.Metrics.Prometheus.Metric.Counter

data Metrics = Metrics
  { metricRequestsServed :: Counter
  , metricInfoHit :: Counter
  , metricInfoMiss :: Counter
  , metricNarHit :: Counter
  , metricNarMiss :: Counter
  , metricNarBytesHit :: Counter
  , metricNarBytesMiss :: Counter
  }
