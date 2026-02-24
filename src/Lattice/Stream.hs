-- | Conduit pipeline for processing Lattice entity streams.
--
-- Reads newline-delimited JSON entities from stdin, deduplicates burst
-- updates, tracks entity expiry, and emits structured 'EntityEvent's
-- for downstream consumption by the assessment engine.
module Lattice.Stream
  ( EntityEvent(..)
  , entityStream
  , dedupConduit
  , expiryConduit
  ) where

import Control.Monad (when)
import Conduit
import Data.Aeson (eitherDecodeStrict')
import Data.ByteString (ByteString)
import qualified Data.Conduit.Binary as CB
import qualified Data.Map.Strict as Map
import Data.Time (UTCTime, NominalDiffTime, getCurrentTime, diffUTCTime)
import System.IO (hPutStrLn, stderr, stdin)

import Lattice.Types (Entity(..), EntityId)

-- Events emitted by the entity stream processor
data EntityEvent
  = EntityUpdate !Entity       -- When an entity is refreshed/updated
  | EntityExpired !EntityId    -- When an entity is considered expired
  | Heartbeat                 -- Emitted on lulls 
  deriving stock (Show, Eq)

-- | Full entity ingestion pipeline: stdin -> parse -> dedup -> expiry/heartbeat
entityStream :: ConduitT () EntityEvent IO ()
entityStream =
  CB.sourceHandle stdin    -- read bytes from stdin
    .| linesUnboundedAsciiC      -- Split input into lines
    .| parseC                    -- Parse each line as Entity via JSON
    .| dedupConduit              -- Drop burst duplicates for same entity ID
    .| expiryConduit             -- Handle expiry and heartbeat
  where
    -- Parse each line as JSON; log and drop malformed input.
    parseC :: ConduitT ByteString Entity IO ()
    parseC = awaitForever $ \line ->
      case eitherDecodeStrict' line of
        Left err ->
          liftIO $ hPutStrLn stderr ("dropping malformed entity JSON: " ++ err)
        Right entity ->
          yield entity

-- Suppress duplicate entity updates within a short time window.
-- Prevents burst flooding when the same entity is published at high frequency
-- Stale entries are pruned on each pass to bound memory usage
dedupConduit :: ConduitT Entity Entity IO ()
dedupConduit = go Map.empty
  where
    dedupWindowSeconds :: NominalDiffTime
    dedupWindowSeconds = 0.1

    pruneAfterSeconds :: NominalDiffTime
    pruneAfterSeconds = 5.0

    go :: Map.Map EntityId UTCTime -> ConduitT Entity Entity IO ()
    go seen = do
      mEntity <- await
      case mEntity of
        Nothing -> pure ()
        Just entity -> do
          now <- liftIO getCurrentTime
          let eid = entityId entity
              shouldDrop = case Map.lookup eid seen of
                Just prevSeen -> diffUTCTime now prevSeen < dedupWindowSeconds
                Nothing -> False
              -- insert current entity, then prune anything stale
              seen' = Map.filter (\t -> diffUTCTime now t < pruneAfterSeconds)
                    $ Map.insert eid now seen
          if shouldDrop
            then go seen'
            else yield entity >> go seen'

-- Track entity liveness: emit 'EntityExpired' when an entity goes stale,
-- and 'Heartbeat' after prolonged gaps between updates
-- The expiry map is self-pruning — 'emitExpired' partition out stale entries
-- every iteration, so no separate prune pass is needed here
expiryConduit :: ConduitT Entity EntityEvent IO ()
expiryConduit = do
  start <- liftIO getCurrentTime
  go Map.empty start
  where
    heartbeatSeconds :: NominalDiffTime
    heartbeatSeconds = 0.5

    expirySeconds :: NominalDiffTime
    expirySeconds = 2.0

    go :: Map.Map EntityId UTCTime -> UTCTime -> ConduitT Entity EntityEvent IO ()
    go seen lastEventAt = do
      mEntity <- await
      case mEntity of
        Nothing -> do
          now <- liftIO getCurrentTime
          _ <- emitExpired now seen
          pure ()
        Just entity -> do
          now <- liftIO getCurrentTime
          -- Heartbeat only fires when an entity arrives after a gap, not on true idle
          -- a real timeout would need racing a timer against await
          
          when (diffUTCTime now lastEventAt > heartbeatSeconds) $
            yield Heartbeat
          live <- emitExpired now seen
          yield (EntityUpdate entity)
          let live' = Map.insert (entityId entity) now live
          go live' now

    emitExpired
      :: UTCTime
      -> Map.Map EntityId UTCTime
      -> ConduitT Entity EntityEvent IO (Map.Map EntityId UTCTime)
    emitExpired now seen =
      let (expired, live) = Map.partition (\t -> diffUTCTime now t > expirySeconds) seen
      in do
        mapM_ (yield . EntityExpired) (Map.keys expired)
        pure live
