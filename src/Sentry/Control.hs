-- | STM-based coordinator: 5 concurrent threads sharing world state.
--
-- Haskell never touches the serial port. IPC is via JSON files in run\/:
--
-- @
--   T1 ingestion     stdin entities        → TVar
--   T2 assessment    100ms pure scoring    → TVar
--   T3 target writer TVar                  → run\/target.json
--   T4 turret reader run\/turret.json      → TVar
--   T5 LLM overrides run\/llm_override.json → TVar
-- @
--
-- tracker.py is the sole serial port owner.
module Sentry.Control
  ( WorldState(..)
  , SentryApp(..)
  , runSentry
  , ingestionThread
  , assessmentThread
  , targetWriterThread
  , turretReaderThread
  , llmOverrideThread
  ) where

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.STM
import Control.Exception (IOException, finally, try)
import Control.Monad (forever)
import Conduit (awaitForever, liftIO, runConduit, (.|))
import Data.Aeson (FromJSON(..), Value, (.:), (.=), eitherDecodeStrict', encode, object, withObject)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe, listToMaybe)
import Data.Text (Text)
import Data.Time (NominalDiffTime, UTCTime, diffUTCTime, getCurrentTime)
import System.Directory (createDirectoryIfMissing, getModificationTime, renameFile)
import System.FilePath (takeDirectory, (</>))
import System.IO (hPutStrLn, stderr)

import Lattice.Types (Entity(..), EntityId, Disposition, MilView(..))
import Lattice.Stream (EntityEvent(..), entityStream)
import Sentry.Assess (SentryConfig, RankedTarget, assessThreats, Degrees(..))
import Sentry.Assign (TurretState(..), TrackingState(..), evaluateTransition, transitionCommand)
import Sentry.Turret (TurretStatus(..), TurretHealth(..), updateTurretStatus, isTurretOnTarget)

---------------------------------------------------------------------------
-- Shared state
---------------------------------------------------------------------------

-- | Shared mutable state — accessed only through STM.
data WorldState = WorldState
  { wsEntities      :: !(Map EntityId Entity)
  , wsTurretStatus  :: !TurretStatus
  , wsTrackingState :: !TrackingState
  , wsTopTarget     :: !(Maybe RankedTarget)
  , wsLastUpdate    :: !(Maybe UTCTime)
  , wsLlmOverrides  :: !(Map EntityId Disposition)
  } deriving stock (Show, Eq)

-- | Runtime handles. Haskell communicates with tracker.py via JSON files:
--   run\/target.json       — Haskell writes target commands
--   run\/turret.json       — tracker.py writes turret feedback
--   run\/llm_override.json — llm_classify.py writes disposition overrides
data SentryApp = SentryApp
  { appWorldState   :: !(TVar WorldState)
  , appSentryConfig :: !SentryConfig
  , appTargetFile   :: !FilePath
  , appTurretFile   :: !FilePath
  , appOverrideFile :: !FilePath
  }

initialWorldState :: WorldState
initialWorldState = WorldState
  { wsEntities      = Map.empty
  , wsTurretStatus  = TurretStatus Nothing False TurretOffline 9999
  , wsTrackingState = Searching
  , wsTopTarget     = Nothing
  , wsLastUpdate    = Nothing
  , wsLlmOverrides  = Map.empty
  }

--ENTRYPOINT!
-- ctrl c writes SHUTDOWN to target.json so tracker.py stops the motor
runSentry :: SentryConfig -> FilePath -> FilePath -> IO ()
runSentry config targetFile turretFile = do
  let runDir       = takeDirectory targetFile
      overrideFile = runDir </> "llm_override.json"
  createDirectoryIfMissing True runDir
  wsVar <- newTVarIO initialWorldState
  let app = SentryApp wsVar config targetFile turretFile overrideFile
  hPutStrLn stderr "sentry: starting threads"
  _ <- forkIO (ingestionThread wsVar)
  _ <- forkIO (assessmentThread app)
  _ <- forkIO (targetWriterThread app)
  _ <- forkIO (llmOverrideThread app)
  turretReaderThread app `finally` shutdown app

shutdown :: SentryApp -> IO ()
shutdown app = do
  hPutStrLn stderr "sentry: shutting down"
  writeAtomic (appTargetFile app)
    (encode (object ["state" .= ("SHUTDOWN" :: Text)]))


-- Entity ingestion 

-- Read EntityEvents from the conduit pipeline and update world state
ingestionThread :: TVar WorldState -> IO ()
ingestionThread wsVar = runConduit $ entityStream .| awaitForever handleEvent
  where
    handleEvent (EntityUpdate entity) = liftIO $ do
      now <- getCurrentTime
      atomically $ modifyTVar' wsVar $ \ws ->
        ws { wsEntities  = Map.insert (entityId entity) entity (wsEntities ws)
           , wsLastUpdate = Just now
           }
    handleEvent (EntityExpired eid) = liftIO $
      atomically $ modifyTVar' wsVar $ \ws ->
        ws { wsEntities = Map.delete eid (wsEntities ws) }
    handleEvent Heartbeat = liftIO $ do
      now <- getCurrentTime
      atomically $ modifyTVar' wsVar $ \ws ->
        ws { wsLastUpdate = Just now }


-- Assessment cycle logic on STM snapshot --> stm snapshot comes from the ingestion thread

-- Every 100ms: snapshot state, run pure threat assessment and state
-- machine, write results back
assessmentThread :: SentryApp -> IO ()
assessmentThread app = forever $ do
  threadDelay assessmentIntervalUs
  snapshot <- atomically $ readTVar (appWorldState app)
  let entities   = Map.elems (wsEntities snapshot)
      overrides  = wsLlmOverrides snapshot
      fused      = applyOverrides overrides entities
      ranked     = assessThreats (appSentryConfig app) fused
      trackSt    = wsTrackingState snapshot
      transition = evaluateTransition trackSt ranked hysteresisMargin
      turret     = toAssignTurret (wsTurretStatus snapshot)
      (newState, _cmd) = transitionCommand trackSt transition turret
      topTarget  = listToMaybe ranked
  atomically $ modifyTVar' (appWorldState app) $ \ws ->
    ws { wsTrackingState = newState
       , wsTopTarget     = topTarget
       }

-- Target writer 


-- Serialize current tracking state to JSON for tracker.py
-- Atomic rename prevents partial reads
targetWriterThread :: SentryApp -> IO ()
targetWriterThread app = forever $ do
  threadDelay targetWriteIntervalUs
  trackState <- atomically $ wsTrackingState <$> readTVar (appWorldState app)
  writeAtomic (appTargetFile app) (encode (mkTargetJson trackState))

-- Turret reader 

-- check turret status written by tracker.py. File staleness determines health 
-- stale file degrades to TurretFault then TurretOffline

turretReaderThread :: SentryApp -> IO ()
turretReaderThread app = forever $ do
  threadDelay turretReadIntervalUs
  result <- try @IOException $ do
    contents <- BS.readFile (appTurretFile app)
    modTime  <- getModificationTime (appTurretFile app)
    now      <- getCurrentTime
    pure (contents, round (diffUTCTime now modTime * 1000) :: Int)
  case result of
    Left _ -> pure ()
    Right (contents, ageMs) ->
      case parseTurretReport contents ageMs of
        Left err ->
          hPutStrLn stderr $ "turret.json: " ++ err
        Right status ->
          atomically $ modifyTVar' (appWorldState app) $ \ws ->
            ws { wsTurretStatus = status }


-- LLM override reader run/llm_override.json --> TVar


-- Fuse LLM disposition overrides into the assessment loop
-- Stale or missing file --> clear overrides --> classical fallback
llmOverrideThread :: SentryApp -> IO ()
llmOverrideThread app = forever $ do
  threadDelay llmReadIntervalUs
  result <- try @IOException $ do
    contents <- BS.readFile (appOverrideFile app)
    modTime  <- getModificationTime (appOverrideFile app)
    now      <- getCurrentTime
    pure (contents, diffUTCTime now modTime)
  case result of
    Left _ -> clearOverrides
    Right (contents, ageSec)
      | ageSec > llmStaleThreshold -> clearOverrides
      | otherwise ->
          case eitherDecodeStrict' contents of
            Left err ->
              hPutStrLn stderr $ "llm_override.json: " ++ err
            Right file ->
              atomically $ modifyTVar' (appWorldState app) $ \ws ->
                ws { wsLlmOverrides = overrideFileToMap file }
  where
    clearOverrides = atomically $ modifyTVar' (appWorldState app) $ \ws ->
      ws { wsLlmOverrides = Map.empty }


-- Named constants bc this is getting really messy


assessmentIntervalUs :: Int
assessmentIntervalUs = 100_000

targetWriteIntervalUs :: Int
targetWriteIntervalUs = 100_000

turretReadIntervalUs :: Int
turretReadIntervalUs = 50_000

llmReadIntervalUs :: Int
llmReadIntervalUs = 500_000

llmStaleThreshold :: NominalDiffTime
llmStaleThreshold = 10.0

hysteresisMargin :: Double
hysteresisMargin = 0.2


-- Pure helpers, ways to do this without helper but need to learn


-- Apply LLM disposition overrides before threat assessment
-- Entities not in the override map pass through unchanged
applyOverrides :: Map EntityId Disposition -> [Entity] -> [Entity]
applyOverrides overrides
  | Map.null overrides = id
  | otherwise = map $ \entity ->
      case Map.lookup (entityId entity) overrides of
        Nothing   -> entity
        Just disp -> entity
          { milView = Just $ case milView entity of
              Nothing -> MilView { disposition = Just disp }
              Just mv -> mv { disposition = Just disp }
          }

-- Bridge TurretStatus  to TurretState. avatar state between hardware and state machine. 
toAssignTurret :: TurretStatus -> TurretState
toAssignTurret ts = TurretState
  { turretBearing    = fromMaybe (Degrees 0) (tsBearing ts)
  , turretConfidence = maybe 0.0 (const 1.0) (tsBearing ts)
  , turretOnTarget   = isTurretOnTarget ts
  }

-- Build the JSON command tracker.py reads from target.json
-- tracker.py interprets "mode" to decide whether to PID-lock or coarse-slew mode
mkTargetJson :: TrackingState -> Value
mkTargetJson Searching = object
  [ "state" .= ("SEARCHING" :: Text), "mode" .= ("idle" :: Text) ]
mkTargetJson (Acquiring eid (Degrees bearing)) = object
  [ "state"            .= ("ACQUIRING" :: Text)
  , "mode"             .= ("slew" :: Text)
  , "target_bearing"   .= bearing
  , "target_entity_id" .= eid
  ]
mkTargetJson (Tracking eid) = object
  [ "state"            .= ("TRACKING" :: Text)
  , "mode"             .= ("pid" :: Text)
  , "target_entity_id" .= eid
  ]
mkTargetJson (Reacquiring eid _retries) = object
  [ "state"            .= ("REACQUIRING" :: Text)
  , "mode"             .= ("pid" :: Text)
  , "target_entity_id" .= eid
  ]
mkTargetJson Lost = object
  [ "state" .= ("LOST" :: Text), "mode" .= ("idle" :: Text) ]

-- File I/O helpers for file mutability


-- | Atomic file write via temp + rename. Prevents tracker.py from reading
-- a partially written JSON file
writeAtomic :: FilePath -> BL.ByteString -> IO ()
writeAtomic path contents = do
  let tmp = path <> ".tmp"
  BL.writeFile tmp contents
  renameFile tmp path

-- Parse tracker.py's turret status JSON. File age feeds into health
parseTurretReport :: BS.ByteString -> Int -> Either String TurretStatus
parseTurretReport bs ageMs = do
  TurretReport bearing centered confidence <- eitherDecodeStrict' bs
  pure $ updateTurretStatus (Just (Degrees bearing)) centered confidence ageMs

overrideFileToMap :: OverrideFile -> Map EntityId Disposition
overrideFileToMap (OverrideFile entries) =
  Map.fromList [ (eid, disp) | OverrideEntry eid disp <- entries ]


-- IPC JSON types that are internal, not exported

-- tracker.py writes: {"turret_bearing": 47.3, "target_centered": true, "confidence": 0.85} NOT OFFICIAL
data TurretReport = TurretReport !Double !Bool !Double

instance FromJSON TurretReport where
  parseJSON = withObject "TurretReport" $ \o ->
    TurretReport
      <$> o .: "turret_bearing"
      <*> o .: "target_centered"
      <*> o .: "confidence"

-- llm_classify.py writes: {"overrides": [{"entity_id": "trk-42", "disposition": "DISPOSITION_HOSTILE"}]} NOT OFFICIAL
data OverrideEntry = OverrideEntry !EntityId !Disposition

instance FromJSON OverrideEntry where
  parseJSON = withObject "OverrideEntry" $ \o ->
    OverrideEntry
      <$> o .: "entity_id"
      <*> o .: "disposition"

newtype OverrideFile = OverrideFile [OverrideEntry]

instance FromJSON OverrideFile where
  parseJSON = withObject "OverrideFile" $ \o ->
    OverrideFile <$> o .: "overrides"
