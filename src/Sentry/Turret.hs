module Sentry.Turret
  ( TurretStatus(..)
  , TurretHealth(..)
  , updateTurretStatus
  , isTurretOnTarget
  ) where

import Sentry.Assess (Degrees(..))

-- Health of the turret hardware.
data TurretHealth = TurretHealthy | TurretFault | TurretOffline
  deriving stock (Show, Eq, Ord)

-- Full turret status shared from STM TVar.
data TurretStatus = TurretStatus
  { tsBearing    :: !(Maybe Degrees)  -- estimated from turret cam FOV + pixel offset (Nothing if no detection)
  , tsOnTarget   :: !Bool             -- tracker.py PID reports target centered in turret cam
  , tsHealth     :: !TurretHealth     -- is the motor responding?
  , tsLastAckMs  :: !Int              -- ms since last ACK from Arduino
  } deriving stock (Show, Eq)

-- Build a TurretStatus from tracker.py's JSON fields.
-- Low confidence  means YOLO detection on the turret cam is unreliable,
-- discard the bearing. Stale ACKs degrade health to Fault then Offline
updateTurretStatus :: Maybe Degrees -> Bool -> Double -> Int -> TurretStatus
updateTurretStatus mBearing centered confidence lastAckMs =
  TurretStatus
    { tsBearing = if confidence < 0.3 then Nothing else mBearing
    , tsOnTarget = centered
    , tsHealth = healthFromAck lastAckMs
    , tsLastAckMs = lastAckMs
    }


isTurretOnTarget :: TurretStatus -> Bool
isTurretOnTarget ts =
  case tsBearing ts of
    Nothing -> False
    Just _ ->
      tsOnTarget ts && tsHealth ts == TurretHealthy

healthFromAck :: Int -> TurretHealth
healthFromAck lastAckMs
  | lastAckMs > 1000 = TurretOffline
  | lastAckMs > 250 = TurretFault
  | otherwise = TurretHealthy
