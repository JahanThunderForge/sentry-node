module Sentry.Assign
  ( TurretState(..)
  , TurretCommand(..)
  , TrackingState(..)
  , TargetTransition(..)
  , computeSlew
  , slewDirection
  , evaluateTransition
  , transitionCommand
  ) where

import Data.Fixed (mod')
import Data.List (find)

import Sentry.Assess (RankedTarget(..), Degrees(..))
import Lattice.Types (EntityId, entityId)


-- | Tracking lifecycle state machine.
-- SEARCHING: no target, turret sweeping or idle
-- ACQUIRING: new target selected, coarse slew toward it
-- TRACKING:  tracker.py PID loop has the target centered
-- REACQUIRING: target lost from turret cam, trying to re-center
-- LOST: target gone from both cameras, fall back to next-best
data TrackingState
  = Searching
  | Acquiring !EntityId !Degrees  -- entity being acquired, target bearing
  | Tracking  !EntityId           -- entity actively tracked by PID
  | Reacquiring !EntityId !Int    -- entity lost, retry count
  | Lost
  deriving stock (Show, Eq)

-- | What changed between assessment cycles.
data TargetTransition
  = SameTarget                     -- same entity still top priority
  | NewTarget !EntityId !Degrees   -- different entity now top priority
  | TargetDisappeared              -- top target no longer in entity list
  | NoTargets                      -- no hostile/suspect entities
  deriving stock (Show, Eq)

-- | Current turret state (read from tracker.py via IPC).
data TurretState = TurretState
  { turretBearing    :: !Degrees
  , turretConfidence :: !Double
  , turretOnTarget   :: !Bool
  } deriving stock (Show, Eq)

-- | Command to send to the turret motor.
data TurretCommand
  = SlewLeft !Int      -- speed 0-255
  | SlewRight !Int     -- speed 0-255
  | HoldPosition       -- motor stopped, on target
  | NoTarget           -- nothing to track, return to home
  | YieldToTracker     -- tracker.py PID has control, don't send commands
  deriving stock (Show, Eq)

-- | Evaluate what transition occurred between two assessment cycles.
-- Compares current top-ranked target against the previously tracked entity.
-- Hysteresis: only switches targets when the new score exceeds the current
-- by a configurable margin, preventing thrash between close-scored threats.
evaluateTransition :: TrackingState -> [RankedTarget] -> Double -> TargetTransition
evaluateTransition _ [] _ = NoTargets
evaluateTransition st ranked hysteresisMargin =
  case (trackedEntity st, ranked) of
    (Nothing, top:_) ->
      NewTarget (entityId (rtEntity top)) (rtBearing top)
    (Just currentId, top:_) ->
      let topId = entityId (rtEntity top)
      in if topId == currentId
           then SameTarget
           else case find ((== currentId) . entityId . rtEntity) ranked of
                  Nothing -> TargetDisappeared
                  Just currentRt ->
                    if rtScore top > rtScore currentRt * (1.0 + hysteresisMargin)
                      then NewTarget topId (rtBearing top)
                      else SameTarget

-- | Given a transition and current turret feedback, compute the next
-- tracking state and the motor command to issue. Pure state machine:
-- each (TrackingState, TargetTransition) pair maps to exactly one outcome.
transitionCommand :: TrackingState -> TargetTransition -> TurretState -> (TrackingState, TurretCommand)
transitionCommand state transition turret =
  case (state, transition) of
    (Searching, NewTarget eid bearing) ->
      (Acquiring eid bearing, commandToBearing turret bearing)
    (Searching, NoTargets) ->
      (Searching, NoTarget)
    (Searching, _) ->
      (Searching, NoTarget)

    (Acquiring eid bearing, SameTarget) ->
      if turretOnTarget turret
        then (Tracking eid, YieldToTracker)
        else (Acquiring eid bearing, commandToBearing turret bearing)
    (Acquiring _ _, NewTarget eid bearing) ->
      (Acquiring eid bearing, commandToBearing turret bearing)
    (Acquiring _ _, TargetDisappeared) ->
      (Lost, NoTarget)
    (Acquiring _ _, NoTargets) ->
      (Searching, NoTarget)

    (Tracking eid, SameTarget) ->
      (Tracking eid, YieldToTracker)
    (Tracking _oldEid, NewTarget eid bearing) ->
      (Acquiring eid bearing, commandToBearing turret bearing)
    (Tracking eid, TargetDisappeared) ->
      -- PID may still see target in narrow FOV even if wide cam lost it
      (Reacquiring eid 0, YieldToTracker)
    (Tracking _eid, NoTargets) ->
      (Lost, NoTarget)

    (Reacquiring eid _retries, SameTarget) ->
      (Tracking eid, YieldToTracker)
    (Reacquiring eid retries, TargetDisappeared)
      | retries < maxReacquireRetries ->
          (Reacquiring eid (retries + 1), YieldToTracker)
      | otherwise ->
          (Lost, NoTarget)
    (Reacquiring _eid _retries, NewTarget eid bearing) ->
      (Acquiring eid bearing, commandToBearing turret bearing)
    (Reacquiring _eid _retries, NoTargets) ->
      (Lost, NoTarget)

    (Lost, NewTarget eid bearing) ->
      (Acquiring eid bearing, commandToBearing turret bearing)
    (Lost, NoTargets) ->
      (Searching, NoTarget)
    (Lost, _) ->
      (Lost, NoTarget)


-- tracker.py's PID loop once we transition to Tracking state.
computeSlew :: TurretState -> Maybe RankedTarget -> TurretCommand
computeSlew _ Nothing = NoTarget
computeSlew turret (Just target) = commandToBearing turret (rtBearing target)

-- | Shortest angular difference: positive = clockwise, negative = counter-clockwise.
slewDirection :: Degrees -> Degrees -> Double
slewDirection (Degrees current) (Degrees target) =
  normalizeSigned180 (target - current)

maxReacquireRetries :: Int
maxReacquireRetries = 3
-- max number of times to retry to reacquire a target

deadZoneDeg :: Double
deadZoneDeg = 3.0
-- dead zone to prevent jitter

slewGain :: Double
slewGain = 255.0 / 90.0
-- gain for the slew speed

trackedEntity :: TrackingState -> Maybe EntityId
trackedEntity Searching = Nothing
trackedEntity Lost = Nothing
trackedEntity (Acquiring eid _) = Just eid
trackedEntity (Tracking eid) = Just eid
trackedEntity (Reacquiring eid _) = Just eid

normalizeSigned180 :: Double -> Double
normalizeSigned180 raw =
  let wrapped = mod' (raw + 180.0) 360.0 - 180.0
  in if wrapped <= (-180.0) then wrapped + 360.0 else wrapped

commandToBearing :: TurretState -> Degrees -> TurretCommand
commandToBearing turret targetBearing =
  let err = slewDirection (turretBearing turret) targetBearing
      absErr = abs err
      speed = min 255 (round (absErr * slewGain))
  in if absErr < deadZoneDeg
       then HoldPosition
       else if err > 0
              then SlewRight speed
              else SlewLeft speed
