{-# OPTIONS_GHC -Wno-unused-imports #-}
module Test.Sentry.Assign (spec) where

import qualified Data.Text as T
import Test.Hspec
import Test.QuickCheck

import Sentry.Assign
import Sentry.Assess (Degrees(..), Meters(..), RankedTarget(..))
import Lattice.Types

spec :: Spec
spec = do
  -- tests for calculates shortest direction to slew
  describe "slewDirection" $ do
    it "returns 0 for identical bearings" $
      slewDirection (Degrees 42.0) (Degrees 42.0) `shouldBe` 0.0  -- bearings are the same

    it "wraps clockwise across 0°/360° boundary" $
      slewDirection (Degrees 350.0) (Degrees 10.0) `shouldBe` 20.0

    it "wraps counter-clockwise across 0°/360° boundary" $
      slewDirection (Degrees 10.0) (Degrees 350.0) `shouldBe` (-20.0)

    it "is positive when target is clockwise from turret" $
      property $ \(Positive deltaRaw) ->
        let delta = min 179.0 deltaRaw
            d = slewDirection (Degrees 10.0) (Degrees (10.0 + delta))
        in d > 0

    it "is negative when target is counter-clockwise from turret" $
      property $ \(Positive deltaRaw) ->
        let delta = min 179.0 deltaRaw
            d = slewDirection (Degrees 10.0) (Degrees (10.0 - delta))
        in d < 0

    it "result always in [-180, 180]" $
      property $ \(a :: Double) (b :: Double) ->
        let d = slewDirection (Degrees a) (Degrees b)
        in d >= (-180.0) && d <= 180.0

    it "is antisymmetric" $
      property $ \(a :: Double) (b :: Double) ->
        let d1 = slewDirection (Degrees a) (Degrees b)
            d2 = slewDirection (Degrees b) (Degrees a)
        in approxEq d1 (negate d2) || (approxEq (abs d1) 180.0 && approxEq (abs d2) 180.0)
        -- going the other way flips sign, except at 180

  describe "computeSlew" $ do
    it "HoldPosition when error < 3°" $
      computeSlew turret0 (Just (mkTarget "a" 2.0 1.0)) `shouldBe` HoldPosition

    it "SlewRight for clockwise error" $
      computeSlew turret0 (Just (mkTarget "a" 45.0 1.0)) `shouldSatisfy` isRight

    it "SlewLeft for counter-clockwise error" $
      computeSlew turret0 (Just (mkTarget "a" 315.0 1.0)) `shouldSatisfy` isLeft

    it "NoTarget when target is Nothing" $
      computeSlew turret0 Nothing `shouldBe` NoTarget

    it "speed never exceeds 255" $
      property $ \(a :: Double) (b :: Double) ->
        case computeSlew (turretAt a) (Just (mkTarget "a" b 1.0)) of
          SlewLeft s -> s >= 0 && s <= 255
          SlewRight s -> s >= 0 && s <= 255
          _ -> True  -- clamp to 255

  -- state transition detection
  describe "evaluateTransition" $ do
    it "SameTarget when top target unchanged" $
      evaluateTransition (Tracking (eid "a")) [mkTarget "a" 10.0 100.0] 0.2 `shouldBe` SameTarget
      -- same ent, stays on target

    it "NewTarget when different entity becomes top" $
      evaluateTransition (Tracking (eid "a")) [mkTarget "b" 20.0 130.0, mkTarget "a" 10.0 100.0] 0.2
        `shouldBe` NewTarget (eid "b") (Degrees 20.0)
      -- b is now #1

    it "TargetDisappeared when previous target is missing from list" $
      evaluateTransition (Tracking (eid "a")) [mkTarget "b" 20.0 130.0] 0.2 `shouldBe` TargetDisappeared
      -- a is gone

    it "hysteresis: doesn't switch for <20% score difference" $
      evaluateTransition (Tracking (eid "a")) [mkTarget "b" 20.0 119.0, mkTarget "a" 10.0 100.0] 0.2
        `shouldBe` SameTarget
      -- not enough margin to switch

    it "switches when score exceeds 20%" $
      evaluateTransition (Tracking (eid "a")) [mkTarget "b" 20.0 121.0, mkTarget "a" 10.0 100.0] 0.2
        `shouldBe` NewTarget (eid "b") (Degrees 20.0)
      -- b is >20% higher, switch

    it "NoTargets on empty list" $
      evaluateTransition (Tracking (eid "a")) [] 0.2 `shouldBe` NoTargets
      -- no targets available

  -- Tests for transitionCommand 
  describe "transitionCommand" $ do
    it "Searching + NewTarget → Acquiring" $
      transitionCommand Searching (NewTarget (eid "a") (Degrees 45.0)) turret0
        `shouldBe` (Acquiring (eid "a") (Degrees 45.0), SlewRight 128)
        -- new target found, start acquiring

    it "Acquiring + onTarget → Tracking, YieldToTracker" $
      transitionCommand (Acquiring (eid "a") (Degrees 45.0)) SameTarget turretOn
        `shouldBe` (Tracking (eid "a"), YieldToTracker)
      -- finished acquiring, start tracking

    it "Tracking + disappeared → Reacquiring" $
      transitionCommand (Tracking (eid "a")) TargetDisappeared turret0
        `shouldBe` (Reacquiring (eid "a") 0, YieldToTracker)
      -- lost target, try to reacquire

    it "Reacquiring × 3 failures → Lost" $
      transitionCommand (Reacquiring (eid "a") 3) TargetDisappeared turret0
        `shouldBe` (Lost, NoTarget)
      -- too many reacquire tries, give up

    it "Lost + NewTarget → Acquiring" $
      transitionCommand Lost (NewTarget (eid "b") (Degrees 330.0)) turret0
        `shouldBe` (Acquiring (eid "b") (Degrees 330.0), SlewLeft 85)
      -- found a new target after being lost

    -- recovery and fallback transitions the happy path doesn't cover

    it "Searching + NoTargets → Searching (stay idle)" $
      transitionCommand Searching NoTargets turret0
        `shouldBe` (Searching, NoTarget)

    it "Acquiring + SameTarget (not on target) → keep slewing" $
      transitionCommand (Acquiring (eid "a") (Degrees 45.0)) SameTarget turret0
        `shouldBe` (Acquiring (eid "a") (Degrees 45.0), SlewRight 128)
      -- turret hasn't reached target yet, keep going

    it "Acquiring + NewTarget → abort old, acquire new" $
      transitionCommand (Acquiring (eid "a") (Degrees 45.0)) (NewTarget (eid "b") (Degrees 90.0)) turret0
        `shouldBe` (Acquiring (eid "b") (Degrees 90.0), SlewRight 255)
      -- higher priority target appeared mid-acquire

    it "Acquiring + TargetDisappeared → Lost" $
      transitionCommand (Acquiring (eid "a") (Degrees 45.0)) TargetDisappeared turret0
        `shouldBe` (Lost, NoTarget)

    it "Acquiring + NoTargets → Searching" $
      transitionCommand (Acquiring (eid "a") (Degrees 45.0)) NoTargets turret0
        `shouldBe` (Searching, NoTarget)

    it "Tracking + SameTarget → stay Tracking, yield to PID" $
      transitionCommand (Tracking (eid "a")) SameTarget turret0
        `shouldBe` (Tracking (eid "a"), YieldToTracker)

    it "Tracking + NewTarget → Acquiring new" $
      transitionCommand (Tracking (eid "a")) (NewTarget (eid "b") (Degrees 270.0)) turret0
        `shouldBe` (Acquiring (eid "b") (Degrees 270.0), SlewLeft 255)

    it "Tracking + NoTargets → Lost" $
      transitionCommand (Tracking (eid "a")) NoTargets turret0
        `shouldBe` (Lost, NoTarget)

    it "Reacquiring + SameTarget → Tracking (found it again)" $
      transitionCommand (Reacquiring (eid "a") 2) SameTarget turret0
        `shouldBe` (Tracking (eid "a"), YieldToTracker)
      -- PID refound the target before retries ran out

    it "Reacquiring + TargetDisappeared (retries < 3) → increment retry" $
      transitionCommand (Reacquiring (eid "a") 1) TargetDisappeared turret0
        `shouldBe` (Reacquiring (eid "a") 2, YieldToTracker)
      -- still trying, bump the counter

    it "Reacquiring + NewTarget → Acquiring new" $
      transitionCommand (Reacquiring (eid "a") 1) (NewTarget (eid "c") (Degrees 200.0)) turret0
        `shouldBe` (Acquiring (eid "c") (Degrees 200.0), SlewLeft 255)
      -- better target appeared, abandon reacquire

    it "Reacquiring + NoTargets → Lost" $
      transitionCommand (Reacquiring (eid "a") 1) NoTargets turret0
        `shouldBe` (Lost, NoTarget)

    it "Lost + NoTargets → Searching" $
      transitionCommand Lost NoTargets turret0
        `shouldBe` (Searching, NoTarget)
      -- nothing out there, go back to sweep

-- shorthand for building EntityId from String in tests
eid :: String -> EntityId
eid = EntityId . T.pack

-- create dummy Entity with minimum fields set
mkEntity :: String -> Entity
mkEntity name =
  Entity
    { entityId = eid name
    , isLive = Just True
    , location = Nothing
    , milView = Nothing
    , ontology = Nothing
    , tracked = Nothing
    , provenance = Nothing
    , health = Nothing
    , targetPriority = Nothing
    }

-- construct a RankedTarget for unit tests
mkTarget :: String -> Double -> Double -> RankedTarget
mkTarget name bearing score =
  RankedTarget
    { rtEntity = mkEntity name
    , rtBearing = Degrees bearing
    , rtRange = Meters 100.0
    , rtScore = score
    }

-- Turret at zero degrees, not on target
turret0 :: TurretState
turret0 = turretAt 0.0

-- Turret at any given bearing, not on target
turretAt :: Double -> TurretState
turretAt b = TurretState (Degrees b) 1.0 False

-- Turret exactly on target isOnTarget is True
turretOn :: TurretState
turretOn = TurretState (Degrees 0.0) 1.0 True

-- Detect if TurretCommand is SlewRight
isRight :: TurretCommand -> Bool
isRight (SlewRight _) = True
isRight _ = False

-- Detect if TurretCommand is SlewLeft
isLeft :: TurretCommand -> Bool
isLeft (SlewLeft _) = True
isLeft _ = False

-- Floating point equality for QuickCheck property
approxEq :: Double -> Double -> Bool
approxEq x y = abs (x - y) < 1e-9
