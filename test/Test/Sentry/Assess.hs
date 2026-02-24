{-# OPTIONS_GHC -Wno-orphans #-}
module Test.Sentry.Assess (spec) where

import Test.Hspec
import Test.QuickCheck

import Data.Maybe (isJust)
import qualified Data.Text as T

import Sentry.Assess
import Lattice.Types


-- Helper! check if two Doubles are within epsilon of each other
approx :: Double -> Double -> Double -> Bool
approx eps expected actual = abs (actual - expected) <= eps


-- Arbitrary instances: teach QuickCheck how to generate random values
-- for every type that appears in Entity. This is what powers
-- property-based testing 
-- QuickCheck generates hundreds of random ones automatically


-- Latitude must stay in [-90, 90]
instance Arbitrary Latitude where
  arbitrary = Latitude <$> choose (-90.0, 90.0)
  shrink (Latitude x) = Latitude <$> shrink x

-- Longitude must stay in [-180, 180]
instance Arbitrary Longitude where
  arbitrary = Longitude <$> choose (-180.0, 180.0)
  shrink (Longitude x) = Longitude <$> shrink x

-- Altitude: 500m (using these numbers before we add in the actual logic)
instance Arbitrary AltitudeHaeMeters where
  arbitrary = AltitudeHaeMeters <$> choose (-500.0, 20000.0)
  shrink (AltitudeHaeMeters x) = AltitudeHaeMeters <$> shrink x

-- Position: built from its three components 
-- Shrinking: tries reducing each field independently 
-- QuickCheck: find the minimal failing case
instance Arbitrary Position where
  arbitrary =
    Position
      <$> arbitrary
      <*> arbitrary
      <*> arbitrary
  shrink (Position lat lon alt) =
    [ Position lat' lon  alt  | lat' <- shrink lat ] ++
    [ Position lat  lon' alt  | lon' <- shrink lon ] ++
    [ Position lat  lon  alt' | alt' <- shrink alt ]

-- Velocity in East/North/Up.
-- Up component is smaller since most threats are ground
instance Arbitrary VelocityEnu where
  arbitrary =
    VelocityEnu
      <$> choose (-300.0, 300.0)
      <*> choose (-300.0, 300.0)
      <*> choose (-50.0, 50.0)
  shrink (VelocityEnu e n u) =
    [ VelocityEnu e' n  u  | e' <- shrink e ] ++
    [ VelocityEnu e  n' u  | n' <- shrink n ] ++
    [ VelocityEnu e  n  u' | u' <- shrink u ]

-- Speed: 0 to 400 m/s 
instance Arbitrary SpeedMps where
  arbitrary = SpeedMps <$> choose (0.0, 400.0)
  shrink (SpeedMps s) = SpeedMps <$> shrink s

-- for location, each sub-field is randomly present or absent.
-- frequency weights control how often Nothing vs Just appears 
-- we want a mix of complete and partial entities to stress-test how missing data is handled
instance Arbitrary Location where
  arbitrary = do
    pos <- frequency [(3, pure Nothing), (7, Just <$> arbitrary)]
    vel <- frequency [(5, pure Nothing), (5, Just <$> arbitrary)]
    spd <- frequency [(6, pure Nothing), (4, Just <$> arbitrary)]
    pure $ Location pos vel spd
  shrink (Location pos vel spd) =
    --  remove entire optional fields
    [ Location Nothing vel     spd     | isJust pos ] ++
    [ Location pos     Nothing spd     | isJust vel ] ++
    [ Location pos     vel     Nothing | isJust spd ] ++
    -- smallershrinking inside each field 
    [ Location pos' vel  spd  | pos' <- shrink pos ] ++
    [ Location pos  vel' spd  | vel' <- shrink vel ] ++
    [ Location pos  vel  spd' | spd' <- shrink spd ]

-- Disposition: one of the 7 enum values, uniformly distributed
-- enums don't need shrinking 
instance Arbitrary Disposition where
  arbitrary = elements
    [ DispositionFriendly
    , DispositionHostile
    , DispositionSuspect
    , DispositionNeutral
    , DispositionPending
    , DispositionAssumedFriend
    , DispositionUnknown
    ]
  shrink _ = []

-- MilView: 70% chance of having a disposition, 30% Nothing.
instance Arbitrary MilView where
  arbitrary = MilView <$> frequency [(3, pure Nothing), (7, Just <$> arbitrary)]
  shrink (MilView d) =
    [ MilView Nothing | isJust d ] ++
    [ MilView d' | d' <- shrink d ]

-- ThreatLevel: 5 enum values.
instance Arbitrary ThreatLevel where
  arbitrary = elements
    [ ThreatNone
    , ThreatLow
    , ThreatMedium
    , ThreatHigh
    , ThreatCritical
    ]
  shrink _ = []

-- TargetPriority: 60% chance of having a threat level.
instance Arbitrary TargetPriority where
  arbitrary = TargetPriority <$> frequency [(4, pure Nothing), (6, Just <$> arbitrary)]
  shrink (TargetPriority p) =
    [ TargetPriority Nothing | isJust p ] ++
    [ TargetPriority p' | p' <- shrink p ]

-- EntityId: random short string wrapped in the newtype.
instance Arbitrary EntityId where
  arbitrary = EntityId . T.pack <$> listOf (elements ['a'..'z'])
  shrink (EntityId t) = EntityId . T.pack <$> shrink (T.unpack t)

-- Entity: the top-level type. Fields we don't test  are always Nothing to keep generation fast (ontology, tracked, provenance, health)
instance Arbitrary Entity where
  arbitrary = do
    eid  <- arbitrary
    live <- frequency [(5, pure Nothing), (5, Just <$> arbitrary)]
    loc  <- frequency [(5, pure Nothing), (5, Just <$> arbitrary)]
    mv   <- frequency [(5, pure Nothing), (5, Just <$> arbitrary)]
    tp   <- frequency [(6, pure Nothing), (4, Just <$> arbitrary)]
    pure $ Entity eid live loc mv Nothing Nothing Nothing Nothing tp
  shrink (Entity eid live loc mv ont tr prov hlth tp) =
    -- try removing entire optional fields
    [ Entity eid live Nothing mv  ont tr prov hlth tp  | isJust loc ] ++
    [ Entity eid live loc  Nothing ont tr prov hlth tp | isJust mv  ] ++
    [ Entity eid live loc  mv  ont tr prov hlth Nothing | isJust tp  ] ++
    -- shrinking inside each field
    [ Entity eid' live loc mv ont tr prov hlth tp | eid' <- shrink eid  ] ++
    [ Entity eid live' loc mv ont tr prov hlth tp | live' <- shrink live ] ++
    [ Entity eid live loc' mv ont tr prov hlth tp | loc' <- shrink loc  ] ++
    [ Entity eid live loc mv' ont tr prov hlth tp | mv'  <- shrink mv   ] ++
    [ Entity eid live loc mv ont tr prov hlth tp' | tp'  <- shrink tp   ]

-- builds an entity with all required fields filled in
mkEntityAt
  :: EntityId
  -> Disposition
  -> ThreatLevel
  -> Latitude
  -> Longitude
  -> Maybe VelocityEnu
  -> Entity
mkEntityAt eid disp thr lat lon mVel =
  let pos = Position lat lon (AltitudeHaeMeters 0.0)
      loc = Location (Just pos) mVel Nothing
      mv  = MilView (Just disp)
      tp  = TargetPriority (Just thr)
  in Entity
      { entityId       = eid
      , isLive         = Just True
      , location       = Just loc
      , milView        = Just mv
      , ontology       = Nothing
      , tracked        = Nothing
      , provenance     = Nothing
      , health         = Nothing
      , targetPriority = Just tp
      }

--shared config for all property tests
configForProps :: SentryConfig
configForProps = SentryConfig
  { sentryLat      = Latitude 0.0
  , sentryLon      = Longitude 0.0
  , weightDistance  = 1.0
  , weightPriority = 1.0
  , weightVelocity = 1.0
  }

--if you're closer, you're more dangerous
propCloserHostileRanksHigher :: Positive Double -> Positive Double -> Property
propCloserHostileRanksHigher (Positive r1) (Positive r2) =
  let d1 = min r1 r2
      d2 = max r1 r2
      -- clamping to reasonable longitude offsets 
      d1' = max 0.001 (min 0.5 d1)
      d2' = max (d1' + 0.001) (min 1.0 d2)

      eClose = mkEntityAt (EntityId "close") DispositionHostile ThreatMedium
                 (Latitude 0.0) (Longitude d1') Nothing
      eFar   = mkEntityAt (EntityId "far") DispositionHostile ThreatMedium
                 (Latitude 0.0) (Longitude d2') Nothing

      out = assessThreats configForProps [eClose, eFar]
  in counterexample ("Output: " ++ show out) $
     case out of
       [first, second] ->
         conjoin
           [ property (rtScore first >= rtScore second)
           , entityId (rtEntity first) === EntityId "close"
           ]
       _ -> property False

--turret will never target a friendly
propFriendlyNeverRanked :: [Entity] -> Bool
propFriendlyNeverRanked entities =
  let out = assessThreats configForProps entities
  in all (\rt ->
        case milView (rtEntity rt) >>= disposition of
          Just DispositionFriendly -> False
          _ -> True
      ) out


-- output is always sorted by score descending
-- pairwise comparison, each score >= the next one in the list

propOutputSorted :: [Entity] -> Bool
propOutputSorted entities =
  let out = assessThreats configForProps entities
      scores = map rtScore out
  in and (zipWith (>=) scores (drop 1 scores))

--empty input always produces empty input 
propEmptyInput :: Bool
propEmptyInput = assessThreats configForProps [] == []

--scores are non-negative when velocity weight is zero
-- This property isolates distance and priority to verify they're always positive  

configNoVelocity :: SentryConfig
configNoVelocity = SentryConfig
  { sentryLat      = Latitude 0.0
  , sentryLon      = Longitude 0.0
  , weightDistance  = 1.0
  , weightPriority = 1.0
  , weightVelocity = 0.0
  }

propScoreNonNegative :: [Entity] -> Bool
propScoreNonNegative entities =
  let out = assessThreats configNoVelocity entities
  in all (\rt -> rtScore rt >= 0.0) out

-- actual test runner
spec :: Spec
spec = do
  describe "bearingTo" $ do
    it "returns ~90° for due east (0,0) -> (0,1)" $ do
      let Degrees b = bearingTo (Latitude 0.0) (Longitude 0.0)
                                (Latitude 0.0) (Longitude 1.0)
      b `shouldSatisfy` approx 0.5 90.0

    it "returns ~0° for due north (0,0) -> (1,0)" $ do
      let Degrees b = bearingTo (Latitude 0.0) (Longitude 0.0)
                                (Latitude 1.0) (Longitude 0.0)
      (approx 0.5 0.0 b || approx 0.5 360.0 b) `shouldBe` True

  describe "haversineDistance" $ do
    it "returns ~111km for 1° of longitude at equator" $ do
      let Meters d = haversineDistance (Latitude 0.0) (Longitude 0.0)
                                       (Latitude 0.0) (Longitude 1.0)
      d `shouldSatisfy` approx 300.0 111195.0

  describe "closingSpeed" $ do
    it "is positive when target moves toward sentry" $ do
      -- sentry at (0,0), target at (0,1) — target is due east.
      -- "toward sentry" means moving west, so east velocity is negative.
      let vToward = VelocityEnu (-10.0) 0.0 0.0
          spd = closingSpeed (Latitude 0.0) (Longitude 0.0)
                             (Latitude 0.0) (Longitude 1.0) (Just vToward)
      spd `shouldSatisfy` (> 0.0)

  describe "assessThreats" $ do
    it "returns [] for empty input" $
      assessThreats configForProps [] `shouldBe` []

    it "filters out friendly entities" $ do
      let friendly = mkEntityAt (EntityId "f") DispositionFriendly ThreatHigh
                       (Latitude 0.0) (Longitude 0.2) Nothing
          hostile  = mkEntityAt (EntityId "h") DispositionHostile ThreatHigh
                       (Latitude 0.0) (Longitude 0.3) Nothing
          out = assessThreats configForProps [friendly, hostile]
      map (entityId . rtEntity) out `shouldBe` [EntityId "h"]

    it "closer hostile ranks higher (QuickCheck)" $
      property propCloserHostileRanksHigher

    it "friendly never appears in output (QuickCheck)" $
      property propFriendlyNeverRanked

    it "output is always sorted descending (QuickCheck)" $
      property propOutputSorted

    it "empty input → empty output (QuickCheck)" $
      property propEmptyInput

    it "all scores are non-negative (QuickCheck)" $
      property propScoreNonNegative
