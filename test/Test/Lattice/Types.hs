{-# OPTIONS_GHC -Wno-orphans #-}
module Test.Lattice.Types (spec) where

import Data.Aeson (decode, encode, eitherDecode)
import qualified Data.ByteString.Lazy as LBS
import Data.Maybe (isJust)
import qualified Data.Text as T
import Test.Hspec
import Test.QuickCheck

import Lattice.Types

-- bring the Arbitrary instances from our Assess tests into scope
-- they already define Arbitrary for Entity, Latitude, Longitude,
-- Location, MilView, Disposition, VelocityEnu, Position, etc
import Test.Sentry.Assess ()


-- FullEntity: a newtype wrapper around Entity
-- the Arbitrary Entity in Assess always sets ontology/tracked/provenance/health to Nothing
-- for serialization testing we need every field exercised
-- newtype lets us define a separate Arbitrary without conflicting
newtype FullEntity = FullEntity Entity
  deriving stock (Show, Eq)

-- Arbitrary for sub-types that Assess didn't need

-- PlatformType: 6 enum values
instance Arbitrary PlatformType where
  arbitrary = elements
    [ PlatformOther
    , PlatformAir
    , PlatformLand
    , PlatformSurface
    , PlatformSubsurface
    , PlatformSpace
    ]
  shrink _ = []

-- Ontology: optional platform type and optional free-text specific type
instance Arbitrary Ontology where
  arbitrary = Ontology
    <$> frequency [(3, pure Nothing), (7, Just <$> arbitrary)]
    <*> frequency [(4, pure Nothing), (6, Just . T.pack <$> listOf (elements ['a'..'z']))]
  shrink (Ontology pt st) =
    [ Ontology Nothing st  | isJust pt ] ++
    [ Ontology pt  Nothing | isJust st ] ++
    [ Ontology pt' st | pt' <- shrink pt ]

-- TrackQuality: 0.0 to 1.0 confidence score
instance Arbitrary TrackQuality where
  arbitrary = TrackQuality <$> choose (0.0, 1.0)
  shrink (TrackQuality q) = TrackQuality <$> shrink q

-- Tracked: optional quality and optional timestamp string
instance Arbitrary Tracked where
  arbitrary = Tracked
    <$> frequency [(4, pure Nothing), (6, Just <$> arbitrary)]
    <*> frequency [(5, pure Nothing), (5, Just . T.pack <$> pure "2025-01-15T12:00:00Z")]
  shrink (Tracked tq t) =
    [ Tracked Nothing t  | isJust tq ] ++
    [ Tracked tq Nothing | isJust t  ] ++
    [ Tracked tq' t | tq' <- shrink tq ]

-- ConnectionStatus: 2 enum values
instance Arbitrary ConnectionStatus where
  arbitrary = elements [ConnectionOnline, ConnectionOffline]
  shrink _ = []

-- DataType: 3 enum values
instance Arbitrary DataType where
  arbitrary = elements [DataTypeLive, DataTypeSimulated, DataTypeReplay]
  shrink _ = []

-- Provenance: how the entity data was produced
instance Arbitrary Provenance where
  arbitrary = Provenance
    <$> frequency [(4, pure Nothing), (6, Just . T.pack <$> listOf (elements ['a'..'z']))]
    <*> frequency [(4, pure Nothing), (6, Just <$> arbitrary)]
    <*> frequency [(4, pure Nothing), (6, Just . T.pack <$> listOf (elements ['a'..'z']))]
  shrink (Provenance nm dt sid) =
    [ Provenance Nothing dt  sid  | isJust nm  ] ++
    [ Provenance nm  Nothing sid  | isJust dt  ] ++
    [ Provenance nm  dt  Nothing  | isJust sid ] ++
    [ Provenance nm  dt' sid | dt'  <- shrink dt  ]

-- Health: just a connection status
instance Arbitrary Health where
  arbitrary = Health <$> frequency [(3, pure Nothing), (7, Just <$> arbitrary)]
  shrink (Health cs) =
    [ Health Nothing | isJust cs ] ++
    [ Health cs' | cs' <- shrink cs ]

-- FullEntity: every field has a chance of being populated
-- this is the one we use for the serialization round-trip property
instance Arbitrary FullEntity where
  arbitrary = do
    eid  <- arbitrary
    live <- frequency [(3, pure Nothing), (7, Just <$> arbitrary)]
    loc  <- frequency [(3, pure Nothing), (7, Just <$> arbitrary)]
    mv   <- frequency [(3, pure Nothing), (7, Just <$> arbitrary)]
    ont  <- frequency [(4, pure Nothing), (6, Just <$> arbitrary)]
    tr   <- frequency [(4, pure Nothing), (6, Just <$> arbitrary)]
    prov <- frequency [(4, pure Nothing), (6, Just <$> arbitrary)]
    hlth <- frequency [(4, pure Nothing), (6, Just <$> arbitrary)]
    tp   <- frequency [(4, pure Nothing), (6, Just <$> arbitrary)]
    pure $ FullEntity (Entity eid live loc mv ont tr prov hlth tp)
  shrink (FullEntity (Entity eid live loc mv ont tr prov hlth tp)) =
    [ FullEntity (Entity eid live Nothing mv  ont tr  prov hlth tp)  | isJust loc  ] ++
    [ FullEntity (Entity eid live loc  Nothing ont tr  prov hlth tp) | isJust mv   ] ++
    [ FullEntity (Entity eid live loc  mv  Nothing tr  prov hlth tp) | isJust ont  ] ++
    [ FullEntity (Entity eid live loc  mv  ont Nothing prov hlth tp) | isJust tr   ] ++
    [ FullEntity (Entity eid live loc  mv  ont tr  Nothing hlth tp)  | isJust prov ] ++
    [ FullEntity (Entity eid live loc  mv  ont tr  prov Nothing tp)  | isJust hlth ] ++
    [ FullEntity (Entity eid live loc  mv  ont tr  prov hlth Nothing)| isJust tp   ]


-- the big one: for ANY random entity (with ALL fields exercised),
-- encoding to JSON then decoding back should give us the exact same entity
-- if this passes 100 times, our entire serialization layer is correct
propFullRoundTrip :: FullEntity -> Bool
propFullRoundTrip (FullEntity entity) =
  decode (encode entity) == Just entity

-- same idea but for Disposition specifically
-- every enum value should survive a JSON round trip
propDispositionRoundTrip :: Disposition -> Bool
propDispositionRoundTrip d =
  decode (encode d) == Just d

-- same for ThreatLevel
propThreatLevelRoundTrip :: ThreatLevel -> Bool
propThreatLevelRoundTrip tl =
  decode (encode tl) == Just tl

-- same for PlatformType
propPlatformTypeRoundTrip :: PlatformType -> Bool
propPlatformTypeRoundTrip pt =
  decode (encode pt) == Just pt

-- same for ConnectionStatus
propConnectionStatusRoundTrip :: ConnectionStatus -> Bool
propConnectionStatusRoundTrip cs =
  decode (encode cs) == Just cs

-- same for DataType
propDataTypeRoundTrip :: DataType -> Bool
propDataTypeRoundTrip dt =
  decode (encode dt) == Just dt


-- actual test runner
spec :: Spec
spec = do
  describe "Entity JSON round-trip" $ do
    -- minimal entity: just an id, everything else Nothing
    -- proves our FromJSON handles missing optional fields
    it "round-trips a minimal entity (entityId only)" $ do
      let json = "{\"entity_id\": \"trk-001\"}"
          result = eitherDecode json :: Either String Entity
      case result of
        Left err -> expectationFailure $ "parse failed: " ++ err
        Right entity -> do
          entityId entity `shouldBe` EntityId "trk-001"
          decode (encode entity) `shouldBe` Just entity

    -- full entity with every field populated
    -- proves our ToJSON and FromJSON are symmetric across all sub-types
    it "round-trips a full entity with all fields" $ do
      let entity = Entity
            { entityId       = EntityId "trk-042"
            , isLive         = Just True
            , location       = Just $ Location
                (Just $ Position (Latitude 33.6) (Longitude (-117.8)) (AltitudeHaeMeters 100.0))
                (Just $ VelocityEnu 5.0 (-3.0) 0.0)
                (Just $ SpeedMps 5.83)
            , milView        = Just $ MilView (Just DispositionHostile)
            , ontology       = Just $ Ontology (Just PlatformAir) (Just "quadcopter")
            , tracked        = Just $ Tracked (Just $ TrackQuality 0.95) (Just "2025-01-15T12:00:00Z")
            , provenance     = Just $ Provenance (Just "sentry-cam-01") (Just DataTypeLive) (Just "usb-cam")
            , health         = Just $ Health (Just ConnectionOnline)
            , targetPriority = Just $ TargetPriority (Just ThreatHigh)
            }
      decode (encode entity) `shouldBe` Just entity

    -- missing optional fields should parse fine, not crash
    it "handles missing optional fields gracefully" $ do
      let json = "{\"entity_id\": \"trk-002\", \"is_live\": true}"
          result = eitherDecode json :: Either String Entity
      case result of
        Left err -> expectationFailure $ "parse failed: " ++ err
        Right entity -> do
          entityId entity `shouldBe` EntityId "trk-002"
          isLive entity `shouldBe` Just True
          location entity `shouldBe` Nothing
          milView entity `shouldBe` Nothing

    -- garbage JSON should fail with a useful error, not crash
    it "rejects garbage JSON" $ do
      let garbage = "{\"not_an_entity\": true}" :: LBS.ByteString
          result = eitherDecode garbage :: Either String Entity
      result `shouldSatisfy` isLeft

    -- the big property: decode . encode == id for ALL random entities
    -- QuickCheck generates hundreds with every field randomly populated
    it "QuickCheck: decode . encode == id for Entity (all fields)" $
      property propFullRoundTrip

  describe "Disposition JSON" $ do
    -- check a couple of variants by hand to make sure wire format is correct
    it "encodes HOSTILE correctly" $
      encode DispositionHostile `shouldBe` "\"DISPOSITION_HOSTILE\""

    it "encodes FRIENDLY correctly" $
      encode DispositionFriendly `shouldBe` "\"DISPOSITION_FRIENDLY\""

    -- rejects unknown disposition strings instead of silently succeeding
    it "rejects unknown disposition" $ do
      let bad = "\"DISPOSITION_BANANA\"" :: LBS.ByteString
          result = eitherDecode bad :: Either String Disposition
      result `shouldSatisfy` isLeft

    -- QuickCheck: every disposition survives round trip
    it "QuickCheck: all dispositions round-trip" $
      property propDispositionRoundTrip

  describe "ThreatLevel JSON" $ do
    it "encodes CRITICAL correctly" $
      encode ThreatCritical `shouldBe` "\"THREAT_LEVEL_CRITICAL\""

    it "QuickCheck: all threat levels round-trip" $
      property propThreatLevelRoundTrip

  describe "PlatformType JSON" $ do
    it "encodes AIR correctly" $
      encode PlatformAir `shouldBe` "\"PLATFORM_TYPE_AIR\""

    it "QuickCheck: all platform types round-trip" $
      property propPlatformTypeRoundTrip

  describe "ConnectionStatus JSON" $ do
    it "encodes ONLINE correctly" $
      encode ConnectionOnline `shouldBe` "\"CONNECTION_STATUS_ONLINE\""

    it "QuickCheck: all connection statuses round-trip" $
      property propConnectionStatusRoundTrip

  describe "DataType JSON" $ do
    it "encodes LIVE correctly" $
      encode DataTypeLive `shouldBe` "\"DATA_TYPE_LIVE\""

    it "QuickCheck: all data types round-trip" $
      property propDataTypeRoundTrip


-- helper thats not in prelude for older GHCs
isLeft :: Either a b -> Bool
isLeft (Left _) = True
isLeft _        = False
