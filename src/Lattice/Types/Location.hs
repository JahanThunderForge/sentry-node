module Lattice.Types.Location
  ( Location(..)
  , Position(..)
  , VelocityEnu(..)
  , SpeedMps(..)
  , Latitude(..)
  , Longitude(..)
  , AltitudeHaeMeters(..)
  ) where

import Data.Aeson (FromJSON(..), ToJSON(..), (.:), (.:?), (.=), object, withObject)
import GHC.Generics (Generic)


newtype Latitude = Latitude { unLatitude :: Double }
  deriving stock (Show, Eq, Ord, Generic)
  deriving newtype (FromJSON, ToJSON)

newtype Longitude = Longitude { unLongitude :: Double }
  deriving stock (Show, Eq, Ord, Generic)
  deriving newtype (FromJSON, ToJSON)

newtype AltitudeHaeMeters = AltitudeHaeMeters { unAltitude :: Double }
  deriving stock (Show, Eq, Ord, Generic)
  deriving newtype (FromJSON, ToJSON)

data Position = Position
  { latitudeDegrees   :: !Latitude
  , longitudeDegrees  :: !Longitude
  , altitudeHaeMeters :: !AltitudeHaeMeters
  } deriving stock (Show, Eq, Generic)

instance FromJSON Position where
  parseJSON = withObject "Position" $ \o -> 
    Position 
    <$> o .: "latitude_degrees"      
    <*> o .: "longitude_degrees"
    <*> o .: "altitude_hae_meters"
    
instance ToJSON Position where
  toJSON (Position lat lon alt) = 
    object 
    [ "latitude_degrees"         .= lat
    , "longitude_degrees"        .= lon 
    , "altitude_hae_meters"      .= alt
    ]

-- ENU for velocity enu, keys being da e n u
data VelocityEnu = VelocityEnu
  { velEast  :: !Double
  , velNorth :: !Double
  , velUp    :: !Double
  } deriving stock (Show, Eq, Generic)

instance FromJSON VelocityEnu where
  parseJSON = withObject "VelocityEnu" $ \o -> 
    VelocityEnu
    <$> o .: "e"
    <*> o .: "n"
    <*> o .: "u"

instance ToJSON VelocityEnu where
  toJSON (VelocityEnu e n u) =
    object 
    [ "e" .= e
    , "n" .= n
    , "u" .= u
    ]


newtype SpeedMps = SpeedMps { unSpeed :: Double }
  deriving stock (Show, Eq, Ord, Generic)

instance FromJSON SpeedMps where
  parseJSON = withObject "SpeedMps" $ \o -> 
    SpeedMps <$> o .: "speed"


instance ToJSON SpeedMps where
  toJSON (SpeedMps s) =
    object [ "speed" .= s ]

data Location = Location
  { position    :: !(Maybe Position)
  , velocityEnu :: !(Maybe VelocityEnu)
  , speedMps    :: !(Maybe SpeedMps)
  } deriving stock (Show, Eq, Generic)

instance FromJSON Location where
  parseJSON = withObject "Location" $ \o ->
    Location
    <$> o .:? "position"
    <*> o .:? "velocity_enu"
    <*> o .:? "speed_mps"

instance ToJSON Location where
  toJSON (Location pos vel spd) =
    object 
    [ "position"    .= pos
    , "velocity_enu" .= vel
    , "speed_mps"   .= spd
    ]
  
