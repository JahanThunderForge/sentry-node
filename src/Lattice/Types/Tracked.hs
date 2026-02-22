module Lattice.Types.Tracked
  ( Tracked(..)
  , TrackQuality(..)
  ) where

import Data.Aeson (FromJSON(..), ToJSON(..), (.:?), (.:), (.=), object, withObject)
import qualified Data.Text as T
import GHC.Generics (Generic)

newtype TrackQuality = TrackQuality { unTrackQuality :: Double }
  deriving stock (Show, Eq, Ord, Generic)

instance FromJSON TrackQuality where
  parseJSON = withObject "TrackQuality" $ \o ->
    TrackQuality <$> o .: "quality"

instance ToJSON TrackQuality where
  toJSON (TrackQuality q) =
    object [ "quality" .= q ]


-- | Tracking metadata for an entity.
--   JSON keys: "track_quality", "last_measurement_time"
data Tracked = Tracked
  { trackQuality       :: !(Maybe TrackQuality)
  , lastMeasurementTime :: !(Maybe T.Text)
  } deriving stock (Show, Eq, Generic)

instance FromJSON Tracked where
  parseJSON = withObject "Tracked" $ \o -> 
    Tracked 
    <$> o .:? "track_quality"
    <*> o .:? "last_measurement_time"

instance ToJSON Tracked where
  toJSON (Tracked tq t) = 
    object 
    [
      "track_quality" .= tq
    ,  "last_measurement_time" .= t
    ]

