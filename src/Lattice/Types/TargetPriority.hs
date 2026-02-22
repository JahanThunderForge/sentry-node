module Lattice.Types.TargetPriority
  ( TargetPriority(..)
  , ThreatLevel(..)
  ) where

import Data.Aeson (FromJSON(..), ToJSON(..), (.:?), (.=), object, withObject, withText)
import qualified Data.Text as T
import GHC.Generics (Generic)

-- | How dangerous this entity is assessed to be.
-- Mirrors: lattice-sdk-python/src/anduril/types/target_priority.py :: Threat
--
-- YOUR ASSIGNMENT:
--   Wire values: "THREAT_LEVEL_NONE", "THREAT_LEVEL_LOW", "THREAT_LEVEL_MEDIUM",
--                "THREAT_LEVEL_HIGH", "THREAT_LEVEL_CRITICAL"
data ThreatLevel
  = ThreatNone
  | ThreatLow
  | ThreatMedium
  | ThreatHigh
  | ThreatCritical
  deriving stock (Show, Eq, Ord, Bounded, Enum, Generic)

instance FromJSON ThreatLevel where
  parseJSON = withText "ThreatLevel" $ \t -> 
    case t of 
      "THREAT_LEVEL_NONE" -> pure ThreatNone
      "THREAT_LEVEL_LOW" -> pure ThreatLow
      "THREAT_LEVEL_MEDIUM" -> pure ThreatMedium
      "THREAT_LEVEL_HIGH" -> pure ThreatHigh
      "THREAT_LEVEL_CRITICAL" -> pure ThreatCritical
      _ -> fail $ "invalid threat level wire value: " ++ T.unpack t

instance ToJSON ThreatLevel where
  toJSON tl = 
    case tl of 
      ThreatNone -> "THREAT_LEVEL_NONE"
      ThreatLow -> "THREAT_LEVEL_LOW"
      ThreatMedium -> "THREAT_LEVEL_MEDIUM"
      ThreatHigh -> "THREAT_LEVEL_HIGH"
      ThreatCritical -> "THREAT_LEVEL_CRITICAL"

data TargetPriority = TargetPriority
  { threat :: !(Maybe ThreatLevel)
  } deriving stock (Show, Eq, Generic)

instance FromJSON TargetPriority where
  parseJSON = withObject "TargetPriority" $ \o ->
    TargetPriority <$> o .:? "threat"

instance ToJSON TargetPriority where
  toJSON (TargetPriority t) = 
    object [ "threat" .= t ]
