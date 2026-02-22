module Lattice.Types.Health
  ( Health(..)
  , ConnectionStatus(..)
  ) where

import Data.Aeson (FromJSON(..), ToJSON(..), (.:?), (.=), object, withObject, withText)
import qualified Data.Text as T
import GHC.Generics (Generic)

-- we mirroring health.py and its jsut to check if entit source sensor is connected.
data ConnectionStatus
  = ConnectionOnline
  | ConnectionOffline
  deriving stock (Show, Eq, Ord, Bounded, Enum, Generic)

instance FromJSON ConnectionStatus where
  parseJSON = withText "ConnectionStatus" $ \t ->
    case t of
      "CONNECTION_STATUS_ONLINE" -> pure ConnectionOnline
      "CONNECTION_STATUS_OFFLINE" -> pure ConnectionOffline
      _ -> fail $ "invalid connection status wire value: " ++ T.unpack t
instance ToJSON ConnectionStatus where
  toJSON cs = 
    case cs of 
      ConnectionOnline -> "CONNECTION_STATUS_ONLINE"
      ConnectionOffline -> "CONNECTION_STATUS_OFFLINE"

data Health = Health
  { connectionStatus :: !(Maybe ConnectionStatus)
  } deriving stock (Show, Eq, Generic)

instance FromJSON Health where
  parseJSON = withObject "Health" $ \o ->
    Health <$> o .:? "connection_status"

instance ToJSON Health where
  toJSON (Health cs )= 
    object [ "connection_status" .= cs ]
