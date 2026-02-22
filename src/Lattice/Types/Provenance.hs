module Lattice.Types.Provenance
  ( Provenance(..)
  , DataType(..)
  ) where

import Data.Aeson (FromJSON(..), ToJSON(..), (.:?), (.=), object, withObject, withText)
import qualified Data.Text as T
import GHC.Generics (Generic)

-- How the entity data was produced.


data DataType
  = DataTypeLive
  | DataTypeSimulated
  | DataTypeReplay
  deriving stock (Show, Eq, Ord, Bounded, Enum, Generic)

instance FromJSON DataType where
  parseJSON = withText "DataType" $ \t ->
    case t of
      "DATA_TYPE_LIVE" -> pure DataTypeLive
      "DATA_TYPE_SIMULATED" -> pure DataTypeSimulated
      "DATA_TYPE_REPLAY" -> pure DataTypeReplay
      _ -> fail $ "invalid data type wire value: " ++ T.unpack t

instance ToJSON DataType where
  toJSON dt = 
    case dt of 
      DataTypeLive -> "DATA_TYPE_LIVE"
      DataTypeSimulated -> "DATA_TYPE_SIMULATED"
      DataTypeReplay -> "DATA_TYPE_REPLAY"
      

-- JSON keys: "integration_name", "data_type", "source_id"
data Provenance = Provenance
  { integrationName :: !(Maybe T.Text)
  , dataType        :: !(Maybe DataType)
  , sourceId        :: !(Maybe T.Text)
  } deriving stock (Show, Eq, Generic)

instance FromJSON Provenance where
  parseJSON = withObject "Provenance" $ \o -> 
    Provenance 
    <$> o .:? "integration_name"
    <*> o .:? "data_type"
    <*> o .:? "source_id"

instance ToJSON Provenance where
  toJSON (Provenance name dt sid) = 
    object 
    [ "integration_name" .= name
    , "data_type" .= dt
    , "source_id" .= sid
    ]
    