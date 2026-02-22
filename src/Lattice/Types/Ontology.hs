module Lattice.Types.Ontology
  ( Ontology(..)
  , PlatformType(..)
  ) where

import Data.Aeson (FromJSON(..), ToJSON(..), (.:?), (.=), object, withObject, withText)
import qualified Data.Text as T
import GHC.Generics (Generic)


data PlatformType
  = PlatformOther
  | PlatformAir
  | PlatformLand
  | PlatformSurface
  | PlatformSubsurface
  | PlatformSpace
  deriving stock (Show, Eq, Ord, Bounded, Enum, Generic)

instance FromJSON PlatformType where
  parseJSON = withText "PlatformType" $ \t ->
    case t of
      "PLATFORM_TYPE_OTHER" -> pure PlatformOther
      "PLATFORM_TYPE_AIR" -> pure PlatformAir
      "PLATFORM_TYPE_LAND" -> pure PlatformLand
      "PLATFORM_TYPE_SURFACE" -> pure PlatformSurface
      "PLATFORM_TYPE_SUBSURFACE" -> pure PlatformSubsurface
      "PLATFORM_TYPE_SPACE" -> pure PlatformSpace
      _ -> fail $ "invalid platform type wire value: " ++ T.unpack t

instance ToJSON PlatformType where
  toJSON pt =
    case pt of
      PlatformOther -> "PLATFORM_TYPE_OTHER"
      PlatformAir -> "PLATFORM_TYPE_AIR"
      PlatformLand -> "PLATFORM_TYPE_LAND"
      PlatformSurface -> "PLATFORM_TYPE_SURFACE"
      PlatformSubsurface -> "PLATFORM_TYPE_SUBSURFACE"
      PlatformSpace -> "PLATFORM_TYPE_SPACE"

data Ontology = Ontology
  { platformType :: !(Maybe PlatformType)
  , specificType :: !(Maybe T.Text)
  } deriving stock (Show, Eq, Generic)

instance FromJSON Ontology where
  parseJSON = withObject "Ontology" $ \o ->
    Ontology
    <$> o .:? "platform_type"
    <*> o .:? "specific_type"

instance ToJSON Ontology where
  toJSON (Ontology pt st) =
    object
    [ "platform_type" .= pt
    , "specific_type" .= st
    ]