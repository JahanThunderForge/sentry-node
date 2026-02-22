module Lattice.Types
  ( Entity(..)
  , EntityId(..)
  , module Lattice.Types.Location
  , module Lattice.Types.MilView
  , module Lattice.Types.Ontology
  , module Lattice.Types.Tracked
  , module Lattice.Types.Provenance
  , module Lattice.Types.Health
  , module Lattice.Types.TargetPriority
  ) where

import Data.Aeson (FromJSON(..), ToJSON(..), (.:), (.:?), (.=), object, withObject)
import Data.Text (Text)
import GHC.Generics (Generic)

import Lattice.Types.Location
import Lattice.Types.MilView
import Lattice.Types.Ontology
import Lattice.Types.Tracked
import Lattice.Types.Provenance
import Lattice.Types.Health
import Lattice.Types.TargetPriority

-- Wrapper for entity identifiers. Newtype prevents mixing with other Text values.
newtype EntityId = EntityId { unEntityId :: Text }
  deriving stock (Show, Eq, Ord, Generic)
  deriving newtype (FromJSON, ToJSON)


data Entity = Entity
  { entityId       :: !EntityId
  , isLive         :: !(Maybe Bool)
  , location       :: !(Maybe Location)
  , milView        :: !(Maybe MilView)
  , ontology       :: !(Maybe Ontology)
  , tracked        :: !(Maybe Tracked)
  , provenance     :: !(Maybe Provenance)
  , health         :: !(Maybe Health)
  , targetPriority :: !(Maybe TargetPriority)
  } deriving stock (Show, Eq, Generic)


instance FromJSON Entity where
  parseJSON = withObject "Entity" $ \o ->
    Entity
      <$> o .:  "entity_id"
      <*> o .:? "is_live"
      <*> o .:? "location"
      <*> o .:? "mil_view"
      <*> o .:? "ontology"
      <*> o .:? "tracked"
      <*> o .:? "provenance"
      <*> o .:? "health"
      <*> o .:? "target_priority"

instance ToJSON Entity where
  toJSON (Entity eid live loc mv ont tr prov hlth tp) =
    object
      [ "entity_id"       .= eid
      , "is_live"         .= live
      , "location"        .= loc
      , "mil_view"        .= mv
      , "ontology"        .= ont
      , "tracked"         .= tr
      , "provenance"      .= prov
      , "health"          .= hlth
      , "target_priority" .= tp
      ]
