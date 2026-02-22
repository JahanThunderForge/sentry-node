
-- automake some helper info for the Disposition type
-- derive normally in a clear way

module Lattice.Types.MilView
  ( MilView(..)
  , Disposition(..)
  ) where

import Data.Aeson (FromJSON(..), ToJSON(..), (.:?), (.=), object, withObject, withText)
import qualified Data.Text as T
import GHC.Generics (Generic)



data Disposition
  = DispositionFriendly
  | DispositionHostile
  | DispositionSuspect
  | DispositionNeutral
  | DispositionPending
  | DispositionAssumedFriend
  | DispositionUnknown
  deriving stock (Show, Eq, Ord, Bounded, Enum, Generic)

instance FromJSON Disposition where
  parseJSON = withText "Disposition" $ \t ->
    case t of
      "DISPOSITION_FRIENDLY" -> pure DispositionFriendly
      "DISPOSITION_HOSTILE" -> pure DispositionHostile
      "DISPOSITION_SUSPECT" -> pure DispositionSuspect
      "DISPOSITION_NEUTRAL" -> pure DispositionNeutral
      "DISPOSITION_PENDING" -> pure DispositionPending
      "DISPOSITION_ASSUMED_FRIEND" -> pure DispositionAssumedFriend
      "DISPOSITION_UNKNOWN" -> pure DispositionUnknown
      _ -> fail $ "Unknown disposition wire value: " ++ T.unpack t
   
instance ToJSON Disposition where
  toJSON d = case d of  
      DispositionFriendly      -> toJSON ("DISPOSITION_FRIENDLY" :: T.Text)
      DispositionHostile       -> toJSON ("DISPOSITION_HOSTILE" :: T.Text)
      DispositionSuspect       -> toJSON ("DISPOSITION_SUSPECT" :: T.Text)
      DispositionNeutral       -> toJSON ("DISPOSITION_NEUTRAL" :: T.Text)
      DispositionPending       -> toJSON ("DISPOSITION_PENDING" :: T.Text)
      DispositionAssumedFriend -> toJSON ("DISPOSITION_ASSUMED_FRIEND" :: T.Text)
      DispositionUnknown       -> toJSON ("DISPOSITION_UNKNOWN" :: T.Text)


data MilView = MilView
  { disposition :: !(Maybe Disposition)
  } deriving stock (Show, Eq, Generic)

instance FromJSON MilView where
  parseJSON = withObject "MilView" $ \o ->
    MilView <$> o .:? "disposition"

instance ToJSON MilView where
  toJSON (MilView disp) = 
    object [ "disposition" .= disp ]
