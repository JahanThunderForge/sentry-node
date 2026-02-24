module Sentry.Assess
  ( SentryConfig(..)
  , RankedTarget(..)
  , Degrees(..)
  , Meters(..)
  , assessThreats
  , bearingTo
  , haversineDistance
  , closingSpeed
  , threatLevelValue
  ) where

import Data.Fixed (mod')
import Data.List (sortBy)
import Data.Maybe (mapMaybe)
import Data.Ord (Down(..), comparing)

import Lattice.Types

-- Newtype wrappers for degrees and meters
newtype Degrees = Degrees { unDegrees :: Double }
  deriving stock (Show, Eq, Ord)

newtype Meters = Meters { unMeters :: Double }
  deriving stock (Show, Eq, Ord)

-- Config for a sentry position.
data SentryConfig = SentryConfig
  { sentryLat      :: !Latitude
  , sentryLon      :: !Longitude
  , weightDistance  :: !Double
  , weightPriority :: !Double
  , weightVelocity :: !Double
  } deriving stock (Show, Eq)

-- ranking targets: entity, bearing, range, score, 
data RankedTarget = RankedTarget
  { rtEntity  :: !Entity
  , rtBearing :: !Degrees
  , rtRange   :: !Meters
  , rtScore   :: !Double
  } deriving stock (Show, Eq)

-- first we gonna make some helpder functions 
degToRad :: Double -> Double
degToRad = (*) (pi / 180)

radToDeg :: Double -> Double
radToDeg = (*) (180 / pi)

normalize360 :: Double -> Double
normalize360 x =
  let y = mod' x 360.0
  in if y < 0 then y + 360.0 else y



-- formula bearingto use: atan2(sin(Δlon) × cos(lat2), cos(lat1) × sin(lat2) - sin(lat1) × cos(lat2) × cos(Δlon))
-- it tells us which direction to point if youre standing at first point and want t oface pont b. our assessthreats calls it computing the bearing from the sentrys position to the target
-- sentry.assign using rankedTarget to decide which direction to slew the turret

bearingTo :: Latitude -> Longitude -> Latitude -> Longitude -> Degrees
bearingTo (Latitude lat1d) (Longitude lon1d) (Latitude lat2d) (Longitude lon2d) =
  let lat1 = degToRad lat1d
      lat2 = degToRad lat2d
      dlon = degToRad (lon2d - lon1d)
      -- dlon is the difference in longitude between the two points
      y = sin dlon * cos lat2
      x = cos lat1 * sin lat2 - sin lat1 * cos lat2 * cos dlon

      bearing = radToDeg (atan2 y x)
      bearing' = normalize360 bearing
  in Degrees bearing'

-- compute closing speed: how fast is the target approaching the sentry?
haversineDistance :: Latitude -> Longitude -> Latitude -> Longitude -> Meters
haversineDistance (Latitude lat1d) (Longitude lon1d) (Latitude lat2d) (Longitude lon2d) =
  let r = 6371000 :: Double -- Earth radius in meters
      lat1 = degToRad lat1d
      lat2 = degToRad lat2d
      dlat = degToRad (lat2d - lat1d)
      dlon = degToRad (lon2d - lon1d)

      a = sin (dlat / 2) ** 2 + cos lat1 * cos lat2 * sin (dlon/2) ** 2
      c = 2 * atan2 (sqrt a) (sqrt (1 - a))
  in Meters (r * c)
     

  
closingSpeed :: Latitude -> Longitude -> Latitude -> Longitude -> Maybe VelocityEnu -> Double
closingSpeed _ _ _ _ Nothing = 0 -- this is because if the target has no velocity, we can't compute the closing speed
closingSpeed sentLat sentLon targetLat targetLon (Just (VelocityEnu ve vn _vu)) =
  let Degrees bDeg = bearingTo targetLat targetLon sentLat sentLon -- target bearing from sentry to target
      bRad = degToRad bDeg
      uE = sin bRad
      uN = cos bRad 
  in ve * uE + vn * uN
  -- ve * uE is the velocity of the target in the east direction, vn * uN is the velocity of the target in the north direction
  -- sum of them is closing speed



assessThreats :: SentryConfig -> [Entity] -> [RankedTarget]
assessThreats config entities = 
  let sentLat = sentryLat config
      sentLon = sentryLon config

      isThreatEntity :: Entity -> Bool
      isThreatEntity entity = 
        case milView entity >>= disposition of 
          Just DispositionHostile -> True
          Just DispositionSuspect -> True
          _ -> False

      extractKinematics :: Entity -> Maybe (Latitude, Longitude, Maybe VelocityEnu)
      extractKinematics entity = do
        loc <- location entity
        pos <- position loc
        let lat = latitudeDegrees pos
            lon = longitudeDegrees pos
        let vel = velocityEnu loc 
        pure (lat, lon, vel) -- global frame of reference

      mkRanked :: Entity -> Maybe RankedTarget
      mkRanked entity = do 
        (tLat, tLon, tVel) <- extractKinematics entity

        let bearing = bearingTo sentLat sentLon tLat tLon
            rng@(Meters rngm) = haversineDistance sentLat sentLon tLat tLon
            rMeters = max 0.1 rngm -- minimum range of 0.1 meters
            closingSpd = closingSpeed sentLat sentLon tLat tLon tVel
            priority = threatLevelValue (threat =<< targetPriority entity)

            score = weightDistance config * (1.0 / rMeters) + weightPriority config * priority + weightVelocity config * closingSpd
        pure $ RankedTarget
          { rtEntity  = entity
          , rtBearing = bearing
          , rtRange   = rng
          , rtScore   = score
          }
      ranked = mapMaybe mkRanked (filter isThreatEntity entities)
    in sortBy (comparing (Down . rtScore)) ranked

-- Map ThreatLevel numeric priority for scoring
threatLevelValue :: Maybe ThreatLevel -> Double
threatLevelValue (Just ThreatCritical) = 5.0
threatLevelValue (Just ThreatHigh)     = 4.0
threatLevelValue (Just ThreatMedium)   = 3.0
threatLevelValue (Just ThreatLow)      = 2.0
threatLevelValue (Just ThreatNone)     = 1.0
threatLevelValue Nothing               = 1.0
