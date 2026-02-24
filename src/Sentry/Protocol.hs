module Sentry.Protocol
  ( MotorCommand(..)
  , MotorResponse(..)
  , Ack(..)
  , Direction(..)
  , encode
  , decode
  , checksum
  ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Bits (xor)
import Data.Word (Word8)

-- dc motor direction 
data Direction = Forward | Reverse | Stop
  deriving stock (Show, Eq)

-- commands sent from haskell to arduino 
data MotorCommand
  = SetMotor !Direction !Word8    -- direction + speed (0-255 PWM)
  | Ping
  | SafeStop   -- stop motor immediately                   
  deriving stock (Show, Eq)

-- arduino response
data Ack = AckOk | AckNak
  deriving stock (Show, Eq)

data MotorResponse = MotorResponse
  { respAck       :: !Ack
  , respDirection :: !Direction
  , respSpeed     :: !Word8
  } deriving stock (Show, Eq)

-- Wire format (6 bytes each direction):
-- [0xAA] [cmd:1] [direction:1] [speed:1] [unused:1] [checksum:1]
--
-- cmd byte: 0x01 = SetMotor, 0x02 = Ping, 0x03 = SafeStop
-- direction byte: 0x01 = Forward (clockwise), 0x02 = Reverse (counterclockwise), 0x00 = Stop
-- speed byte: 0-255 (PWM duty cycle)
-- unused byte: 0x00 (future use)
-- checksum: XOR of bytes 1-4
--
-- [0xAA] [ack:1] [direction:1] [speed:1] [unused:1] [checksum:1]
-- ack byte: 0x06 = ACK, 0x15 = NAK

-- Encode a command to 6 bytes.

encode :: MotorCommand -> ByteString
encode cmd =
  let (cmdByte, dirByte, speedByte) =
        case cmd of
          SetMotor dir speed -> (0x01, directionToByte dir, speed)
          Ping               -> (0x02, 0x00, 0x00)
          SafeStop           -> (0x03, 0x00, 0x00)
      chk = cmdByte `xor` dirByte `xor` speedByte `xor` 0x00
  in BS.pack [syncByte, cmdByte, dirByte, speedByte, 0x00, chk]

-- Decode a 6-byte response.

decode :: ByteString -> Either String MotorResponse
decode frame
  | BS.length frame /= 6 = Left "bad length"
  | BS.index frame 0 /= syncByte = Left "bad sync byte"
  | checksum frame /= BS.index frame 5 = Left "bad checksum"
  | otherwise = do
      ack <- ackFromByte (BS.index frame 1)
      dir <- directionFromByte (BS.index frame 2)
      let speed = BS.index frame 3
      pure MotorResponse
        { respAck = ack
        , respDirection = dir
        , respSpeed = speed
        }

-- XOR checksum of bytes 1-4 of a 6-byte frame, xor is the bitwise operation
checksum :: ByteString -> Word8
checksum frame = BS.foldl' xor 0 (BS.take 4 (BS.drop 1 frame))

syncByte :: Word8
syncByte = 0xAA

directionToByte :: Direction -> Word8
directionToByte Forward = 0x01
directionToByte Reverse = 0x02
directionToByte Stop = 0x00

directionFromByte :: Word8 -> Either String Direction
directionFromByte 0x01 = Right Forward
directionFromByte 0x02 = Right Reverse
directionFromByte 0x00 = Right Stop
directionFromByte _ = Left "unknown direction"

ackFromByte :: Word8 -> Either String Ack
ackFromByte 0x06 = Right AckOk
ackFromByte 0x15 = Right AckNak
ackFromByte _ = Left "unknown ack"
