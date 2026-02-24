module Test.Sentry.Protocol (spec) where

import Data.Bits (xor)
import qualified Data.ByteString as BS
import Data.Word (Word8)
import Test.Hspec
import Test.QuickCheck

import Sentry.Protocol

spec :: Spec
spec = do
  describe "encode" $ do
    it "produces exactly 6 bytes" $
      BS.length (encode (SetMotor Forward 128)) `shouldBe` 6

    it "starts with sync byte 0xAA" $
      BS.index (encode Ping) 0 `shouldBe` 0xAA

    it "checksum of encoded frame matches byte 5" $
      let frame = encode (SetMotor Forward 128)
      in checksum frame `shouldBe` BS.index frame 5

    it "SafeStop has direction=0x00 and speed=0x00" $
      let frame = encode SafeStop
      in do
        BS.index frame 2 `shouldBe` 0x00
        BS.index frame 3 `shouldBe` 0x00

    it "SetMotor Forward encodes direction as 0x01" $
      BS.index (encode (SetMotor Forward 200)) 2 `shouldBe` 0x01

    it "SetMotor Reverse encodes direction as 0x02" $
      BS.index (encode (SetMotor Reverse 100)) 2 `shouldBe` 0x02

    it "Ping encodes cmd byte as 0x02" $
      BS.index (encode Ping) 1 `shouldBe` 0x02

    -- the big property: every encoded command has a valid checksum
    it "QuickCheck: any encoded command has valid checksum" $
      property $
        forAll genCommand $ \cmd ->
          let frame = encode cmd
          in checksum frame == BS.index frame 5

  describe "decode" $ do
    -- actually decode valid frames and check the fields
    it "decodes a valid ACK response" $
      let frame = mkResponse 0x06 0x01 0x80
      in decode frame `shouldBe` Right MotorResponse
           { respAck = AckOk, respDirection = Forward, respSpeed = 0x80 }

    it "decodes a valid NAK response" $
      let frame = mkResponse 0x15 0x00 0x00
      in decode frame `shouldBe` Right MotorResponse
           { respAck = AckNak, respDirection = Stop, respSpeed = 0x00 }

    -- reject malformed frames
    it "rejects bad sync byte" $
      decode (BS.pack [0xBB, 0x06, 0x00, 0x00, 0x00, 0x06])
        `shouldBe` Left "bad sync byte"

    it "rejects bad checksum" $
      decode (BS.pack [0xAA, 0x06, 0x01, 0x80, 0x00, 0x00])
        `shouldBe` Left "bad checksum"

    it "rejects wrong length (too short)" $
      decode (BS.pack [0xAA, 0x06, 0x00])
        `shouldBe` Left "bad length"

    it "rejects wrong length (too long)" $
      decode (BS.pack [0xAA, 0x06, 0x00, 0x00, 0x00, 0x06, 0xFF])
        `shouldBe` Left "bad length"

    it "rejects empty frame" $
      decode BS.empty `shouldBe` Left "bad length"

    it "rejects unknown ack byte" $
      let frame = mkResponse 0xFF 0x00 0x00
      in decode frame `shouldBe` Left "unknown ack"

    it "rejects unknown direction byte" $ do
      let chk = 0x06 `xor` 0x99 `xor` 0x00 `xor` 0x00 :: Word8
          frame = BS.pack [0xAA, 0x06, 0x99, 0x00, 0x00, chk]
      decode frame `shouldBe` Left "unknown direction"

mkResponse :: Word8 -> Word8 -> Word8 -> BS.ByteString
mkResponse ackByte dirByte speedByte =
  let chk = ackByte `xor` dirByte `xor` speedByte `xor` 0x00
  in BS.pack [0xAA, ackByte, dirByte, speedByte, 0x00, chk]

genCommand :: Gen MotorCommand
genCommand =
  oneof
    [ SetMotor <$> genDirection <*> arbitrary
    , pure Ping
    , pure SafeStop
    ]

genDirection :: Gen Direction
genDirection = elements [Forward, Reverse, Stop]
