{-# LANGUAGE OverloadedStrings #-}

module ContainerSpec (spec) where

import Codec.Picture (convertRGBA8)
import Codec.Picture.Types (DynamicImage (..), PixelRGB8 (..), PixelRGBA8 (..), generateImage, pixelAt)
import Codec.Picture.WebP (WebPAnimFrame (..), decodeWebP, decodeWebPAnimation, decodeWebPAnimationComposited, decodeWebPFirstFrame, encodeWebPLossless, encodeWebPLossy)
import Codec.Picture.WebP.Internal.Container
import Control.Monad (forM_)
import Data.Bits
import qualified Data.ByteString as B
import Data.Word
import Test.Hspec

spec :: Spec
spec = describe "Container" $ do
  describe "RIFF Header Validation" $ do
    it "rejects non-RIFF files" $ do
      let invalidData = B.pack [0, 0, 0, 0]
      parseWebP invalidData `shouldSatisfy` isLeft

    it "rejects RIFF files that aren't WebP" $ do
      let invalidRiff = makeRIFFHeader "WAVE" 0
      parseWebP invalidRiff `shouldSatisfy` isLeft

    it "rejects truncated RIFF header" $ do
      let truncated = B.pack [82, 73, 70, 70] -- Just "RIFF"
      parseWebP truncated `shouldSatisfy` isLeft

    it "validates correct RIFF+WEBP header" $ do
      let header = makeRIFFHeader "WEBP" 20
      parseWebP header `shouldSatisfy` isLeft -- Will fail on missing chunks but header is valid
    it "rejects missing RIFF bytes even when the image chunk is complete" $ do
      let complete = encodeWebPLossless $ generateImage (\_ _ -> PixelRGBA8 12 34 56 255) 1 1
          truncated = B.take 4 complete <> word32ToBytes (fromIntegral $ B.length complete + 100) <> B.drop 8 complete
      fmap (const ()) (decodeWebP truncated) `shouldSatisfy` isLeft

    it "ignores data after the declared RIFF size" $ do
      let complete = encodeWebPLossless $ generateImage (\_ _ -> PixelRGBA8 12 34 56 255) 1 1
      fmap (const ()) (decodeWebP (complete <> "trailing data")) `shouldBe` Right ()
  describe "Simple VP8L Format" $ do
    it "parses simple VP8L structure" $ do
      let vp8lChunk = makeVP8LChunk [0x2F, 0, 0, 0, 0, 0, 0, 0]
          webp = makeRIFFHeader "WEBP" (fromIntegral $ B.length vp8lChunk) <> vp8lChunk
      case parseWebP webp of
        Right (WebPSimpleLossless _) -> return ()
        Right other -> expectationFailure $ "Expected WebPSimpleLossless, got: " ++ show other
        Left err -> expectationFailure $ "Parse failed: " ++ err

    it "parses VP8L with minimal valid header" $ do
      -- VP8L signature (0x2F) + 14-bit width + 14-bit height + alpha + version
      let vp8lData = [0x2F, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00]
          vp8lChunk = makeVP8LChunk vp8lData
          webp = makeRIFFHeader "WEBP" (fromIntegral $ B.length vp8lChunk) <> vp8lChunk
      case parseWebP webp of
        Right (WebPSimpleLossless dat) -> do
          B.length dat `shouldBe` 8
        other -> expectationFailure $ "Unexpected result: " ++ show other

  describe "Simple VP8 Format" $ do
    it "parses simple VP8 structure" $ do
      let vp8Chunk = makeVP8Chunk [0, 0, 0, 0, 0, 0, 0, 0, 0, 0]
          webp = makeRIFFHeader "WEBP" (fromIntegral $ B.length vp8Chunk) <> vp8Chunk
      case parseWebP webp of
        Right (WebPSimpleLossy _) -> return ()
        Right other -> expectationFailure $ "Expected WebPSimpleLossy, got: " ++ show other
        Left err -> expectationFailure $ "Parse failed: " ++ err

  describe "Extended VP8X Format" $ do
    it "rejects canvas areas above the unsigned 32-bit limit" $ do
      let vp8xChunk = makeVP8XChunk 0x00 65535 65535
          webp = makeRIFFHeader "WEBP" (fromIntegral $ B.length vp8xChunk) <> vp8xChunk
      parseWebP webp `shouldSatisfy` isLeft

    it "accepts the largest permitted canvas area without allocating pixels" $ do
      let vp8xChunk = makeVP8XChunk 0x00 65534 65536
          webp = makeRIFFHeader "WEBP" (fromIntegral $ B.length vp8xChunk) <> vp8xChunk
      case parseWebP webp of
        Right (WebPExtended header _) -> do
          toInteger (vp8xCanvasWidth header) * toInteger (vp8xCanvasHeight header) `shouldBe` 4294967295
        other -> expectationFailure $ "Unexpected result: " ++ show other

    it "parses VP8X header" $ do
      let vp8xChunk = makeVP8XChunk 0x00 100 100
          webp = makeRIFFHeader "WEBP" (fromIntegral $ B.length vp8xChunk) <> vp8xChunk
      case parseWebP webp of
        Right (WebPExtended header _) -> do
          vp8xCanvasWidth header `shouldBe` 101 -- Field + 1
          vp8xCanvasHeight header `shouldBe` 101
        other -> expectationFailure $ "Unexpected result: " ++ show other

    it "parses VP8X with alpha flag" $ do
      let flags = 0x10 -- Alpha bit set
          vp8xChunk = makeVP8XChunk flags 99 99
          webp = makeRIFFHeader "WEBP" (fromIntegral $ B.length vp8xChunk) <> vp8xChunk
      case parseWebP webp of
        Right (WebPExtended header _) -> do
          vp8xHasAlpha header `shouldBe` True
          vp8xHasAnimation header `shouldBe` False
        other -> expectationFailure $ "Unexpected result: " ++ show other

    it "parses VP8X with animation flag" $ do
      let flags = 0x02 -- Animation bit set
          vp8xChunk = makeVP8XChunk flags 199 199
          webp = makeRIFFHeader "WEBP" (fromIntegral $ B.length vp8xChunk) <> vp8xChunk
      case parseWebP webp of
        Right (WebPExtended header _) -> do
          vp8xHasAnimation header `shouldBe` True
          vp8xHasAlpha header `shouldBe` False
        other -> expectationFailure $ "Unexpected result: " ++ show other

    it "parses VP8X with multiple chunks" $ do
      let vp8xChunk = makeVP8XChunk 0x00 50 50
          vp8lChunk = makeVP8LChunk [0x2F, 0, 0, 0, 0, 0, 0, 0]
          totalSize = B.length vp8xChunk + B.length vp8lChunk
          webp = makeRIFFHeader "WEBP" (fromIntegral totalSize) <> vp8xChunk <> vp8lChunk
      case parseWebP webp of
        Right (WebPExtended _ chunks) -> do
          length chunks `shouldBe` 1
        other -> expectationFailure $ "Unexpected result: " ++ show other

  describe "Still-image animation flag" $ do
    let pixel = PixelRGBA8 12 34 56 255
        lossless = B.drop 12 $ encodeWebPLossless $ generateImage (\_ _ -> pixel) 1 1
        lossy = B.drop 12 $ encodeWebPLossy (generateImage (\_ _ -> PixelRGB8 12 34 56) 1 1) 80
        animHeader = makeChunk "ANIM" $ replicate 6 0
        expectedError = "Animated WebP images require an animation decoding API"

    forM_ [("VP8", 0x00, lossy), ("VP8L", 0x00, lossless), ("ALPH + VP8", 0x10, makeChunk "ALPH" [0, 128] <> lossy)] $ \(label, flags, imageChunks) -> do
      forM_ [("without ANIM", B.empty), ("with ANIM but no ANMF", animHeader)] $ \(description, prefix) ->
        it ("rejects top-level " ++ label ++ " in an animated container " ++ description) $ do
          let encoded = makeExtendedWebP (flags .|. 0x02) (prefix <> imageChunks)
          fmap (const ()) (decodeWebP encoded) `shouldBe` Left expectedError

      it ("accepts a still " ++ label ++ " image with the animation flag unset") $ do
        fmap (const ()) (decodeWebP $ makeExtendedWebP flags imageChunks) `shouldBe` Right ()

    it "leaves valid animations to the dedicated decoding APIs" $ do
      let frame = makeChunk "ANMF" $ replicate 16 0 ++ B.unpack lossless
          encoded = makeExtendedWebP 0x02 (animHeader <> frame)
      fmap (const ()) (decodeWebP encoded) `shouldBe` Left expectedError
      fmap (\img -> pixelAt (convertRGBA8 img) 0 0) (decodeWebPFirstFrame encoded) `shouldBe` Right pixel
      fmap (map (\frameData -> pixelAt (convertRGBA8 $ webpFrameImage frameData) 0 0)) (decodeWebPAnimation encoded) `shouldBe` Right [pixel]
      fmap (map (\img -> pixelAt img 0 0)) (decodeWebPAnimationComposited encoded) `shouldBe` Right [pixel]

  describe "Reconstruction chunk ordering" $ do
    let pixel = PixelRGBA8 12 34 56 255
        lossless = B.drop 12 $ encodeWebPLossless $ generateImage (\_ _ -> pixel) 1 1
        lossy = B.drop 12 $ encodeWebPLossy (generateImage (\_ _ -> PixelRGB8 12 34 56) 1 1) 80
        alpha = makeChunk "ALPH" [0, 128]
        animHeader = makeChunk "ANIM" $ replicate 6 0
        frame chunks = makeChunk "ANMF" $ replicate 16 0 ++ B.unpack chunks
        expectStillRejection encoded = do
          fmap (const ()) (decodeWebP encoded) `shouldSatisfy` isLeft
          parseWebP encoded `shouldSatisfy` isLeft
        expectAnimationRejection encoded = do
          fmap (const ()) (decodeWebPFirstFrame encoded) `shouldSatisfy` isLeft
          fmap (const ()) (decodeWebPAnimation encoded) `shouldSatisfy` isLeft
          fmap (const ()) (decodeWebPAnimationComposited encoded) `shouldSatisfy` isLeft
          parseWebP encoded `shouldSatisfy` isLeft
        alphaAtOrigin image = case pixelAt (convertRGBA8 image) 0 0 of
          PixelRGBA8 _ _ _ a -> a

    it "rejects ALPH after the top-level VP8 bitstream" $
      expectStillRejection $
        makeExtendedWebP 0x10 (lossy <> alpha)

    it "rejects ALPH after the VP8 bitstream inside ANMF" $
      expectAnimationRejection $
        makeExtendedWebP 0x12 (animHeader <> frame (lossy <> alpha))

    it "rejects ICCP after the image bitstream" $
      expectStillRejection $
        makeExtendedWebP 0x20 (lossless <> makeChunk "ICCP" [0])

    it "rejects ANIM after an animation frame" $
      expectAnimationRejection $
        makeExtendedWebP 0x02 (frame lossless <> animHeader)

    it "rejects VP8X after the image bitstream" $
      expectStillRejection $
        makeExtendedWebP 0x00 (lossless <> makeVP8XChunk 0x00 0 0)

    it "accepts ICCP before alpha and image data" $ do
      let encoded = makeExtendedWebP 0x30 (makeChunk "ICCP" [0] <> alpha <> lossy)
      fmap (const ()) (parseWebP encoded) `shouldBe` Right ()
      fmap alphaAtOrigin (decodeWebP encoded) `shouldBe` Right 128

    forM_ [("EXIF", 0x08), ("XMP ", 0x04), ("TEST", 0x00)] $ \(tag, flag) ->
      forM_ [("before alpha", 0), ("between alpha and VP8", 1), ("after VP8", 2)] $ \(position, index) ->
        it ("accepts " ++ show tag ++ " " ++ position) $ do
          let chunks = [alpha, lossy]
              payload = B.concat $ take index chunks ++ [makeChunk tag [1, 2, 3]] ++ drop index chunks
              encoded = makeExtendedWebP (0x10 .|. flag) payload
          fmap (const ()) (parseWebP encoded) `shouldBe` Right ()
          fmap alphaAtOrigin (decodeWebP encoded) `shouldBe` Right 128

    it "accepts multiple ALPH + VP8 frames with interleaved metadata and unknown chunks" $ do
      let unknown = makeChunk "TEST" [1, 2, 3]
          firstFrame = frame (unknown <> alpha <> unknown <> lossy <> unknown)
          secondFrame = frame (makeChunk "ALPH" [0, 255] <> lossy)
          chunks = makeChunk "EXIF" [1] <> animHeader <> unknown <> firstFrame <> makeChunk "XMP " [2] <> secondFrame <> unknown
          encoded = makeExtendedWebP 0x1E chunks
      fmap (const ()) (parseWebP encoded) `shouldBe` Right ()
      fmap alphaAtOrigin (decodeWebPFirstFrame encoded) `shouldBe` Right 128
      fmap (map (alphaAtOrigin . webpFrameImage)) (decodeWebPAnimation encoded) `shouldBe` Right [128, 255]
      fmap (map (alphaAtOrigin . ImageRGBA8)) (decodeWebPAnimationComposited encoded) `shouldBe` Right [128, 255]

    forM_ [[], [0], replicate 6 0] $ \payload ->
      it ("ignores " ++ show (length payload) ++ "-byte ANIM after a still image") $ do
        let encoded = makeExtendedWebP 0x00 (lossless <> makeChunk "ANIM" payload)
        case parseWebP encoded of
          Right (WebPExtended _ chunks) -> any isAnimChunk chunks `shouldBe` False
          other -> expectationFailure $ "Unexpected result: " ++ show other
        fmap (\image -> pixelAt (convertRGBA8 image) 0 0) (decodeWebP encoded) `shouldBe` Right pixel

  describe "Chunk Parsing" $ do
    it "parses ALPH chunk" $ do
      let alphChunk = makeChunk "ALPH" [0x00, 0xFF, 0xFF]
          vp8xChunk = makeVP8XChunk 0x10 10 10
          totalSize = B.length vp8xChunk + B.length alphChunk
          webp = makeRIFFHeader "WEBP" (fromIntegral totalSize) <> vp8xChunk <> alphChunk
      case parseWebP webp of
        Right (WebPExtended _ chunks) -> do
          any isAlphChunk chunks `shouldBe` True
        other -> expectationFailure $ "Unexpected result: " ++ show other

    it "handles chunk padding correctly" $ do
      -- Odd-sized chunk should have padding byte
      let oddChunk = makeChunk "ICCP" [0x01, 0x02, 0x03] -- 3 bytes (odd)
          vp8xChunk = makeVP8XChunk 0x20 10 10
          totalSize = B.length vp8xChunk + B.length oddChunk
          webp = makeRIFFHeader "WEBP" (fromIntegral totalSize) <> vp8xChunk <> oddChunk
      case parseWebP webp of
        Right (WebPExtended _ chunks) -> do
          length chunks `shouldBe` 1
        Left err -> expectationFailure $ "Parse failed: " ++ err

  describe "Animation Chunks" $ do
    forM_ [([], "empty"), ([0x58], "short"), ([1, 2, 3, 4, 5, 6], "complete")] $ \(payload, label) ->
      it ("ignores " ++ label ++ " ANIM chunks when the animation flag is unset") $ do
        let pixel = PixelRGBA8 12 34 56 255
            imageChunk = B.drop 12 $ encodeWebPLossless $ generateImage (\_ _ -> pixel) 1 1
            chunks = makeChunk "ANIM" payload <> imageChunk
            webp = makeExtendedWebP 0x00 chunks
        case parseWebP webp of
          Right (WebPExtended _ parsedChunks) -> any isAnimChunk parsedChunks `shouldBe` False
          other -> expectationFailure $ "Unexpected result: " ++ show other
        case decodeWebP webp of
          Right (ImageRGBA8 image) -> pixelAt image 0 0 `shouldBe` pixel
          Right _ -> expectationFailure "Expected an RGBA image"
          Left err -> expectationFailure $ "Decode failed: " ++ err

    it "rejects truncated ignored ANIM payloads" $ do
      let truncatedChunk = B.init $ makeChunk "ANIM" [0, 0]
      parseWebP (makeExtendedWebP 0x00 truncatedChunk) `shouldSatisfy` isLeft

    it "requires padding after odd-sized ignored ANIM chunks" $ do
      let unpaddedChunk = B.init $ makeChunk "ANIM" [0]
      parseWebP (makeExtendedWebP 0x00 unpaddedChunk) `shouldSatisfy` isLeft

    forM_ [0 .. 5] $ \size ->
      it ("rejects " ++ show size ++ "-byte ANIM payloads when the animation flag is set") $ do
        let animChunk = makeChunk "ANIM" $ replicate size 0
        parseWebP (makeExtendedWebP 0x02 animChunk) `shouldSatisfy` isLeft

    it "parses ANIM chunk" $ do
      let animChunk = makeChunk "ANIM" [0xFF, 0xFF, 0xFF, 0xFF, 0x00, 0x00]
          vp8xChunk = makeVP8XChunk 0x02 10 10
          totalSize = B.length vp8xChunk + B.length animChunk
          webp = makeRIFFHeader "WEBP" (fromIntegral totalSize) <> vp8xChunk <> animChunk
      case parseWebP webp of
        Right (WebPExtended _ chunks) -> do
          any isAnimChunk chunks `shouldBe` True
        other -> expectationFailure $ "Unexpected result: " ++ show other

    it "retains the animation header and frame image when the animation flag is set" $ do
      let imagePayload = [0x2F, 0, 0, 0, 0]
          imageChunk = makeVP8LChunk imagePayload
          animChunk = makeChunk "ANIM" [1, 2, 3, 4, 5, 6]
          frameChunk = makeChunk "ANMF" $ replicate 16 0 ++ B.unpack imageChunk
      parseWebP (makeExtendedWebP 0x02 (animChunk <> frameChunk))
        `shouldBe` Right
          ( WebPExtended
              (VP8XHeader False True 1 1)
              [ ChunkANIM (AnimHeader 0x04030201 0x0605),
                ChunkANMF (AnimFrame 0 0 1 1 0 True False) [ChunkVP8L $ B.pack imagePayload]
              ]
          )

    it "does not apply top-level ANIM ignoring to ANMF subchunks" $ do
      let frameChunk = makeChunk "ANMF" $ replicate 16 0 ++ B.unpack (makeChunk "ANIM" [])
      parseWebP (makeExtendedWebP 0x00 frameChunk) `shouldSatisfy` isLeft

  describe "Error Handling" $ do
    it "reports error for empty input" $ do
      parseWebP B.empty `shouldSatisfy` isLeft

    it "reports error for truncated file" $ do
      let truncated = B.pack [82, 73, 70, 70, 10, 0, 0, 0] -- RIFF header only
      parseWebP truncated `shouldSatisfy` isLeft

    it "reports error for invalid chunk size" $ do
      let badSize = makeRIFFHeader "WEBP" 999999
      parseWebP badSize `shouldSatisfy` isLeft

-- Helper functions

-- The RIFF size field counts from offset 8, so it includes the 4-byte
-- format FourCC ("WEBP") in addition to the chunk payload passed in.
makeRIFFHeader :: B.ByteString -> Word32 -> B.ByteString
makeRIFFHeader fourCC size =
  B.pack [82, 73, 70, 70] -- "RIFF"
    <> word32ToBytes (size + 4)
    <> fourCC

makeExtendedWebP :: Word8 -> B.ByteString -> B.ByteString
makeExtendedWebP flags chunks =
  let body = makeVP8XChunk flags 0 0 <> chunks
   in makeRIFFHeader "WEBP" (fromIntegral $ B.length body) <> body

makeVP8LChunk :: [Word8] -> B.ByteString
makeVP8LChunk payload =
  makeChunk "VP8L" payload

makeVP8Chunk :: [Word8] -> B.ByteString
makeVP8Chunk payload =
  makeChunk "VP8 " payload

makeVP8XChunk :: Word8 -> Word32 -> Word32 -> B.ByteString
makeVP8XChunk flags w h =
  let payload =
        [flags, 0, 0, 0] -- flags + 3 reserved bytes
          ++ word24ToBytes w
          ++ word24ToBytes h
   in makeChunk "VP8X" payload

makeChunk :: B.ByteString -> [Word8] -> B.ByteString
makeChunk fourCC payload =
  let size = fromIntegral (length payload) :: Word32
      padded = if odd (length payload) then payload ++ [0] else payload
   in fourCC <> word32ToBytes size <> B.pack padded

word32ToBytes :: Word32 -> B.ByteString
word32ToBytes w =
  B.pack
    [ fromIntegral w,
      fromIntegral (w `shiftR` 8),
      fromIntegral (w `shiftR` 16),
      fromIntegral (w `shiftR` 24)
    ]

word24ToBytes :: Word32 -> [Word8]
word24ToBytes w =
  [ fromIntegral w,
    fromIntegral (w `shiftR` 8),
    fromIntegral (w `shiftR` 16)
  ]

isLeft :: Either a b -> Bool
isLeft (Left _) = True
isLeft (Right _) = False

isAlphChunk :: WebPChunk -> Bool
isAlphChunk (ChunkALPH _) = True
isAlphChunk _ = False

isAnimChunk :: WebPChunk -> Bool
isAnimChunk (ChunkANIM _) = True
isAnimChunk _ = False
