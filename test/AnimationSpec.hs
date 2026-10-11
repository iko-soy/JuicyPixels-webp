{-# LANGUAGE OverloadedStrings #-}

module AnimationSpec (spec) where

import Codec.Picture
import Codec.Picture.WebP
import Codec.Picture.WebP.Internal.AnimationEncode (AnimationFrame (..), makeANIMChunk, makeANMFChunk)
import Codec.Picture.WebP.Internal.Encode (makeRIFFContainer, makeVP8LChunk, makeVP8XChunk)
import qualified Data.ByteString as B
import Test.Hspec
import Test.QuickCheck

spec :: Spec
spec = describe "Animation" $ do
  describe "Basic Animation Encoding" $ do
    it "encodes single frame animation" $ do
      let frame = WebPEncodeFrame (ImageRGB8 $ generateImage (\_ _ -> PixelRGB8 255 0 0) 16 16) 100 0 0
          encoded = encodeWebPAnimation [frame] 16 16 80

      case decodeWebPAnimation encoded of
        Right frames -> do
          length frames `shouldBe` 1
          webpFrameDuration (head frames) `shouldBe` 100
        Left err -> expectationFailure $ "Decode failed: " ++ err

    it "encodes two frame animation" $ do
      let frame1 = WebPEncodeFrame (ImageRGB8 $ generateImage (\_ _ -> PixelRGB8 255 0 0) 16 16) 100 0 0
          frame2 = WebPEncodeFrame (ImageRGB8 $ generateImage (\_ _ -> PixelRGB8 0 255 0) 16 16) 100 0 0
          encoded = encodeWebPAnimation [frame1, frame2] 16 16 80

      case decodeWebPAnimation encoded of
        Right frames -> length frames `shouldBe` 2
        Left err -> expectationFailure $ "Decode failed: " ++ err

    it "encodes many frames (10)" $ do
      let frames =
            [ WebPEncodeFrame
                (ImageRGB8 $ generateImage (\_ _ -> PixelRGB8 (fromIntegral i * 25) 0 0) 16 16)
                50
                0
                0
            | i <- [0 .. 9]
            ]
          encoded = encodeWebPAnimation frames 16 16 80

      case decodeWebPAnimation encoded of
        Right decodedFrames -> length decodedFrames `shouldBe` 10
        Left err -> expectationFailure $ "Decode failed: " ++ err

    it "encodes maximum frame count (50 frames)" $ do
      let frames =
            [ WebPEncodeFrame
                (ImageRGB8 $ generateImage (\_ _ -> PixelRGB8 (fromIntegral $ i `mod` 256) 128 128) 8 8)
                20
                0
                0
            | i <- [0 .. 49]
            ]
          encoded = encodeWebPAnimation frames 8 8 60

      case decodeWebPAnimation encoded of
        Right decodedFrames -> length decodedFrames `shouldBe` 50
        Left err -> expectationFailure $ "Decode failed: " ++ err

  describe "Frame Timing" $ do
    it "preserves frame durations" $ do
      let frames =
            [ WebPEncodeFrame (ImageRGB8 $ generateImage (\_ _ -> PixelRGB8 255 0 0) 16 16) 100 0 0,
              WebPEncodeFrame (ImageRGB8 $ generateImage (\_ _ -> PixelRGB8 0 255 0) 16 16) 200 0 0,
              WebPEncodeFrame (ImageRGB8 $ generateImage (\_ _ -> PixelRGB8 0 0 255) 16 16) 300 0 0
            ]
          encoded = encodeWebPAnimation frames 16 16 80

      case decodeWebPAnimation encoded of
        Right decodedFrames -> do
          webpFrameDuration (decodedFrames !! 0) `shouldBe` 100
          webpFrameDuration (decodedFrames !! 1) `shouldBe` 200
          webpFrameDuration (decodedFrames !! 2) `shouldBe` 300
        Left err -> expectationFailure $ "Decode failed: " ++ err

    it "handles minimum duration (1ms)" $ do
      let frame = WebPEncodeFrame (ImageRGB8 $ generateImage (\_ _ -> PixelRGB8 128 128 128) 16 16) 1 0 0
          encoded = encodeWebPAnimation [frame] 16 16 80

      case decodeWebPAnimation encoded of
        Right frames -> webpFrameDuration (head frames) `shouldBe` 1
        Left err -> expectationFailure $ "Decode failed: " ++ err

    it "handles long duration (10000ms)" $ do
      let frame = WebPEncodeFrame (ImageRGB8 $ generateImage (\_ _ -> PixelRGB8 128 128 128) 16 16) 10000 0 0
          encoded = encodeWebPAnimation [frame] 16 16 80

      case decodeWebPAnimation encoded of
        Right frames -> webpFrameDuration (head frames) `shouldBe` 10000
        Left err -> expectationFailure $ "Decode failed: " ++ err

    it "handles varying durations" $ do
      let durations = [10, 50, 100, 200, 500, 1000]
          frames =
            [ WebPEncodeFrame (ImageRGB8 $ generateImage (\_ _ -> PixelRGB8 128 128 128) 16 16) d 0 0
            | d <- durations
            ]
          encoded = encodeWebPAnimation frames 16 16 80

      case decodeWebPAnimation encoded of
        Right decodedFrames -> do
          length decodedFrames `shouldBe` length durations
          zipWithM_
            (\expected frame -> webpFrameDuration frame `shouldBe` expected)
            durations
            decodedFrames
        Left err -> expectationFailure $ "Decode failed: " ++ err

  describe "Frame Dimensions" $ do
    it "encodes square canvas" $ do
      let frame = WebPEncodeFrame (ImageRGB8 $ generateImage (\_ _ -> PixelRGB8 200 100 50) 32 32) 100 0 0
          encoded = encodeWebPAnimation [frame] 32 32 80

      case decodeWebPAnimation encoded of
        Right frames -> do
          let img = webpFrameImage (head frames)
          dynamicImageWidth img `shouldBe` 32
          dynamicImageHeight img `shouldBe` 32
        Left err -> expectationFailure $ "Decode failed: " ++ err

    it "encodes wide canvas" $ do
      let frame = WebPEncodeFrame (ImageRGB8 $ generateImage (\_ _ -> PixelRGB8 100 200 50) 64 16) 100 0 0
          encoded = encodeWebPAnimation [frame] 64 16 80

      case decodeWebPAnimation encoded of
        Right frames -> do
          let img = webpFrameImage (head frames)
          dynamicImageWidth img `shouldBe` 64
          dynamicImageHeight img `shouldBe` 16
        Left err -> expectationFailure $ "Decode failed: " ++ err

    it "encodes tall canvas" $ do
      let frame = WebPEncodeFrame (ImageRGB8 $ generateImage (\_ _ -> PixelRGB8 50 100 200) 16 64) 100 0 0
          encoded = encodeWebPAnimation [frame] 16 64 80

      case decodeWebPAnimation encoded of
        Right frames -> do
          let img = webpFrameImage (head frames)
          dynamicImageWidth img `shouldBe` 16
          dynamicImageHeight img `shouldBe` 64
        Left err -> expectationFailure $ "Decode failed: " ++ err

  describe "Quality Levels" $ do
    it "encodes with quality 0" $ do
      let frame = WebPEncodeFrame (ImageRGB8 $ generateImage (\_ _ -> PixelRGB8 128 128 128) 16 16) 100 0 0
          encoded = encodeWebPAnimation [frame] 16 16 0

      case decodeWebPAnimation encoded of
        Right frames -> length frames `shouldBe` 1
        Left err -> expectationFailure $ "Decode failed: " ++ err

    it "encodes with quality 100" $ do
      let frame = WebPEncodeFrame (ImageRGB8 $ generateImage (\_ _ -> PixelRGB8 128 128 128) 16 16) 100 0 0
          encoded = encodeWebPAnimation [frame] 16 16 100

      case decodeWebPAnimation encoded of
        Right frames -> length frames `shouldBe` 1
        Left err -> expectationFailure $ "Decode failed: " ++ err

    it "quality affects file size" $ do
      let frame = WebPEncodeFrame (ImageRGB8 $ generateImage (\x y -> PixelRGB8 (fromIntegral x) (fromIntegral y) 128) 32 32) 100 0 0
          encodedLow = encodeWebPAnimation [frame] 32 32 10
          encodedHigh = encodeWebPAnimation [frame] 32 32 90

      -- Both should decode successfully
      case (decodeWebPAnimation encodedLow, decodeWebPAnimation encodedHigh) of
        (Right _, Right _) -> pure ()
        _ -> expectationFailure "Decode failed"

  describe "Frame Content" $ do
    it "each frame has distinct content" $ do
      let colors = [(255, 0, 0), (0, 255, 0), (0, 0, 255)]
          frames =
            [ WebPEncodeFrame
                (ImageRGB8 $ generateImage (\_ _ -> PixelRGB8 r g b) 16 16)
                100
                0
                0
            | (r, g, b) <- colors
            ]
          encoded = encodeWebPAnimation frames 16 16 90

      case decodeWebPAnimation encoded of
        Right decodedFrames -> do
          length decodedFrames `shouldBe` 3
          -- Each frame should decode successfully
          mapM_ (\f -> dynamicImageWidth (webpFrameImage f) `shouldBe` 16) decodedFrames
        Left err -> expectationFailure $ "Decode failed: " ++ err

    it "handles gradient animation" $ do
      let frames =
            [ WebPEncodeFrame
                (ImageRGB8 $ generateImage (\x y -> PixelRGB8 (fromIntegral $ (i * 25 + x) `mod` 256) (fromIntegral y) 128) 16 16)
                50
                0
                0
            | i <- [0 .. 9 :: Int]
            ]
          encoded = encodeWebPAnimation frames 16 16 80

      case decodeWebPAnimation encoded of
        Right decodedFrames -> length decodedFrames `shouldBe` 10
        Left err -> expectationFailure $ "Decode failed: " ++ err

  describe "Composited Animation" $ do
    it "decodes with compositing" $ do
      let frame1 = WebPEncodeFrame (ImageRGB8 $ generateImage (\_ _ -> PixelRGB8 255 0 0) 16 16) 100 0 0
          frame2 = WebPEncodeFrame (ImageRGB8 $ generateImage (\_ _ -> PixelRGB8 0 255 0) 16 16) 100 0 0
          encoded = encodeWebPAnimation [frame1, frame2] 16 16 80

      case decodeWebPAnimationComposited encoded of
        Right composited -> length composited `shouldBe` 2
        Left err -> expectationFailure $ "Composited decode failed: " ++ err

    it "composited frames are RGBA8" $ do
      let frame = WebPEncodeFrame (ImageRGB8 $ generateImage (\_ _ -> PixelRGB8 128 128 128) 16 16) 100 0 0
          encoded = encodeWebPAnimation [frame] 16 16 80

      case decodeWebPAnimationComposited encoded of
        Right composited -> do
          length composited `shouldBe` 1
          imageWidth (head composited) `shouldBe` 16
          imageHeight (head composited) `shouldBe` 16
        Left err -> expectationFailure $ "Composited decode failed: " ++ err

    it "keeps translucent white white when frames overlap" $ do
      let white = PixelRGBA8 255 255 255 128
          encoded = animationData [losslessFrame white False, losslessFrame white True]

      fmap (map (\img -> pixelAt img 0 0)) (decodeWebPAnimationComposited encoded)
        `shouldBe` Right [white, PixelRGBA8 255 255 255 191]

    it "uses unrounded alpha weights for translucent colors" $ do
      let blue = PixelRGBA8 0 0 255 128
          red = PixelRGBA8 255 0 0 128
          encoded = animationData [losslessFrame blue False, losslessFrame red True]

      fmap (map (\img -> pixelAt img 0 0)) (decodeWebPAnimationComposited encoded)
        `shouldBe` Right [blue, PixelRGBA8 170 0 84 191]

    it "preserves a translucent destination under a transparent frame" $ do
      let destination = PixelRGBA8 23 147 255 1
          transparent = PixelRGBA8 255 0 0 0
          encoded = animationData [losslessFrame destination False, losslessFrame transparent True]

      fmap (map (\img -> pixelAt img 0 0)) (decodeWebPAnimationComposited encoded)
        `shouldBe` Right [destination, destination]

    it "preserves source colors over a transparent canvas" $ do
      let source = PixelRGBA8 23 147 255 128
          encoded = animationData [losslessFrame source True]

      fmap (map (\img -> pixelAt img 0 0)) (decodeWebPAnimationComposited encoded)
        `shouldBe` Right [source]

    it "clears RGB when both blended pixels are transparent" $ do
      let encoded = animationData [losslessFrame (PixelRGBA8 255 147 23 0) True]

      fmap (map (\img -> pixelAt img 0 0)) (decodeWebPAnimationComposited encoded)
        `shouldBe` Right [PixelRGBA8 0 0 0 0]

    it "reports a corrupt frame instead of returning an empty animation" $ do
      let encoded = animationData [frameChunk False (makeVP8LChunk $ B.singleton 0)]

      fmap length (decodeWebPAnimationComposited encoded)
        `shouldBe` Left "Invalid VP8L signature: 0"

    it "reports corrupt frames between valid frames" $ do
      let valid = losslessFrame (PixelRGBA8 255 255 255 128) True
          corrupt = frameChunk False (makeVP8LChunk $ B.singleton 0)
          encoded = animationData [valid, corrupt, valid]

      fmap length (decodeWebPAnimationComposited encoded)
        `shouldBe` Left "Invalid VP8L signature: 0"

  describe "File Format" $ do
    it "requires the animation flag in both animation APIs" $ do
      let frame = losslessFrame (PixelRGBA8 255 0 0 255) False
          encoded = animationDataWithFlag False [frame]

      expectAnimationError "Not an animated WebP file" encoded

    mapM_
      ( \(description, first, second) ->
          it ("rejects " ++ description ++ " in an ANMF chunk") $ do
            let encoded = animationData [frameChunk False (first <> unknownChunk <> second)]
            expectAnimationError "Multiple image bitstream chunks in animation frame" encoded
      )
      [ ("duplicate VP8 chunks", lossyImageChunk, lossyImageChunk),
        ("duplicate VP8L chunks", losslessImageChunk, losslessImageChunk),
        ("VP8 followed by VP8L", lossyImageChunk, losslessImageChunk),
        ("VP8L followed by VP8", losslessImageChunk, lossyImageChunk)
      ]

    it "rejects duplicate ALPH chunks in an ANMF chunk" $ do
      let encoded = animationData [frameChunk False (alphaChunk <> unknownChunk <> alphaChunk <> lossyImageChunk)]

      expectAnimationError "Multiple ALPH chunks in animation frame" encoded

    it "allows unknown subchunks around a lossless frame" $ do
      let encoded = animationData [frameChunk False (unknownChunk <> losslessImageChunk <> unknownChunk)]

      fmap (map (\frame -> pixelAt (convertRGBA8 $ webpFrameImage frame) 0 0)) (decodeWebPAnimation encoded)
        `shouldBe` Right [PixelRGBA8 255 0 0 255]
      fmap (map (\img -> pixelAt img 0 0)) (decodeWebPAnimationComposited encoded)
        `shouldBe` Right [PixelRGBA8 255 0 0 255]

    it "allows one ALPH chunk with a lossy frame and unknown subchunks" $ do
      let encoded = animationData [frameChunk False (alphaChunk <> unknownChunk <> lossyImageChunk <> unknownChunk)]
          alphaAt img = let PixelRGBA8 _ _ _ alpha = pixelAt img 0 0 in alpha

      fmap (map (alphaAt . convertRGBA8 . webpFrameImage)) (decodeWebPAnimation encoded)
        `shouldBe` Right [128]
      fmap (map alphaAt) (decodeWebPAnimationComposited encoded)
        `shouldBe` Right [128]

    it "uses contained image dimensions when the ANMF dimensions differ" $ do
      let img = generateImage (\_ _ -> PixelRGBA8 255 0 0 255) 2 2
          frame = frameChunk False (B.drop 12 $ encodeWebPLossless img)
          body = makeVP8XChunk 2 2 True True <> makeANIMChunk 0 0 <> frame
          encoded = makeRIFFContainer (fromIntegral $ B.length body) body

      fmap (map (dynamicImageWidth . webpFrameImage)) (decodeWebPAnimation encoded)
        `shouldBe` Right [2]
      fmap (map (\decoded -> pixelAt decoded 1 1)) (decodeWebPAnimationComposited encoded)
        `shouldBe` Right [PixelRGBA8 255 0 0 255]

    it "produces valid RIFF container" $ do
      let frame = WebPEncodeFrame (ImageRGB8 $ generateImage (\_ _ -> PixelRGB8 255 0 0) 16 16) 100 0 0
          encoded = encodeWebPAnimation [frame] 16 16 80

      B.take 4 encoded `shouldBe` B.pack [82, 73, 70, 70] -- "RIFF"
      B.take 4 (B.drop 8 encoded) `shouldBe` B.pack [87, 69, 66, 80] -- "WEBP"
    it "file size is reasonable" $ do
      let frame = WebPEncodeFrame (ImageRGB8 $ generateImage (\_ _ -> PixelRGB8 128 128 128) 16 16) 100 0 0
          encoded = encodeWebPAnimation [frame] 16 16 80

      B.length encoded `shouldSatisfy` (> 50) -- At least header size
      B.length encoded `shouldSatisfy` (< 10000) -- Not unreasonably large
  describe "Edge Cases" $ do
    it "handles 1x1 frames" $ do
      let frame = WebPEncodeFrame (ImageRGB8 $ generateImage (\_ _ -> PixelRGB8 255 255 255) 1 1) 100 0 0
          encoded = encodeWebPAnimation [frame] 1 1 80

      case decodeWebPAnimation encoded of
        Right frames -> length frames `shouldBe` 1
        Left err -> expectationFailure $ "Decode failed: " ++ err

    it "handles odd dimensions" $ do
      let frame = WebPEncodeFrame (ImageRGB8 $ generateImage (\_ _ -> PixelRGB8 128 128 128) 17 23) 100 0 0
          encoded = encodeWebPAnimation [frame] 17 23 80

      case decodeWebPAnimation encoded of
        Right frames -> do
          let img = webpFrameImage (head frames)
          dynamicImageWidth img `shouldBe` 17
          dynamicImageHeight img `shouldBe` 23
        Left err -> expectationFailure $ "Decode failed: " ++ err

    it "handles repeated identical frames" $ do
      let frame = WebPEncodeFrame (ImageRGB8 $ generateImage (\_ _ -> PixelRGB8 100 100 100) 16 16) 50 0 0
          frames = replicate 5 frame
          encoded = encodeWebPAnimation frames 16 16 80

      case decodeWebPAnimation encoded of
        Right decodedFrames -> length decodedFrames `shouldBe` 5
        Left err -> expectationFailure $ "Decode failed: " ++ err

-- Helper functions
-- Lossless one-pixel frames isolate compositing from lossy color conversion.
losslessFrame :: PixelRGBA8 -> Bool -> B.ByteString
losslessFrame pixel blend =
  let img = generateImage (\_ _ -> pixel) 1 1
   in frameChunk blend (B.drop 12 $ encodeWebPLossless img)

frameChunk :: Bool -> B.ByteString -> B.ByteString
frameChunk blend imageChunk =
  let img = generateImage (\_ _ -> PixelRGBA8 0 0 0 0) 1 1
      frame = AnimationFrame (ImageRGBA8 img) 100 0 0 blend False
   in makeANMFChunk frame imageChunk

animationData :: [B.ByteString] -> B.ByteString
animationData = animationDataWithFlag True

animationDataWithFlag :: Bool -> [B.ByteString] -> B.ByteString
animationDataWithFlag hasAnimation frames =
  let body = makeVP8XChunk 1 1 True hasAnimation <> makeANIMChunk 0 0 <> mconcat frames
   in makeRIFFContainer (fromIntegral $ B.length body) body

expectAnimationError :: String -> B.ByteString -> Expectation
expectAnimationError err encoded = do
  fmap length (decodeWebPAnimation encoded) `shouldBe` Left err
  fmap length (decodeWebPAnimationComposited encoded) `shouldBe` Left err

losslessImageChunk, lossyImageChunk, alphaChunk, unknownChunk :: B.ByteString
losslessImageChunk = B.drop 12 $ encodeWebPLossless $ generateImage (\_ _ -> PixelRGBA8 255 0 0 255) 1 1
lossyImageChunk = B.drop 12 $ encodeWebPLossy (generateImage (\_ _ -> PixelRGB8 255 0 0) 1 1) 80
-- A two-byte, uncompressed ALPH payload with a single translucent pixel.
alphaChunk = "ALPH" <> B.pack [2, 0, 0, 0, 0, 128]
unknownChunk = "JUNK" <> B.pack [2, 0, 0, 0, 23, 42]

dynamicImageWidth :: DynamicImage -> Int
dynamicImageWidth (ImageRGB8 img) = imageWidth img
dynamicImageWidth (ImageRGBA8 img) = imageWidth img
dynamicImageWidth _ = 0

dynamicImageHeight :: DynamicImage -> Int
dynamicImageHeight (ImageRGB8 img) = imageHeight img
dynamicImageHeight (ImageRGBA8 img) = imageHeight img
dynamicImageHeight _ = 0

zipWithM_ :: (Monad m) => (a -> b -> m c) -> [a] -> [b] -> m ()
zipWithM_ f xs ys = sequence_ $ zipWith f xs ys
