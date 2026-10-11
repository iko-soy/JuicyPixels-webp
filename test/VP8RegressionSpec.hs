module VP8RegressionSpec (spec) where

import Codec.Picture
import Codec.Picture.WebP (decodeWebP)
import Codec.Picture.WebP.Internal.VP8.Dequant
import Control.Monad (forM_)
import qualified Data.ByteString as B
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import Data.Word (Word8)
import Test.Hspec

spec :: Spec
spec = describe "VP8 interoperability regressions" $ do
  describe "Quantization" $ do
    it "uses the VP8 DC and AC scales across the quantizer range" $ do
      forM_ [(5, 9, 9), (32, 29, 36), (64, 59, 78), (95, 89, 152), (127, 157, 284)] $ \(q, dc, ac) -> do
        let factors = computeDequantFactors (QuantIndices q 0 0 0 0 0) Nothing V.! 0
        (dqYDC factors, dqYAC factors) `shouldBe` (dc, ac)

    it "derives Y2 and capped chroma factors from the VP8 tables" $ do
      let factors = computeDequantFactors (QuantIndices 127 0 0 0 0 0) Nothing V.! 0
      factors `shouldBe` DequantFactors 157 284 314 440 132 284

    it "adds component deltas before clipping negative segment QIs" $ do
      let factors = computeDequantFactors (QuantIndices 10 10 10 10 10 10) (Just $ segment False (-20)) V.! 0
      factors `shouldBe` DequantFactors 4 4 8 8 4 4

    it "adds component deltas before clipping segment QIs above 127" $ do
      let factors = computeDequantFactors (QuantIndices 120 (-15) (-15) (-15) (-15) (-15)) (Just $ segment False 20) V.! 0
      factors `shouldBe` DequantFactors 151 284 302 424 132 274

    it "applies the same clipping order to absolute segment QIs" $ do
      let factors = computeDequantFactors (QuantIndices 80 10 10 10 10 10) (Just $ segment True (-10)) V.! 0
      factors `shouldBe` DequantFactors 4 4 8 8 4 4

  -- These fixed bitstreams were independently decoded with libwebp.
  -- All have neutral chroma; expected samples are
  -- reference RGB values, rather than an encode/decode roundtrip oracle.
  describe "Reference bitstreams" $ do
    it "reconstructs QI 5 coefficients with the reference DC scale" $
      checkPixels "vp8-quantizer.webp" [((0, 0), 214), ((3, 3), 214)]

    it "includes synthetic top and left samples in B_DC_PRED" $
      checkPixels
        "vp8-b-dc-borders.webp"
        [ ((0, 0), 214),
          ((4, 0), 172),
          ((0, 4), 173),
          ((4, 4), 173),
          ((8, 0), 151),
          ((12, 0), 141)
        ]

    it "clips the complete segmented quantizer index" $
      checkPixels "vp8-segment-quantizer.webp" [((0, 0), 168), ((3, 3), 168)]

    it "uses absolute segment defaults when feature data is not updated" $
      checkPixels "vp8-segment-default-absolute.webp" [((0, 0), 140), ((3, 3), 140), ((4, 0), 135), ((4, 4), 136)]

    it "allows an empty coefficient partition when the macroblock skips it" $
      checkPixels "vp8-empty-coefficients.webp" [((0, 0), 130), ((15, 15), 130)]

    it "allows empty coefficients for skipped B_PRED macroblocks" $
      checkPixels "vp8-empty-bpred-coefficients.webp" [((0, 0), 130), ((4, 4), 132), ((15, 15), 132)]

    it "rejects an empty partition when a macroblock needs coefficients" $ do
      bytes <- B.readFile "test/data/regression/vp8-missing-coefficients.webp"
      fmap (const ()) (decodeWebP bytes)
        `shouldBe` Left "Empty VP8 coefficient partition used by a non-skipped macroblock"

    it "clips negative filter levels after applying reference deltas" $
      checkPixels "vp8-filter-delta-clipping.webp" [((3, 1), 140), ((4, 1), 135), ((3, 3), 140)]

    it "clips high filter levels after applying reference deltas" $
      checkPixels "vp8-filter-delta-clipping-high.webp" [((4, 1), 222), ((5, 1), 213), ((3, 3), 241), ((4, 4), 212)]

    it "preserves coefficient contexts after a block of explicit zero tokens" $
      checkPixels
        "vp8-explicit-zero-context.webp"
        [ ((0, 0), 130),
          ((4, 0), 214),
          ((0, 4), 132),
          ((4, 4), 173),
          ((8, 0), 172),
          ((12, 0), 151)
        ]

  describe "Boolean input exhaustion" $ do
    it "accepts a short mode partition with just its arithmetic termination bytes" $
      checkPixels "vp8-short-bool-modes.webp" [((0, 0), 130), ((4, 4), 132)]

    it "accepts a short coefficient partition without extra lookahead padding" $
      checkPixels "vp8-short-bool-coefficients.webp" [((0, 0), 214), ((3, 3), 214)]

    forM_
      [ ("compressed header", "vp8-truncated-bool-header.webp"),
        ("macroblock modes", "vp8-truncated-bool-modes.webp"),
        ("coefficients", "vp8-truncated-bool-coefficients.webp")
      ]
      $ \(section, fixture) ->
        it ("rejects truncated " ++ section ++ " within nonempty partitions") $ do
          bytes <- B.readFile $ "test/data/regression/" ++ fixture
          fmap (const ()) (decodeWebP bytes) `shouldBe` Left ("Unexpected end of VP8 " ++ section)

segment :: Bool -> Int -> SegmentInfo
segment absoluteMode quantizer =
  SegmentInfo True True absoluteMode (VU.replicate 4 quantizer) (VU.replicate 4 0) (255, 255, 255)

checkPixels :: FilePath -> [((Int, Int), Word8)] -> Expectation
checkPixels name expected = do
  bytes <- B.readFile $ "test/data/regression/" ++ name
  case decodeWebP bytes of
    Left err -> expectationFailure err
    Right (ImageRGB8 img) -> do
      (imageWidth img, imageHeight img) `shouldBe` (16, 16)
      forM_ expected $ \((x, y), gray) ->
        pixelAt img x y `shouldBe` PixelRGB8 gray gray gray
    Right _ -> expectationFailure "Expected an RGB8 image"
