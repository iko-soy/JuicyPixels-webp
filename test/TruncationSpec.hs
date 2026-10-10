module TruncationSpec (spec) where

import Codec.Picture.Types
import Codec.Picture.WebP
import Codec.Picture.WebP.Internal.Alpha (decodeAlpha)
import Codec.Picture.WebP.Internal.Container
import Codec.Picture.WebP.Internal.Encode (makeRIFFContainer, makeVP8LChunk)
import Codec.Picture.WebP.Internal.VP8L (decodeVP8LHeaderless)
import qualified Data.ByteString as B
import Data.Either (isLeft)
import Test.Hspec

spec :: Spec
spec = describe "VP8L input exhaustion" $ do
  it "rejects a pixel stream that ends before the advertised image is complete" $ do
    bytes <- B.readFile "test/data/regression/vp8l-truncated-pixels.webp"
    fmap (const ()) (decodeWebP bytes) `shouldBe` Left "Unexpected end of VP8L bitstream"

  it "rejects exhausted headless streams and compressed alpha planes" $ do
    bytes <- B.readFile "test/data/regression/vp8l-truncated-pixels.webp"
    case parseWebP bytes of
      Right (WebPSimpleLossless payload) -> do
        let headless = B.drop 5 payload
        decodeVP8LHeaderless 100 1 headless `shouldSatisfy` isLeft
        decodeAlpha 100 1 (B.cons 1 headless) `shouldSatisfy` isLeft
      _ -> expectationFailure "Expected a simple lossless fixture"

  it "accepts the same Huffman tables when sufficient pixel bits follow" $ do
    bytes <- B.readFile "test/data/regression/vp8l-truncated-pixels.webp"
    case parseWebP bytes of
      Right (WebPSimpleLossless payload) -> do
        let chunk = makeVP8LChunk (payload <> B.replicate 13 0)
            complete = makeRIFFContainer (fromIntegral $ B.length chunk) chunk
        case decodeWebP complete of
          Right (ImageRGBA8 img) -> do
            (imageWidth img, imageHeight img) `shouldBe` (100, 1)
            map (\x -> pixelAt img x 0) [0 .. 99] `shouldBe` replicate 100 (PixelRGBA8 0 0 0 255)
          Left err -> expectationFailure err
          _ -> expectationFailure "Expected an RGBA image"
      _ -> expectationFailure "Expected a simple lossless fixture"

  it "accepts single-symbol pixel codes that consume no bits" $ do
    let img = generateImage (\_ _ -> PixelRGBA8 0 0 0 255) 1 1
    case decodeWebP (encodeWebPLossless img) of
      Right (ImageRGBA8 decoded) -> imageData decoded `shouldBe` imageData img
      Left err -> expectationFailure err
      _ -> expectationFailure "Expected an RGBA image"
