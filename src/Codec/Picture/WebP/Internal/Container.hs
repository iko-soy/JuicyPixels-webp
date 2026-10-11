{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

module Codec.Picture.WebP.Internal.Container
  ( WebPFile (..),
    VP8XHeader (..),
    WebPChunk (..),
    AnimHeader (..),
    AnimFrame (..),
    FourCC,
    parseWebP,
  )
where

import Control.Monad (replicateM, unless, when)
import Data.Binary.Get
import Data.Bits
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Data.Word

type FourCC = B.ByteString

-- | Top-level WebP file structure
data WebPFile
  = WebPSimpleLossy !B.ByteString
  | WebPSimpleLossless !B.ByteString
  | WebPExtended !VP8XHeader ![WebPChunk]
  deriving (Show, Eq)

-- | VP8X extended format header
data VP8XHeader = VP8XHeader
  { vp8xHasAlpha :: !Bool,
    vp8xHasAnimation :: !Bool,
    vp8xCanvasWidth :: !Int,
    vp8xCanvasHeight :: !Int
  }
  deriving (Show, Eq)

-- | Animation header
data AnimHeader = AnimHeader
  { animBackgroundColor :: !Word32,
    animLoopCount :: !Word16
  }
  deriving (Show, Eq)

-- | Animation frame
data AnimFrame = AnimFrame
  { anmfX :: !Int,
    anmfY :: !Int,
    anmfWidth :: !Int,
    anmfHeight :: !Int,
    anmfDuration :: !Int,
    anmfBlend :: !Bool,
    anmfDispose :: !Bool
  }
  deriving (Show, Eq)

-- | Individual WebP chunks
data WebPChunk
  = ChunkVP8 !B.ByteString
  | ChunkVP8L !B.ByteString
  | ChunkALPH !B.ByteString
  | ChunkANIM !AnimHeader
  | ChunkANMF !AnimFrame ![WebPChunk]
  | ChunkUnknown !FourCC !B.ByteString
  deriving (Show, Eq)

-- | Parse a WebP file from ByteString
parseWebP :: B.ByteString -> Either String WebPFile
parseWebP bs =
  case runGetOrFail getWebP (BL.fromStrict bs) of
    Left (_, _, err) -> Left err
    Right (_, _, result) -> Right result

-- | Main WebP parser
getWebP :: Get WebPFile
getWebP = do
  riffTag <- getByteString 4
  unless (riffTag == "RIFF") $
    fail "Not a RIFF file"

  fileSize <- getWord32le
  when (fileSize < 4) $
    fail "RIFF size too small"

  webpTag <- getByteString 4
  unless (webpTag == "WEBP") $
    fail "Not a WebP file"

  rest <- getRemainingLazyByteString
  let declared = fromIntegral fileSize - 4
  when (BL.length rest < declared) $
    fail "RIFF size exceeds the available data"
  let payload = BL.take declared rest

  case runGetOrFail getWebPBody payload of
    Left (_, _, err) -> fail err
    Right (_, _, result) -> return result

-- | Parse the WebP payload (chunks after the RIFF header)
getWebPBody :: Get WebPFile
getWebPBody = do
  firstChunkTag <- lookAhead (getByteString 4)

  case firstChunkTag of
    "VP8 " -> WebPSimpleLossy <$> getChunkData "VP8 "
    "VP8L" -> WebPSimpleLossless <$> getChunkData "VP8L"
    "VP8X" -> do
      header <- getVP8XChunk
      chunks <- getChunksUntilEnd (not $ vp8xHasAnimation header)
      return $ WebPExtended header chunks
    _ -> fail $ "Unknown first chunk: " ++ show firstChunkTag

-- | Get VP8X header
getVP8XChunk :: Get VP8XHeader
getVP8XChunk = do
  tag <- getByteString 4
  unless (tag == "VP8X") $
    fail "Expected VP8X chunk"

  chunkSize <- getWord32le
  unless (chunkSize == 10) $
    fail "VP8X chunk size must be 10"

  flags <- getWord8
  let hasAlpha = testBit flags 4
      hasAnimation = testBit flags 1

  _reserved <- getWord8
  _reserved2 <- getWord8
  _reserved3 <- getWord8

  canvasWidthMinus1 <- getWord24le
  canvasHeightMinus1 <- getWord24le
  let canvasWidth = fromIntegral canvasWidthMinus1 + 1
      canvasHeight = fromIntegral canvasHeightMinus1 + 1
  when (toInteger canvasWidth * toInteger canvasHeight > 0xFFFFFFFF) $
    fail "VP8X canvas area exceeds the unsigned 32-bit limit"

  return $
    VP8XHeader
      { vp8xHasAlpha = hasAlpha,
        vp8xHasAnimation = hasAnimation,
        vp8xCanvasWidth = canvasWidth,
        vp8xCanvasHeight = canvasHeight
      }

-- | Read chunks until end of input, optionally ignoring top-level ANIM chunks.
getChunksUntilEnd :: Bool -> Get [WebPChunk]
getChunksUntilEnd ignoreAnim = do
  empty <- isEmpty
  if empty
    then return []
    else do
      chunk <- getChunk ignoreAnim
      rest <- getChunksUntilEnd ignoreAnim
      return $ maybe rest (: rest) chunk

-- | Parse a single chunk
getChunk :: Bool -> Get (Maybe WebPChunk)
getChunk ignoreAnim = do
  fourCC <- getByteString 4
  chunkSize <- getWord32le
  let size = fromIntegral chunkSize

  if ignoreAnim && fourCC == "ANIM"
    then do
      -- RFC 9649 section 2.7.1 requires ignoring ANIM when the VP8X
      -- animation flag is unset, but its RIFF framing must still be valid.
      skip size
      skipPadding chunkSize
      return Nothing
    else
      Just <$> case fourCC of
        "VP8 " -> ChunkVP8 <$> getByteString size <* skipPadding chunkSize
        "VP8L" -> ChunkVP8L <$> getByteString size <* skipPadding chunkSize
        "ALPH" -> ChunkALPH <$> getByteString size <* skipPadding chunkSize
        "ANIM" -> do
          when (size < 6) $
            fail "ANIM chunk too small"
          chunk <- getANIMChunk
          skip (size - 6)
          skipPadding chunkSize
          return $ ChunkANIM chunk
        "ANMF" -> do
          chunk <- getANMFChunk size
          skipPadding chunkSize
          return chunk
        _ -> ChunkUnknown fourCC <$> getByteString size <* skipPadding chunkSize

-- | Get chunk data for simple formats
getChunkData :: FourCC -> Get B.ByteString
getChunkData expectedTag = do
  tag <- getByteString 4
  unless (tag == expectedTag) $
    fail $
      "Expected " ++ show expectedTag ++ " chunk"

  chunkSize <- getWord32le
  let size = fromIntegral chunkSize

  payload <- getByteString size
  skipPadding chunkSize
  return payload

-- | Parse ANIM chunk
getANIMChunk :: Get AnimHeader
getANIMChunk = do
  bgColor <- getWord32le
  loopCount <- getWord16le
  return $ AnimHeader bgColor loopCount

-- | Parse ANMF chunk
getANMFChunk :: Int -> Get WebPChunk
getANMFChunk totalSize = do
  when (totalSize < 16) $
    fail "ANMF chunk too small"
  frameX <- (* 2) . fromIntegral <$> getWord24le
  frameY <- (* 2) . fromIntegral <$> getWord24le
  frameWidthMinus1 <- getWord24le
  frameHeightMinus1 <- getWord24le
  duration <- fromIntegral <$> getWord24le

  flags <- getWord8
  let blend = not (testBit flags 1)
      dispose = testBit flags 0

  let headerSize = 16
      remainingSize = totalSize - headerSize

  subChunksData <- getByteString remainingSize

  let frame =
        AnimFrame
          { anmfX = frameX,
            anmfY = frameY,
            anmfWidth = fromIntegral frameWidthMinus1 + 1,
            anmfHeight = fromIntegral frameHeightMinus1 + 1,
            anmfDuration = duration,
            anmfBlend = blend,
            anmfDispose = dispose
          }

  -- The VP8X flag applies to top-level ANIM chunks, not frame subchunks.
  case runGetOrFail (getChunksUntilEnd False) (BL.fromStrict subChunksData) of
    Left (_, _, err) -> fail $ "Failed to parse ANMF sub-chunks: " ++ err
    Right (_, _, subChunks) -> return $ ChunkANMF frame subChunks

-- | Skip padding byte if chunk size is odd
skipPadding :: Word32 -> Get ()
skipPadding chunkSize = when (chunkSize .&. 1 == 1) $ skip 1

-- | Read 24-bit little-endian value
getWord24le :: Get Word32
getWord24le = do
  b0 <- fromIntegral <$> getWord8
  b1 <- fromIntegral <$> getWord8
  b2 <- fromIntegral <$> getWord8
  return $ b0 .|. (b1 `shiftL` 8) .|. (b2 `shiftL` 16)
