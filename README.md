# JuicyPixels-webp

Pure Haskell WebP decoder and encoder for JuicyPixels.

## Features

- **VP8L Lossless**: Full decode and encode support
- **VP8 Lossy**: Full decode and encode with quality control (0-100)
- **Alpha Channels**: RGBA support for both lossless and lossy
- **Animations**: Decode and encode animated WebP files
- **All Container Formats**: Simple and extended WebP

## Installation

```bash
stack build
```

## Usage

### Decoding

```haskell
import Codec.Picture.WebP
import qualified Data.ByteString as B

-- Decode a still WebP image
fileData <- B.readFile "image.webp"
case decodeWebP fileData of
  Right (ImageRGB8 img)  -> -- VP8 lossy without alpha
  Right (ImageRGBA8 img) -> -- VP8L lossless or VP8 with alpha
  Left err               -> -- Handle error
```

For animated images, use `decodeWebPFirstFrame` for the first frame or
`decodeWebPAnimation` / `decodeWebPAnimationComposited` for all frames.

### Encoding

```haskell
import Codec.Picture
import Codec.Picture.WebP

-- Lossless encoding (takes Image PixelRGBA8)
let webpData = encodeWebPLossless img
B.writeFile "output.webp" webpData

-- Lossy encoding with quality (0-100, takes Image PixelRGB8)
let webpData = encodeWebPLossy img 80
B.writeFile "output.webp" webpData

-- Lossy with alpha channel (takes Image PixelRGBA8)
let webpData = encodeWebPLossyWithAlpha img 80
B.writeFile "output.webp" webpData
```

### Animations

```haskell
-- Decode animation frames
case decodeWebPAnimation fileData of
  Right frames -> mapM_ processFrame frames
  Left err     -> handleError err

-- Encode animation
let frames = [WebPEncodeFrame img1 100 0 0, WebPEncodeFrame img2 100 0 0]
let webpData = encodeWebPAnimation frames canvasWidth canvasHeight quality
```

Animation frame offsets must be nonnegative and even, and each frame must fit
within the canvas. Durations are milliseconds in `0..16777215`. Canvas dimensions
must be in `1..16777216`, with at most `4294967295` pixels; VP8 frame dimensions
must be in `1..16383`. Invalid parameters raise an error. VP8 encoding also raises
an error if its first partition exceeds the format's `524287`-byte limit.

## Testing

```bash
stack test
```

## API

### Decoding

- `decodeWebP :: ByteString -> Either String DynamicImage`
- `decodeWebPFirstFrame :: ByteString -> Either String DynamicImage`
- `decodeWebPAnimation :: ByteString -> Either String [WebPAnimFrame]`
- `decodeWebPAnimationComposited :: ByteString -> Either String [Image PixelRGBA8]`

### Encoding

- `encodeWebPLossless :: Image PixelRGBA8 -> ByteString`
- `encodeWebPLossy :: Image PixelRGB8 -> Int -> ByteString`
- `encodeWebPLossyWithAlpha :: Image PixelRGBA8 -> Int -> ByteString`
- `encodeWebPAnimation :: [WebPEncodeFrame] -> Int -> Int -> Int -> ByteString`

## Documentation

- `docs/webp-format.md` - VP8L lossless specification
- `docs/vp8-bitstream.md` - VP8 lossy specification
- `STATUS.md` - Current implementation status
- `PLAN.md` - Original implementation plan (historical)

## License

BSD-3-Clause
