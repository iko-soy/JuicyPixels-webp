These three 16×16 grayscale VP8 keyframes have filtering disabled, all sixteen
luma subblocks using B_DC_PRED, neutral chroma, and only the first luma
subblock containing a nonzero coefficient. Their expected RGB samples were
checked independently with libwebp 1.5.0 (`WebPDecodeYUV`) and Pillow's WebP
decoder. The fixed bitstreams avoid relying on this library's encoder as the
test oracle.

| Fixture | Frame QI | Segment QI delta | Y DC delta | First block's quantized DC |
| --- | ---: | ---: | ---: | ---: |
| `vp8-quantizer.webp` | 5 | disabled | 0 | 64 |
| `vp8-b-dc-borders.webp` | 0 | disabled | 0 | 144 |
| `vp8-segment-quantizer.webp` | 10 | −20 | 10 | 64 |

The first two frames start with reconstructed Y=200 (RGB=214). The third
starts with Y=160 (RGB=168), because its final DC lookup index is
`clip(10 - 20 + 10) = 0`. For the border fixture, the next top-row subblock
averages four reconstructed Y=200 samples with four synthetic Y=127 samples,
giving Y=164 (RGB=172). The first subblock in the next row includes the
synthetic left samples Y=129 and predicts Y=165 (RGB=173).
