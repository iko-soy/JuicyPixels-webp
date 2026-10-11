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

The following additional 16×16 fixtures exercise coefficient partition use,
coefficient contexts, and filter delta ordering. Expected RGB samples were
checked with libwebp and Pillow, except for the deliberately rejected missing
coefficient case described below.

| Fixture | Encoded case | Expected result |
| --- | --- | --- |
| `vp8-empty-coefficients.webp` | Skipped DC macroblock with an empty coefficient partition | Uniform RGB `(130,130,130)` |
| `vp8-empty-bpred-coefficients.webp` | Skipped B_PRED macroblock, all B_DC subblocks, empty coefficient partition | Gray 130 at `(0,0)` and 132 at `(4,4)` and `(15,15)` |
| `vp8-missing-coefficients.webp` | Non-skipped macroblock with an empty coefficient partition | `Left`; the library requires coefficient data for a non-skipped macroblock, while libwebp permissively zero-fills this input |
| `vp8-filter-delta-clipping.webp` | Frame filter level 5, segment delta −10, reference delta +5, first DC coefficient 16 at QI 0 | Final level 0; gray 140 at `(3,1)` |
| `vp8-filter-delta-clipping-high.webp` | Frame filter level 60, segment delta +10, reference delta −10, first DC coefficient 254 at QI 0 | Final level 60; gray 222 at `(4,1)`, 213 at `(5,1)`, 241 at `(3,3)`, and 212 at `(4,4)` |
| `vp8-explicit-zero-context.webp` | First B_DC subblock has sixteen explicit zero tokens; second has DC 144 at QI 0 | Gray 130 at `(0,0)` and 214 at `(4,0)` |

The following fixtures were checked with Pillow's libwebp 1.6.0 decoder.
The segmentation fixture was constructed with an independent boolean writer;
its segment map and feature data are not updated, the frame QI is 127, filtering
is disabled, and the first B_DC subblock has quantized DC 16. The keyframe's
default absolute segment QI is zero, so the DC factor is 4 and the first
reconstructed luma value is 136 (RGB 140).

| Fixture | Encoded case and provenance | Expected result |
| --- | --- | --- |
| `vp8-segment-default-absolute.webp` | Segmentation enabled without feature updates; frame QI 127 and first DC 16 | Gray 140 at `(0,0)` and `(3,3)`, 135 at `(4,0)`, and 136 at `(4,4)` |
| `vp8-short-bool-modes.webp` | `vp8-empty-bpred-coefficients.webp` with its mode partition shortened to eight bytes and one unused coefficient byte | Gray 130 at `(0,0)` and 132 at `(4,4)`; libwebp accepts this shortest mode prefix |
| `vp8-short-bool-coefficients.webp` | `vp8-quantizer.webp` with only its first five coefficient bytes | Gray 214 at `(0,0)` and `(3,3)`; libwebp accepts this shortest coefficient prefix |
| `vp8-truncated-bool-header.webp` | One zero byte for the compressed header and one zero coefficient byte | `Left "Unexpected end of VP8 compressed header"`; libwebp rejects it |
| `vp8-truncated-bool-modes.webp` | Independently constructed skipped B_PRED frame with sixteen B_HU modes, retaining eight of the fifteen mode-partition bytes | `Left "Unexpected end of VP8 macroblock modes"`; libwebp accepts the complete frame and rejects this truncation |
| `vp8-truncated-bool-coefficients.webp` | `vp8-quantizer.webp` with only its first coefficient byte | `Left "Unexpected end of VP8 coefficients"`; libwebp rejects it |

Every shortened fixture has its RIFF, chunk, and first-partition sizes adjusted
to the bytes actually stored. The rejected files therefore exercise boolean
reader exhaustion rather than the declared partition-length bounds checks.
