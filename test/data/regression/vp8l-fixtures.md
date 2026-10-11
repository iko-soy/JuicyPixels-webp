These tiny RIFF/VP8L fixtures were assembled directly from the lossless
bitstream format and checked independently with libwebp (`WebPDecodeRGBA`).
Tests use fixed expected values rather than an encoder/decoder round trip.

| Fixture | Encoded case | Reference result |
| --- | --- | --- |
| `vp8l-long-code.webp` | A complete green Huffman tree with lengths `[1,2,3,4,5,6,7,8,9,9]`; the pixel uses symbol 8 | 1×1 RGBA `(0,8,0,255)` |
| `vp8l-meta-prefix.webp` | Raw entropy exponent 0 (four-pixel blocks), two spatial Huffman groups | 8×1, green values `[10,10,10,10,20,20,20,20]`, red/blue 0, alpha 255 |
| `vp8l-invalid-backref.webp` | The first green symbol is length code 256, with distance 1 and no preceding pixel | Rejected |
| `vp8l-truncated-pixels.webp` | 100×1 header and a two-symbol green tree, with the pixel data omitted | Rejected |
| `transform-order.webp` | Subtract-green followed by color-indexing, with one palette entry `(8,7,9,255)` | 1×1 RGBA `(15,7,16,255)` |
| `vp8l-oversubscribed.webp` | Green Huffman lengths `[1,1,1,1,15]` exceed the available code space | Rejected |
| `vp8l-incomplete.webp` | Green Huffman lengths `[2,2]` leave unused codes; pixel data selects `11` | Rejected |
| `vp8l-copy-overrun.webp` | 2×1 header, one literal followed by a length-2 backreference at distance 1 | Rejected |
| `vp8l-copy-exact.webp` | The same literal and backreference with a 3×1 header | Three opaque black pixels |

The truncation tests also append enough zero bits to the same Huffman tables
to verify that a complete stream succeeds, and remove the five-byte VP8L
header to exercise the compressed-alpha decoding path.

The following 1×1 fixtures were also hand-packed from RFC 9649 and checked
with Pillow's libwebp decoder. They use single-symbol prefix codes without
a color cache or backward references, isolating transform-type validation.

| Fixture | Encoded case | Reference result |
| --- | --- | --- |
| `vp8l-duplicate-predictor.webp` | Predictor transform repeated twice | Rejected |
| `vp8l-duplicate-color.webp` | Color transform repeated twice | Rejected |
| `vp8l-duplicate-subgreen.webp` | Subtract-green transform repeated twice | Rejected |
| `vp8l-duplicate-palette.webp` | Color-indexing transform repeated twice | Rejected |
| `vp8l-duplicate-separated.webp` | Subtract-green, color, then subtract-green again | Rejected |
| `vp8l-unique-transforms.webp` | Subtract-green, predictor, color-indexing, then color; all four types occur once | RGBA `(30,20,50,254)` |

Predictor maps use mode 0, color-transform coefficients are zero, and each
palette contains RGBA `(10,20,30,255)`. The duplicate-transform tests also
remove the container and five-byte VP8L header to verify rejection through
the headerless decoder used by ALPH.
