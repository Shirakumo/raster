(in-package #:org.shirakumo.raster)

;;; pixel-ops.lisp
(docs:define-docs
  (function clear
    "Clears the pixel buffer to :white or :black.

This is equivalent to (fill buffer 0) for black or (fill buffer 255)
for white, but may be more efficient.

See BUFFER")

  (function alpha-blend
    "Performs source-over alpha blending for a single color channel.

SRC should be the source color to be overlaid, DST should be the
target color to be overlaid onto. ALPHA should be the alpha value of
the new color.

Returns the blended result.

All values should be (UNSIGNED-BYTE 8).")

  (function encode-color
    "Encodes a B G R A quadruplet into an (UNSIGNED-BYTE 32) color.

See DECODE-COLOR")

  (function decode-color
    "Decodes an (UNSIGNED-BYTE 32) into its B G R A color components.

See ENCODE-COLOR")

  (function color-ref
    "Accesses a single pixel in a color buffer.

The color buffer must be a (SIMPLE-ARRAY (UNSIGNED-BYTE 8) (*)) and
must contain the pixels in standard B G R A channel order.

See ENCODE-COLOR
See DECODE-COLOR
See COLOR-REF*")

  (function color-ref*
    "Accesses a single pixel in a color buffer with border handling.

BORDER defines how to deal with coordinates that lie outside of the
buffer region. It can be one of the following:

  :CLAMP   --- Accesses the nearest pixel on the border of the buffer.
  :REPEAT  --- Wraps the coordinate around.
  COLOR    --- Returns the BORDER color instead.

When setting this place, :BORDER and :CLAMP do not modify anything
when the pixel lies outside the buffer.

The color buffer must be a (SIMPLE-ARRAY (UNSIGNED-BYTE 8) (*)) and
must contain the pixels in standard B G R A channel order.

See ENCODE-COLOR
See DECODE-COLOR
See COLOR-REF")

  (function composite
    "Composite two image buffers together.

This performs basic \"source-over\" alpha blending.

Both SOURCE and TARGET must be (SIMPLE-ARRAY (UNSIGNED-BYTE 8) (*))
vectors that hold colour values in row-major B G R A order.

SW and SH should be the dimensions of the source image.
TW and TH should be the dimensions of the target image.
SX and SY should be offsets of the region to read from the source.
TX and TY should be offsets of the region to write into the target.
W and H should be the width and height of the region to transfer.

If any part of the source or target region should be out of bounds,
the operation is truncated to only transfer valid pixels.

See ALPHA-BLEND
See BUFFER
See COMPOSITE-MASK")

  (function composite-mask
    "Composite a mask onto a target buffer using a color function.

This performs basic \"source-over\" alpha blending.

Both SOURCE and TARGET must be (SIMPLE-ARRAY (UNSIGNED-BYTE 8) (*))
vectors that hold colour values in row-major order. For SOURCE it
should only hold A values, and for TARGET it should hold B G R A
values.

COLOR-FUN should be a function that takes an X and Y pixel index, and
returns a single (UNSIGNED-BYTE 32) encoding the B G R A color
quadruplet in little-endian order, meaning A has bits 24-32. COLOR-FUN
is only invoked for pixels for which the MASK contains a value greater
than zero. The X and Y passed into the COLOR-FUN are relative to the
source.

SW and SH should be the dimensions of the source image.
TW and TH should be the dimensions of the target image.
SX and SY should be offsets of the region to read from the source.
TX and TY should be offsets of the region to write into the target.
W and H should be the width and height of the region to transfer.

If any part of the source or target region should be out of bounds,
the operation is truncated to only transfer valid pixels.

See ALPHA-BLEND
See BUFFER
See COMPOSITE")

  (function composite-sdf
    "Composite a shape described by a signed distance function onto a target buffer using a color function.

This performs basic \"source-over\" alpha blending.

Both TARGET must be a (SIMPLE-ARRAY (UNSIGNED-BYTE 8) (*)) vector that
holds colour values the B G R A color values in row-major order.

SDF should be a function that takes an X and Y pixel index, and
returns a single SINGLE-FLOAT that describes the signed distance
function value at that point. Positive values lie outside, and
negative values lie inside the described shape. The X and Y passed
into the COLOR-FUN are relative to the source.

COLOR-FUN should be a function that takes an X and Y pixel index, and
returns a single (UNSIGNED-BYTE 32) encoding the B G R A color
quadruplet in little-endian order, meaning A has bits 24-32. COLOR-FUN
is only invoked for pixels for which the SDF returns a value smaller or
equal to zero. The X and Y passed into the COLOR-FUN are relative to
the source.

SW and SH should be the dimensions of the source image.
TW and TH should be the dimensions of the target image.
SX and SY should be offsets of the region to read from the source.
TX and TY should be offsets of the region to write into the target.
W and H should be the width and height of the region to transfer.

If any part of the source or target region should be out of bounds,
the operation is truncated to only transfer valid pixels.

See ALPHA-BLEND
See BUFFER
See COMPOSITE")

  (function sampler
    "Creates a texture sampler function.

The returned function will take two arguments, the X and Y coordinate
into the texture to index.

The color buffer must be a (SIMPLE-ARRAY (UNSIGNED-BYTE 8) (*)) and
must contain the pixels in standard B G R A channel order.

The WRAPPING and BORDER arguments are the same as for COLOR-REF*

See COLOR-REF*
See COMPOSITE-MASK
See COMPOSITE-SDF")

  (function solid-color
    "Creates a color sampler function.

The returned function will take two arguments, the X and Y coordinate
into to index.

See ENCODE-COLOR
See COMPOSITE-MASK
See COMPOSITE-SDF"))
