(in-package #:org.shirakumo.raster)

;; buffer.lisp
(docs:define-docs
  (type buffer
    "Type representing a pixel buffer.

This is an alias for (SIMPLE-ARRAY (UNSIGNED-BYTE 8) (*)) with the
following additional constraint: the data contained within should be
pixels in row-major order laid out as B G R A quadruplets. As such,
the length of a buffer should always be a multiple of four.

A buffer can hold a maximum of 1073741823 (2^30-1) pixels.

See COLOR (type)
See CHANNEL (type)
See COLOR-REF
See COLOR-REF*")

  (type color
    "Type representing a single color or pixel.

This is an alias for (UNSIGNED-BYTE 32) with four eight-bit color
channels laid out as B G R A in little-endian order, meaning B
occupies the lowest bits and A the highest bits.

See BUFFER (type)
See ENCODE-COLOR
See DECODE-COLOR
See COLOR-REF
See COLOR-REF*")

  (type channel
    "Type representing a single color channel.

This is an alias for (UNSIGNED-BYTE 8).

See BUFFER (type)
See COLOR (type)")

  (type index
    "Type representing an index into a color buffer.

This is an alias for (UNSIGNED-BYTE 32) and is used to represent
dimensions and coordinates for a color buffer.

The index of a pixel must not exceed 1073741823 (2^30-1).

See BUFFER (type)")

  (type coordinate
    "Type representing a coordinate within an SDF.

This is an alias for SINGLE-FLOAT.

See COORDINATE")

  (function coordinate
    "Coerces the input into a coordinate.

See COORDINATE (type)")

  (type image
    "Representation of a pixel buffer along with its image dimensions.

See IMAGE-BUFFER
See IMAGE-WIDTH
See IMAGE-HEIGHT
See MAKE-IMAGE")

  (function image-buffer
    "Accesses the underlying pixel buffer of the image.

The buffer must be 4*WIDTH*HEIGHT in size.

See IMAGE (type)
See BUFFER (type)")

  (function image-width
    "Accesses the width of the pixel buffer.

See IMAGE (type)
See INDEX")

  (function image-height
    "Accesses the height of the pixel buffer.

See IMAGE (type)
See INDEX")

  (function make-image
    "Creates a new image.

If BUFFER is given and it is already a BUFFER, then the value is
shared with the resulting IMAGE instance. Otherwise, a new BUFFER is
allocated and the contents are copied over.

See IMAGE (type)
See BUFFER")

  (function make-buffer
    "Creates a fresh pixel buffer.

If CONTENTS is given, it must be a sequence that equals 4*W*H in
length.

See BUFFER (type)")

 (function clear
   "Clears the pixel buffer to :white or :black.

This is equivalent to (fill buffer 0) for black or (fill buffer 255)
for white, but may be more efficient.

See BUFFER")

 (function encode-color
   "Encodes a B G R A quadruplet into an (UNSIGNED-BYTE 32) color.

See DECODE-COLOR")

 (function decode-color
   "Decodes an (UNSIGNED-BYTE 32) into its B G R A color components.

See ENCODE-COLOR")

 (function color-ref
   "Accesses a single pixel in a color buffer.

The color buffer must be a BUFFER, and the INDEX must be in the valid
buffer range. Attempting to reference a pixel outside the buffer leads
to undefined behaviour.

See COLOR (type)
see BUFFER (type)
See INDEX (type)
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

The color buffer must be a BUFFER and the X, Y, W, and H descriptors
must be INDEXes. X and Y may lay outside their respective ranges, but
the W and H must be accurate to the buffer's contents. If they are
not, undefined behaviour occurs.

See COLOR (type)
See INDEX (type)
See BUFFER (type)
See COLOR-REF")

 (function lerp-color
   "Linearly interpolates between two colors.

A and B must be COLORs, and X must be a SINGLE-FLOAT in range [0,1]
that encompasses the interpolation distance between the two for the
resulting color. Each CHANNEL of the colors is interpolated
separately.

If X is out of range or A or B aren't COLORs the results are
undefined.

See COLOR (type)"))

;; composite.lisp
(docs:define-docs
  (function alpha-blend
    "Performs source-over alpha blending for a single color channel.

SRC should be the source color to be overlaid, DST should be the
target color to be overlaid onto. ALPHA should be the alpha value of
the new color.

Returns the blended result.

All values should be (UNSIGNED-BYTE 8).")
  
  (function blit-buffer
    "Copy one pixel buffer to another.

This simply replaces pixels with no attempt at blending and as such is
much faster of an operation than COMPOSITE-BUFFER.

Both SOURCE and TARGET must be BUFFERs.

SW and SH should be the dimensions of the source image.
TW and TH should be the dimensions of the target image.
SX and SY should be offsets of the region to read from the source.
TX and TY should be offsets of the region to write into the target.
W and H should be the width and height of the region to transfer.

If any part of the source or target region should be out of bounds,
the operation is truncated to only transfer valid pixels.

See BUFFER (type)
See COMPOSITE-BUFFER")

 (function composite-buffer
   "Composite two image buffers together.

This performs basic \"source-over\" alpha blending.

Both SOURCE and TARGET must be BUFFERs.

SW and SH should be the dimensions of the source image.
TW and TH should be the dimensions of the target image.
SX and SY should be offsets of the region to read from the source.
TX and TY should be offsets of the region to write into the target.
W and H should be the width and height of the region to transfer.

If any part of the source or target region should be out of bounds,
the operation is truncated to only transfer valid pixels.

See ALPHA-BLEND
See BUFFER (type)
See COMPOSITE-MASK")

 (function composite-mask
   "Composite a mask onto a target buffer using a color function.

This performs basic \"source-over\" alpha blending.

TARGET must be a BUFFER. SOURCE must be a W*H sized
(SIMPLE-ARRAY (UNSIGNED-BYTE 8) (*)) that holds the alpha mask.

SAMPLER should be a SAMPLER.

SW and SH should be the dimensions of the source image.
TW and TH should be the dimensions of the target image.
SX and SY should be offsets of the region to read from the source.
TX and TY should be offsets of the region to write into the target.
W and H should be the width and height of the region to transfer.

If any part of the source or target region should be out of bounds,
the operation is truncated to only transfer valid pixels.

See ALPHA-BLEND
See BUFFER (type)
See SAMPLER (type)")
 
 (function composite-sdf
   "Composite a shape described by a signed distance function onto a target buffer using a color function.

This performs basic \"source-over\" alpha blending.

TARGET must be a BUFFER.

SDF should be an SDF.

SAMPLER should be a SAMPLER.

SW and SH should be the dimensions of the source image.
TW and TH should be the dimensions of the target image.
SX and SY should be offsets of the region to read from the source.
TX and TY should be offsets of the region to write into the target.
W and H should be the width and height of the region to transfer.

If any part of the source or target region should be out of bounds,
the operation is truncated to only transfer valid pixels.

See ALPHA-BLEND
See BUFFER (type)
See SDF (type)
See SAMPLER (type)"))


;; raster.lisp
(docs:define-docs
  (function clip
    "Clip the SDF to be within the clipping region only.

See PUSH-CLIP
See POP-CLIP
See WITH-CLIP
See WITH-NO-CLIPPING
See SDF (type)")

  (function push-clip
    "Push a clipping SDF onto the stack.

This will only ever //narrow// the clipping region to become the
intersection of the current clipping region and the newly passed one.

See POP-CLIP
See CLIP
See WITH-CLIP
See WITH-NO-CLIPPING
See SDF (type)")

  (function pop-clip
    "Pop a clipping SDF from the stack.

If no corresponding push operation happened first, an error is
signalled.

See PUSH-CLIP
See CLIP
See WITH-CLIP
See WITH-NO-CLIPPING")

  (function with-clip
    "Narrow the clipping region during BODY by SDF.

See WITH-NO-CLIPPING
See PUSH-CLIP
See POP-CLIP
See CLIP
See SDF (type)
See WITH-RECT-CLIP")

  (function with-rect-clip
    "Shorthand to narrow by a rectangular clipping region.

See WITH-CLIP
See RECTANGLE")

  (function with-no-clipping
    "Disables previously applied clipping during BODY.

See WITH-CLIP")

  (function draw-line
    "Draw a line onto the given buffer.

BUFFER must be a pixel buffer and BW and BH its dimensions.

SAMPLER is coerced via ENSURE-SAMPLER. It is used to derive the actual
color information to fill the shape.

LINE-WIDTH should be the width of the stroke.

FEATHER should be a feathering (blurring) radius applied to the shape.
Note that the blur only extends inwards, meaning the shape only ever
gets perceptually \"smaller\". You may have to increase its size to
compensate.

The shape is clipped by the current clipping region prior to
rasterisation.

See BUFFER (type)
See INDEX (type)
See SAMPLER (type)
See ENSURE-SAMPLER
See CLIP
See LINE")

  (function draw-curve
    "Draw a cubic bezier curve onto the given buffer.

A and B are the end points and W and V the corresponding handle
points.

BUFFER must be a pixel buffer and BW and BH its dimensions.

SAMPLER is coerced via ENSURE-SAMPLER. It is used to derive the actual
color information to fill the shape.

LINE-WIDTH should be the width of the stroke.

FEATHER should be a feathering (blurring) radius applied to the shape.
Note that the blur only extends inwards, meaning the shape only ever
gets perceptually \"smaller\". You may have to increase its size to
compensate.

The shape is clipped by the current clipping region prior to
rasterisation.

See BUFFER (type)
See INDEX (type)
See SAMPLER (type)
See ENSURE-SAMPLER
See CLIP
See CURVE")

  (function draw-lines
    "TODO: Implement")

  (function draw-curves
    "TODO: Implement")

  (function draw-rectangle
    "Draw a rectangle onto the given buffer.

BUFFER must be a pixel buffer and BW and BH its dimensions.

SAMPLER is coerced via ENSURE-SAMPLER. It is used to derive the actual
color information to fill the shape.

LINE-WIDTH should be the width of the stroke. If none is passed, the
rectangle is filled instead.

FEATHER should be a feathering (blurring) radius applied to the shape.
Note that the blur only extends inwards, meaning the shape only ever
gets perceptually \"smaller\". You may have to increase its size to
compensate.

CORNER-RADII if given should be a sequence of four elements, which
describe the radii of the corners of the rectangle, with the first
being top left, second top right, third bottom right, and fourth
bottom left.

The shape is clipped by the current clipping region prior to
rasterisation.

See BUFFER (type)
See INDEX (type)
See SAMPLER (type)
See ENSURE-SAMPLER
See CLIP
See RECTANGLE")

  (function draw-ellipse
    "Draw an ellipse onto the given buffer.

BUFFER must be a pixel buffer and BW and BH its dimensions.

SAMPLER is coerced via ENSURE-SAMPLER. It is used to derive the actual
color information to fill the shape.

LINE-WIDTH should be the width of the stroke. If none is passed, the
rectangle is filled instead.

FEATHER should be a feathering (blurring) radius applied to the shape.
Note that the blur only extends inwards, meaning the shape only ever
gets perceptually \"smaller\". You may have to increase its size to
compensate.

START and END, if given, describe the starting and ending angles of
the pie that is drawn.

INNER-RADIUS, if given, describes the radius of the inner cutout of
the arc that is drawn. If the arc is not symmetrical, this radius is
relative to the width and the inner radius in height is derived
automatically.

The shape is clipped by the current clipping region prior to
rasterisation.

See BUFFER (type)
See INDEX (type)
See SAMPLER (type)
See ENSURE-SAMPLER
See CLIP
See ELLIPSE")

  (function draw-polygon
    "Draw a closed polygon onto the given buffer.

BUFFER must be a pixel buffer and BW and BH its dimensions.

SAMPLER is coerced via ENSURE-SAMPLER. It is used to derive the actual
color information to fill the shape.

LINE-WIDTH should be the width of the stroke. If none is passed, the
rectangle is filled instead.

FEATHER should be a feathering (blurring) radius applied to the shape.
Note that the blur only extends inwards, meaning the shape only ever
gets perceptually \"smaller\". You may have to increase its size to
compensate.

The POINTS should be a SEQUENCE in which the points are packed.
Meaning the sequence contains [X1 Y1 X2 Y2 ...].

The shape is clipped by the current clipping region prior to
rasterisation.

See BUFFER (type)
See INDEX (type)
See SAMPLER (type)
See ENSURE-SAMPLER
See CLIP
See POLYGON")

  (function draw-image
    "Draw another image onto the given buffer.

BUFFER must be a pixel buffer and BW and BH its dimensions.

IMAGE must be an IMAGE.

X and Y must be INDEX coordinates of where to draw the image in the
buffer.

TRANSFORM may be a transform matrix by which to transform the image.

The shape is clipped by the current clipping region prior to
rasterisation.

See BUFFER (type)
See INDEX (type)
See IMAGE (type)
See CLIP"))

;; sampler.lisp
(docs:define-docs
  (type sampler
    "Type representing sampling functions.

This is an alias for (FUNCTION (INDEX INDEX) COLOR).

A sampler should be a function that takes an X and Y pixel index, and
returns a single COLOR for that point.

See COLOR (type)
See INDEX (type)")

  (type transform
    "Type representing a 2D affine transformation matrix.

This is an alias for (SIMPLE-ARRAY SINGLE-FLOAT (6)).

The matrix elements are stored in row-major order. The identity matrix
is therefore #(1 0 0 0 1 0).

See MAKE-TRANSFORM")

  (function make-transform
    "Create a transform matrix.

If no arguments are passed, an identity matrix is returned. Otherwise
you must pass 6 arguments, which form the 6 elements of the matrix in
row-major order.

See TRANSFORM (type)")

  (function sample-color
    "Samples a color from a buffer.

Unlike COLOR-REF and COLOR-REF* this allows COORDINATEs for X and Y,
performing bilinear interpolation as necessary if the requested index
should lie between pixels.

The BORDER argument is the same as for COLOR-REF*

See BUFFER (type)
See COORDINATE (type)
See COLOR (type)
See COLOR-REF
See COLOR-REF*")

  (function sampler
    "Creates a texture sampler function.

The color buffer must be a BUFFER.

The BORDER argument is the same as for COLOR-REF*

The TRANSFORM may be a TRANSFORM matrix by which the coordinates
passed to the resulting SAMPLER are transformed before sampling the
color buffer, thereby allowing you to rotate, scale, and otherwise
transform the buffer.

See SAMPLER (type)
See TRANSFORM (type)
See COLOR-REF*
See COMPOSITE-MASK
See COMPOSITE-SDF")
    
 (function solid-color
   "Creates a sampler with a uniform color.

See SAMPLER (type)
See COLOR (type)
See COMPOSITE-MASK
See COMPOSITE-SDF")

 (function evaluate-gradient
   "Evaluates a linear gradient.

STOPS should be a list where each entry describes a griadent stop. A
gradient stop should be a list of two elements, the first being the
stop index and the second being the colour at that stop.

I should be the point at which to evaluate the gradient. If I is
smaller than the first stop, the first stop's color is returned. If I
is larger than the last stop, the last stop's color is returned. If I
lies between two stops, the returned color is a linear interpolation
of the two stops, respective to I's position between them.

See COLOR (type)
See LERP-COLOR")

 (function radial-gradient
   "Creates a radial gradient sampler.

STOPS should be a list where each entry describes a griadent stop. A
gradient stop should be a list of two elements, the first being the
stop index and the second being the colour at that stop. The indices
are relative to the actual coordinate system in which the sampler is
evaluated. 

X and Y should be the coordinates of the gradient's center.

See SAMPLER (type)")

 (function linear-gradient
   "Creates a linear gradient sampler.

STOPS should be a list where each entry describes a griadent stop. A
gradient stop should be a list of two elements, the first being the
stop index and the second being the colour at that stop. The indices
should be in the range of [0,1].

AX and AY should be the coordinates of the gradient's start.
BX and BY should be the coordinates of the gradient's stop.

See SAMPLER (type)
See BILINEAR-GRADIENT")

 (function bilinear-gradient
   "Creates a bilinear gradient sampler.

STOPS should be a list where each entry describes a griadent stop. A
gradient stop should be a list of two elements, the first being the
stop index and the second being the colour at that stop. The indices
should be in the range of [0,1].

AX and AY should be the coordinates of the gradient's start.
BX and BY should be the coordinates of the gradient's stop.

The bilinear gradient is like the linear gradient, but mirrored at A.

See SAMPLER (type)
See LINEAR-GRADIENT")

 (function diamond-gradient
   "Creates a diamond gradient sampler.

STOPS should be a list where each entry describes a griadent stop. A
gradient stop should be a list of two elements, the first being the
stop index and the second being the colour at that stop. The indices
are relative to the actual coordinate system in which the sampler is
evaluated. 

X and Y should be the coordinates of the gradient's center.

See SAMPLER (type)")

 (function conical-gradient
   "Creates a conical gradient sampler.

STOPS should be a list where each entry describes a griadent stop. A
gradient stop should be a list of two elements, the first being the
stop index and the second being the colour at that stop. The indices
should be in the range of [0,2*PI].

X and Y should be the coordinates of the gradient's center.

See SAMPLER (type)")

 (function ensure-sampler
   "Tries to ensure the object is a sampler.

If the object is a FUNCTION, it is returned. If the function is not an
actual SAMPLER, the behaviour is undefined.

If the object is a COLOR, a SOLID-COLOR sampler is returned.

If the object is an IMAGE, a SAMPLER with REPEAT bordering is
returned.

If the object is NIL, a SOLID-COLOR sampler of 0 (transparent black)
is returned.

See SOLID-COLOR
See SAMPLER
See COLOR (type)
See SAMPLER (type)"))

;; sdf.lisp
(docs:define-docs
  (type sdf
    "Type representing a signed distance field.

This is an alias for (FUNCTION (COORDINATE COORDINATE) SINGLE-FLOAT).

A SDF should be a function that takes an X and Y pixel index and
returns a single SINGLE-FLOAT that describes the signed distance
function value at that point. Positive values lie outside, and
negative values lie inside the described shape.

See COORDINATE (type)")

  (function rectangle
    "Construct a rectangular SDF.

X and Y should be the centre coordinates of the rectangle.
W and H should be the half-size dimensions of the rectangle.

CORNER-RADII if given should be a sequence of four elements, which
describe the radii of the corners of the rectangle, with the first
being top left, second top right, third bottom right, and fourth
bottom left.

See SDF (type)")

  (function ellipse
    "Construct an ellipse SDF.

X and Y should be the centre coordinates of the ellipse.
W and H should be the half-size dimensions of the ellipse.

START and END, if given, describe the starting and ending angles of
the pie that is drawn.

INNER-RADIUS, if given, describes the radius of the inner cutout of
the arc that is drawn. If the arc is not symmetrical, this radius is
relative to the width and the inner radius in height is derived
automatically.

See SDF (type)")

  (function line
    "Construct a line SDF.

AX and AY describe the start point coordinates.
BX and BY describe the end point coordinates.

THICKNESS describes the width of the line.

See SDF (type)")

  (function bezier
    "Construct a cubic bezier curve SDF.

AX and AY describe the start point coordinates.
WX and WY describe the first handle coordinates.
VX and VY describe the second handle coordinates.
BX and BY describe the end point coordinates.

THICKNESS describes the width of the curve.

See SDF (type)")

  (function polygon
    "Construct a closed polygon SDF.

The POINTS should be a SEQUENCE in which the points are packed.
Meaning the sequence contains [X1 Y1 X2 Y2 ...].

See SDF (type)")

  (function subtract
    "Subtract the second shape from the first.

See SDF (type)")

  (function combine
    "Combine (union) the two shapes.

See SDF (type)")

  (function intersect
    "Intersect the two shapes.

See SDF (type)")

  (function outline
    "Create an outline of the given thickness out of the shape.

See SDF (type)")

  (function translate
    "Trasnlate (move) the shape by the given coordinates.

See SDF (type)")

  (function scale
    "Scale the shape by the given coordinates.

See SDF (type)")

  (function rotate
    "Rotate the shape by the given angle.

See SDF (type)")

  (function skew
    "Skew the shape by the given degrees.

See SDF (type)")

  (function transform
    "Transform the shape by the given affine transform matrix.

See TRANSFORM (type)
See SDF (type)"))

