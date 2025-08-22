Copyright 2025 Przemyslaw Czerpak <druzus /at/ priv.onet.pl>

HBBMP is small library for Harbour dedicated to create and modify
BMP images. It has not any 3rd party libs dependencies.
It allows to create new images load existing ones and draw
simple graphics primitives. I did not implement more advanced
ones to not create math lib dependencies.
Anyhow if someone is interesting in it then please implement it.
For me this library is enough to work as backend for HBZEBRA
library (excellent job of Mindaugas Kavaliauskas).
I hope you will find it useful.

Only uncompressed BMP images are supported. When BMP file with
compression flag is loaded then UNSUPPORTED error is set. This
is the only case when this error can appear.

This library can work images using 1, 4, 8, 16, 24 and 32 color
depth (all officially defined). It can work with 2-bit color depth
(4 colors) BMP images which are not supported in official
documentation. I blocked them inside hb_bmp_new() C function but
if someone needs to create such BMP file then it's enough to
uncomment one line with "case 2:" in this code.
In BMP images with 1, 4 and 8 color depths number of colors is
limited to 2, 16 and 256 colors. If limit exceeds or unsupported
colors are defined then -1 is returned and error is set.
In BMP images with 24 color depth alpha channel does not exists
and this library silently ignores it when user pass it in color
index.
BMP images with 16-bit color depth does not have RGB color table
so only arbitrary color indexes can be used and their real colors
necessary for visualization have to be defined outside BMP file.
BMP images color depth 24 and 32 do not use color table and color
indexes are pure big endian RGB values with optional alpha channel
which is ignored in case of 24 bit depth (0xAARRGGBB) so it's not
necessary to allocate colors and programmer may use it directly,
i.e. red := 0xFF0000
The color and alpha must be in range from 0 to 255.
When new BMP is created user can change the order in which rows
are stored in memory and files from bottom-up to top-down setting
the image height to negative value.

On PRG level the BMP image structure is represented as pointer
item with attached destructor so when last variable (Harbour
item) pointing to given BMP structure is cleared or overwritten
(f.e. by pBMP := NIL) then destructor is called and BMP image
structure removed from memory. Anyhow it's possible to force
release operation by explicit call to hb_bmp_free().



The following PRG functions exist in HBBMP lib:

Create new BMP in memory, return pointer item or NIL on error.
   hb_bmp_new( <nWidth>, <nHeight>, [<nDepth>=1], [<nDPI>=72], [@<nError>] )
         => <pBMP> | NIL (error indicator)

Make independent copy of memory BMP structures:
   hb_bmp_copy( <pBMPsrc> ) => <pBMPcopy>

Free BMP structure from memory:
   hb_bmp_free( <pBMP> ) => NIL

Load BMP image from file, return pointer item BMP image structure
or NIL on error:
   hb_bmp_load( <cFileName>, [@<nError>] ) => <pBMP> | NIL

Save BMP image into file:
   hb_bmp_save( <pBMP>, <cFileName> ) => <lOK>

Create new BMP image structure from passed BMP file body, return
pointer item BMP structure or NIL on error:
   hb_bmp_decode( <cBMPdata>, [@<nError>] ) => <pBMP> | NIL

Create BMP file body from BMP image structure:
   hb_bmp_encode( <pBMP> ) => <cBMPdata>

Get BMP width:
   hb_bmp_width( <pBMP> ) => <nWidth>

Get BMP height:
   hb_bmp_height( <pBMP> ) => <nHeight>

Get BMP color depth:
   hb_bmp_depth( <pBMP> ) => <nDepth>

Allocate new color or take index to existing one, or error -1
is returned:
   hb_bmp_color( <pBMP>, <nRed>, <nGreen>, <nBlue>, <nAlpha> )
      => <nColor> | -1

Convert color index to its RGB value, return .T. on success or
.F. in case of error:
   hb_bmp_color2rgb( <pBMP>, <nColor>,
                     @<nRed>, @<nGreen>, @<nBlue>, @<nAlpha> )
         => <lOK>

Set pixel using given coordinates and color index:
   hb_bmp_putpixel( <pBMP>, <nX>, <nY>, <nColor> ) => <lOK>

Get color index of pixel at given coordinates, on error -1
is returned:
   hb_bmp_getpixel( <pBMP>, <nX>, <nY> ) => <nColor>

Draw line from the given starting and ending points and color index:
   hb_bmp_line( <pBMP>, <nX1>, <nY1>, <nX2>, <nY2>, <nColor> ) => NIL

Draw rectangle (filled by default) using given coordinates, size and
index color:
   hb_bmp_rect( <pBMP>, <nX>, <nY>, <nWidth>, <nHeight>,
                <nColor>, [<lFill>=.t.] ) => NIL
