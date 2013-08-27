/*
 * xHarbour Project source code:
 * FreeImage graphic library header file.
 *
 * Copyright 2005 Francesco Saverio Giudice <info@fsgiudice.com>
 * www - http://www.xharbour.org http://harbour-project.org
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2, or (at your option)
 * any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this software; see the file COPYING.txt.  If not, write to
 * the Free Software Foundation, Inc., 59 Temple Place, Suite 330,
 * Boston, MA 02111-1307 USA (or visit the web site https://www.gnu.org/).
 *
 * As a special exception, the Harbour Project gives permission for
 * additional uses of the text contained in its release of Harbour.
 *
 * The exception is that, if you link the Harbour libraries with other
 * files to produce an executable, this does not by itself cause the
 * resulting executable to be covered by the GNU General Public License.
 * Your use of that executable is in no way restricted on account of
 * linking the Harbour library code into it.
 *
 * This exception does not however invalidate any other reasons why
 * the executable file might be covered by the GNU General Public License.
 *
 * This exception applies only to the code released by the Harbour
 * Project under the name Harbour.  If you copy code from other
 * Harbour Project or Free Software Foundation releases into a copy of
 * Harbour, as the General Public License permits, the exception does
 * not apply to the code that you add in this way.  To avoid misleading
 * anyone as to the status of such modified files, you must delete
 * this exception notice from them.
 *
 * If you write modifications of your own for Harbour, it is your choice
 * whether to permit this exception to apply to your modifications.
 * If you do not wish that, delete this exception notice.
 *
 */

#ifndef FREEIMAGE_CH
#define FREEIMAGE_CH

#ifndef FREEIMAGE_BIGENDIAN
// Little Endian (x86 / MS Windows, Linux) : BGR(A) order
#define FI_RGBA_RED           2
#define FI_RGBA_GREEN         1
#define FI_RGBA_BLUE          0
#define FI_RGBA_ALPHA         3
#define FI_RGBA_RED_MASK      0x00FF0000
#define FI_RGBA_GREEN_MASK    0x0000FF00
#define FI_RGBA_BLUE_MASK     0x000000FF
#define FI_RGBA_ALPHA_MASK    0xFF000000
#define FI_RGBA_RED_SHIFT     16
#define FI_RGBA_GREEN_SHIFT   8
#define FI_RGBA_BLUE_SHIFT    0
#define FI_RGBA_ALPHA_SHIFT   24
#else
// Big Endian (PPC / Linux, MaxOSX) : RGB(A) order
#define FI_RGBA_RED           0
#define FI_RGBA_GREEN         1
#define FI_RGBA_BLUE          2
#define FI_RGBA_ALPHA         3
#define FI_RGBA_RED_MASK      0xFF000000
#define FI_RGBA_GREEN_MASK    0x00FF0000
#define FI_RGBA_BLUE_MASK     0x0000FF00
#define FI_RGBA_ALPHA_MASK    0x000000FF
#define FI_RGBA_RED_SHIFT     24
#define FI_RGBA_GREEN_SHIFT   16
#define FI_RGBA_BLUE_SHIFT    8
#define FI_RGBA_ALPHA_SHIFT   0
#endif // FREEIMAGE_BIGENDIAN

#define FI_RGBA_RGB_MASK    (FI_RGBA_RED_MASK|FI_RGBA_GREEN_MASK|FI_RGBA_BLUE_MASK)

// The 16bit macros only include masks and shifts, since each color element is not byte aligned

#define FI16_555_RED_MASK     0x7C00
#define FI16_555_GREEN_MASK   0x03E0
#define FI16_555_BLUE_MASK    0x001F
#define FI16_555_RED_SHIFT    10
#define FI16_555_GREEN_SHIFT  5
#define FI16_555_BLUE_SHIFT   0
#define FI16_565_RED_MASK     0xF800
#define FI16_565_GREEN_MASK   0x07E0
#define FI16_565_BLUE_MASK    0x001F
#define FI16_565_RED_SHIFT    11
#define FI16_565_GREEN_SHIFT  5
#define FI16_565_BLUE_SHIFT   0

/* ICC profile support */

#define FIICC_DEFAULT         0x00
#define FIICC_COLOR_IS_CMYK   0x01

/** I/O image format identifiers.
*/
//FREE_IMAGE_FORMAT
#define FIF_UNKNOWN  -1
#define FIF_BMP      0
#define FIF_ICO      1
#define FIF_JPEG     2
#define FIF_JNG      3
#define FIF_KOALA    4
#define FIF_LBM      5
#define FIF_IFF      FIF_LBM
#define FIF_MNG      6
#define FIF_PBM      7
#define FIF_PBMRAW   8
#define FIF_PCD      9
#define FIF_PCX      10
#define FIF_PGM      11
#define FIF_PGMRAW   12
#define FIF_PNG      13
#define FIF_PPM      14
#define FIF_PPMRAW   15
#define FIF_RAS      16
#define FIF_TARGA    17
#define FIF_TIFF     18
#define FIF_WBMP     19
#define FIF_PSD      20
#define FIF_CUT      21
#define FIF_XBM      22
#define FIF_XPM      23
#define FIF_DDS      24
#define FIF_GIF      25
#define FIF_HDR      26


/** Image type used in FreeImage.
*/
//FREE_IMAGE_TYPE
#define FIT_UNKNOWN  0   // unknown type
#define FIT_BITMAP   1   // standard image     : 1-, 4-, 8-, 16-, 24-, 32-bit
#define FIT_UINT16   2   // array of unsigned short  : unsigned 16-bit
#define FIT_INT16    3   // array of short     : signed 16-bit
#define FIT_UINT32   4   // array of unsigned long : unsigned 32-bit
#define FIT_INT32    5   // array of long      : signed 32-bit
#define FIT_FLOAT    6   // array of float     : 32-bit IEEE floating point
#define FIT_DOUBLE   7   // array of double      : 64-bit IEEE floating point
#define FIT_COMPLEX  8   // array of FICOMPLEX   : 2 x 64-bit IEEE floating point
#define FIT_RGB16    9   // 48-bit RGB image     : 3 x 16-bit
#define FIT_RGBA16   10  // 64-bit RGBA image    : 4 x 16-bit
#define FIT_RGBF     11  // 96-bit RGB float image : 3 x 32-bit IEEE floating point
#define FIT_RGBAF    12  // 128-bit RGBA float image : 4 x 32-bit IEEE floating point


/** Image color type used in FreeImage.
*/
//FREE_IMAGE_COLOR_TYPE
#define FIC_MINISWHITE  0   // min value is white
#define FIC_MINISBLACK  1   // min value is black
#define FIC_RGB         2   // RGB color model
#define FIC_PALETTE     3   // color map indexed
#define FIC_RGBALPHA    4   // RGB color model with alpha channel
#define FIC_CMYK        5   // CMYK color model


/** Color quantization algorithms.
Constants used in FreeImage_ColorQuantize.
*/
//FREE_IMAGE_QUANTIZE
#define FIQ_WUQUANT     0   // Xiaolin Wu color quantization algorithm
#define FIQ_NNQUANT     1   // NeuQuant neural-net quantization algorithm by Anthony Dekker


/** Dithering algorithms.
Constants used in FreeImage_Dither.
*/
//FREE_IMAGE_DITHER
#define FID_FS           0  // Floyd & Steinberg error diffusion
#define FID_BAYER4x4     1  // Bayer ordered dispersed dot dithering (order 2 dithering matrix)
#define FID_BAYER8x8     2  // Bayer ordered dispersed dot dithering (order 3 dithering matrix)
#define FID_CLUSTER6x6   3  // Ordered clustered dot dithering (order 3 - 6x6 matrix)
#define FID_CLUSTER8x8   4  // Ordered clustered dot dithering (order 4 - 8x8 matrix)
#define FID_CLUSTER16x16 5  // Ordered clustered dot dithering (order 8 - 16x16 matrix)


/** Lossless JPEG transformations
Constants used in FreeImage_JPEGTransform
*/
//FREE_IMAGE_JPEG_OPERATION
#define FIJPEG_OP_NONE        0  // no transformation
#define FIJPEG_OP_FLIP_H      1  // horizontal flip
#define FIJPEG_OP_FLIP_V      2  // vertical flip
#define FIJPEG_OP_TRANSPOSE   3  // transpose across UL-to-LR axis
#define FIJPEG_OP_TRANSVERSE  4  // transpose across UR-to-LL axis
#define FIJPEG_OP_ROTATE_90   5  // 90-degree clockwise rotation
#define FIJPEG_OP_ROTATE_180  6  // 180-degree rotation
#define FIJPEG_OP_ROTATE_270  7  // 270-degree clockwise (or 90 ccw)


/** Tone mapping operators.
Constants used in FreeImage_ToneMapping.
*/
//FREE_IMAGE_TMO
#define FITMO_DRAGO03         0  // Adaptive logarithmic mapping (F. Drago, 2003)
#define FITMO_REINHARD05      1  // Dynamic range reduction inspired by photoreceptor physiology (E. Reinhard, 2005)


/** Upsampling / downsampling filters.
Constants used in FreeImage_Rescale.
*/
//FREE_IMAGE_FILTER
#define FILTER_BOX         0  // Box, pulse, Fourier window, 1st order (constant) b-spline
#define FILTER_BICUBIC     1  // Mitchell & Netravali's two-param cubic filter
#define FILTER_BILINEAR    2  // Bilinear filter
#define FILTER_BSPLINE     3  // 4th order (cubic) b-spline
#define FILTER_CATMULLROM  4  // Catmull-Rom spline, Overhauser spline
#define FILTER_LANCZOS3    5  // Lanczos3 filter


/** Color channels.
Constants used in color manipulation routines.
*/
//FREE_IMAGE_COLOR_CHANNEL
#define FICC_RGB     0  // Use red, green and blue channels
#define FICC_RED     1  // Use red channel
#define FICC_GREEN   2  // Use green channel
#define FICC_BLUE    3  // Use blue channel
#define FICC_ALPHA   4  // Use alpha channel
#define FICC_BLACK   5  // Use black channel
#define FICC_REAL    6  // Complex images: use real part
#define FICC_IMAG    7  // Complex images: use imaginary part
#define FICC_MAG     8  // Complex images: use magnitude
#define FICC_PHASE   9  // Complex images: use phase

/* Metadata support */

/**
  Tag data type information (based on TIFF specifications)

  Note: RATIONALs are the ratio of two 32-bit integer values.
*/
//FREE_IMAGE_MDTYPE
#define FIDT_NOTYPE     0   // placeholder
#define FIDT_BYTE       1   // 8-bit unsigned integer
#define FIDT_ASCII      2   // 8-bit bytes w/ last byte null
#define FIDT_SHORT      3   // 16-bit unsigned integer
#define FIDT_LONG       4   // 32-bit unsigned integer
#define FIDT_RATIONAL   5   // 64-bit unsigned fraction
#define FIDT_SBYTE      6   // 8-bit signed integer
#define FIDT_UNDEFINED  7   // 8-bit untyped data
#define FIDT_SSHORT     8   // 16-bit signed integer
#define FIDT_SLONG      9   // 32-bit signed integer
#define FIDT_SRATIONAL  10  // 64-bit signed fraction
#define FIDT_FLOAT      11  // 32-bit IEEE floating point
#define FIDT_DOUBLE     12  // 64-bit IEEE floating point
#define FIDT_IFD        13  // 32-bit unsigned integer (offset)
#define FIDT_PALETTE    14  // 32-bit RGBQUAD


/**
  Metadata models supported by FreeImage
*/
//FREE_IMAGE_MDMODEL
#define FIMD_NODATA          -1
#define FIMD_COMMENTS        0   // single comment or keywords
#define FIMD_EXIF_MAIN       1   // Exif-TIFF metadata
#define FIMD_EXIF_EXIF       2   // Exif-specific metadata
#define FIMD_EXIF_GPS        3   // Exif GPS metadata
#define FIMD_EXIF_MAKERNOTE  4   // Exif maker note metadata
#define FIMD_EXIF_INTEROP    5   // Exif interoperability metadata
#define FIMD_IPTC            6   // IPTC/NAA metadata
#define FIMD_XMP             7   // Abobe XMP metadata
#define FIMD_GEOTIFF         8   // GeoTIFF metadata
#define FIMD_ANIMATION       9   // Animation metadata
#define FIMD_CUSTOM          10  // Used to attach other metadata types to a dib

/* Load / Save flag constants */

#define BMP_DEFAULT         0
#define BMP_SAVE_RLE        1
#define CUT_DEFAULT         0
#define DDS_DEFAULT         0
#define GIF_DEFAULT         0
#define GIF_LOAD256         1   // Load the image as a 256 color image with ununsed palette entries, if it's 16 or 2 color
#define GIF_PLAYBACK        2   // 'Play' the GIF to generate each frame (as 32bpp) instead of returning raw frame data when loading
#define HDR_DEFAULT         0
#define ICO_DEFAULT         0
#define ICO_MAKEALPHA       1   // convert to 32bpp and create an alpha channel from the AND-mask when loading
#define IFF_DEFAULT         0
#define JPEG_DEFAULT        0
#define JPEG_FAST           1
#define JPEG_ACCURATE       2
#define JPEG_QUALITYSUPERB  0x80
#define JPEG_QUALITYGOOD    0x100
#define JPEG_QUALITYNORMAL  0x200
#define JPEG_QUALITYAVERAGE 0x400
#define JPEG_QUALITYBAD     0x800
#define JPEG_CMYK           0x1000  // load separated CMYK "as is" (use | to combine with other flags)
#define KOALA_DEFAULT       0
#define LBM_DEFAULT         0
#define MNG_DEFAULT         0
#define PCD_DEFAULT         0
#define PCD_BASE            1   // load the bitmap sized 768 x 512
#define PCD_BASEDIV4        2   // load the bitmap sized 384 x 256
#define PCD_BASEDIV16       3   // load the bitmap sized 192 x 128
#define PCX_DEFAULT         0
#define PNG_DEFAULT         0
#define PNG_IGNOREGAMMA     1   // avoid gamma correction
#define PNM_DEFAULT         0
#define PNM_SAVE_RAW        0       // If set the writer saves in RAW format (i.e. P4, P5 or P6)
#define PNM_SAVE_ASCII      1       // If set the writer saves in ASCII format (i.e. P1, P2 or P3)
#define PSD_DEFAULT         0
#define RAS_DEFAULT         0
#define TARGA_DEFAULT       0
#define TARGA_LOAD_RGB888   1       // If set the loader converts RGB555 and ARGB8888 -> RGB888.
#define TIFF_DEFAULT        0
#define TIFF_CMYK           0x0001  // reads/stores tags for separated CMYK (use | to combine with compression flags)
#define TIFF_PACKBITS       0x0100  // save using PACKBITS compression
#define TIFF_DEFLATE        0x0200  // save using DEFLATE compression (a.k.a. ZLIB compression)
#define TIFF_ADOBE_DEFLATE  0x0400  // save using ADOBE DEFLATE compression
#define TIFF_NONE           0x0800  // save without any compression
#define TIFF_CCITTFAX3      0x1000  // save using CCITT Group 3 fax encoding
#define TIFF_CCITTFAX4      0x2000  // save using CCITT Group 4 fax encoding
#define TIFF_LZW            0x4000  // save using LZW compression
#define TIFF_JPEG           0x8000  // save using JPEG compression
#define WBMP_DEFAULT        0
#define XBM_DEFAULT         0
#define XPM_DEFAULT         0

#endif // FREEIMAGE_CH
