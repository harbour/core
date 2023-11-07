/*
  Configuration defines for installed libtiff.
  This file maintained for backward compatibility. Do not use definitions
  from this file in your programs.
*/

/* clang-format off */
/* clang-format disabled because CMake scripts are very sensitive to the
 * formatting of this file. configure_file variables of type "@VAR@" are
 * modified by clang-format and won't be substituted.
 */

#ifndef _TIFFCONF_
#define _TIFFCONF_

#include "hbdefs.h"
#include "hb_io.h"

#include <inttypes.h> /* For PRIu16 */

/* Signed 16-bit type */
#define TIFF_INT16_T HB_I16

/* Signed 32-bit type */
#define TIFF_INT32_T HB_I32

/* Signed 64-bit type */
#define TIFF_INT64_T HB_I64

/* Signed 8-bit type */
#define TIFF_INT8_T HB_I8

/* Unsigned 16-bit type */
#define TIFF_UINT16_T HB_U16

/* Unsigned 32-bit type */
#define TIFF_UINT32_T HB_U32

/* Unsigned 64-bit type */
#define TIFF_UINT64_T HB_U64

/* Unsigned 8-bit type */
#define TIFF_UINT8_T HB_U8

/* Signed size type */
#define TIFF_SSIZE_T HB_ISIZ

/* Signed size type formatter */
#define TIFF_SSIZE_FORMAT "" HB_PFS "d"

/* file handler */
#define TIFF_FILE_HANDLE HB_FHANDLE

/* Compatibility stuff. */

/* Define as 0 or 1 according to the floating point format supported by the
   machine */
#undef HAVE_IEEEFP

/* The concept of HOST_FILLORDER is broken. Since libtiff 4.5.1
 * this macro will always be hardcoded to FILLORDER_LSB2MSB on all
 * architectures, to reflect past long behavior of doing so on x86 architecture.
 * Note however that the default FillOrder used by libtiff is FILLORDER_MSB2LSB,
 * as mandated per the TIFF specification.
 * The influence of HOST_FILLORDER is only when passing the 'H' mode in
 * TIFFOpen().
 * You should NOT rely on this macro to decide the CPU endianness!
 * This macro will be removed in libtiff 4.6
 */
#define HOST_FILLORDER FILLORDER_LSB2MSB

/* Native cpu byte order: 1 if big-endian (Motorola) or 0 if little-endian
   (Intel) */
#if defined( HB_BIG_ENDIAN )
   #define HOST_BIGENDIAN
#endif

#if 0
/* Support CCITT Group 3 & 4 algorithms */
#undef CCITT_SUPPORT
#endif

#if 0
/* Support JPEG compression (requires IJG JPEG library) */
#undef JPEG_SUPPORT
#endif

/* Support JBIG compression (requires JBIG-KIT library) */
#undef JBIG_SUPPORT

#if 0
/* Support LERC compression */
#undef LERC_SUPPORT
#endif

/* Support LogLuv high dynamic range encoding */
#undef LOGLUV_SUPPORT

#if 0
/* Support LZW algorithm */
#undef LZW_SUPPORT
#endif

#if 0
/* Support NeXT 2-bit RLE algorithm */
#undef NEXT_SUPPORT
#endif

/* Support Old JPEG compresson (read contrib/ojpeg/README first! Compilation
   fails with unpatched IJG JPEG library) */
#undef OJPEG_SUPPORT

/* Support Macintosh PackBits algorithm */
#undef PACKBITS_SUPPORT

/* Support Pixar log-format algorithm (requires Zlib) */
#undef PIXARLOG_SUPPORT

/* Support ThunderScan 4-bit RLE algorithm */
#undef THUNDER_SUPPORT

#if 0
/* Support Deflate compression */
#undef ZIP_SUPPORT
#endif

#if 0
/* Support libdeflate enhanced compression */
#undef LIBDEFLATE_SUPPORT
#endif

/* Support strip chopping (whether or not to convert single-strip uncompressed
   images to multiple strips of ~8Kb to reduce memory usage) */
#undef STRIPCHOP_DEFAULT

/* Enable SubIFD tag (330) support */
#undef SUBIFD_SUPPORT

/* Treat extra sample as alpha (default enabled). The RGBA interface will
   treat a fourth sample with no EXTRASAMPLE_ value as being ASSOCALPHA. Many
   packages produce RGBA files but don't mark the alpha properly. */
#undef DEFAULT_EXTRASAMPLE_AS_ALPHA

/* Pick up YCbCr subsampling info from the JPEG data stream to support files
   lacking the tag (default enabled). */
#undef CHECK_JPEG_YCBCR_SUBSAMPLING

/* Support MS MDI magic number files as TIFF */
#undef MDI_SUPPORT

/*
 * Feature support definitions.
 * XXX: These macros are obsoleted. Don't use them in your apps!
 * Macros stays here for backward compatibility and should be always defined.
 */
#define COLORIMETRY_SUPPORT
#define YCBCR_SUPPORT
#define CMYK_SUPPORT
#define ICC_SUPPORT
#define PHOTOSHOP_SUPPORT
#define IPTC_SUPPORT

#if ! defined( HB_OS_UNIX )
#ifndef O_RDONLY
#define O_RDONLY 0
#endif
#ifndef O_RDWR
#define O_RDWR 2
#endif
#ifndef O_WRONLY
#define O_WRONLY 000001
#endif
#ifndef O_CREAT
#define O_CREAT 000100
#endif
#ifndef O_TRUNC
#define O_TRUNC 001000
#endif
#endif

#endif /* _TIFFCONF_ */

/* clang-format on */
