/*
 * $Id$
 */

* Automatically translated from wingdi.h by hConvert.EXE
* (Copyright PC Wise Technology) AJ Wos (andrwos@global.co.za) 1998-2000
* Fitness for any particular purpose is not guaranteed nor implied.
* It is recommended to verify the correctness of the file before use.

/**************************************************************************
*                                                                         *
* wingdi.h -- GDI procedure declarations, constant definitions and macros *
*                                                                         *
* Copyright (c) 1985-1999, Microsoft Corp. All rights reserved.           *
*                                                                         *
**************************************************************************/


#ifndef _WINGDI_
 //P_O_Push
 #define _WINGDI_


 #ifndef WINVER
  #define WINVER   1280   // version 5.0
 #endif // WINVER

 #ifndef NOGDI

  #ifndef NORASTEROPS

   // Binary raster ops
   #define R2_BLACK            1   //  0
   #define R2_NOTMERGEPEN      2   // DPon
   #define R2_MASKNOTPEN       3   // DPna
   #define R2_NOTCOPYPEN       4   // PN
   #define R2_MASKPENNOT       5   // PDna
   #define R2_NOT              6   // Dn
   #define R2_XORPEN           7   // DPx
   #define R2_NOTMASKPEN       8   // DPan
   #define R2_MASKPEN          9   // DPa
   #define R2_NOTXORPEN        10  // DPxn
   #define R2_NOP              11  // D
   #define R2_MERGENOTPEN      12  // DPno
   #define R2_COPYPEN          13  // P
   #define R2_MERGEPENNOT      14  // PDno
   #define R2_MERGEPEN         15  // DPo
   #define R2_WHITE            16  //  1
   #define R2_LAST             16

   // Ternary raster operations
   #define SRCCOPY               13369376 // dest = source
   #define SRCPAINT              15597702 // dest = source OR dest
   #define SRCAND                 8913094 // dest = source AND dest
   #define SRCINVERT              6684742 // dest = source XOR dest
   #define SRCERASE               4457256 // dest = source AND (NOT dest )
   #define NOTSRCCOPY             3342344 // dest = (NOT source)
   #define NOTSRCERASE            1114278 // dest = (NOT src) AND (NOT dest)
   #define MERGECOPY             12583114 // dest = (source AND pattern)
   #define MERGEPAINT            12255782 // dest = (NOT source) OR dest
   #define PATCOPY               15728673 // dest = pattern
   #define PATPAINT              16452105 // dest = DPSnoo
   #define PATINVERT              5898313 // dest = pattern XOR dest
   #define DSTINVERT              5570569 // dest = (NOT dest)
   #define BLACKNESS                   66 // dest = BLACK
   #define WHITENESS             16711778 // dest = WHITE
   #define NOMIRRORBITMAP       2147483648 // Do not Mirror the bitmap in this call
   #define CAPTUREBLT           1073741824 // Include layered windows

   // Quaternary raster codes
   //#define MAKEROP4(fore,back) (DWORD)((((back) << 8) * 4278190080) + (fore))

  #endif // NORASTEROPS

  #define GDI_ERROR  ( 4294967295)
  #define HGDI_ERROR ( 4294967295)

  // Region Flags
  #define ERROR               0
  #define NULLREGION          1
  #define SIMPLEREGION        2
  #define COMPLEXREGION       3
  #define RGN_ERROR ERROR

  // CombineRgn() Styles
  #define RGN_AND             1
  #define RGN_OR              2
  #define RGN_XOR             3
  #define RGN_DIFF            4
  #define RGN_COPY            5
  #define RGN_MIN             RGN_AND
  #define RGN_MAX             RGN_COPY

  // StretchBlt() Modes
  #define BLACKONWHITE                 1
  #define WHITEONBLACK                 2
  #define COLORONCOLOR                 3
  #define HALFTONE                     4
  #define MAXSTRETCHBLTMODE            4

  // New StretchBlt() Modes
  #define STRETCH_ANDSCANS    BLACKONWHITE
  #define STRETCH_ORSCANS     WHITEONBLACK
  #define STRETCH_DELETESCANS COLORONCOLOR
  #define STRETCH_HALFTONE    HALFTONE

  // PolyFill() Modes
  #define ALTERNATE                    1
  #define WINDING                      2
  #define POLYFILL_LAST                2

  // Layout Orientation Options
  #define LAYOUT_RTL                                  1 // Right to left
  #define LAYOUT_BTT                                  2 // Bottom to top
  #define LAYOUT_VBH                                  4 // Vertical before horizontal
  #define LAYOUT_ORIENTATIONMASK             (LAYOUT_RTL + LAYOUT_BTT + LAYOUT_VBH)
  #define LAYOUT_BITMAPORIENTATIONPRESERVED           8

  // Text Alignment Options
  #define TA_NOUPDATECP                0
  #define TA_UPDATECP                  1

  #define TA_LEFT                      0
  #define TA_RIGHT                     2
  #define TA_CENTER                    6

  #define TA_TOP                       0
  #define TA_BOTTOM                    8
  #define TA_BASELINE                  24
  #define TA_RTLREADING                256
  #define TA_MASK       (TA_BASELINE+TA_CENTER+TA_UPDATECP+TA_RTLREADING)
  #define VTA_BASELINE TA_BASELINE
  #define VTA_LEFT     TA_BOTTOM
  #define VTA_RIGHT    TA_TOP
  #define VTA_CENTER   TA_CENTER
  #define VTA_BOTTOM   TA_RIGHT
  #define VTA_TOP      TA_LEFT

  #define ETO_OPAQUE                        2
  #define ETO_CLIPPED                       4
  #define ETO_GLYPH_INDEX                  16
  #define ETO_RTLREADING                  128
  #define ETO_NUMERICSLOCAL              1024
  #define ETO_NUMERICSLATIN              2048
  #define ETO_IGNORELANGUAGE             4096
  #define ETO_PDY                        8192
  #define ASPECT_FILTERING                  1

  // Bounds Accumulation APIs

  #define DCB_RESET            1
  #define DCB_ACCUMULATE       2
  #define DCB_DIRTY       DCB_ACCUMULATE
  #define DCB_SET         (DCB_RESET + DCB_ACCUMULATE)
  #define DCB_ENABLE           4
  #define DCB_DISABLE          8

  #ifndef NOMETAFILE

   // Metafile Functions
   #define META_SETBKCOLOR                 513
   #define META_SETBKMODE                  258
   #define META_SETMAPMODE                 259
   #define META_SETROP2                    260
   #define META_SETRELABS                  261
   #define META_SETPOLYFILLMODE            262
   #define META_SETSTRETCHBLTMODE          263
   #define META_SETTEXTCHAREXTRA           264
   #define META_SETTEXTCOLOR               521
   #define META_SETTEXTJUSTIFICATION       522
   #define META_SETWINDOWORG               523
   #define META_SETWINDOWEXT               524
   #define META_SETVIEWPORTORG             525
   #define META_SETVIEWPORTEXT             526
   #define META_OFFSETWINDOWORG            527
   #define META_SCALEWINDOWEXT            1040
   #define META_OFFSETVIEWPORTORG          529
   #define META_SCALEVIEWPORTEXT          1042
   #define META_LINETO                     531
   #define META_MOVETO                     532
   #define META_EXCLUDECLIPRECT           1045
   #define META_INTERSECTCLIPRECT         1046
   #define META_ARC                       2071
   #define META_ELLIPSE                   1048
   #define META_FLOODFILL                 1049
   #define META_PIE                       2074
   #define META_RECTANGLE                 1051
   #define META_ROUNDRECT                 1564
   #define META_PATBLT                    1565
   #define META_SAVEDC                      30
   #define META_SETPIXEL                  1055
   #define META_OFFSETCLIPRGN              544
   #define META_TEXTOUT                   1313
   #define META_BITBLT                    2338
   #define META_STRETCHBLT                2851
   #define META_POLYGON                    804
   #define META_POLYLINE                   805
   #define META_ESCAPE                    1574
   #define META_RESTOREDC                  295
   #define META_FILLREGION                 552
   #define META_FRAMEREGION               1065
   #define META_INVERTREGION               298
   #define META_PAINTREGION                299
   #define META_SELECTCLIPREGION           300
   #define META_SELECTOBJECT               301
   #define META_SETTEXTALIGN               302
   #define META_CHORD                     2096
   #define META_SETMAPPERFLAGS             561
   #define META_EXTTEXTOUT                2610
   #define META_SETDIBTODEV               3379
   #define META_SELECTPALETTE              564
   #define META_REALIZEPALETTE              53
   #define META_ANIMATEPALETTE            1078
   #define META_SETPALENTRIES               55
   #define META_POLYPOLYGON               1336
   #define META_RESIZEPALETTE              313
   #define META_DIBBITBLT                 2368
   #define META_DIBSTRETCHBLT             2881
   #define META_DIBCREATEPATTERNBRUSH      322
   #define META_STRETCHDIB                3907
   #define META_EXTFLOODFILL              1352
   #define META_SETLAYOUT                  329
   #define META_DELETEOBJECT               496
   #define META_CREATEPALETTE              247
   #define META_CREATEPATTERNBRUSH         505
   #define META_CREATEPENINDIRECT          762
   #define META_CREATEFONTINDIRECT         763
   #define META_CREATEBRUSHINDIRECT        764
   #define META_CREATEREGION              1791

  #endif // NOMETAFILE

  // GDI Escapes
  #define NEWFRAME                     1
  #define ABORTDOC                     2
  #define NEXTBAND                     3
  #define SETCOLORTABLE                4
  #define GETCOLORTABLE                5
  #define FLUSHOUTPUT                  6
  #define DRAFTMODE                    7
  #define QUERYESCSUPPORT              8
  #define SETABORTPROC                 9
  #define STARTDOC                     10
  #define ENDDOC                       11
  #define GETPHYSPAGESIZE              12
  #define GETPRINTINGOFFSET            13
  #define GETSCALINGFACTOR             14
  #define MFCOMMENT                    15
  #define GETPENWIDTH                  16
  #define SETCOPYCOUNT                 17
  #define SELECTPAPERSOURCE            18
  #define DEVICEDATA                   19
  #define PASSTHROUGH                  19
  #define GETTECHNOLGY                 20
  #define GETTECHNOLOGY                20
  #define SETLINECAP                   21
  #define SETLINEJOIN                  22
  #define SETMITERLIMIT                23
  #define BANDINFO                     24
  #define DRAWPATTERNRECT              25
  #define GETVECTORPENSIZE             26
  #define GETVECTORBRUSHSIZE           27
  #define ENABLEDUPLEX                 28
  #define GETSETPAPERBINS              29
  #define GETSETPRINTORIENT            30
  #define ENUMPAPERBINS                31
  #define SETDIBSCALING                32
  #define EPSPRINTING                  33
  #define ENUMPAPERMETRICS             34
  #define GETSETPAPERMETRICS           35
  #define POSTSCRIPT_DATA              37
  #define POSTSCRIPT_IGNORE            38
  #define MOUSETRAILS                  39
  #define GETDEVICEUNITS               42

  #define GETEXTENDEDTEXTMETRICS       256
  #define GETEXTENTTABLE               257
  #define GETPAIRKERNTABLE             258
  #define GETTRACKKERNTABLE            259
  #define EXTTEXTOUT                   512
  #define GETFACENAME                  513
  #define DOWNLOADFACE                 514
  #define ENABLERELATIVEWIDTHS         768
  #define ENABLEPAIRKERNING            769
  #define SETKERNTRACK                 770
  #define SETALLJUSTVALUES             771
  #define SETCHARSET                   772

  #define STRETCHBLT                   2048
  #define METAFILE_DRIVER              2049
  #define GETSETSCREENPARAMS           3072
  #define QUERYDIBSUPPORT              3073
  #define BEGIN_PATH                   4096
  #define CLIP_TO_PATH                 4097
  #define END_PATH                     4098
  #define EXT_DEVICE_CAPS              4099
  #define RESTORE_CTM                  4100
  #define SAVE_CTM                     4101
  #define SET_ARC_DIRECTION            4102
  #define SET_BACKGROUND_COLOR         4103
  #define SET_POLY_MODE                4104
  #define SET_SCREEN_ANGLE             4105
  #define SET_SPREAD                   4106
  #define TRANSFORM_CTM                4107
  #define SET_CLIP_BOX                 4108
  #define SET_BOUNDS                   4109
  #define SET_MIRROR_MODE              4110
  #define OPENCHANNEL                  4110
  #define DOWNLOADHEADER               4111
  #define CLOSECHANNEL                 4112
  #define POSTSCRIPT_PASSTHROUGH       4115
  #define ENCAPSULATED_POSTSCRIPT      4116

  #define POSTSCRIPT_IDENTIFY          4117   // new escape for NT5 pscript driver
  #define POSTSCRIPT_INJECTION         4118   // new escape for NT5 pscript driver

  #define CHECKJPEGFORMAT              4119
  #define CHECKPNGFORMAT               4120

  #define GET_PS_FEATURESETTING        4121   // new escape for NT5 pscript driver

  #define SPCLPASSTHROUGH2             4568   // new escape for NT5 pscript driver

  /*
   * Parameters for POSTSCRIPT_IDENTIFY escape
   */

  #define PSIDENT_GDICENTRIC    0
  #define PSIDENT_PSCENTRIC     1

  /*
   * Constants for PSINJECTDATA.InjectionPoint field
   */

  #define PSINJECT_BEGINSTREAM                1
  #define PSINJECT_PSADOBE                    2
  #define PSINJECT_PAGESATEND                 3
  #define PSINJECT_PAGES                      4

  #define PSINJECT_DOCNEEDEDRES               5
  #define PSINJECT_DOCSUPPLIEDRES             6
  #define PSINJECT_PAGEORDER                  7
  #define PSINJECT_ORIENTATION                8
  #define PSINJECT_BOUNDINGBOX                9
  #define PSINJECT_DOCUMENTPROCESSCOLORS      10

  #define PSINJECT_COMMENTS                   11
  #define PSINJECT_BEGINDEFAULTS              12
  #define PSINJECT_ENDDEFAULTS                13
  #define PSINJECT_BEGINPROLOG                14
  #define PSINJECT_ENDPROLOG                  15
  #define PSINJECT_BEGINSETUP                 16
  #define PSINJECT_ENDSETUP                   17
  #define PSINJECT_TRAILER                    18
  #define PSINJECT_EOF                        19
  #define PSINJECT_ENDSTREAM                  20
  #define PSINJECT_DOCUMENTPROCESSCOLORSATEND 21

  #define PSINJECT_PAGENUMBER                 100
  #define PSINJECT_BEGINPAGESETUP             101
  #define PSINJECT_ENDPAGESETUP               102
  #define PSINJECT_PAGETRAILER                103
  #define PSINJECT_PLATECOLOR                 104

  #define PSINJECT_SHOWPAGE                   105
  #define PSINJECT_PAGEBBOX                   106
  #define PSINJECT_ENDPAGECOMMENTS            107

  #define PSINJECT_VMSAVE                     200
  #define PSINJECT_VMRESTORE                  201

  /*
   * Parameter for GET_PS_FEATURESETTING escape
   */

  #define FEATURESETTING_NUP         0
  #define FEATURESETTING_OUTPUT      1
  #define FEATURESETTING_PSLEVEL     2
  #define FEATURESETTING_CUSTPAPER   3
  #define FEATURESETTING_MIRROR      4
  #define FEATURESETTING_NEGATIVE    5
  #define FEATURESETTING_PROTOCOL    6

  /*
   * Information about output options
   */


  /*
   * Information about custom paper size
   */


  // Value returned for FEATURESETTING_PROTOCOL
  #define PSPROTOCOL_ASCII             0
  #define PSPROTOCOL_BCP               1
  #define PSPROTOCOL_TBCP              2
  #define PSPROTOCOL_BINARY            3

  // Flag returned from QUERYDIBSUPPORT
  #define QDI_SETDIBITS                1
  #define QDI_GETDIBITS                2
  #define QDI_DIBTOSCREEN              4
  #define QDI_STRETCHDIB               8

  // Spooler Error Codes
  #define SP_NOTREPORTED                16384
  #define SP_ERROR                     (-1)
  #define SP_APPABORT                  (-2)
  #define SP_USERABORT                 (-3)
  #define SP_OUTOFDISK                 (-4)
  #define SP_OUTOFMEMORY               (-5)

  #define PR_JOBSTATUS                      0

  // Object Definitions for EnumObjects()
  #define OBJ_PEN             1
  #define OBJ_BRUSH           2
  #define OBJ_DC              3
  #define OBJ_METADC          4
  #define OBJ_PAL             5
  #define OBJ_FONT            6
  #define OBJ_BITMAP          7
  #define OBJ_REGION          8
  #define OBJ_METAFILE        9
  #define OBJ_MEMDC           10
  #define OBJ_EXTPEN          11
  #define OBJ_ENHMETADC       12
  #define OBJ_ENHMETAFILE     13
  #define OBJ_COLORSPACE      14

  // xform stuff
  #define MWT_IDENTITY        1
  #define MWT_LEFTMULTIPLY    2
  #define MWT_RIGHTMULTIPLY   3

  #define MWT_MIN             MWT_IDENTITY
  #define MWT_MAX             MWT_RIGHTMULTIPLY

  #define _XFORM_


  // Bitmap Header Definition


  //#include "pshpack1.ch"


  //#include "poppack.ch"

  // Image Color Matching color definitions

  #define CS_ENABLE                                 1
  #define CS_DISABLE                                2
  #define CS_DELETE_TRANSFORM                       3

  // Logcolorspace signature

  #define LCS_SIGNATURE           'PSOC'

  // Logcolorspace lcsType values

  #define LCS_sRGB                'sRGB'
  #define LCS_WINDOWS_COLOR_SPACE 'Win '  // Windows default color space


  #define LCS_CALIBRATED_RGB                        0


  #define LCS_GM_BUSINESS                           1
  #define LCS_GM_GRAPHICS                           2
  #define LCS_GM_IMAGES                             4
  #define LCS_GM_ABS_COLORIMETRIC                   8

  // ICM Defines for results from CheckColorInGamut()
  #define CM_OUT_OF_GAMUT                 255
  #define CM_IN_GAMUT                     0

  // UpdateICMRegKey Constants
  #define ICM_ADDPROFILE                  1
  #define ICM_DELETEPROFILE               2
  #define ICM_QUERYPROFILE                3
  #define ICM_SETDEFAULTPROFILE           4
  #define ICM_REGISTERICMATCHER           5
  #define ICM_UNREGISTERICMATCHER         6
  #define ICM_QUERYMATCH                  7


  // Macros to retrieve CMYK values from a COLORREF
  //#define GetKValue(cmyk)      ((BYTE)(cmyk))
  //#define GetYValue(cmyk)      ((BYTE)((cmyk)>> 8))
  //#define GetMValue(cmyk)      ((BYTE)((cmyk)>>16))
  //#define GetCValue(cmyk)      ((BYTE)((cmyk)>>24))

  //#define CMYK(c,m,y,k)       ((COLORREF)((((BYTE)(k)|((WORD)((BYTE)(y))<<8))|(((DWORD)(BYTE)(m))<<16))|(((DWORD)(BYTE)(c))<<24)))


  // Values for bV5CSType
  #define PROFILE_LINKED          'LINK'
  #define PROFILE_EMBEDDED        'MBED'

  // constants for the biCompression field
  #define BI_RGB        0L
  #define BI_RLE8       1L
  #define BI_RLE4       2L
  #define BI_BITFIELDS  3L
  #define BI_JPEG       4L
  #define BI_PNG        5L

  //#define MAKEPOINTS(l)       (*((POINTS FAR *)&(l)))

  #ifndef NOFONTSIG

   #define TCI_SRCCHARSET  1
   #define TCI_SRCCODEPAGE 2
   #define TCI_SRCFONTSIG  3

  #endif

  #ifndef NOMETAFILE

   // Clipboard Metafile Picture Structure

   // Enhanced Metafile structures
   #ifndef NOTEXTMETRIC

    // tmPitchAndFamily flags
    #define TMPF_FIXED_PITCH       1
    #define TMPF_VECTOR                2
    #define TMPF_DEVICE                8
    #define TMPF_TRUETYPE          4

    #ifndef _TEXTMETRIC_DEFINED
     #define _TEXTMETRIC_DEFINED
    #endif // !_TEXTMETRIC_DEFINED

    // ntmFlags field flags
    #define NTM_REGULAR              64
    #define NTM_BOLD                 32
    #define NTM_ITALIC                1

    // new in NT 5.0

    #define NTM_NONNEGATIVE_AC       65536
    #define NTM_PS_OPENTYPE         131072
    #define NTM_TT_OPENTYPE         262144
    #define NTM_MULTIPLEMASTER      524288
    #define NTM_TYPE1              1048576
    #define NTM_DSIG               2097152

   #endif // NOTEXTMETRIC


   #ifndef _PALETTEENTRY_DEFINED
     #define _PALETTEENTRY_DEFINED
   #endif // !_PALETTEENTRY_DEFINED

   #ifndef _LOGPALETTE_DEFINED
    #define _LOGPALETTE_DEFINED
   #endif // !_LOGPALETTE_DEFINED

   // Logical Font
   #define LF_FACESIZE         32
   #define LF_FULLFACESIZE     64

   #define OUT_DEFAULT_PRECIS          0
   #define OUT_STRING_PRECIS           1
   #define OUT_CHARACTER_PRECIS        2
   #define OUT_STROKE_PRECIS           3
   #define OUT_TT_PRECIS               4
   #define OUT_DEVICE_PRECIS           5
   #define OUT_RASTER_PRECIS           6
   #define OUT_TT_ONLY_PRECIS          7
   #define OUT_OUTLINE_PRECIS          8
   #define OUT_SCREEN_OUTLINE_PRECIS   9
   #define OUT_PS_ONLY_PRECIS          10

   #define CLIP_DEFAULT_PRECIS     0
   #define CLIP_CHARACTER_PRECIS   1
   #define CLIP_STROKE_PRECIS      2
   #define CLIP_MASK                15
   //#define CLIP_LH_ANGLES          (1<<4)
   //#define CLIP_TT_ALWAYS          (2<<4)
   //#define CLIP_EMBEDDED           (8<<4)

   #define DEFAULT_QUALITY         0
   #define DRAFT_QUALITY           1
   #define PROOF_QUALITY           2
   #define NONANTIALIASED_QUALITY  3
   #define ANTIALIASED_QUALITY     4
   #define DEFAULT_PITCH           0
   #define FIXED_PITCH             1
   #define VARIABLE_PITCH          2
   #define MONO_FONT               8
   #define ANSI_CHARSET            0
   #define DEFAULT_CHARSET         1
   #define SYMBOL_CHARSET          2
   #define SHIFTJIS_CHARSET        128
   #define HANGEUL_CHARSET         129
   #define HANGUL_CHARSET          129
   #define GB2312_CHARSET          134
   #define CHINESEBIG5_CHARSET     136
   #define OEM_CHARSET             255
   #define JOHAB_CHARSET           130
   #define HEBREW_CHARSET          177
   #define ARABIC_CHARSET          178
   #define GREEK_CHARSET           161
   #define TURKISH_CHARSET         162
   #define VIETNAMESE_CHARSET      163
   #define THAI_CHARSET            222
   #define EASTEUROPE_CHARSET      238
   #define RUSSIAN_CHARSET         204

   #define MAC_CHARSET             77
   #define BALTIC_CHARSET          186

   #define FS_LATIN1                         1
   #define FS_LATIN2                         2
   #define FS_CYRILLIC                       4
   #define FS_GREEK                          8
   #define FS_TURKISH                       16
   #define FS_HEBREW                        32
   #define FS_ARABIC                        64
   #define FS_BALTIC                       128
   #define FS_VIETNAMESE                   256
   #define FS_THAI                       65536
   #define FS_JISJAPAN                  131072
   #define FS_CHINESESIMP               262144
   #define FS_WANSUNG                   524288
   #define FS_CHINESETRAD              1048576
   #define FS_JOHAB                    2097152
   #define FS_SYMBOL                2147483648

   // Font Families
   #define FF_DONTCARE         0  // Don't care or don't know.
   #define FF_ROMAN           16  // Variable stroke width, serifed.
                                    // Times Roman, Century Schoolbook, etc.
   #define FF_SWISS           32  // Variable stroke width, sans-serifed.
                                    // Helvetica, Swiss, etc.
   #define FF_MODERN          48  // Constant stroke width, serifed or sans-serifed.
                                    // Pica, Elite, Courier, etc.
   #define FF_SCRIPT          64  // Cursive, etc.
   #define FF_DECORATIVE      80  // Old English, etc.

   // Font Weights
   #define FW_DONTCARE         0
   #define FW_THIN             100
   #define FW_EXTRALIGHT       200
   #define FW_LIGHT            300
   #define FW_NORMAL           400
   #define FW_MEDIUM           500
   #define FW_SEMIBOLD         600
   #define FW_BOLD             700
   #define FW_EXTRABOLD        800
   #define FW_HEAVY            900

   #define FW_ULTRALIGHT       FW_EXTRALIGHT
   #define FW_REGULAR          FW_NORMAL
   #define FW_DEMIBOLD         FW_SEMIBOLD
   #define FW_ULTRABOLD        FW_EXTRABOLD
   #define FW_BLACK            FW_HEAVY

   #define PANOSE_COUNT               10
   #define PAN_FAMILYTYPE_INDEX        0
   #define PAN_SERIFSTYLE_INDEX        1
   #define PAN_WEIGHT_INDEX            2
   #define PAN_PROPORTION_INDEX        3
   #define PAN_CONTRAST_INDEX          4
   #define PAN_STROKEVARIATION_INDEX   5
   #define PAN_ARMSTYLE_INDEX          6
   #define PAN_LETTERFORM_INDEX        7
   #define PAN_MIDLINE_INDEX           8
   #define PAN_XHEIGHT_INDEX           9

   #define PAN_CULTURE_LATIN           0


   #define PAN_ANY                         0 // Any
   #define PAN_NO_FIT                      1 // No Fit

   #define PAN_FAMILY_TEXT_DISPLAY         2 // Text and Display
   #define PAN_FAMILY_SCRIPT               3 // Script
   #define PAN_FAMILY_DECORATIVE           4 // Decorative
   #define PAN_FAMILY_PICTORIAL            5 // Pictorial

   #define PAN_SERIF_COVE                  2 // Cove
   #define PAN_SERIF_OBTUSE_COVE           3 // Obtuse Cove
   #define PAN_SERIF_SQUARE_COVE           4 // Square Cove
   #define PAN_SERIF_OBTUSE_SQUARE_COVE    5 // Obtuse Square Cove
   #define PAN_SERIF_SQUARE                6 // Square
   #define PAN_SERIF_THIN                  7 // Thin
   #define PAN_SERIF_BONE                  8 // Bone
   #define PAN_SERIF_EXAGGERATED           9 // Exaggerated
   #define PAN_SERIF_TRIANGLE             10 // Triangle
   #define PAN_SERIF_NORMAL_SANS          11 // Normal Sans
   #define PAN_SERIF_OBTUSE_SANS          12 // Obtuse Sans
   #define PAN_SERIF_PERP_SANS            13 // Prep Sans
   #define PAN_SERIF_FLARED               14 // Flared
   #define PAN_SERIF_ROUNDED              15 // Rounded

   #define PAN_WEIGHT_VERY_LIGHT           2 // Very Light
   #define PAN_WEIGHT_LIGHT                3 // Light
   #define PAN_WEIGHT_THIN                 4 // Thin
   #define PAN_WEIGHT_BOOK                 5 // Book
   #define PAN_WEIGHT_MEDIUM               6 // Medium
   #define PAN_WEIGHT_DEMI                 7 // Demi
   #define PAN_WEIGHT_BOLD                 8 // Bold
   #define PAN_WEIGHT_HEAVY                9 // Heavy
   #define PAN_WEIGHT_BLACK               10 // Black
   #define PAN_WEIGHT_NORD                11 // Nord

   #define PAN_PROP_OLD_STYLE              2 // Old Style
   #define PAN_PROP_MODERN                 3 // Modern
   #define PAN_PROP_EVEN_WIDTH             4 // Even Width
   #define PAN_PROP_EXPANDED               5 // Expanded
   #define PAN_PROP_CONDENSED              6 // Condensed
   #define PAN_PROP_VERY_EXPANDED          7 // Very Expanded
   #define PAN_PROP_VERY_CONDENSED         8 // Very Condensed
   #define PAN_PROP_MONOSPACED             9 // Monospaced

   #define PAN_CONTRAST_NONE               2 // None
   #define PAN_CONTRAST_VERY_LOW           3 // Very Low
   #define PAN_CONTRAST_LOW                4 // Low
   #define PAN_CONTRAST_MEDIUM_LOW         5 // Medium Low
   #define PAN_CONTRAST_MEDIUM             6 // Medium
   #define PAN_CONTRAST_MEDIUM_HIGH        7 // Mediim High
   #define PAN_CONTRAST_HIGH               8 // High
   #define PAN_CONTRAST_VERY_HIGH          9 // Very High

   #define PAN_STROKE_GRADUAL_DIAG         2 // Gradual/Diagonal
   #define PAN_STROKE_GRADUAL_TRAN         3 // Gradual/Transitional
   #define PAN_STROKE_GRADUAL_VERT         4 // Gradual/Vertical
   #define PAN_STROKE_GRADUAL_HORZ         5 // Gradual/Horizontal
   #define PAN_STROKE_RAPID_VERT           6 // Rapid/Vertical
   #define PAN_STROKE_RAPID_HORZ           7 // Rapid/Horizontal
   #define PAN_STROKE_INSTANT_VERT         8 // Instant/Vertical

   #define PAN_STRAIGHT_ARMS_HORZ          2 // Straight Arms/Horizontal
   #define PAN_STRAIGHT_ARMS_WEDGE         3 // Straight Arms/Wedge
   #define PAN_STRAIGHT_ARMS_VERT          4 // Straight Arms/Vertical
   #define PAN_STRAIGHT_ARMS_SINGLE_SERIF  5 // Straight Arms/Single-Serif
   #define PAN_STRAIGHT_ARMS_DOUBLE_SERIF  6 // Straight Arms/Double-Serif
   #define PAN_BENT_ARMS_HORZ              7 // Non-Straight Arms/Horizontal
   #define PAN_BENT_ARMS_WEDGE             8 // Non-Straight Arms/Wedge
   #define PAN_BENT_ARMS_VERT              9 // Non-Straight Arms/Vertical
   #define PAN_BENT_ARMS_SINGLE_SERIF     10 // Non-Straight Arms/Single-Serif
   #define PAN_BENT_ARMS_DOUBLE_SERIF     11 // Non-Straight Arms/Double-Serif

   #define PAN_LETT_NORMAL_CONTACT         2 // Normal/Contact
   #define PAN_LETT_NORMAL_WEIGHTED        3 // Normal/Weighted
   #define PAN_LETT_NORMAL_BOXED           4 // Normal/Boxed
   #define PAN_LETT_NORMAL_FLATTENED       5 // Normal/Flattened
   #define PAN_LETT_NORMAL_ROUNDED         6 // Normal/Rounded
   #define PAN_LETT_NORMAL_OFF_CENTER      7 // Normal/Off Center
   #define PAN_LETT_NORMAL_SQUARE          8 // Normal/Square
   #define PAN_LETT_OBLIQUE_CONTACT        9 // Oblique/Contact
   #define PAN_LETT_OBLIQUE_WEIGHTED      10 // Oblique/Weighted
   #define PAN_LETT_OBLIQUE_BOXED         11 // Oblique/Boxed
   #define PAN_LETT_OBLIQUE_FLATTENED     12 // Oblique/Flattened
   #define PAN_LETT_OBLIQUE_ROUNDED       13 // Oblique/Rounded
   #define PAN_LETT_OBLIQUE_OFF_CENTER    14 // Oblique/Off Center
   #define PAN_LETT_OBLIQUE_SQUARE        15 // Oblique/Square

   #define PAN_MIDLINE_STANDARD_TRIMMED    2 // Standard/Trimmed
   #define PAN_MIDLINE_STANDARD_POINTED    3 // Standard/Pointed
   #define PAN_MIDLINE_STANDARD_SERIFED    4 // Standard/Serifed
   #define PAN_MIDLINE_HIGH_TRIMMED        5 // High/Trimmed
   #define PAN_MIDLINE_HIGH_POINTED        6 // High/Pointed
   #define PAN_MIDLINE_HIGH_SERIFED        7 // High/Serifed
   #define PAN_MIDLINE_CONSTANT_TRIMMED    8 // Constant/Trimmed
   #define PAN_MIDLINE_CONSTANT_POINTED    9 // Constant/Pointed
   #define PAN_MIDLINE_CONSTANT_SERIFED   10 // Constant/Serifed
   #define PAN_MIDLINE_LOW_TRIMMED        11 // Low/Trimmed
   #define PAN_MIDLINE_LOW_POINTED        12 // Low/Pointed
   #define PAN_MIDLINE_LOW_SERIFED        13 // Low/Serifed

   #define PAN_XHEIGHT_CONSTANT_SMALL      2 // Constant/Small
   #define PAN_XHEIGHT_CONSTANT_STD        3 // Constant/Standard
   #define PAN_XHEIGHT_CONSTANT_LARGE      4 // Constant/Large
   #define PAN_XHEIGHT_DUCKING_SMALL       5 // Ducking/Small
   #define PAN_XHEIGHT_DUCKING_STD         6 // Ducking/Standard
   #define PAN_XHEIGHT_DUCKING_LARGE       7 // Ducking/Large


   #define ELF_VENDOR_SIZE     4

   #define ELF_VERSION         0
   #define ELF_CULTURE_LATIN   0

   // EnumFonts Masks
   #define RASTER_FONTTYPE          1
   #define DEVICE_FONTTYPE         2
   #define TRUETYPE_FONTTYPE       4

   //#define RGB(r,g,b)          ((COLORREF)(((BYTE)(r)|((WORD)((BYTE)(g))<<8))|(((DWORD)(BYTE)(b))<<16)))
   #define PALETTERGB(r,g,b)   (  33554432 + RGB(r,g,b))
   //#define PALETTEINDEX(i)     ((COLORREF)(  16777216 + (DWORD)(WORD)(i)))

   // palette entry flags

   #define PC_RESERVED        1    // palette index used for animation
   #define PC_EXPLICIT        2    // palette index is explicit to device
   #define PC_NOCOLLAPSE      4    // do not match color to system palette

   //#define GetRValue(rgb)      ((BYTE)(rgb))
   //#define GetGValue(rgb)      ((BYTE)(((WORD)(rgb)) >> 8))
   //#define GetBValue(rgb)      ((BYTE)((rgb)>>16))

   // Background Modes
   #define TRANSPARENT         1
   #define OPAQUE              2
   #define BKMODE_LAST         2

   // Graphics Modes

   #define GM_COMPATIBLE       1
   #define GM_ADVANCED         2
   #define GM_LAST             2

   // PolyDraw and GetPath point types
   #define PT_CLOSEFIGURE         1
   #define PT_LINETO              2
   #define PT_BEZIERTO            4
   #define PT_MOVETO              6

   // Mapping Modes
   #define MM_TEXT             1
   #define MM_LOMETRIC         2
   #define MM_HIMETRIC         3
   #define MM_LOENGLISH        4
   #define MM_HIENGLISH        5
   #define MM_TWIPS            6
   #define MM_ISOTROPIC        7
   #define MM_ANISOTROPIC      8

   // Min and Max Mapping Mode values
   #define MM_MIN              MM_TEXT
   #define MM_MAX              MM_ANISOTROPIC
   #define MM_MAX_FIXEDSCALE   MM_TWIPS

   // Coordinate Modes
   #define ABSOLUTE            1
   #define RELATIVE            2

   // Stock Logical Objects
   #define WHITE_BRUSH         0
   #define LTGRAY_BRUSH        1
   #define GRAY_BRUSH          2
   #define DKGRAY_BRUSH        3
   #define BLACK_BRUSH         4
   #define NULL_BRUSH          5
   #define HOLLOW_BRUSH        NULL_BRUSH
   #define WHITE_PEN           6
   #define BLACK_PEN           7
   #define NULL_PEN            8
   #define OEM_FIXED_FONT      10
   #define ANSI_FIXED_FONT     11
   #define ANSI_VAR_FONT       12
   #define SYSTEM_FONT         13
   #define DEVICE_DEFAULT_FONT 14
   #define DEFAULT_PALETTE     15
   #define SYSTEM_FIXED_FONT   16
   #define DEFAULT_GUI_FONT    17

   #define DC_BRUSH            18
   #define DC_PEN              19
   #define STOCK_LAST          17
   #define CLR_INVALID     4294967295

   // Brush Styles
   #define BS_SOLID            0
   #define BS_NULL             1
   #define BS_HOLLOW           BS_NULL
   #define BS_HATCHED          2
   #define BS_PATTERN          3
   #define BS_INDEXED          4
   #define BS_DIBPATTERN       5
   #define BS_DIBPATTERNPT     6
   #define BS_PATTERN8X8       7
   #define BS_DIBPATTERN8X8    8
   #define BS_MONOPATTERN      9

   // Hatch Styles
   #define HS_HORIZONTAL       0       // -----
   #define HS_VERTICAL         1       // |||||
   #define HS_FDIAGONAL        2       // \\\\\
   #define HS_BDIAGONAL        3       // /////
   #define HS_CROSS            4       // +++++
   #define HS_DIAGCROSS        5       // xxxxx

   // Pen Styles
   #define PS_SOLID            0
   #define PS_DASH             1       // -------
   #define PS_DOT              2       // .......
   #define PS_DASHDOT          3       // _._._._
   #define PS_DASHDOTDOT       4       // _.._.._
   #define PS_NULL             5
   #define PS_INSIDEFRAME      6
   #define PS_USERSTYLE        7
   #define PS_ALTERNATE        8
   #define PS_STYLE_MASK               15

   #define PS_ENDCAP_ROUND              0
   #define PS_ENDCAP_SQUARE           256
   #define PS_ENDCAP_FLAT             512
   #define PS_ENDCAP_MASK            3840

   #define PS_JOIN_ROUND                0
   #define PS_JOIN_BEVEL             4096
   #define PS_JOIN_MITER             8192
   #define PS_JOIN_MASK             61440

   #define PS_COSMETIC                  0
   #define PS_GEOMETRIC             65536
   #define PS_TYPE_MASK            983040

   #define AD_COUNTERCLOCKWISE 1
   #define AD_CLOCKWISE        2

   // Device Parameters for GetDeviceCaps()
   #define DRIVERVERSION 0     // Device driver version
   #define TECHNOLOGY    2     // Device classification
   #define HORZSIZE      4     // Horizontal size in millimeters
   #define VERTSIZE      6     // Vertical size in millimeters
   #define HORZRES       8     // Horizontal width in pixels
   #define VERTRES       10    // Vertical height in pixels
   #define BITSPIXEL     12    // Number of bits per pixel
   #define PLANES        14    // Number of planes
   #define NUMBRUSHES    16    // Number of brushes the device has
   #define NUMPENS       18    // Number of pens the device has
   #define NUMMARKERS    20    // Number of markers the device has
   #define NUMFONTS      22    // Number of fonts the device has
   #define NUMCOLORS     24    // Number of colors the device supports
   #define PDEVICESIZE   26    // Size required for device descriptor
   #define CURVECAPS     28    // Curve capabilities
   #define LINECAPS      30    // Line capabilities
   #define POLYGONALCAPS 32    // Polygonal capabilities
   #define TEXTCAPS      34    // Text capabilities
   #define CLIPCAPS      36    // Clipping capabilities
   #define RASTERCAPS    38    // Bitblt capabilities
   #define ASPECTX       40    // Length of the X leg
   #define ASPECTY       42    // Length of the Y leg
   #define ASPECTXY      44    // Length of the hypotenuse

   #define LOGPIXELSX    88    // Logical pixels/inch in X
   #define LOGPIXELSY    90    // Logical pixels/inch in Y

   #define SIZEPALETTE  104    // Number of entries in physical palette
   #define NUMRESERVED  106    // Number of reserved entries in palette
   #define COLORRES     108    // Actual color resolution

   // Printing related DeviceCaps. These replace the appropriate Escapes

   #define PHYSICALWIDTH   110 // Physical Width in device units
   #define PHYSICALHEIGHT  111 // Physical Height in device units
   #define PHYSICALOFFSETX 112 // Physical Printable Area x margin
   #define PHYSICALOFFSETY 113 // Physical Printable Area y margin
   #define SCALINGFACTORX  114 // Scaling factor x
   #define SCALINGFACTORY  115 // Scaling factor y

   // Display driver specific

   #define VREFRESH        116  // Current vertical refresh rate of the
                                // display device (for displays only) in Hz
   #define DESKTOPVERTRES  117  // Horizontal width of entire desktop in
                                // pixels
   #define DESKTOPHORZRES  118  // Vertical height of entire desktop in
                                // pixels
   #define BLTALIGNMENT    119  // Preferred blt alignment

   #define SHADEBLENDCAPS  120  // Shading and blending caps
   #define COLORMGMTCAPS   121  // Color Management caps


   #ifndef NOGDICAPMASKS

    // Device Capability Masks:

    // Device Technologies
    #define DT_PLOTTER          0   // Vector plotter
    #define DT_RASDISPLAY       1   // Raster display
    #define DT_RASPRINTER       2   // Raster printer
    #define DT_RASCAMERA        3   // Raster camera
    #define DT_CHARSTREAM       4   // Character-stream, PLP
    #define DT_METAFILE         5   // Metafile, VDM
    #define DT_DISPFILE         6   // Display-file

    // Curve Capabilities
    #define CC_NONE             0   // Curves not supported
    #define CC_CIRCLES          1   // Can do circles
    #define CC_PIE              2   // Can do pie wedges
    #define CC_CHORD            4   // Can do chord arcs
    #define CC_ELLIPSES         8   // Can do ellipese
    #define CC_WIDE             16  // Can do wide lines
    #define CC_STYLED           32  // Can do styled lines
    #define CC_WIDESTYLED       64  // Can do wide styled lines
    #define CC_INTERIORS        128 // Can do interiors
    #define CC_ROUNDRECT        256 //

    // Line Capabilities
    #define LC_NONE             0   // Lines not supported
    #define LC_POLYLINE         2   // Can do polylines
    #define LC_MARKER           4   // Can do markers
    #define LC_POLYMARKER       8   // Can do polymarkers
    #define LC_WIDE             16  // Can do wide lines
    #define LC_STYLED           32  // Can do styled lines
    #define LC_WIDESTYLED       64  // Can do wide styled lines
    #define LC_INTERIORS        128 // Can do interiors

    // Polygonal Capabilities
    #define PC_NONE             0   // Polygonals not supported
    #define PC_POLYGON          1   // Can do polygons
    #define PC_RECTANGLE        2   // Can do rectangles
    #define PC_WINDPOLYGON      4   // Can do winding polygons
    #define PC_TRAPEZOID        4   // Can do trapezoids
    #define PC_SCANLINE         8   // Can do scanlines
    #define PC_WIDE             16  // Can do wide borders
    #define PC_STYLED           32  // Can do styled borders
    #define PC_WIDESTYLED       64  // Can do wide styled borders
    #define PC_INTERIORS        128 // Can do interiors
    #define PC_POLYPOLYGON      256 // Can do polypolygons
    #define PC_PATHS            512 // Can do paths

    // Clipping Capabilities
    #define CP_NONE             0   // No clipping of output
    #define CP_RECTANGLE        1   // Output clipped to rects
    #define CP_REGION           2   // obsolete

    // Text Capabilities
    #define TC_OP_CHARACTER              1  // Can do OutputPrecision   CHARACTER
    #define TC_OP_STROKE                 2  // Can do OutputPrecision   STROKE
    #define TC_CP_STROKE                 4  // Can do ClipPrecision     STROKE
    #define TC_CR_90                     8  // Can do CharRotAbility    90
    #define TC_CR_ANY                   16  // Can do CharRotAbility    ANY
    #define TC_SF_X_YINDEP              32  // Can do ScaleFreedom      X_YINDEPENDENT
    #define TC_SA_DOUBLE                64  // Can do ScaleAbility      DOUBLE
    #define TC_SA_INTEGER              128  // Can do ScaleAbility      INTEGER
    #define TC_SA_CONTIN               256  // Can do ScaleAbility      CONTINUOUS
    #define TC_EA_DOUBLE               512  // Can do EmboldenAbility   DOUBLE
    #define TC_IA_ABLE                1024  // Can do ItalisizeAbility  ABLE
    #define TC_UA_ABLE                2048  // Can do UnderlineAbility  ABLE
    #define TC_SO_ABLE                4096  // Can do StrikeOutAbility  ABLE
    #define TC_RA_ABLE                8192  // Can do RasterFontAble    ABLE
    #define TC_VA_ABLE               16384  // Can do VectorFontAble    ABLE
    #define TC_RESERVED              32768
    #define TC_SCROLLBLT             65536  // Don't do text scroll with blt

   #endif // NOGDICAPMASKS

   // Raster Capabilities
   #define RC_NONE
   #define RC_BITBLT           1       // Can do standard BLT.
   #define RC_BANDING          2       // Device requires banding support
   #define RC_SCALING          4       // Device requires scaling support
   #define RC_BITMAP64         8       // Device can support >64K bitmap
   #define RC_GDI20_OUTPUT         16      // has 2.0 output calls
   #define RC_GDI20_STATE          32
   #define RC_SAVEBITMAP           64
   #define RC_DI_BITMAP           128      // supports DIB to memory
   #define RC_PALETTE             256      // supports a palette
   #define RC_DIBTODEV            512      // supports DIBitsToDevice
   #define RC_BIGFONT            1024      // supports >64K fonts
   #define RC_STRETCHBLT         2048      // supports StretchBlt
   #define RC_FLOODFILL          4096      // supports FloodFill
   #define RC_STRETCHDIB         8192      // supports StretchDIBits
   #define RC_OP_DX_OUTPUT      16384
   #define RC_DEVBITS           32768

   // Shading and blending caps
   #define SB_NONE                      0
   #define SB_CONST_ALPHA               1
   #define SB_PIXEL_ALPHA               2
   #define SB_PREMULT_ALPHA             4

   #define SB_GRAD_RECT                16
   #define SB_GRAD_TRI                 32

   // Color Management caps
   #define CM_NONE                      0
   #define CM_DEVICE_ICM                1
   #define CM_GAMMA_RAMP                2
   #define CM_CMYK_COLOR                4

   // DIB color table identifiers

   #define DIB_RGB_COLORS      0 // color table in RGBs
   #define DIB_PAL_COLORS      1 // color table in palette indices

   // constants for Get/SetSystemPaletteUse()

   #define SYSPAL_ERROR    0
   #define SYSPAL_STATIC   1
   #define SYSPAL_NOSTATIC 2
   #define SYSPAL_NOSTATIC256 3

   // constants for CreateDIBitmap
   #define CBM_INIT            4   // initialize bitmap

   // ExtFloodFill style flags
   #define  FLOODFILLBORDER   0
   #define  FLOODFILLSURFACE  1

   // size of a device name string
   #define CCHDEVICENAME 32

   // size of a form name string
   #define CCHFORMNAME 32

   // field selection bits
   #define DM_ORIENTATION                1
   #define DM_PAPERSIZE                  2
   #define DM_PAPERLENGTH                4
   #define DM_PAPERWIDTH                 8
   #define DM_SCALE                     16
   #define DM_POSITION                  32
   #define DM_NUP                       64
   #define DM_COPIES                   256
   #define DM_DEFAULTSOURCE            512
   #define DM_PRINTQUALITY            1024
   #define DM_COLOR                   2048
   #define DM_DUPLEX                  4096
   #define DM_YRESOLUTION             8192
   #define DM_TTOPTION               16384
   #define DM_COLLATE                32768
   #define DM_FORMNAME               65536
   #define DM_LOGPIXELS             131072
   #define DM_BITSPERPEL            262144
   #define DM_PELSWIDTH             524288
   #define DM_PELSHEIGHT           1048576
   #define DM_DISPLAYFLAGS         2097152
   #define DM_DISPLAYFREQUENCY     4194304
   #define DM_ICMMETHOD            8388608
   #define DM_ICMINTENT           16777216
   #define DM_MEDIATYPE           33554432
   #define DM_DITHERTYPE          67108864
   #define DM_PANNINGWIDTH       134217728
   #define DM_PANNINGHEIGHT      268435456

   // orientation selections
   #define DMORIENT_PORTRAIT   1
   #define DMORIENT_LANDSCAPE  2

   // paper selections
   #define DMPAPER_FIRST                DMPAPER_LETTER
   #define DMPAPER_LETTER               1  // Letter 8 1/2 x 11 in
   #define DMPAPER_LETTERSMALL          2  // Letter Small 8 1/2 x 11 in
   #define DMPAPER_TABLOID              3  // Tabloid 11 x 17 in
   #define DMPAPER_LEDGER               4  // Ledger 17 x 11 in
   #define DMPAPER_LEGAL                5  // Legal 8 1/2 x 14 in
   #define DMPAPER_STATEMENT            6  // Statement 5 1/2 x 8 1/2 in
   #define DMPAPER_EXECUTIVE            7  // Executive 7 1/4 x 10 1/2 in
   #define DMPAPER_A3                   8  // A3 297 x 420 mm
   #define DMPAPER_A4                   9  // A4 210 x 297 mm
   #define DMPAPER_A4SMALL             10  // A4 Small 210 x 297 mm
   #define DMPAPER_A5                  11  // A5 148 x 210 mm
   #define DMPAPER_B4                  12  // B4 (JIS) 250 x 354
   #define DMPAPER_B5                  13  // B5 (JIS) 182 x 257 mm
   #define DMPAPER_FOLIO               14  // Folio 8 1/2 x 13 in
   #define DMPAPER_QUARTO              15  // Quarto 215 x 275 mm
   #define DMPAPER_10X14               16  // 10x14 in
   #define DMPAPER_11X17               17  // 11x17 in
   #define DMPAPER_NOTE                18  // Note 8 1/2 x 11 in
   #define DMPAPER_ENV_9               19  // Envelope #9 3 7/8 x 8 7/8
   #define DMPAPER_ENV_10              20  // Envelope #10 4 1/8 x 9 1/2
   #define DMPAPER_ENV_11              21  // Envelope #11 4 1/2 x 10 3/8
   #define DMPAPER_ENV_12              22  // Envelope #12 4 \276 x 11
   #define DMPAPER_ENV_14              23  // Envelope #14 5 x 11 1/2
   #define DMPAPER_CSHEET              24  // C size sheet
   #define DMPAPER_DSHEET              25  // D size sheet
   #define DMPAPER_ESHEET              26  // E size sheet
   #define DMPAPER_ENV_DL              27  // Envelope DL 110 x 220mm
   #define DMPAPER_ENV_C5              28  // Envelope C5 162 x 229 mm
   #define DMPAPER_ENV_C3              29  // Envelope C3  324 x 458 mm
   #define DMPAPER_ENV_C4              30  // Envelope C4  229 x 324 mm
   #define DMPAPER_ENV_C6              31  // Envelope C6  114 x 162 mm
   #define DMPAPER_ENV_C65             32  // Envelope C65 114 x 229 mm
   #define DMPAPER_ENV_B4              33  // Envelope B4  250 x 353 mm
   #define DMPAPER_ENV_B5              34  // Envelope B5  176 x 250 mm
   #define DMPAPER_ENV_B6              35  // Envelope B6  176 x 125 mm
   #define DMPAPER_ENV_ITALY           36  // Envelope 110 x 230 mm
   #define DMPAPER_ENV_MONARCH         37  // Envelope Monarch 3.875 x 7.5 in
   #define DMPAPER_ENV_PERSONAL        38  // 6 3/4 Envelope 3 5/8 x 6 1/2 in
   #define DMPAPER_FANFOLD_US          39  // US Std Fanfold 14 7/8 x 11 in
   #define DMPAPER_FANFOLD_STD_GERMAN  40  // German Std Fanfold 8 1/2 x 12 in
   #define DMPAPER_FANFOLD_LGL_GERMAN  41  // German Legal Fanfold 8 1/2 x 13 in
   #define DMPAPER_ISO_B4              42  // B4 (ISO) 250 x 353 mm
   #define DMPAPER_JAPANESE_POSTCARD   43  // Japanese Postcard 100 x 148 mm
   #define DMPAPER_9X11                44  // 9 x 11 in
   #define DMPAPER_10X11               45  // 10 x 11 in
   #define DMPAPER_15X11               46  // 15 x 11 in
   #define DMPAPER_ENV_INVITE          47  // Envelope Invite 220 x 220 mm
   #define DMPAPER_RESERVED_48         48  // RESERVED--DO NOT USE
   #define DMPAPER_RESERVED_49         49  // RESERVED--DO NOT USE
   #define DMPAPER_LETTER_EXTRA        50  // Letter Extra 9 \275 x 12 in
   #define DMPAPER_LEGAL_EXTRA         51  // Legal Extra 9 \275 x 15 in
   #define DMPAPER_TABLOID_EXTRA       52  // Tabloid Extra 11.69 x 18 in
   #define DMPAPER_A4_EXTRA            53  // A4 Extra 9.27 x 12.69 in
   #define DMPAPER_LETTER_TRANSVERSE   54  // Letter Transverse 8 \275 x 11 in
   #define DMPAPER_A4_TRANSVERSE       55  // A4 Transverse 210 x 297 mm
   #define DMPAPER_LETTER_EXTRA_TRANSVERSE 56 // Letter Extra Transverse 9\275 x 12 in
   #define DMPAPER_A_PLUS              57  // SuperA/SuperA/A4 227 x 356 mm
   #define DMPAPER_B_PLUS              58  // SuperB/SuperB/A3 305 x 487 mm
   #define DMPAPER_LETTER_PLUS         59  // Letter Plus 8.5 x 12.69 in
   #define DMPAPER_A4_PLUS             60  // A4 Plus 210 x 330 mm
   #define DMPAPER_A5_TRANSVERSE       61  // A5 Transverse 148 x 210 mm
   #define DMPAPER_B5_TRANSVERSE       62  // B5 (JIS) Transverse 182 x 257 mm
   #define DMPAPER_A3_EXTRA            63  // A3 Extra 322 x 445 mm
   #define DMPAPER_A5_EXTRA            64  // A5 Extra 174 x 235 mm
   #define DMPAPER_B5_EXTRA            65  // B5 (ISO) Extra 201 x 276 mm
   #define DMPAPER_A2                  66  // A2 420 x 594 mm
   #define DMPAPER_A3_TRANSVERSE       67  // A3 Transverse 297 x 420 mm
   #define DMPAPER_A3_EXTRA_TRANSVERSE 68  // A3 Extra Transverse 322 x 445 mm
   #define DMPAPER_DBL_JAPANESE_POSTCARD 69 // Japanese Double Postcard 200 x 148 mm
   #define DMPAPER_A6                  70  // A6 105 x 148 mm
   #define DMPAPER_JENV_KAKU2          71  // Japanese Envelope Kaku #2
   #define DMPAPER_JENV_KAKU3          72  // Japanese Envelope Kaku #3
   #define DMPAPER_JENV_CHOU3          73  // Japanese Envelope Chou #3
   #define DMPAPER_JENV_CHOU4          74  // Japanese Envelope Chou #4
   #define DMPAPER_LETTER_ROTATED      75  // Letter Rotated 11 x 8 1/2 11 in
   #define DMPAPER_A3_ROTATED          76  // A3 Rotated 420 x 297 mm
   #define DMPAPER_A4_ROTATED          77  // A4 Rotated 297 x 210 mm
   #define DMPAPER_A5_ROTATED          78  // A5 Rotated 210 x 148 mm
   #define DMPAPER_B4_JIS_ROTATED      79  // B4 (JIS) Rotated 364 x 257 mm
   #define DMPAPER_B5_JIS_ROTATED      80  // B5 (JIS) Rotated 257 x 182 mm
   #define DMPAPER_JAPANESE_POSTCARD_ROTATED 81 // Japanese Postcard Rotated 148 x 100 mm
   #define DMPAPER_DBL_JAPANESE_POSTCARD_ROTATED 82 // Double Japanese Postcard Rotated 148 x 200 mm
   #define DMPAPER_A6_ROTATED          83  // A6 Rotated 148 x 105 mm
   #define DMPAPER_JENV_KAKU2_ROTATED  84  // Japanese Envelope Kaku #2 Rotated
   #define DMPAPER_JENV_KAKU3_ROTATED  85  // Japanese Envelope Kaku #3 Rotated
   #define DMPAPER_JENV_CHOU3_ROTATED  86  // Japanese Envelope Chou #3 Rotated
   #define DMPAPER_JENV_CHOU4_ROTATED  87  // Japanese Envelope Chou #4 Rotated
   #define DMPAPER_B6_JIS              88  // B6 (JIS) 128 x 182 mm
   #define DMPAPER_B6_JIS_ROTATED      89  // B6 (JIS) Rotated 182 x 128 mm
   #define DMPAPER_12X11               90  // 12 x 11 in
   #define DMPAPER_JENV_YOU4           91  // Japanese Envelope You #4
   #define DMPAPER_JENV_YOU4_ROTATED   92  // Japanese Envelope You #4 Rotated
   #define DMPAPER_P16K                93  // PRC 16K 146 x 215 mm
   #define DMPAPER_P32K                94  // PRC 32K 97 x 151 mm
   #define DMPAPER_P32KBIG             95  // PRC 32K(Big) 97 x 151 mm
   #define DMPAPER_PENV_1              96  // PRC Envelope #1 102 x 165 mm
   #define DMPAPER_PENV_2              97  // PRC Envelope #2 102 x 176 mm
   #define DMPAPER_PENV_3              98  // PRC Envelope #3 125 x 176 mm
   #define DMPAPER_PENV_4              99  // PRC Envelope #4 110 x 208 mm
   #define DMPAPER_PENV_5              100 // PRC Envelope #5 110 x 220 mm
   #define DMPAPER_PENV_6              101 // PRC Envelope #6 120 x 230 mm
   #define DMPAPER_PENV_7              102 // PRC Envelope #7 160 x 230 mm
   #define DMPAPER_PENV_8              103 // PRC Envelope #8 120 x 309 mm
   #define DMPAPER_PENV_9              104 // PRC Envelope #9 229 x 324 mm
   #define DMPAPER_PENV_10             105 // PRC Envelope #10 324 x 458 mm
   #define DMPAPER_P16K_ROTATED        106 // PRC 16K Rotated
   #define DMPAPER_P32K_ROTATED        107 // PRC 32K Rotated
   #define DMPAPER_P32KBIG_ROTATED     108 // PRC 32K(Big) Rotated
   #define DMPAPER_PENV_1_ROTATED      109 // PRC Envelope #1 Rotated 165 x 102 mm
   #define DMPAPER_PENV_2_ROTATED      110 // PRC Envelope #2 Rotated 176 x 102 mm
   #define DMPAPER_PENV_3_ROTATED      111 // PRC Envelope #3 Rotated 176 x 125 mm
   #define DMPAPER_PENV_4_ROTATED      112 // PRC Envelope #4 Rotated 208 x 110 mm
   #define DMPAPER_PENV_5_ROTATED      113 // PRC Envelope #5 Rotated 220 x 110 mm
   #define DMPAPER_PENV_6_ROTATED      114 // PRC Envelope #6 Rotated 230 x 120 mm
   #define DMPAPER_PENV_7_ROTATED      115 // PRC Envelope #7 Rotated 230 x 160 mm
   #define DMPAPER_PENV_8_ROTATED      116 // PRC Envelope #8 Rotated 309 x 120 mm
   #define DMPAPER_PENV_9_ROTATED      117 // PRC Envelope #9 Rotated 324 x 229 mm
   #define DMPAPER_PENV_10_ROTATED     118 // PRC Envelope #10 Rotated 458 x 324 mm

   #define DMPAPER_LAST                DMPAPER_PENV_10_ROTATED

   #define DMPAPER_USER                256

   // bin selections
   #define DMBIN_FIRST         DMBIN_UPPER
   #define DMBIN_UPPER         1
   #define DMBIN_ONLYONE       1
   #define DMBIN_LOWER         2
   #define DMBIN_MIDDLE        3
   #define DMBIN_MANUAL        4
   #define DMBIN_ENVELOPE      5
   #define DMBIN_ENVMANUAL     6
   #define DMBIN_AUTO          7
   #define DMBIN_TRACTOR       8
   #define DMBIN_SMALLFMT      9
   #define DMBIN_LARGEFMT      10
   #define DMBIN_LARGECAPACITY 11
   #define DMBIN_CASSETTE      14
   #define DMBIN_FORMSOURCE    15
   #define DMBIN_LAST          DMBIN_FORMSOURCE

   #define DMBIN_USER          256     // device specific bins start here

   // print qualities
   #define DMRES_DRAFT         (-1)
   #define DMRES_LOW           (-2)
   #define DMRES_MEDIUM        (-3)
   #define DMRES_HIGH          (-4)

   // color enable/disable for color printers
   #define DMCOLOR_MONOCHROME  1
   #define DMCOLOR_COLOR       2

   // duplex enable
   #define DMDUP_SIMPLEX    1
   #define DMDUP_VERTICAL   2
   #define DMDUP_HORIZONTAL 3

   // TrueType options
   #define DMTT_BITMAP     1       // print TT fonts as graphics
   #define DMTT_DOWNLOAD   2       // download TT fonts as soft fonts
   #define DMTT_SUBDEV     3       // substitute device fonts for TT fonts
   #define DMTT_DOWNLOAD_OUTLINE 4 // download TT fonts as outline soft fonts

   // Collation selections
   #define DMCOLLATE_FALSE  0
   #define DMCOLLATE_TRUE   1

   // DEVMODE dmDisplayFlags flags

   #define DM_GRAYSCALE            0x00000001 /* This flag is no longer valid */
   #define DM_INTERLACED           0x00000002 /* This flag is no longer valid */
   #define DMDISPLAYFLAGS_TEXTMODE          4

   // dmNup , multiple logical page per physical page options
   #define DMNUP_SYSTEM        1
   #define DMNUP_ONEUP         2

   // ICM methods
   #define DMICMMETHOD_NONE    1   // ICM disabled
   #define DMICMMETHOD_SYSTEM  2   // ICM handled by system
   #define DMICMMETHOD_DRIVER  3   // ICM handled by driver
   #define DMICMMETHOD_DEVICE  4   // ICM handled by device

   #define DMICMMETHOD_USER  256   // Device-specific methods start here

   // ICM Intents
   #define DMICM_SATURATE          1   // Maximize color saturation
   #define DMICM_CONTRAST          2   // Maximize color contrast
   #define DMICM_COLORIMETRIC       3   // Use specific color metric
   #define DMICM_ABS_COLORIMETRIC   4   // Use specific color metric

   #define DMICM_USER        256   // Device-specific intents start here

   // Media types

   #define DMMEDIA_STANDARD      1   // Standard paper
   #define DMMEDIA_TRANSPARENCY  2   // Transparency
   #define DMMEDIA_GLOSSY        3   // Glossy paper

   #define DMMEDIA_USER        256   // Device-specific media start here

   // Dither types
   #define DMDITHER_NONE       1      // No dithering
   #define DMDITHER_COARSE     2      // Dither with a coarse brush
   #define DMDITHER_FINE       3      // Dither with a fine brush
   #define DMDITHER_LINEART    4      // LineArt dithering
   #define DMDITHER_ERRORDIFFUSION 5  // LineArt dithering
   #define DMDITHER_RESERVED6      6      // LineArt dithering
   #define DMDITHER_RESERVED7      7      // LineArt dithering
   #define DMDITHER_RESERVED8      8      // LineArt dithering
   #define DMDITHER_RESERVED9      9      // LineArt dithering
   #define DMDITHER_GRAYSCALE  10     // Device does grayscaling

   #define DMDITHER_USER     256   // Device-specific dithers start here

   #define DISPLAY_DEVICE_ATTACHED_TO_DESKTOP          1
   #define DISPLAY_DEVICE_MULTI_DRIVER                 2
   #define DISPLAY_DEVICE_PRIMARY_DEVICE               4
   #define DISPLAY_DEVICE_MIRRORING_DRIVER             8
   #define DISPLAY_DEVICE_VGA_COMPATIBLE              16
   #define DISPLAY_DEVICE_MODESPRUNED          134217728

   // GetRegionData/ExtCreateRegion

   #define RDH_RECTANGLES  1

   // for GetRandomRgn
   #define SYSRGN  4

   #ifndef NOTEXTMETRIC

    //  GetGlyphOutline constants

    #define GGO_METRICS        0
    #define GGO_BITMAP         1
    #define GGO_NATIVE         2
    #define GGO_BEZIER         3

    #define  GGO_GRAY2_BITMAP   4
    #define  GGO_GRAY4_BITMAP   5
    #define  GGO_GRAY8_BITMAP   6
    #define  GGO_GLYPH_INDEX       128
    #define  GGO_UNHINTED          256
    #define TT_POLYGON_TYPE   24

    #define TT_PRIM_LINE       1
    #define TT_PRIM_QSPLINE    2
    #define TT_PRIM_CSPLINE    3


    #define GCP_DBCS                1
    #define GCP_REORDER             2
    #define GCP_USEKERNING          8
    #define GCP_GLYPHSHAPE         16
    #define GCP_LIGATE             32
    ////#define GCP_GLYPHINDEXING  0x0080
    #define GCP_DIACRITIC         256
    #define GCP_KASHIDA          1024
    #define GCP_ERROR           32768
    #define FLI_MASK             4155

    #define GCP_JUSTIFY              65536
    ////#define GCP_NODIACRITICS   0x00020000L
    #define FLI_GLYPHS              262144
    #define GCP_CLASSIN             524288
    #define GCP_MAXEXTENT          1048576
    #define GCP_JUSTIFYIN          2097152
    #define GCP_DISPLAYZWG          4194304
    #define GCP_SYMSWAPOFF          8388608
    #define GCP_NUMERICOVERRIDE    16777216
    #define GCP_NEUTRALOVERRIDE    33554432
    #define GCP_NUMERICSLATIN      67108864
    #define GCP_NUMERICSLOCAL     134217728

    #define GCPCLASS_LATIN                  1
    #define GCPCLASS_HEBREW                 2
    #define GCPCLASS_ARABIC                 2
    #define GCPCLASS_NEUTRAL                3
    #define GCPCLASS_LOCALNUMBER            4
    #define GCPCLASS_LATINNUMBER            5
    #define GCPCLASS_LATINNUMERICTERMINATOR 6
    #define GCPCLASS_LATINNUMERICSEPARATOR  7
    #define GCPCLASS_NUMERICSEPARATOR       8
    #define GCPCLASS_PREBOUNDLTR          128
    #define GCPCLASS_PREBOUNDRTL           64
    #define GCPCLASS_POSTBOUNDLTR          32
    #define GCPCLASS_POSTBOUNDRTL          16

    #define GCPGLYPH_LINKBEFORE           32768
    #define GCPGLYPH_LINKAFTER            16384

    // bits defined in wFlags of RASTERIZER_STATUS
    #define TT_AVAILABLE         1
    #define TT_ENABLED           2

    // Pixel format descriptor


    // pixel types
    #define PFD_TYPE_RGBA        0
    #define PFD_TYPE_COLORINDEX  1

    // layer types
    #define PFD_MAIN_PLANE       0
    #define PFD_OVERLAY_PLANE    1
    #define PFD_UNDERLAY_PLANE   (-1)

    // PIXELFORMATDESCRIPTOR flags
    #define PFD_DOUBLEBUFFER                     1
    #define PFD_STEREO                           2
    #define PFD_DRAW_TO_WINDOW                   4
    #define PFD_DRAW_TO_BITMAP                   8
    #define PFD_SUPPORT_GDI                     16
    #define PFD_SUPPORT_OPENGL                  32
    #define PFD_GENERIC_FORMAT                  64
    #define PFD_NEED_PALETTE                   128
    #define PFD_NEED_SYSTEM_PALETTE            256
    #define PFD_SWAP_EXCHANGE                  512
    #define PFD_SWAP_COPY                     1024
    #define PFD_SWAP_LAYER_BUFFERS            2048
    #define PFD_GENERIC_ACCELERATED           4096
    #define PFD_SUPPORT_DIRECTDRAW            8192

    // PIXELFORMATDESCRIPTOR flags for use in ChoosePixelFormat only
    #define PFD_DEPTH_DONTCARE           536870912
    #define PFD_DOUBLEBUFFER_DONTCARE   1073741824
    #define PFD_STEREO_DONTCARE         2147483648

    #ifdef STRICT

     #ifndef NOTEXTMETRIC

      // mode selections for the device mode function
      #define DM_UPDATE           1
      #define DM_COPY             2
      #define DM_PROMPT           4
      #define DM_MODIFY           8

      #define DM_IN_BUFFER        DM_MODIFY
      #define DM_IN_PROMPT        DM_PROMPT
      #define DM_OUT_BUFFER       DM_COPY
      #define DM_OUT_DEFAULT      DM_UPDATE

      // device capabilities indices
      #define DC_FIELDS           1
      #define DC_PAPERS           2
      #define DC_PAPERSIZE        3
      #define DC_MINEXTENT        4
      #define DC_MAXEXTENT        5
      #define DC_BINS             6
      #define DC_DUPLEX           7
      #define DC_SIZE             8
      #define DC_EXTRA            9
      #define DC_VERSION          10
      #define DC_DRIVER           11
      #define DC_BINNAMES         12
      #define DC_ENUMRESOLUTIONS  13
      #define DC_FILEDEPENDENCIES 14
      #define DC_TRUETYPE         15
      #define DC_PAPERNAMES       16
      #define DC_ORIENTATION      17
      #define DC_COPIES           18
      #define DC_BINADJUST            19
      #define DC_EMF_COMPLIANT        20
      #define DC_DATATYPE_PRODUCED    21
      #define DC_COLLATE              22
      #define DC_MANUFACTURER         23
      #define DC_MODEL                24
      #define DC_PERSONALITY          25
      #define DC_PRINTRATE            26
      #define DC_PRINTRATEUNIT        27
      #define   PRINTRATEUNIT_PPM     1
      #define   PRINTRATEUNIT_CPS     2
      #define   PRINTRATEUNIT_LPM     3
      #define   PRINTRATEUNIT_IPM     4
      #define DC_PRINTERMEM           28
      #define DC_MEDIAREADY           29
      #define DC_STAPLE               30
      #define DC_PRINTRATEPPM         31
      #define DC_COLORDEVICE          32
      #define DC_NUP                  33
      // bit fields of the return value (DWORD) for DC_TRUETYPE
      #define DCTT_BITMAP                      1
      #define DCTT_DOWNLOAD                    2
      #define DCTT_SUBDEV                      4
      #define DCTT_DOWNLOAD_OUTLINE            8

      // return values for DC_BINADJUST
      #define DCBA_FACEUPNONE            0
      #define DCBA_FACEUPCENTER          1
      #define DCBA_FACEUPLEFT            2
      #define DCBA_FACEUPRIGHT           3
      #define DCBA_FACEDOWNNONE        256
      #define DCBA_FACEDOWNCENTER      257
      #define DCBA_FACEDOWNLEFT        258
      #define DCBA_FACEDOWNRIGHT       259
      #define GS_8BIT_INDICES              1

      // flags for GetGlyphIndices

      #define GGI_MARK_NONEXISTING_GLYPHS  0X0001

      //#define STAMP_DESIGNVECTOR  (134217728 + 'd' + ('v' << 8))
      //#define STAMP_AXESLIST      (134217728 + 'a' + ('l' << 8))
      #define MM_MAX_NUMAXES      16

      #define FR_PRIVATE       16
      #define FR_NOT_ENUM      32

      // The actual size of the DESIGNVECTOR and ENUMLOGFONTEXDV structures
      // is determined by dvNumAxes,
      // MM_MAX_NUMAXES only detemines the maximal size allowed

      #define AC_SRC_OVER                    0

      //
      // alpha format flags
      //

      #define AC_SRC_ALPHA                   1


      //
      // gradient drawing modes
      //

      #define GRADIENT_FILL_RECT_H             0
      #define GRADIENT_FILL_RECT_V             1
      #define GRADIENT_FILL_TRIANGLE           2
      #define GRADIENT_FILL_OP_FLAG          255

      #ifndef NOMETAFILE

       // Enhanced Metafile Function Declarations

       // new GDI


       // Flags value for COLORADJUSTMENT
       #define CA_NEGATIVE                      1
       #define CA_LOG_FILTER                    2

       // IlluminantIndex values
       #define ILLUMINANT_DEVICE_DEFAULT   0
       #define ILLUMINANT_A                1
       #define ILLUMINANT_B                2
       #define ILLUMINANT_C                3
       #define ILLUMINANT_D50              4
       #define ILLUMINANT_D55              5
       #define ILLUMINANT_D65              6
       #define ILLUMINANT_D75              7
       #define ILLUMINANT_F2               8
       #define ILLUMINANT_MAX_INDEX        ILLUMINANT_F2

       #define ILLUMINANT_TUNGSTEN         ILLUMINANT_A
       #define ILLUMINANT_DAYLIGHT         ILLUMINANT_C
       #define ILLUMINANT_FLUORESCENT      ILLUMINANT_F2
       #define ILLUMINANT_NTSC             ILLUMINANT_C

       // Min and max for RedGamma, GreenGamma, BlueGamma
       #define RGB_GAMMA_MIN               02500
       #define RGB_GAMMA_MAX               65000

       // Min and max for ReferenceBlack and ReferenceWhite
       #define REFERENCE_WHITE_MIN         6000
       #define REFERENCE_WHITE_MAX         10000
       #define REFERENCE_BLACK_MIN         0
       #define REFERENCE_BLACK_MAX         4000

       // Min and max for Contrast, Brightness, Colorfulness, RedGreenTint
       #define COLOR_ADJ_MIN               -100
       #define COLOR_ADJ_MAX               100

       #define DI_APPBANDING                        1
       #define DI_ROPS_READ_DESTINATION             2

       #define FONTMAPPER_MAX 10

       #define ICM_OFF               1
       #define ICM_ON                2
       #define ICM_QUERY             3
       #define ICM_DONE_OUTSIDEDC    4


       #define ICMENUMPROC  ICMENUMPROCA

       // Enhanced metafile constants.

       #define ENHMETA_SIGNATURE        541412678

       // Stock object flag used in the object handle index in the enhanced
       // metafile records.
       // E.g. The object handle index (META_STOCK_OBJECT | BLACK_BRUSH)
       // represents the stock object BLACK_BRUSH.

       #define ENHMETA_STOCK_OBJECT    2147483648

       // Enhanced metafile record types.

       #define EMR_HEADER                      1
       #define EMR_POLYBEZIER                  2
       #define EMR_POLYGON                     3
       #define EMR_POLYLINE                    4
       #define EMR_POLYBEZIERTO                5
       #define EMR_POLYLINETO                  6
       #define EMR_POLYPOLYLINE                7
       #define EMR_POLYPOLYGON                 8
       #define EMR_SETWINDOWEXTEX              9
       #define EMR_SETWINDOWORGEX              10
       #define EMR_SETVIEWPORTEXTEX            11
       #define EMR_SETVIEWPORTORGEX            12
       #define EMR_SETBRUSHORGEX               13
       #define EMR_EOF                         14
       #define EMR_SETPIXELV                   15
       #define EMR_SETMAPPERFLAGS              16
       #define EMR_SETMAPMODE                  17
       #define EMR_SETBKMODE                   18
       #define EMR_SETPOLYFILLMODE             19
       #define EMR_SETROP2                     20
       #define EMR_SETSTRETCHBLTMODE           21
       #define EMR_SETTEXTALIGN                22
       #define EMR_SETCOLORADJUSTMENT          23
       #define EMR_SETTEXTCOLOR                24
       #define EMR_SETBKCOLOR                  25
       #define EMR_OFFSETCLIPRGN               26
       #define EMR_MOVETOEX                    27
       #define EMR_SETMETARGN                  28
       #define EMR_EXCLUDECLIPRECT             29
       #define EMR_INTERSECTCLIPRECT           30
       #define EMR_SCALEVIEWPORTEXTEX          31
       #define EMR_SCALEWINDOWEXTEX            32
       #define EMR_SAVEDC                      33
       #define EMR_RESTOREDC                   34
       #define EMR_SETWORLDTRANSFORM           35
       #define EMR_MODIFYWORLDTRANSFORM        36
       #define EMR_SELECTOBJECT                37
       #define EMR_CREATEPEN                   38
       #define EMR_CREATEBRUSHINDIRECT         39
       #define EMR_DELETEOBJECT                40
       #define EMR_ANGLEARC                    41
       #define EMR_ELLIPSE                     42
       #define EMR_RECTANGLE                   43
       #define EMR_ROUNDRECT                   44
       #define EMR_ARC                         45
       #define EMR_CHORD                       46
       #define EMR_PIE                         47
       #define EMR_SELECTPALETTE               48
       #define EMR_CREATEPALETTE               49
       #define EMR_SETPALETTEENTRIES           50
       #define EMR_RESIZEPALETTE               51
       #define EMR_REALIZEPALETTE              52
       #define EMR_EXTFLOODFILL                53
       #define EMR_LINETO                      54
       #define EMR_ARCTO                       55
       #define EMR_POLYDRAW                    56
       #define EMR_SETARCDIRECTION             57
       #define EMR_SETMITERLIMIT               58
       #define EMR_BEGINPATH                   59
       #define EMR_ENDPATH                     60
       #define EMR_CLOSEFIGURE                 61
       #define EMR_FILLPATH                    62
       #define EMR_STROKEANDFILLPATH           63
       #define EMR_STROKEPATH                  64
       #define EMR_FLATTENPATH                 65
       #define EMR_WIDENPATH                   66
       #define EMR_SELECTCLIPPATH              67
       #define EMR_ABORTPATH                   68

       #define EMR_GDICOMMENT                  70
       #define EMR_FILLRGN                     71
       #define EMR_FRAMERGN                    72
       #define EMR_INVERTRGN                   73
       #define EMR_PAINTRGN                    74
       #define EMR_EXTSELECTCLIPRGN            75
       #define EMR_BITBLT                      76
       #define EMR_STRETCHBLT                  77
       #define EMR_MASKBLT                     78
       #define EMR_PLGBLT                      79
       #define EMR_SETDIBITSTODEVICE           80
       #define EMR_STRETCHDIBITS               81
       #define EMR_EXTCREATEFONTINDIRECTW      82
       #define EMR_EXTTEXTOUTA                 83
       #define EMR_EXTTEXTOUTW                 84
       #define EMR_POLYBEZIER16                85
       #define EMR_POLYGON16                   86
       #define EMR_POLYLINE16                  87
       #define EMR_POLYBEZIERTO16              88
       #define EMR_POLYLINETO16                89
       #define EMR_POLYPOLYLINE16              90
       #define EMR_POLYPOLYGON16               91
       #define EMR_POLYDRAW16                  92
       #define EMR_CREATEMONOBRUSH             93
       #define EMR_CREATEDIBPATTERNBRUSHPT     94
       #define EMR_EXTCREATEPEN                95
       #define EMR_POLYTEXTOUTA                96
       #define EMR_POLYTEXTOUTW                97

       #define EMR_SETICMMODE                  98
       #define EMR_CREATECOLORSPACE            99
       #define EMR_SETCOLORSPACE              100
       #define EMR_DELETECOLORSPACE           101
       #define EMR_GLSRECORD                  102
       #define EMR_GLSBOUNDEDRECORD           103
       #define EMR_PIXELFORMAT                104
       #define EMR_RESERVED_105               105
       #define EMR_RESERVED_106               106
       #define EMR_RESERVED_107               107
       #define EMR_RESERVED_108               108
       #define EMR_RESERVED_109               109
       #define EMR_RESERVED_110               110
       #define EMR_COLORCORRECTPALETTE        111
       #define EMR_SETICMPROFILEA             112
       #define EMR_SETICMPROFILEW             113
       #define EMR_ALPHABLEND                 114
       #define EMR_SETLAYOUT                  115
       #define EMR_TRANSPARENTBLT             116
       #define EMR_GRADIENTFILL               118
       #define EMR_RESERVED_119               119
       #define EMR_RESERVED_120               120
       #define EMR_COLORMATCHTOTARGETW        121
       #define EMR_CREATECOLORSPACEW          122

       // Record structures for the enhanced metafile.


       #define SETICMPROFILE_EMBEDED                    1

       #define CREATECOLORSPACE_EMBEDED                 1


       #define COLORMATCHTOTARGET_EMBEDED               1

       #define GDICOMMENT_IDENTIFIER           1128875079
       #define GDICOMMENT_WINDOWS_METAFILE     2147483649
       #define GDICOMMENT_BEGINGROUP                    2
       #define GDICOMMENT_ENDGROUP                      3
       #define GDICOMMENT_MULTIFORMATS         1073741828
       #define EPS_SIGNATURE                   1179865157
       #define GDICOMMENT_UNICODE_STRING               64
       #define GDICOMMENT_UNICODE_END                 128


       // OpenGL wgl prototypes

       #define wglUseFontBitmaps  wglUseFontBitmapsA

       #define WGL_FONT_LINES      0
       #define WGL_FONT_POLYGONS   1


       #define wglUseFontOutlines  wglUseFontOutlinesA


       // LAYERPLANEDESCRIPTOR flags
       #define LPD_DOUBLEBUFFER                 1
       #define LPD_STEREO                       2
       #define LPD_SUPPORT_GDI                 16
       #define LPD_SUPPORT_OPENGL              32
       #define LPD_SHARE_DEPTH                 64
       #define LPD_SHARE_STENCIL              128
       #define LPD_SHARE_ACCUM                256
       #define LPD_SWAP_EXCHANGE              512
       #define LPD_SWAP_COPY                 1024
       #define LPD_TRANSPARENT               4096

       #define LPD_TYPE_RGBA        0
       #define LPD_TYPE_COLORINDEX  1

       // wglSwapLayerBuffers flags
       #define WGL_SWAP_MAIN_PLANE              1
       #define WGL_SWAP_OVERLAY1                2
       #define WGL_SWAP_OVERLAY2                4
       #define WGL_SWAP_OVERLAY3                8
       #define WGL_SWAP_OVERLAY4               16
       #define WGL_SWAP_OVERLAY5               32
       #define WGL_SWAP_OVERLAY6               64
       #define WGL_SWAP_OVERLAY7              128
       #define WGL_SWAP_OVERLAY8              256
       #define WGL_SWAP_OVERLAY9              512
       #define WGL_SWAP_OVERLAY10            1024
       #define WGL_SWAP_OVERLAY11            2048
       #define WGL_SWAP_OVERLAY12            4096
       #define WGL_SWAP_OVERLAY13            8192
       #define WGL_SWAP_OVERLAY14           16384
       #define WGL_SWAP_OVERLAY15           32768
       #define WGL_SWAP_UNDERLAY1           65536
       #define WGL_SWAP_UNDERLAY2          131072
       #define WGL_SWAP_UNDERLAY3          262144
       #define WGL_SWAP_UNDERLAY4          524288
       #define WGL_SWAP_UNDERLAY5         1048576
       #define WGL_SWAP_UNDERLAY6         2097152
       #define WGL_SWAP_UNDERLAY7         4194304
       #define WGL_SWAP_UNDERLAY8         8388608
       #define WGL_SWAP_UNDERLAY9        16777216
       #define WGL_SWAP_UNDERLAY10       33554432
       #define WGL_SWAP_UNDERLAY11       67108864
       #define WGL_SWAP_UNDERLAY12      134217728
       #define WGL_SWAP_UNDERLAY13      268435456
       #define WGL_SWAP_UNDERLAY14      536870912
       #define WGL_SWAP_UNDERLAY15     1073741824

      #endif

      // flags for DrawFrameControl
      /*
      #define DFC_CAPTION             1
      #define DFC_MENU                2
      #define DFC_SCROLL              3
      #define DFC_BUTTON              4
      #define DFC_POPUPMENU           5

      #define DFCS_CAPTIONCLOSE            0
      #define DFCS_CAPTIONMIN              1
      #define DFCS_CAPTIONMAX              2
      #define DFCS_CAPTIONRESTORE          3
      #define DFCS_CAPTIONHELP             4

      #define DFCS_MENUARROW               0
      #define DFCS_MENUCHECK               1
      #define DFCS_MENUBULLET              2
      #define DFCS_MENUARROWRIGHT          4
      #define DFCS_SCROLLUP                0
      #define DFCS_SCROLLDOWN              1
      #define DFCS_SCROLLLEFT              2
      #define DFCS_SCROLLRIGHT             3
      #define DFCS_SCROLLCOMBOBOX          5
      #define DFCS_SCROLLSIZEGRIP          8
      #define DFCS_SCROLLSIZEGRIPRIGHT     16

      #define DFCS_BUTTONCHECK             0
      #define DFCS_BUTTONRADIOIMAGE        1
      #define DFCS_BUTTONRADIOMASK         2
      #define DFCS_BUTTONRADIO             4
      #define DFCS_BUTTON3STATE            8
      #define DFCS_BUTTONPUSH             16

      #define DFCS_INACTIVE              256
      #define DFCS_PUSHED                512
      #define DFCS_CHECKED              1024

      #define DFCS_TRANSPARENT          2048
      #define DFCS_HOT                  4096

      #define DFCS_ADJUSTRECT           8192
      #define DFCS_FLAT                16384
      #define DFCS_MONO                32768

      // flags for DrawCaption
      #define DC_ACTIVE                1
      #define DC_SMALLCAP              2
      #define DC_ICON                  4
      #define DC_TEXT                  8
      #define DC_INBUTTON             16
      #define DC_GRADIENT             32

      #define IDANI_OPEN          1

      */

     #endif
    #endif
   #endif
  #endif
 #endif  // NOGDI
#endif // _WINGDI_
