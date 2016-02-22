/*
 * hbwin header
 *
 * Copyright 2008 Viktor Szakats (vszakats.net/harbour)
 * Copyright 2004 Peter Rees <peter@rees.co.nz> Rees Software and Systems Ltd
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

/* NOTE: This file is also used by C code. */

#ifndef HBWIN_CH_
#define HBWIN_CH_

/* macros */
#define WIN_LOWORD( dw )                   hb_bitAnd( dw, 0xFFFF )
#define WIN_HIWORD( dw )                   hb_bitAnd( hb_bitShift( dw, -16 ), 0xFFFF )

#define WIN_MAKEWORD( lb, hb )             hb_bitOr( hb_bitShift( hb_bitAnd( hb, 0xFF ), 8 ), hb_bitAnd( lb, 0xFF ) )
#define WIN_MAKELONG( lw, hw )             hb_bitOr( hb_bitShift( hb_bitAnd( hw, 0xFFFF ), 16 ), hb_bitAnd( lw, 0xFFFF ) )

#define WIN_MAKELPARAM( lw, hw )           WIN_MAKELONG( lw, hw )
#define WIN_MAKEWPARAM( lw, hw )           WIN_MAKELONG( lw, hw )
#define WIN_MAKELRESULT( lw, hw )          WIN_MAKELONG( lw, hw )

#define WIN_RGB( nR, nG, nB )              ( nR + ( nG * 256 ) + ( nB * 256 * 256 ) )

#define WIN_MM_TO_INCH                     25.4

#define WIN_MAX_PATH                       260

/* special color value */
#define WIN_CLR_INVALID                    0xFFFFFFFF

/* win_MAPISendMail() address types */
#define WIN_MAPI_TO                        1
#define WIN_MAPI_CC                        2
#define WIN_MAPI_BCC                       3

/* Windows registry hives */
#define WIN_HKEY_CLASSES_ROOT              0x80000000
#define WIN_HKEY_CURRENT_USER              0x80000001
#define WIN_HKEY_LOCAL_MACHINE             0x80000002
#define WIN_HKEY_USERS                     0x80000003
#define WIN_HKEY_PERFORMANCE_DATA          0x80000004
#define WIN_HKEY_CURRENT_CONFIG            0x80000005
#define WIN_HKEY_DYN_DATA                  0x80000006

/* Windows registry key types in win_regWrite()/win_regSet()/win_regSetValueEx() functions */
#define WIN_REG_NONE                       0   /* No value type */
#define WIN_REG_SZ                         1   /* Unicode nul terminated string */
#define WIN_REG_EXPAND_SZ                  2   /* Unicode nul terminated string (with environment variable references) */
#define WIN_REG_BINARY                     3   /* Free form binary */
#define WIN_REG_DWORD                      4   /* 32-bit number */
#define WIN_REG_DWORD_LITTLE_ENDIAN        4   /* 32-bit number (same as REG_DWORD) */
#define WIN_REG_DWORD_BIG_ENDIAN           5   /* 32-bit number */
#define WIN_REG_LINK                       6   /* Symbolic Link (unicode) */
#define WIN_REG_MULTI_SZ                   7   /* Multiple Unicode strings */
#define WIN_REG_RESOURCE_LIST              8   /* Resource list in the resource map */
#define WIN_REG_FULL_RESOURCE_DESCRIPTOR   9   /* Resource list in the hardware description */
#define WIN_REG_RESOURCE_REQUIREMENTS_LIST 10
#define WIN_REG_QWORD                      11  /* 64-bit number */
#define WIN_REG_QWORD_LITTLE_ENDIAN        11  /* 64-bit number (same as REG_QWORD) */

/* win_com*() related values */

/* win_comOpen() bit rates */
#define WIN_CBR_110                        110
#define WIN_CBR_300                        300
#define WIN_CBR_600                        600
#define WIN_CBR_1200                       1200
#define WIN_CBR_2400                       2400
#define WIN_CBR_4800                       4800
#define WIN_CBR_9600                       9600
#define WIN_CBR_14400                      14400
#define WIN_CBR_19200                      19200
#define WIN_CBR_38400                      38400
#define WIN_CBR_56000                      56000
#define WIN_CBR_57600                      57600
#define WIN_CBR_115200                     115200
#define WIN_CBR_128000                     128000
#define WIN_CBR_256000                     256000

/* win_comOpen() parity modes */
#define WIN_NOPARITY                       0
#define WIN_ODDPARITY                      1
#define WIN_EVENPARITY                     2
#define WIN_MARKPARITY                     3
#define WIN_SPACEPARITY                    4

/* win_comOpen() stopbit modes */
#define WIN_ONESTOPBIT                     0
#define WIN_ONE5STOPBITS                   1
#define WIN_TWOSTOPBITS                    2

/* win_comDTRFlow() parameters */
#define WIN_DTR_CONTROL_DISABLE            0x00
#define WIN_DTR_CONTROL_ENABLE             0x01
#define WIN_DTR_CONTROL_HANDSHAKE          0x02

/* win_comRTSFlow() parameters */
#define WIN_RTS_CONTROL_DISABLE            0x00
#define WIN_RTS_CONTROL_ENABLE             0x01
#define WIN_RTS_CONTROL_HANDSHAKE          0x02
#define WIN_RTS_CONTROL_TOGGLE             0x03

/* win_comDebugDCB() debug levels */
#define HB_WIN_COM_DBGBASIC                0x01
#define HB_WIN_COM_DBGFLOW                 0x02
#define HB_WIN_COM_DBGXTRAFLOW             0x04
#define HB_WIN_COM_DBGOTHER                0x08
#define HB_WIN_COM_DBGTIMEOUTS             0x10
#define HB_WIN_COM_DBGQUEUE                0x20
#define HB_WIN_COM_DBGALL                  0x3F

/* win_comFuncLast() return values */
#define HB_WIN_COM_FUN_CREATEFILE          1
#define HB_WIN_COM_FUN_GETCOMMSTATE        2
#define HB_WIN_COM_FUN_SETCOMMSTATE        3
#define HB_WIN_COM_FUN_SETUPCOMM           4
#define HB_WIN_COM_FUN_GETCOMMTIMEOUTS     5
#define HB_WIN_COM_FUN_SETCOMMTIMEOUTS     6
#define HB_WIN_COM_FUN_CLOSEHANDLE         7
#define HB_WIN_COM_FUN_WRITEFILE           8
#define HB_WIN_COM_FUN_READFILE            9
#define HB_WIN_COM_FUN_GETCOMMMODEMSTATUS  10  /* win_com:Status() */
#define HB_WIN_COM_FUN_PURGECOMM           11
#define HB_WIN_COM_FUN_CLEARCOMMERROR      12  /* win_com:QueueStatus() */
#define HB_WIN_COM_FUN_ESCAPECOMMFUNCTION  13
#define HB_WIN_COM_FUN_GETCOMMPROPERTIES   14
#define HB_WIN_COM_FUN_MAX                 14

/* win_Prn() related values */

/* win_printerList() positions for array returned */
#define HB_WINPRN_NAME                     1
#define HB_WINPRN_PORT                     2
#define HB_WINPRN_TYPE                     3
#define HB_WINPRN_DRIVER                   4
#define HB_WINPRN_SHARE                    5
#define HB_WINPRN_SERVER                   6
#define HB_WINPRN_LEN_                     6

/* win_EnumFonts()/win_EnumFontFamilies() positions for array returned */
#define HB_WINFONT_NAME                    1
#define HB_WINFONT_FIXED                   2
#define HB_WINFONT_TRUETYPE                3
#define HB_WINFONT_CHARSET                 4
#define HB_WINFONT_LEN_                    4

/* win_bitmapType() return values */
#define HB_WIN_BITMAP_UNKNOWN              0
#define HB_WIN_BITMAP_BMP                  1
#define HB_WIN_BITMAP_JPEG                 2
#define HB_WIN_BITMAP_PNG                  3

/* Color constants for convenience */
#define HB_WIN_RGB_BLACK                   WIN_RGB( 0x00, 0x00, 0x00 )
#define HB_WIN_RGB_BLUE                    WIN_RGB( 0x00, 0x00, 0xFF )
#define HB_WIN_RGB_GREEN                   WIN_RGB( 0x00, 0xFF, 0x00 )
#define HB_WIN_RGB_RED                     WIN_RGB( 0xFF, 0x00, 0x00 )
#define HB_WIN_RGB_CYAN                    WIN_RGB( 0x00, 0xFF, 0xFF )
#define HB_WIN_RGB_MAGENTA                 WIN_RGB( 0xFF, 0x00, 0xFF )
#define HB_WIN_RGB_YELLOW                  WIN_RGB( 0xFF, 0xFF, 0x00 )
#define HB_WIN_RGB_WHITE                   WIN_RGB( 0xFF, 0xFF, 0xFF )

/* win_SetDocumentProperties() paper types */
#define WIN_DMPAPER_FIRST                  WIN_DMPAPER_LETTER
#define WIN_DMPAPER_LETTER                 1
#define WIN_DMPAPER_LETTERSMALL            2
#define WIN_DMPAPER_TABLOID                3
#define WIN_DMPAPER_LEDGER                 4
#define WIN_DMPAPER_LEGAL                  5
#define WIN_DMPAPER_STATEMENT              6
#define WIN_DMPAPER_EXECUTIVE              7
#define WIN_DMPAPER_A3                     8
#define WIN_DMPAPER_A4                     9
#define WIN_DMPAPER_A4SMALL                10
#define WIN_DMPAPER_A5                     11
#define WIN_DMPAPER_B4                     12
#define WIN_DMPAPER_B5                     13
#define WIN_DMPAPER_FOLIO                  14
#define WIN_DMPAPER_QUARTO                 15
#define WIN_DMPAPER_10X14                  16
#define WIN_DMPAPER_11X17                  17
#define WIN_DMPAPER_NOTE                   18
#define WIN_DMPAPER_ENV_9                  19
#define WIN_DMPAPER_ENV_10                 20
#define WIN_DMPAPER_ENV_11                 21
#define WIN_DMPAPER_ENV_12                 22
#define WIN_DMPAPER_ENV_14                 23
#define WIN_DMPAPER_CSHEET                 24
#define WIN_DMPAPER_DSHEET                 25
#define WIN_DMPAPER_ESHEET                 26
#define WIN_DMPAPER_ENV_DL                 27
#define WIN_DMPAPER_ENV_C5                 28
#define WIN_DMPAPER_ENV_C3                 29
#define WIN_DMPAPER_ENV_C4                 30
#define WIN_DMPAPER_ENV_C6                 31
#define WIN_DMPAPER_ENV_C65                32
#define WIN_DMPAPER_ENV_B4                 33
#define WIN_DMPAPER_ENV_B5                 34
#define WIN_DMPAPER_ENV_B6                 35
#define WIN_DMPAPER_ENV_ITALY              36
#define WIN_DMPAPER_ENV_MONARCH            37
#define WIN_DMPAPER_ENV_PERSONAL           38
#define WIN_DMPAPER_FANFOLD_US             39
#define WIN_DMPAPER_FANFOLD_STD_GERMAN     40
#define WIN_DMPAPER_FANFOLD_LGL_GERMAN     41
#define WIN_DMPAPER_ISO_B4                 42
#define WIN_DMPAPER_JAPANESE_POSTCARD      43
#define WIN_DMPAPER_9X11                   44
#define WIN_DMPAPER_10X11                  45
#define WIN_DMPAPER_15X11                  46
#define WIN_DMPAPER_ENV_INVITE             47
#define WIN_DMPAPER_RESERVED_48            48
#define WIN_DMPAPER_RESERVED_49            49
#define WIN_DMPAPER_LETTER_EXTRA           50
#define WIN_DMPAPER_LEGAL_EXTRA            51
#define WIN_DMPAPER_TABLOID_EXTRA          52
#define WIN_DMPAPER_A4_EXTRA               53
#define WIN_DMPAPER_LETTER_TRANSVERSE      54
#define WIN_DMPAPER_A4_TRANSVERSE          55
#define WIN_DMPAPER_LETTER_EXTRA_TRANSVERSE  56
#define WIN_DMPAPER_A_PLUS                 57
#define WIN_DMPAPER_B_PLUS                 58
#define WIN_DMPAPER_LETTER_PLUS            59
#define WIN_DMPAPER_A4_PLUS                60
#define WIN_DMPAPER_A5_TRANSVERSE          61
#define WIN_DMPAPER_B5_TRANSVERSE          62
#define WIN_DMPAPER_A3_EXTRA               63
#define WIN_DMPAPER_A5_EXTRA               64
#define WIN_DMPAPER_B5_EXTRA               65
#define WIN_DMPAPER_A2                     66
#define WIN_DMPAPER_A3_TRANSVERSE          67
#define WIN_DMPAPER_A3_EXTRA_TRANSVERSE    68
#define WIN_DMPAPER_DBL_JAPANESE_POSTCARD  69
#define WIN_DMPAPER_A6                     70
#define WIN_DMPAPER_JENV_KAKU2             71
#define WIN_DMPAPER_JENV_KAKU3             72
#define WIN_DMPAPER_JENV_CHOU3             73
#define WIN_DMPAPER_JENV_CHOU4             74
#define WIN_DMPAPER_LETTER_ROTATED         75
#define WIN_DMPAPER_A3_ROTATED             76
#define WIN_DMPAPER_A4_ROTATED             77
#define WIN_DMPAPER_A5_ROTATED             78
#define WIN_DMPAPER_B4_JIS_ROTATED         79
#define WIN_DMPAPER_B5_JIS_ROTATED         80
#define WIN_DMPAPER_JAPANESE_POSTCARD_ROTATED      81
#define WIN_DMPAPER_DBL_JAPANESE_POSTCARD_ROTATED  82
#define WIN_DMPAPER_A6_ROTATED             83
#define WIN_DMPAPER_JENV_KAKU2_ROTATED     84
#define WIN_DMPAPER_JENV_KAKU3_ROTATED     85
#define WIN_DMPAPER_JENV_CHOU3_ROTATED     86
#define WIN_DMPAPER_JENV_CHOU4_ROTATED     87
#define WIN_DMPAPER_B6_JIS                 88
#define WIN_DMPAPER_B6_JIS_ROTATED         89
#define WIN_DMPAPER_12X11                  90
#define WIN_DMPAPER_JENV_YOU4              91
#define WIN_DMPAPER_JENV_YOU4_ROTATED      92
#define WIN_DMPAPER_P16K                   93
#define WIN_DMPAPER_P32K                   94
#define WIN_DMPAPER_P32KBIG                95
#define WIN_DMPAPER_PENV_1                 96
#define WIN_DMPAPER_PENV_2                 97
#define WIN_DMPAPER_PENV_3                 98
#define WIN_DMPAPER_PENV_4                 99
#define WIN_DMPAPER_PENV_5                 100
#define WIN_DMPAPER_PENV_6                 101
#define WIN_DMPAPER_PENV_7                 102
#define WIN_DMPAPER_PENV_8                 103
#define WIN_DMPAPER_PENV_9                 104
#define WIN_DMPAPER_PENV_10                105
#define WIN_DMPAPER_P16K_ROTATED           106
#define WIN_DMPAPER_P32K_ROTATED           107
#define WIN_DMPAPER_P32KBIG_ROTATED        108
#define WIN_DMPAPER_PENV_1_ROTATED         109
#define WIN_DMPAPER_PENV_2_ROTATED         110
#define WIN_DMPAPER_PENV_3_ROTATED         111
#define WIN_DMPAPER_PENV_4_ROTATED         112
#define WIN_DMPAPER_PENV_5_ROTATED         113
#define WIN_DMPAPER_PENV_6_ROTATED         114
#define WIN_DMPAPER_PENV_7_ROTATED         115
#define WIN_DMPAPER_PENV_8_ROTATED         116
#define WIN_DMPAPER_PENV_9_ROTATED         117
#define WIN_DMPAPER_PENV_10_ROTATED        118
#define WIN_DMPAPER_USER                   256

/* win_SetDocumentProperties() bin selections */
#define WIN_DMBIN_UPPER                    1
#define WIN_DMBIN_ONLYONE                  1
#define WIN_DMBIN_LOWER                    2
#define WIN_DMBIN_MIDDLE                   3
#define WIN_DMBIN_MANUAL                   4
#define WIN_DMBIN_ENVELOPE                 5
#define WIN_DMBIN_ENVMANUAL                6
#define WIN_DMBIN_AUTO                     7
#define WIN_DMBIN_TRACTOR                  8
#define WIN_DMBIN_SMALLFMT                 9
#define WIN_DMBIN_LARGEFMT                 10
#define WIN_DMBIN_LARGECAPACITY            11
#define WIN_DMBIN_CASSETTE                 14
#define WIN_DMBIN_FORMSOURCE               15
#define WIN_DMBIN_USER                     256
#define WIN_DMBIN_FIRST                    WIN_DMBIN_UPPER
#define WIN_DMBIN_LAST                     WIN_DMBIN_FORMSOURCE

/* win_SetDocumentProperties() print qualities */
#define WIN_DMRES_DRAFT                    ( -1 )
#define WIN_DMRES_LOW                      ( -2 )
#define WIN_DMRES_MEDIUM                   ( -3 )
#define WIN_DMRES_HIGH                     ( -4 )

/* win_SetDocumentProperties() duplex modes */
#define WIN_DMDUP_SIMPLEX                  1
#define WIN_DMDUP_VERTICAL                 2
#define WIN_DMDUP_HORIZONTAL               3

/* __wapi_DEVMODE_*() "dmOrientation" modes */
#define WIN_DMORIENT_PORTRAIT              1
#define WIN_DMORIENT_LANDSCAPE             2

/* wapi_SetMapMode() parameters */
#define WIN_MM_TEXT                        1
#define WIN_MM_LOMETRIC                    2
#define WIN_MM_HIMETRIC                    3
#define WIN_MM_LOENGLISH                   4
#define WIN_MM_HIENGLISH                   5
#define WIN_MM_TWIPS                       6
#define WIN_MM_ISOTROPIC                   7
#define WIN_MM_ANISOTROPIC                 8
#define WIN_MM_MAX_FIXEDSCALE              WIN_MM_TWIPS

/* wapi_CreateFont() font weights */
#define WIN_FW_DONTCARE                    0
#define WIN_FW_THIN                        100
#define WIN_FW_EXTRALIGHT                  200
#define WIN_FW_ULTRALIGHT                  WIN_FW_EXTRALIGHT
#define WIN_FW_LIGHT                       300
#define WIN_FW_NORMAL                      400
#define WIN_FW_REGULAR                     WIN_FW_NORMAL
#define WIN_FW_MEDIUM                      500
#define WIN_FW_SEMIBOLD                    600
#define WIN_FW_DEMIBOLD                    WIN_FW_SEMIBOLD
#define WIN_FW_BOLD                        700
#define WIN_FW_EXTRABOLD                   800
#define WIN_FW_ULTRABOLD                   WIN_FW_EXTRABOLD
#define WIN_FW_HEAVY                       900
#define WIN_FW_BLACK                       WIN_FW_HEAVY

/* wapi_CreateFont() font qualities */
#define WIN_DEFAULT_QUALITY                0
#define WIN_DRAFT_QUALITY                  1
#define WIN_PROOF_QUALITY                  2
#define WIN_NONANTIALIASED_QUALITY         3  /* WINVER >= 0x0400 */
#define WIN_ANTIALIASED_QUALITY            4  /* WINVER >= 0x0400 */

/* wapi_CreateFont() output precision values */
#define WIN_OUT_DEFAULT_PRECIS             0
#define WIN_OUT_STRING_PRECIS              1
#define WIN_OUT_CHARACTER_PRECIS           2
#define WIN_OUT_STROKE_PRECIS              3
#define WIN_OUT_TT_PRECIS                  4
#define WIN_OUT_DEVICE_PRECIS              5
#define WIN_OUT_RASTER_PRECIS              6
#define WIN_OUT_TT_ONLY_PRECIS             7
#define WIN_OUT_OUTLINE_PRECIS             8
#define WIN_OUT_PS_ONLY_PRECIS             10

/* wapi_SetBkMode() modes */
#define WIN_TRANSPARENT                    1
#define WIN_OPAQUE                         2

/* wapi_ExtTextOut() options */
#define WIN_ETO_OPAQUE                     0x00002
#define WIN_ETO_CLIPPED                    0x00004
#define WIN_ETO_GLYPH_INDEX                0x00010
#define WIN_ETO_RTLREADING                 0x00080
#define WIN_ETO_NUMERICSLOCAL              0x00400
#define WIN_ETO_NUMERICSLATIN              0x00800
#define WIN_ETO_IGNORELANGUAGE             0x01000
#define WIN_ETO_PDY                        0x02000
#define WIN_ETO_REVERSE_INDEX_MAP          0x10000

/* wapi_DrawText() format methods */
#define WIN_DT_TOP                         0x000000
#define WIN_DT_LEFT                        0x000000
#define WIN_DT_CENTER                      0x000001
#define WIN_DT_RIGHT                       0x000002
#define WIN_DT_VCENTER                     0x000004
#define WIN_DT_BOTTOM                      0x000008
#define WIN_DT_WORDBREAK                   0x000010
#define WIN_DT_SINGLELINE                  0x000020
#define WIN_DT_EXPANDTABS                  0x000040
#define WIN_DT_TABSTOP                     0x000080
#define WIN_DT_NOCLIP                      0x000100
#define WIN_DT_EXTERNALLEADING             0x000200
#define WIN_DT_CALCRECT                    0x000400
#define WIN_DT_NOPREFIX                    0x000800
#define WIN_DT_INTERNAL                    0x001000
#define WIN_DT_EDITCONTROL                 0x002000
#define WIN_DT_PATH_ELLIPSIS               0x004000
#define WIN_DT_END_ELLIPSIS                0x008000
#define WIN_DT_MODIFYSTRING                0x010000
#define WIN_DT_RTLREADING                  0x020000
#define WIN_DT_WORD_ELLIPSIS               0x040000
#define WIN_DT_NOFULLWIDTHCHARBREAK        0x080000
#define WIN_DT_HIDEPREFIX                  0x100000
#define WIN_DT_PREFIXONLY                  0x200000

/* wapi_CreateHatchBrush() styles */
#define WIN_HS_HORIZONTAL                  0  /* ----- */
#define WIN_HS_VERTICAL                    1  /* ||||| */
#define WIN_HS_FDIAGONAL                   2  /* \\\\\ */
#define WIN_HS_BDIAGONAL                   3  /* ///// */
#define WIN_HS_CROSS                       4  /* +++++ */
#define WIN_HS_DIAGCROSS                   5  /* xxxxx */

/* wapi_AddFontResourceEx()/wapi_RemoveFontResourceEx() flags */
#define WIN_FR_PRIVATE                     0x10
#define WIN_FR_NOT_ENUM                    0x20

/* Windows charset values (wapi_CreateFont()) */
#define WIN_ANSI_CHARSET                   0
#define WIN_DEFAULT_CHARSET                1
#define WIN_SYMBOL_CHARSET                 2
#define WIN_MAC_CHARSET                    77
#define WIN_SHIFTJIS_CHARSET               128
#define WIN_HANGEUL_CHARSET                129
#define WIN_HANGUL_CHARSET                 129
#define WIN_JOHAB_CHARSET                  130
#define WIN_GB2312_CHARSET                 134
#define WIN_CHINESEBIG5_CHARSET            136
#define WIN_GREEK_CHARSET                  161
#define WIN_TURKISH_CHARSET                162
#define WIN_VIETNAMESE_CHARSET             163
#define WIN_HEBREW_CHARSET                 177
#define WIN_ARABIC_CHARSET                 178
#define WIN_BALTIC_CHARSET                 186
#define WIN_RUSSIAN_CHARSET                204
#define WIN_THAI_CHARSET                   222
#define WIN_EASTEUROPE_CHARSET             238
#define WIN_OEM_CHARSET                    255

/* wapi_GetDeviceCaps() parameters */
#define WIN_DRIVERVERSION                  0     /* Device driver version */
#define WIN_TECHNOLOGY                     2     /* Device classification */
#define WIN_HORZSIZE                       4     /* Horizontal size in millimeters */
#define WIN_VERTSIZE                       6     /* Vertical size in millimeters */
#define WIN_HORZRES                        8     /* Horizontal width in pixels */
#define WIN_VERTRES                        10    /* Vertical height in pixels */
#define WIN_BITSPIXEL                      12    /* Number of bits per pixel */
#define WIN_PLANES                         14    /* Number of planes */
#define WIN_NUMBRUSHES                     16    /* Number of brushes the device has */
#define WIN_NUMPENS                        18    /* Number of pens the device has */
#define WIN_NUMMARKERS                     20    /* Number of markers the device has */
#define WIN_NUMFONTS                       22    /* Number of fonts the device has */
#define WIN_NUMCOLORS                      24    /* Number of colors the device supports */
#define WIN_PDEVICESIZE                    26    /* Size required for device descriptor */
#define WIN_CURVECAPS                      28    /* Curve capabilities */
#define WIN_LINECAPS                       30    /* Line capabilities */
#define WIN_POLYGONALCAPS                  32    /* Polygonal capabilities */
#define WIN_TEXTCAPS                       34    /* Text capabilities */
#define WIN_CLIPCAPS                       36    /* Clipping capabilities */
#define WIN_RASTERCAPS                     38    /* Bitblt capabilities */
#define WIN_ASPECTX                        40    /* Length of the X leg */
#define WIN_ASPECTY                        42    /* Length of the Y leg */
#define WIN_ASPECTXY                       44    /* Length of the hypotenuse */
#define WIN_LOGPIXELSX                     88    /* Logical pixels/inch in X */
#define WIN_LOGPIXELSY                     90    /* Logical pixels/inch in Y */
#define WIN_SIZEPALETTE                    104   /* Number of entries in physical palette */
#define WIN_NUMRESERVED                    106   /* Number of reserved entries in palette */
#define WIN_COLORRES                       108   /* Actual color resolution */
#define WIN_PHYSICALWIDTH                  110   /* Physical Width in device units */
#define WIN_PHYSICALHEIGHT                 111   /* Physical Height in device units */
#define WIN_PHYSICALOFFSETX                112   /* Physical Printable Area x margin */
#define WIN_PHYSICALOFFSETY                113   /* Physical Printable Area y margin */
#define WIN_SCALINGFACTORX                 114   /* Scaling factor x */
#define WIN_SCALINGFACTORY                 115   /* Scaling factor y */

/* wapi_GetSystemMetrics() parameters */
#define WIN_SM_CXSCREEN                    0
#define WIN_SM_CYSCREEN                    1
#define WIN_SM_CXVSCROLL                   2
#define WIN_SM_CYHSCROLL                   3
#define WIN_SM_CYCAPTION                   4
#define WIN_SM_CXBORDER                    5
#define WIN_SM_CYBORDER                    6
#define WIN_SM_CXDLGFRAME                  7
#define WIN_SM_CXFIXEDFRAME                7
#define WIN_SM_CYDLGFRAME                  8
#define WIN_SM_CYFIXEDFRAME                8
#define WIN_SM_CYVTHUMB                    9
#define WIN_SM_CXHTHUMB                    10
#define WIN_SM_CXICON                      11
#define WIN_SM_CYICON                      12
#define WIN_SM_CXCURSOR                    13
#define WIN_SM_CYCURSOR                    14
#define WIN_SM_CYMENU                      15
#define WIN_SM_CXFULLSCREEN                16
#define WIN_SM_CYFULLSCREEN                17
#define WIN_SM_CYKANJIWINDOW               18
#define WIN_SM_MOUSEPRESENT                19
#define WIN_SM_CYVSCROLL                   20
#define WIN_SM_CXHSCROLL                   21
#define WIN_SM_DEBUG                       22
#define WIN_SM_SWAPBUTTON                  23
#define WIN_SM_RESERVED1                   24
#define WIN_SM_RESERVED2                   25
#define WIN_SM_RESERVED3                   26
#define WIN_SM_RESERVED4                   27
#define WIN_SM_CXMIN                       28
#define WIN_SM_CYMIN                       29
#define WIN_SM_CXSIZE                      30
#define WIN_SM_CYSIZE                      31
#define WIN_SM_CXSIZEFRAME                 32
#define WIN_SM_CXFRAME                     32
#define WIN_SM_CYSIZEFRAME                 33
#define WIN_SM_CYFRAME                     33
#define WIN_SM_CXMINTRACK                  34
#define WIN_SM_CYMINTRACK                  35
#define WIN_SM_CXDOUBLECLK                 36
#define WIN_SM_CYDOUBLECLK                 37
#define WIN_SM_CXICONSPACING               38
#define WIN_SM_CYICONSPACING               39
#define WIN_SM_MENUDROPALIGNMENT           40
#define WIN_SM_PENWINDOWS                  41
#define WIN_SM_DBCSENABLED                 42
#define WIN_SM_CMOUSEBUTTONS               43
#define WIN_SM_SECURE                      44
#define WIN_SM_CXEDGE                      45
#define WIN_SM_CYEDGE                      46
#define WIN_SM_CXMINSPACING                47
#define WIN_SM_CYMINSPACING                48
#define WIN_SM_CXSMICON                    49
#define WIN_SM_CYSMICON                    50
#define WIN_SM_CYSMCAPTION                 51
#define WIN_SM_CXSMSIZE                    52
#define WIN_SM_CYSMSIZE                    53
#define WIN_SM_CXMENUSIZE                  54
#define WIN_SM_CYMENUSIZE                  55
#define WIN_SM_ARRANGE                     56
#define WIN_SM_CXMINIMIZED                 57
#define WIN_SM_CYMINIMIZED                 58
#define WIN_SM_CXMAXTRACK                  59
#define WIN_SM_CYMAXTRACK                  60
#define WIN_SM_CXMAXIMIZED                 61
#define WIN_SM_CYMAXIMIZED                 62
#define WIN_SM_NETWORK                     63
#define WIN_SM_CLEANBOOT                   67
#define WIN_SM_CXDRAG                      68
#define WIN_SM_CYDRAG                      69
#define WIN_SM_SHOWSOUNDS                  70
#define WIN_SM_CXMENUCHECK                 71
#define WIN_SM_CYMENUCHECK                 72
#define WIN_SM_SLOWMACHINE                 73
#define WIN_SM_MIDEASTENABLED              74
#define WIN_SM_MOUSEWHEELPRESENT           75
#define WIN_SM_XVIRTUALSCREEN              76
#define WIN_SM_YVIRTUALSCREEN              77
#define WIN_SM_CXVIRTUALSCREEN             78
#define WIN_SM_CYVIRTUALSCREEN             79
#define WIN_SM_CMONITORS                   80
#define WIN_SM_SAMEDISPLAYFORMAT           81
#define WIN_SM_IMMENABLED                  82
#define WIN_SM_CXFOCUSBORDER               83
#define WIN_SM_CYFOCUSBORDER               84
#define WIN_SM_TABLETPC                    86
#define WIN_SM_MEDIACENTER                 87
#define WIN_SM_STARTER                     88
#define WIN_SM_SERVERR2                    89
#define WIN_SM_REMOTESESSION               4096
#define WIN_SM_SHUTTINGDOWN                8192
#define WIN_SM_REMOTECONTROL               8193
#define WIN_SM_CARETBLINKINGENABLED        8194

/* wapi_TextOut() text alignment modes */
#define WIN_TA_NOUPDATECP                  0
#define WIN_TA_UPDATECP                    1
#define WIN_TA_LEFT                        0
#define WIN_TA_RIGHT                       2
#define WIN_TA_CENTER                      6
#define WIN_TA_TOP                         0
#define WIN_TA_BOTTOM                      8
#define WIN_TA_BASELINE                    24
#define WIN_TA_RTLREADING                  256
#define WIN_TA_MASK                        ( TA_BASELINE + TA_CENTER + TA_UPDATECP + TA_RTLREADING )

/* win_SetPen() pen styles */
#define WIN_PS_SOLID                       0
#define WIN_PS_DASH                        1  /* ------- */
#define WIN_PS_DOT                         2  /* ....... */
#define WIN_PS_DASHDOT                     3  /* _._._._ */
#define WIN_PS_DASHDOTDOT                  4  /* _.._.._ */
#define WIN_PS_NULL                        5
#define WIN_PS_INSIDEFRAME                 6
#define WIN_PS_USERSTYLE                   7
#define WIN_PS_ALTERNATE                   8
#define WIN_PS_STYLE_MASK                  15

/* win_GetOpenFileName()/win_GetSaveFileName() */
#define WIN_OFN_READONLY                   0x00000001
#define WIN_OFN_OVERWRITEPROMPT            0x00000002
#define WIN_OFN_HIDEREADONLY               0x00000004
#define WIN_OFN_NOCHANGEDIR                0x00000008
#define WIN_OFN_SHOWHELP                   0x00000010
#define WIN_OFN_ENABLEHOOK                 0x00000020
#define WIN_OFN_ENABLETEMPLATE             0x00000040
#define WIN_OFN_ENABLETEMPLATEHANDLE       0x00000080
#define WIN_OFN_NOVALIDATE                 0x00000100
#define WIN_OFN_ALLOWMULTISELECT           0x00000200
#define WIN_OFN_EXTENSIONDIFFERENT         0x00000400
#define WIN_OFN_PATHMUSTEXIST              0x00000800
#define WIN_OFN_FILEMUSTEXIST              0x00001000
#define WIN_OFN_CREATEPROMPT               0x00002000
#define WIN_OFN_SHAREAWARE                 0x00004000
#define WIN_OFN_NOREADONLYRETURN           0x00008000
#define WIN_OFN_NOTESTFILECREATE           0x00010000
#define WIN_OFN_NONETWORKBUTTON            0x00020000
#define WIN_OFN_NOLONGNAMES                0x00040000
#define WIN_OFN_EXPLORER                   0x00080000
#define WIN_OFN_NODEREFERENCELINKS         0x00100000
#define WIN_OFN_LONGNAMES                  0x00200000
#define WIN_OFN_ENABLEINCLUDENOTIFY        0x00400000
#define WIN_OFN_ENABLESIZING               0x00800000
#define WIN_OFN_DONTADDTORECENT            0x02000000
#define WIN_OFN_FORCESHOWHIDDEN            0x10000000

#define WIN_OFN_SHAREFALLTHROUGH           0x00000002
#define WIN_OFN_SHARENOWARN                0x00000001
#define WIN_OFN_SHAREWARN                  0x00000000

/* wapi_LoadImage() <nType> values */
#define WIN_IMAGE_BITMAP                   0
#define WIN_IMAGE_ICON                     1
#define WIN_IMAGE_CURSOR                   2
#define WIN_IMAGE_ENHMETAFILE              3

/* wapi_LoadImage() <nFlags> values */
#define WIN_LR_DEFAULTCOLOR                0x0000
#define WIN_LR_MONOCHROME                  0x0001
#define WIN_LR_COLOR                       0x0002
#define WIN_LR_COPYRETURNORG               0x0004
#define WIN_LR_COPYDELETEORG               0x0008
#define WIN_LR_LOADFROMFILE                0x0010
#define WIN_LR_LOADTRANSPARENT             0x0020
#define WIN_LR_DEFAULTSIZE                 0x0040
#define WIN_LR_VGACOLOR                    0x0080
#define WIN_LR_LOADMAP3DCOLORS             0x1000
#define WIN_LR_CREATEDIBSECTION            0x2000
#define WIN_LR_COPYFROMRESOURCE            0x4000
#define WIN_LR_SHARED                      0x8000

/* win_ShellNotifyIcon() <nInfoFlags> values */
#define WIN_NIIF_NONE                      0x00000000
#define WIN_NIIF_INFO                      0x00000001
#define WIN_NIIF_WARNING                   0x00000002
#define WIN_NIIF_ERROR                     0x00000003
#define WIN_NIIF_USER                      0x00000004
#define WIN_NIIF_NOSOUND                   0x00000010
#define WIN_NIIF_LARGE_ICON                0x00000020
#define WIN_NIIF_RESPECT_QUIET_TIME        0x00000080
#define WIN_NIIF_ICON_MASK                 0x0000000F

/* constants for MENU functions */
#define WIN_MF_INSERT                      0x0000
#define WIN_MF_CHANGE                      0x0080
#define WIN_MF_APPEND                      0x0100
#define WIN_MF_DELETE                      0x0200
#define WIN_MF_REMOVE                      0x1000

#define WIN_MF_BYCOMMAND                   0x0000
#define WIN_MF_BYPOSITION                  0x0400

#define WIN_MF_SEPARATOR                   0x0800

#define WIN_MF_ENABLED                     0x0000
#define WIN_MF_GRAYED                      0x0001
#define WIN_MF_DISABLED                    0x0002

#define WIN_MF_UNCHECKED                   0x0000
#define WIN_MF_CHECKED                     0x0008
#define WIN_MF_USECHECKBITMAPS             0x0200

#define WIN_MF_STRING                      0x0000
#define WIN_MF_BITMAP                      0x0004
#define WIN_MF_OWNERDRAW                   0x0100

#define WIN_MF_POPUP                       0x0010
#define WIN_MF_MENUBARBREAK                0x0020
#define WIN_MF_MENUBREAK                   0x0040

#define WIN_MF_UNHILITE                    0x0000
#define WIN_MF_HILITE                      0x0080

#define WIN_MF_DEFAULT                     0x1000
#define WIN_MF_SYSMENU                     0x2000
#define WIN_MF_HELP                        0x4000
#define WIN_MF_END                         0x0080
#define WIN_MF_RIGHTJUSTIFY                0x4000
#define WIN_MF_MOUSESELECT                 0x8000

/* wapi_TrackPopupMenu() */
#define WIN_TPM_LEFTBUTTON                 0
#define WIN_TPM_RIGHTBUTTON                2

#define WIN_TPM_LEFTALIGN                  0
#define WIN_TPM_CENTERALIGN                4
#define WIN_TPM_RIGHTALIGN                 8

#define WIN_TPM_TOPALIGN                   0
#define WIN_TPM_VCENTERALIGN               16
#define WIN_TPM_BOTTOMALIGN                32

#define WIN_TPM_HORIZONTAL                 0
#define WIN_TPM_VERTICAL                   64
#define WIN_TPM_NONOTIFY                   128
#define WIN_TPM_RETURNCMD                  256
#define WIN_TPM_RECURSE                    1

/* wapi_PlaySound() flags */
#define WIN_SND_SYNC                       0x00000000
#define WIN_SND_ASYNC                      0x00000001
#define WIN_SND_NODEFAULT                  0x00000002
#define WIN_SND_MEMORY                     0x00000004
#define WIN_SND_LOOP                       0x00000008
#define WIN_SND_NOSTOP                     0x00000010
#define WIN_SND_NOWAIT                     0x00002000
#define WIN_SND_ALIAS                      0x00010000
#define WIN_SND_ALIAS_ID                   0x00110000
#define WIN_SND_FILENAME                   0x00020000
#define WIN_SND_RESOURCE                   0x00040004
#define WIN_SND_PURGE                      0x00000040
#define WIN_SND_APPLICATION                0x00000080
#define WIN_SND_SENTRY                     0x00080000
#define WIN_SND_RING                       0x00100000
#define WIN_SND_SYSTEM                     0x00200000
#define WIN_SND_ALIAS_START                0

/* wapi_SetWindowPos() flags */
#define WIN_SWP_NOSIZE                     0x0001
#define WIN_SWP_NOMOVE                     0x0002
#define WIN_SWP_NOZORDER                   0x0004
#define WIN_SWP_NOREDRAW                   0x0008
#define WIN_SWP_NOACTIVATE                 0x0010
#define WIN_SWP_FRAMECHANGED               0x0020
#define WIN_SWP_SHOWWINDOW                 0x0040
#define WIN_SWP_HIDEWINDOW                 0x0080
#define WIN_SWP_NOCOPYBITS                 0x0100
#define WIN_SWP_NOOWNERZORDER              0x0200
#define WIN_SWP_NOSENDCHANGING             0x0400
#define WIN_SWP_DEFERERASE                 0x2000
#define WIN_SWP_ASYNCWINDOWPOS             0x4000
#define WIN_SWP_DRAWFRAME                  WIN_SWP_FRAMECHANGED
#define WIN_SWP_NOREPOSITION               WIN_SWP_NOOWNERZORDER

/* wapi_SetWindowPos() predefined window IDs */
#define WIN_HWND_DESKTOP                   0
#define WIN_HWND_TOP                       0
#define WIN_HWND_BOTTOM                    1
#define WIN_HWND_TOPMOST                   -1
#define WIN_HWND_NOTOPMOST                 -2
#define WIN_HWND_MESSAGE                   -3
#define WIN_HWND_BROADCAST                 0xFFFF  /* for wapi_PostMessage() */

/* window messages */
#define WIN_WM_CREATE                      1
#define WIN_WM_DESTROY                     2
#define WIN_WM_MOVE                        3
#define WIN_WM_SIZE                        5
#define WIN_WM_ACTIVATE                    6
#define WIN_WM_SETFOCUS                    7
#define WIN_WM_KILLFOCUS                   8
#define WIN_WM_ENABLE                      10
#define WIN_WM_SETREDRAW                   11
#define WIN_WM_SETTEXT                     12
#define WIN_WM_GETTEXT                     13
#define WIN_WM_GETTEXTLENGTH               14
#define WIN_WM_PAINT                       15
#define WIN_WM_CLOSE                       16
#define WIN_WM_QUIT                        18
#define WIN_WM_ERASEBKGND                  20
#define WIN_WM_SYSCOLORCHANGE              21
#define WIN_WM_SHOWWINDOW                  24
#define WIN_WM_WININICHANGE                26
#define WIN_WM_DEVMODECHANGE               27
#define WIN_WM_ACTIVATEAPP                 28
#define WIN_WM_FONTCHANGE                  29
#define WIN_WM_TIMECHANGE                  30
#define WIN_WM_CANCELMODE                  31
#define WIN_WM_SETCURSOR                   32
#define WIN_WM_MOUSEACTIVATE               33
#define WIN_WM_CHILDACTIVATE               34
#define WIN_WM_QUEUESYNC                   35
#define WIN_WM_GETMINMAXINFO               36

#define WIN_WM_PAINTICON                   38
#define WIN_WM_ICONERASEBKGND              39
#define WIN_WM_NEXTDLGCTL                  40
#define WIN_WM_SPOOLERSTATUS               42
#define WIN_WM_DRAWITEM                    43
#define WIN_WM_MEASUREITEM                 44
#define WIN_WM_DELETEITEM                  45
#define WIN_WM_VKEYTOITEM                  46
#define WIN_WM_CHARTOITEM                  47
#define WIN_WM_SETFONT                     48
#define WIN_WM_GETFONT                     49
#define WIN_WM_SETHOTKEY                   50
#define WIN_WM_GETHOTKEY                   51
#define WIN_WM_QUERYDRAGICON               55
#define WIN_WM_COMPAREITEM                 57
#define WIN_WM_GETOBJECT                   61
#define WIN_WM_COMPACTING                  65
#define WIN_WM_COMMNOTIFY                  68  /* obsolete */
#define WIN_WM_WINDOWPOSCHANGING           70
#define WIN_WM_WINDOWPOSCHANGED            71
#define WIN_WM_POWER                       72
#define WIN_WM_NOTIFY                      78
#define WIN_WM_INPUTLANGCHANGEREQUEST      79
#define WIN_WM_INPUTLANGCHANGE             80
#define WIN_WM_TCARD                       81
#define WIN_WM_HELP                        82
#define WIN_WM_USERCHANGED                 83
#define WIN_WM_NOTIFYFORMAT                84

#define WIN_WM_CONTEXTMENU                 123
#define WIN_WM_STYLECHANGING               124
#define WIN_WM_STYLECHANGED                125
#define WIN_WM_DISPLAYCHANGE               126
#define WIN_WM_GETICON                     127
#define WIN_WM_SETICON                     128

#define WIN_WM_NCCREATE                    129
#define WIN_WM_NCDESTROY                   130
#define WIN_WM_NCCALCSIZE                  131
#define WIN_WM_NCHITTEST                   132
#define WIN_WM_NCPAINT                     133
#define WIN_WM_NCACTIVATE                  134
#define WIN_WM_GETDLGCODE                  135

#define WIN_WM_NCMOUSEMOVE                 160
#define WIN_WM_NCLBUTTONDOWN               161
#define WIN_WM_NCLBUTTONUP                 162
#define WIN_WM_NCLBUTTONDBLCLK             163
#define WIN_WM_NCRBUTTONDOWN               164
#define WIN_WM_NCRBUTTONUP                 165
#define WIN_WM_NCRBUTTONDBLCLK             166
#define WIN_WM_NCMBUTTONDOWN               167
#define WIN_WM_NCMBUTTONUP                 168
#define WIN_WM_NCMBUTTONDBLCLK             169

#define WIN_WM_KEYDOWN                     256
#define WIN_WM_KEYFIRST                    256
#define WIN_WM_KEYUP                       257
#define WIN_WM_CHAR                        258
#define WIN_WM_DEADCHAR                    259
#define WIN_WM_SYSKEYDOWN                  260
#define WIN_WM_SYSKEYUP                    261
#define WIN_WM_SYSCHAR                     262
#define WIN_WM_SYSDEADCHAR                 263
#define WIN_WM_KEYLAST                     264

#define WIN_WM_INITDIALOG                  272
#define WIN_WM_COMMAND                     273
#define WIN_WM_SYSCOMMAND                  274
#define WIN_WM_TIMER                       275
#define WIN_WM_HSCROLL                     276
#define WIN_WM_VSCROLL                     277

#define WIN_WM_INITMENUPOPUP               279
#define WIN_WM_MENUSELECT                  287
#define WIN_WM_MENUCOMMAND                 294

#define WIN_WM_CTLCOLORMSGBOX              306
#define WIN_WM_CTLCOLOREDIT                307
#define WIN_WM_CTLCOLORLISTBOX             308
#define WIN_WM_CTLCOLORBTN                 309
#define WIN_WM_CTLCOLORDLG                 310
#define WIN_WM_CTLCOLORSCROLLBAR           311
#define WIN_WM_CTLCOLORSTATIC              312

#define WIN_WM_MOUSEFIRST                  512
#define WIN_WM_MOUSEMOVE                   512
#define WIN_WM_LBUTTONDOWN                 513
#define WIN_WM_LBUTTONUP                   514
#define WIN_WM_LBUTTONDBLCLK               515
#define WIN_WM_RBUTTONDOWN                 516
#define WIN_WM_RBUTTONUP                   517
#define WIN_WM_RBUTTONDBLCLK               518
#define WIN_WM_MBUTTONDOWN                 519
#define WIN_WM_MBUTTONUP                   520
#define WIN_WM_MBUTTONDBLCLK               521
#define WIN_WM_MOUSEWHEEL                  522
#define WIN_WM_XBUTTONDOWN                 523
#define WIN_WM_XBUTTONUP                   524
#define WIN_WM_XBUTTONDBLCLK               525

#define WIN_WM_PARENTNOTIFY                528
#define WIN_WM_ENTERMENULOOP               529
#define WIN_WM_EXITMENULOOP                530

#define WIN_WM_NEXTMENU                    531
#define WIN_WM_SIZING                      532
#define WIN_WM_CAPTURECHANGED              533
#define WIN_WM_MOVING                      534
#define WIN_WM_POWERBROADCAST              536

#define WIN_WM_MDICREATE                   544
#define WIN_WM_MDIDESTROY                  545
#define WIN_WM_MDIACTIVATE                 546
#define WIN_WM_MDIRESTORE                  547
#define WIN_WM_MDINEXT                     548
#define WIN_WM_MDIMAXIMIZE                 549
#define WIN_WM_MDITILE                     550
#define WIN_WM_MDICASCADE                  551
#define WIN_WM_MDIICONARRANGE              552
#define WIN_WM_MDIGETACTIVE                553

#define WIN_WM_MDISETMENU                  560
#define WIN_WM_ENTERSIZEMOVE               561
#define WIN_WM_EXITSIZEMOVE                562
#define WIN_WM_DROPFILES                   563
#define WIN_WM_MDIREFRESHMENU              564

#define WIN_WM_MOUSEHOVER                  673
#define WIN_WM_MOUSELEAVE                  675

#define WIN_WM_CUT                         768
#define WIN_WM_COPY                        769
#define WIN_WM_PASTE                       770
#define WIN_WM_CLEAR                       771
#define WIN_WM_UNDO                        772
#define WIN_WM_RENDERFORMAT                773
#define WIN_WM_RENDERALLFORMATS            774
#define WIN_WM_DESTROYCLIPBOARD            775
#define WIN_WM_DRAWCLIPBOARD               776
#define WIN_WM_PAINTCLIPBOARD              777
#define WIN_WM_VSCROLLCLIPBOARD            778
#define WIN_WM_SIZECLIPBOARD               779
#define WIN_WM_ASKCBFORMATNAME             780
#define WIN_WM_CHANGECBCHAIN               781
#define WIN_WM_HSCROLLCLIPBOARD            782
#define WIN_WM_QUERYNEWPALETTE             783
#define WIN_WM_PALETTEISCHANGING           784
#define WIN_WM_PALETTECHANGED              785
#define WIN_WM_HOTKEY                      786

#define WIN_WM_USER                        1024

#define WIN_WM_CHOOSEFONT_GETLOGFONT       ( WIN_WM_USER + 1 )
#define WIN_WM_CHOOSEFONT_SETLOGFONT       ( WIN_WM_USER + 101 )
#define WIN_WM_CHOOSEFONT_SETFLAGS         ( WIN_WM_USER + 102 )

/* window styles */
#define WIN_WS_OVERLAPPED                  0x00000000
#define WIN_WS_TABSTOP                     0x00010000
#define WIN_WS_MAXIMIZEBOX                 0x00010000
#define WIN_WS_MINIMIZEBOX                 0x00020000
#define WIN_WS_GROUP                       0x00020000
#define WIN_WS_THICKFRAME                  0x00040000
#define WIN_WS_SYSMENU                     0x00080000
#define WIN_WS_HSCROLL                     0x00100000
#define WIN_WS_VSCROLL                     0x00200000
#define WIN_WS_DLGFRAME                    0x00400000
#define WIN_WS_BORDER                      0x00800000
#define WIN_WS_MAXIMIZE                    0x01000000
#define WIN_WS_CLIPCHILDREN                0x02000000
#define WIN_WS_CLIPSIBLINGS                0x04000000
#define WIN_WS_DISABLED                    0x08000000
#define WIN_WS_VISIBLE                     0x10000000
#define WIN_WS_MINIMIZE                    0x20000000
#define WIN_WS_CHILD                       0x40000000
#define WIN_WS_POPUP                       0x80000000
#define WIN_WS_TILED                       WIN_WS_OVERLAPPED
#define WIN_WS_ICONIC                      WIN_WS_MINIMIZE
#define WIN_WS_SIZEBOX                     WIN_WS_THICKFRAME
#define WIN_WS_TILEDWINDOW                 WIN_WS_OVERLAPPEDWINDOW
#define WIN_WS_CAPTION                     hb_bitOr( WIN_WS_BORDER, WIN_WS_DLGFRAME )
#define WIN_WS_OVERLAPPEDWINDOW            hb_bitOr( WIN_WS_OVERLAPPED, WIN_WS_CAPTION, WIN_WS_SYSMENU, WIN_WS_THICKFRAME, WIN_WS_MINIMIZEBOX, WIN_WS_MAXIMIZEBOX )
#define WIN_WS_POPUPWINDOW                 hb_bitOr( WIN_WS_POPUP, WIN_WS_BORDER, WIN_WS_SYSMENU )
#define WIN_WS_CHILDWINDOW                 WIN_WS_CHILD

/* window styles (extended) */
#define WIN_WS_EX_DLGMODALFRAME            0x00000001
#define WIN_WS_EX_NOPARENTNOTIFY           0x00000004
#define WIN_WS_EX_TOPMOST                  0x00000008
#define WIN_WS_EX_ACCEPTFILES              0x00000010
#define WIN_WS_EX_TRANSPARENT              0x00000020
#define WIN_WS_EX_MDICHILD                 0x00000040
#define WIN_WS_EX_TOOLWINDOW               0x00000080
#define WIN_WS_EX_WINDOWEDGE               0x00000100
#define WIN_WS_EX_CLIENTEDGE               0x00000200
#define WIN_WS_EX_CONTEXTHELP              0x00000400

#define WIN_WS_EX_RIGHT                    0x00001000
#define WIN_WS_EX_LEFT                     0x00000000
#define WIN_WS_EX_RTLREADING               0x00002000
#define WIN_WS_EX_LTRREADING               0x00000000
#define WIN_WS_EX_LEFTSCROLLBAR            0x00004000
#define WIN_WS_EX_RIGHTSCROLLBAR           0x00000000

#define WIN_WS_EX_CONTROLPARENT            0x00010000
#define WIN_WS_EX_STATICEDGE               0x00020000
#define WIN_WS_EX_APPWINDOW                0x00040000

#define WIN_WS_EX_OVERLAPPEDWINDOW         hb_bitOr( WIN_WS_EX_WINDOWEDGE, WIN_WS_EX_CLIENTEDGE )
#define WIN_WS_EX_PALETTEWINDOW            hb_bitOr( WIN_WS_EX_WINDOWEDGE, WIN_WS_EX_TOOLWINDOW, WIN_WS_EX_TOPMOST )

#define WIN_WS_EX_LAYERED                  0x00080000
#define WIN_WS_EX_NOINHERITLAYOUT          0x00100000
#define WIN_WS_EX_LAYOUTRTL                0x00400000
#define WIN_WS_EX_NOACTIVATE               0x08000000

/* win_ReportEvent() event types */
#define WIN_EVENTLOG_SUCCESS               0x0000
#define WIN_EVENTLOG_ERROR_TYPE            0x0001
#define WIN_EVENTLOG_WARNING_TYPE          0x0002
#define WIN_EVENTLOG_INFORMATION_TYPE      0x0004
#define WIN_EVENTLOG_AUDIT_SUCCESS         0x0008
#define WIN_EVENTLOG_AUDIT_FAILURE         0x0010

/* win_serviceGetStatus() return values */
#define WIN_SERVICE_NO_CHANGE              0xFFFFFFFF
#define WIN_SERVICE_STOPPED                1
#define WIN_SERVICE_START_PENDING          2
#define WIN_SERVICE_STOP_PENDING           3
#define WIN_SERVICE_RUNNING                4
#define WIN_SERVICE_CONTINUE_PENDING       5
#define WIN_SERVICE_PAUSE_PENDING          6
#define WIN_SERVICE_PAUSED                 7

/* win_serviceInstall() start types */
#define WIN_SERVICE_AUTO_START             0x00000002
#define WIN_SERVICE_BOOT_START             0x00000000
#define WIN_SERVICE_DEMAND_START           0x00000003
#define WIN_SERVICE_DISABLED               0x00000004
#define WIN_SERVICE_SYSTEM_START           0x00000001

/* win_serviceControl() control codes */
#define WIN_SERVICE_CONTROL_STOP           0x00000001
#define WIN_SERVICE_CONTROL_PAUSE          0x00000002
#define WIN_SERVICE_CONTROL_CONTINUE       0x00000003
#define WIN_SERVICE_CONTROL_INTERROGATE    0x00000004
#define WIN_SERVICE_CONTROL_PARAMCHANGE    0x00000006
#define WIN_SERVICE_CONTROL_NETBINDADD     0x00000007
#define WIN_SERVICE_CONTROL_NETBINDREMOVE  0x00000008
#define WIN_SERVICE_CONTROL_NETBINDENABLE  0x00000009
#define WIN_SERVICE_CONTROL_NETBINDDISABLE 0x0000000A

/* wapi_MessageBox()/wapi_MessageBeep() flags */
#define WIN_MB_OK                          0x00000000
#define WIN_MB_OKCANCEL                    0x00000001
#define WIN_MB_ABORTRETRYIGNORE            0x00000002
#define WIN_MB_YESNOCANCEL                 0x00000003
#define WIN_MB_YESNO                       0x00000004
#define WIN_MB_RETRYCANCEL                 0x00000005
#define WIN_MB_CANCELTRYCONTINUE           0x00000006

#define WIN_MB_ICONHAND                    0x00000010
#define WIN_MB_ICONQUESTION                0x00000020
#define WIN_MB_ICONEXCLAMATION             0x00000030
#define WIN_MB_ICONASTERISK                0x00000040
#define WIN_MB_USERICON                    0x00000080
#define WIN_MB_ICONWARNING                 WIN_MB_ICONEXCLAMATION
#define WIN_MB_ICONERROR                   WIN_MB_ICONHAND
#define WIN_MB_ICONINFORMATION             WIN_MB_ICONASTERISK
#define WIN_MB_ICONSTOP                    WIN_MB_ICONHAND

#define WIN_MB_DEFBUTTON1                  0x00000000
#define WIN_MB_DEFBUTTON2                  0x00000100
#define WIN_MB_DEFBUTTON3                  0x00000200
#define WIN_MB_DEFBUTTON4                  0x00000300

#define WIN_MB_APPLMODAL                   0x00000000
#define WIN_MB_SYSTEMMODAL                 0x00001000
#define WIN_MB_TASKMODAL                   0x00002000
#define WIN_MB_HELP                        0x00004000

#define WIN_MB_NOFOCUS                     0x00008000
#define WIN_MB_SETFOREGROUND               0x00010000
#define WIN_MB_DEFAULT_DESKTOP_ONLY        0x00020000

#define WIN_MB_TOPMOST                     0x00040000
#define WIN_MB_RIGHT                       0x00080000
#define WIN_MB_RTLREADING                  0x00100000

#define WIN_MB_TYPEMASK                    0x0000000F
#define WIN_MB_ICONMASK                    0x000000F0
#define WIN_MB_DEFMASK                     0x00000F00
#define WIN_MB_MODEMASK                    0x00003000
#define WIN_MB_MISCMASK                    0x0000C000

#define HB_WIN_MB_BEEP                     0xFFFFFFFF  /* Windows has no constant for that */

/* wapi_MessageBox() return values */
#define WIN_IDOK                           1
#define WIN_IDCANCEL                       2
#define WIN_IDABORT                        3
#define WIN_IDRETRY                        4
#define WIN_IDIGNORE                       5
#define WIN_IDYES                          6
#define WIN_IDNO                           7
#define WIN_IDTRYAGAIN                     10  /* WINVER >= 0x0500 */
#define WIN_IDCONTINUE                     11  /* WINVER >= 0x0500 */

/* win_SHFileOperation() functions */
#define WIN_FO_MOVE                        0x0001
#define WIN_FO_COPY                        0x0002
#define WIN_FO_DELETE                      0x0003
#define WIN_FO_RENAME                      0x0004

/* win_SHFileOperation() flags */
#define WIN_FOF_MULTIDESTFILES             0x0001
#define WIN_FOF_CONFIRMMOUSE               0x0002
#define WIN_FOF_SILENT                     0x0004
#define WIN_FOF_RENAMEONCOLLISION          0x0008
#define WIN_FOF_NOCONFIRMATION             0x0010
#define WIN_FOF_WANTMAPPINGHANDLE          0x0020
#define WIN_FOF_ALLOWUNDO                  0x0040
#define WIN_FOF_FILESONLY                  0x0080
#define WIN_FOF_SIMPLEPROGRESS             0x0100
#define WIN_FOF_NOCONFIRMMKDIR             0x0200
#define WIN_FOF_NOERRORUI                  0x0400
#define WIN_FOF_NOCOPYSECURITYATTRIBS      0x0800
#define WIN_FOF_NORECURSION                0x1000
#define WIN_FOF_NO_CONNECTED_ELEMENTS      0x2000
#define WIN_FOF_WANTNUKEWARNING            0x4000

/* win_SHFileOperation() results */
#define HB_WIN_DE_SAMEFILE                 0x71      /* The source and destination files are the same file. */
#define HB_WIN_DE_MANYSRC1DEST             0x72      /* Multiple file paths were specified in the source buffer, but only one destination file path. */
#define HB_WIN_DE_DIFFDIR                  0x73      /* Rename operation was specified but the destination path is a different directory. Use the move operation instead. */
#define HB_WIN_DE_ROOTDIR                  0x74      /* The source is a root directory, which cannot be moved or renamed. */
#define HB_WIN_DE_OPCANCELLED              0x75      /* The operation was cancelled by the user, or silently cancelled if the appropriate flags were supplied to SHFileOperation. */
#define HB_WIN_DE_DESTSUBTREE              0x76      /* The destination is a subtree of the source. */
#define HB_WIN_DE_ACCESSDENIEDSRC          0x78      /* Security settings denied access to the source. */
#define HB_WIN_DE_PATHTOODEEP              0x79      /* The source or destination path exceeded or would exceed MAX_PATH. */
#define HB_WIN_DE_MANYDEST                 0x7A      /* The operation involved multiple destination paths, which can fail in the case of a move operation. */
#define HB_WIN_DE_INVALIDFILES             0x7C      /* The path in the source or destination or both was invalid. */
#define HB_WIN_DE_DESTSAMETREE             0x7D      /* The source and destination have the same parent directory. */
#define HB_WIN_DE_FLDDESTISFILE            0x7E      /* The destination path is an existing file. */
#define HB_WIN_DE_FILEDESTISFLD            0x80      /* The destination path is an existing directory. */
#define HB_WIN_DE_FILENAMETOOLONG          0x81      /* The name of the file exceeds MAX_PATH. */
#define HB_WIN_DE_DEST_IS_CDROM            0x82      /* The destination is a read-only CD-ROM, possibly unformatted. */
#define HB_WIN_DE_DEST_IS_DVD              0x83      /* The destination is a read-only DVD, possibly unformatted. */
#define HB_WIN_DE_DEST_IS_CDRECORD         0x84      /* The destination is a writable CD-ROM, possibly unformatted. */
#define HB_WIN_DE_FILE_TOO_LARGE           0x85      /* The file involved in the operation is too large for the destination media or file system. */
#define HB_WIN_DE_SRC_IS_CDROM             0x86      /* The source is a read-only CD-ROM, possibly unformatted. */
#define HB_WIN_DE_SRC_IS_DVD               0x87      /* The source is a read-only DVD, possibly unformatted. */
#define HB_WIN_DE_SRC_IS_CDRECORD          0x88      /* The source is a writable CD-ROM, possibly unformatted. */
#define HB_WIN_DE_ERROR_MAX                0xB7      /* MAX_PATH was exceeded during the operation. */
#define HB_WIN_DE_UNKNOWN_                 0x402     /* An unknown error occurred. This is typically due to an invalid path in the source or destination. This error does not occur on Windows Vista and later. */
#define HB_WIN_DE_ERRORONDEST_             0x10000   /* An unspecified error occurred on the destination. */
#define HB_WIN_DE_ROOTDIR_ERRORONDEST_     0x10074   /* Destination is a root directory and cannot be renamed. */

/* RPC status code (win_UuidCreateString() first reference parameter) */
#define HB_RPC_S_ERROR                     ( -1 )
#define HB_RPC_S_OK                        0
#define HB_RPC_S_ACCESS_DENIED             5
#define HB_RPC_S_OUT_OF_MEMORY             14
#define HB_RPC_S_INVALID_ARG               87
#define HB_RPC_S_INVALID_LEVEL             87
#define HB_RPC_S_BUFFER_TOO_SMALL          122
#define HB_RPC_S_OUT_OF_THREADS            164
#define HB_RPC_S_ASYNC_CALL_PENDING        997
#define HB_RPC_S_SERVER_OUT_OF_MEMORY      1130
#define HB_RPC_S_UNKNOWN_PRINCIPAL         1332
#define HB_RPC_S_INVALID_SECURITY_DESC     1338
#define HB_RPC_S_TIMEOUT                   1460

/* Resource types (win_LoadResource() second parameter) */
#define WIN_RT_CURSOR                      1
#define WIN_RT_BITMAP                      2
#define WIN_RT_ICON                        3
#define WIN_RT_MENU                        4
#define WIN_RT_DIALOG                      5
#define WIN_RT_STRING                      6
#define WIN_RT_FONTDIR                     7
#define WIN_RT_FONT                        8
#define WIN_RT_ACCELERATOR                 9
#define WIN_RT_RCDATA                      10
#define WIN_RT_MESSAGETABLE                11
#define WIN_RT_GROUP_CURSOR                12
#define WIN_RT_GROUP_ICON                  14
#define WIN_RT_VERSION                     16
#define WIN_RT_DLGINCLUDE                  17
#define WIN_RT_PLUGPLAY                    19
#define WIN_RT_VXD                         20
#define WIN_RT_ANICURSOR                   21
#define WIN_RT_ANIICON                     22
#define WIN_RT_HTML                        23

/* Color indexes (wapi_GetSysColor() parameter) */
#define WIN_CTLCOLOR_MSGBOX                0
#define WIN_CTLCOLOR_EDIT                  1
#define WIN_CTLCOLOR_LISTBOX               2
#define WIN_CTLCOLOR_BTN                   3
#define WIN_CTLCOLOR_DLG                   4
#define WIN_CTLCOLOR_SCROLLBAR             5
#define WIN_CTLCOLOR_STATIC                6
#define WIN_CTLCOLOR_MAX                   7
#define WIN_COLOR_SCROLLBAR                0
#define WIN_COLOR_BACKGROUND               1
#define WIN_COLOR_ACTIVECAPTION            2
#define WIN_COLOR_INACTIVECAPTION          3
#define WIN_COLOR_MENU                     4
#define WIN_COLOR_WINDOW                   5
#define WIN_COLOR_WINDOWFRAME              6
#define WIN_COLOR_MENUTEXT                 7
#define WIN_COLOR_WINDOWTEXT               8
#define WIN_COLOR_CAPTIONTEXT              9
#define WIN_COLOR_ACTIVEBORDER             10
#define WIN_COLOR_INACTIVEBORDER           11
#define WIN_COLOR_APPWORKSPACE             12
#define WIN_COLOR_HIGHLIGHT                13
#define WIN_COLOR_HIGHLIGHTTEXT            14
#define WIN_COLOR_BTNFACE                  15
#define WIN_COLOR_BTNSHADOW                16
#define WIN_COLOR_GRAYTEXT                 17
#define WIN_COLOR_BTNTEXT                  18
#define WIN_COLOR_INACTIVECAPTIONTEXT      19
#define WIN_COLOR_BTNHIGHLIGHT             20
#define WIN_COLOR_3DDKSHADOW               21
#define WIN_COLOR_3DLIGHT                  22
#define WIN_COLOR_INFOTEXT                 23
#define WIN_COLOR_INFOBK                   24
#define WIN_COLOR_HOTLIGHT                 26
#define WIN_COLOR_GRADIENTACTIVECAPTION    27
#define WIN_COLOR_GRADIENTINACTIVECAPTION  28
#define WIN_COLOR_DESKTOP                  WIN_COLOR_BACKGROUND
#define WIN_COLOR_3DFACE                   WIN_COLOR_BTNFACE
#define WIN_COLOR_3DSHADOW                 WIN_COLOR_BTNSHADOW
#define WIN_COLOR_3DHIGHLIGHT              WIN_COLOR_BTNHIGHLIGHT
#define WIN_COLOR_3DHILIGHT                WIN_COLOR_BTNHIGHLIGHT
#define WIN_COLOR_BTNHILIGHT               WIN_COLOR_BTNHIGHLIGHT

/* wapi_ShowWindow() commands */
#define WIN_SW_FORCEMINIMIZE               11
#define WIN_SW_HIDE                        0
#define WIN_SW_MAXIMIZE                    3
#define WIN_SW_MINIMIZE                    6
#define WIN_SW_NORMAL                      WIN_SW_SHOWNORMAL
#define WIN_SW_RESTORE                     9
#define WIN_SW_SHOW                        5
#define WIN_SW_SHOWDEFAULT                 10
#define WIN_SW_SHOWMAXIMIZED               3
#define WIN_SW_SHOWMINIMIZED               2
#define WIN_SW_SHOWMINNOACTIVE             7
#define WIN_SW_SHOWNA                      8
#define WIN_SW_SHOWNOACTIVATE              4
#define WIN_SW_SHOWNORMAL                  1
#define WIN_SW_MAX                         11

/* System Menu command values */
#define WIN_SC_SIZE                        0xF000
#define WIN_SC_MOVE                        0xF010
#define WIN_SC_MINIMIZE                    0xF020
#define WIN_SC_MAXIMIZE                    0xF030
#define WIN_SC_NEXTWINDOW                  0xF040
#define WIN_SC_PREVWINDOW                  0xF050
#define WIN_SC_CLOSE                       0xF060
#define WIN_SC_VSCROLL                     0xF070
#define WIN_SC_HSCROLL                     0xF080
#define WIN_SC_MOUSEMENU                   0xF090
#define WIN_SC_KEYMENU                     0xF100
#define WIN_SC_ARRANGE                     0xF110
#define WIN_SC_RESTORE                     0xF120
#define WIN_SC_TASKLIST                    0xF130
#define WIN_SC_SCREENSAVE                  0xF140
#define WIN_SC_HOTKEY                      0xF150
#define WIN_SC_DEFAULT                     0xF160
#define WIN_SC_MONITORPOWER                0xF170
#define WIN_SC_CONTEXTHELP                 0xF180
#define WIN_SC_SEPARATOR                   0xF00F

/* wapi_SendMessageTimeout() flag values */
#define WIN_SMTO_NORMAL                    0x0000
#define WIN_SMTO_BLOCK                     0x0001
#define WIN_SMTO_ABORTIFHUNG               0x0002
#define WIN_SMTO_NOTIMEOUTIFNOTHUNG        0x0008
#define WIN_SMTO_ERRORONEXIT               0x0020

/* wapi_SetWindowLongPtr()/wapi_GetWindowLongPtr() index values */
#define WIN_GWLP_WNDPROC                   ( -4 )
#define WIN_GWLP_HINSTANCE                 ( -6 )
#define WIN_GWLP_HWNDPARENT                ( -8 )
#define WIN_GWL_ID                         ( -12 )
#define WIN_GWL_STYLE                      ( -16 )
#define WIN_GWL_EXSTYLE                    ( -20 )
#define WIN_GWLP_USERDATA                  ( -21 )
#define WIN_DWLP_MSGRESULT                 0
#define WIN_DWLP_DLGPROC                   4
#define WIN_DWLP_USER                      8

/* Virtual Key Codes */
#define WIN_VK_LBUTTON                     1
#define WIN_VK_RBUTTON                     2
#define WIN_VK_CANCEL                      3
#define WIN_VK_MBUTTON                     4
#define WIN_VK_BACK                        8
#define WIN_VK_TAB                         9
#define WIN_VK_CLEAR                       12
#define WIN_VK_RETURN                      13
#define WIN_VK_SHIFT                       16
#define WIN_VK_CONTROL                     17
#define WIN_VK_MENU                        18
#define WIN_VK_PAUSE                       19
#define WIN_VK_CAPITAL                     20
#define WIN_VK_ESCAPE                      27
#define WIN_VK_SPACE                       32
#define WIN_VK_PRIOR                       33
#define WIN_VK_NEXT                        34
#define WIN_VK_END                         35
#define WIN_VK_HOME                        36
#define WIN_VK_LEFT                        37
#define WIN_VK_UP                          38
#define WIN_VK_RIGHT                       39
#define WIN_VK_DOWN                        40
#define WIN_VK_SELECT                      41
#define WIN_VK_PRINT                       42
#define WIN_VK_EXECUTE                     43
#define WIN_VK_SNAPSHOT                    44
#define WIN_VK_INSERT                      45
#define WIN_VK_DELETE                      46
#define WIN_VK_HELP                        47
#define WIN_VK_NUMPAD0                     96
#define WIN_VK_NUMPAD1                     97
#define WIN_VK_NUMPAD2                     98
#define WIN_VK_NUMPAD3                     99
#define WIN_VK_NUMPAD4                     100
#define WIN_VK_NUMPAD5                     101
#define WIN_VK_NUMPAD6                     102
#define WIN_VK_NUMPAD7                     103
#define WIN_VK_NUMPAD8                     104
#define WIN_VK_NUMPAD9                     105
#define WIN_VK_MULTIPLY                    106
#define WIN_VK_ADD                         107
#define WIN_VK_SEPARATOR                   108
#define WIN_VK_SUBTRACT                    109
#define WIN_VK_DECIMAL                     110
#define WIN_VK_DIVIDE                      111
#define WIN_VK_F1                          112
#define WIN_VK_F2                          113
#define WIN_VK_F3                          114
#define WIN_VK_F4                          115
#define WIN_VK_F5                          116
#define WIN_VK_F6                          117
#define WIN_VK_F7                          118
#define WIN_VK_F8                          119
#define WIN_VK_F9                          120
#define WIN_VK_F10                         121
#define WIN_VK_F11                         122
#define WIN_VK_F12                         123
#define WIN_VK_F13                         124
#define WIN_VK_F14                         125
#define WIN_VK_F15                         126
#define WIN_VK_F16                         127
#define WIN_VK_F17                         128
#define WIN_VK_F18                         129
#define WIN_VK_F19                         130
#define WIN_VK_F20                         131
#define WIN_VK_F21                         132
#define WIN_VK_F22                         133
#define WIN_VK_F23                         134
#define WIN_VK_F24                         135
#define WIN_VK_NUMLOCK                     144
#define WIN_VK_SCROLL                      145

/* wapi_CreateBrushIndirect() brush styles */
#define WIN_BS_SOLID                       0
#define WIN_BS_NULL                        1
#define WIN_BS_HOLLOW                      WIN_BS_NULL
#define WIN_BS_HATCHED                     2
#define WIN_BS_PATTERN                     3
#define WIN_BS_INDEXED                     4
#define WIN_BS_DIBPATTERN                  5
#define WIN_BS_DIBPATTERNPT                6
#define WIN_BS_PATTERN8X8                  7
#define WIN_BS_DIBPATTERN8X8               8
#define WIN_BS_MONOPATTERN                 9

/* wapi_CreateBrushIndirect() special color values */
#define WIN_DIB_RGB_COLORS                 0
#define WIN_DIB_PAL_COLORS                 1

/* wapi_SetArcDirection() values */
#define WIN_AD_COUNTERCLOCKWISE            1
#define WIN_AD_CLOCKWISE                   2

/* WIN_WM_SETICON / WIN_WM_GETICON types */
#define WIN_ICON_SMALL                     0
#define WIN_ICON_BIG                       1

/* wapi_GetStockObject() types */
#define WIN_WHITE_BRUSH                    0
#define WIN_LTGRAY_BRUSH                   1
#define WIN_GRAY_BRUSH                     2
#define WIN_DKGRAY_BRUSH                   3
#define WIN_BLACK_BRUSH                    4
#define WIN_NULL_BRUSH                     5
#define WIN_HOLLOW_BRUSH                   WIN_NULL_BRUSH

#define WIN_WHITE_PEN                      6
#define WIN_BLACK_PEN                      7
#define WIN_NULL_PEN                       8

#define WIN_OEM_FIXED_FONT                 10
#define WIN_ANSI_FIXED_FONT                11
#define WIN_ANSI_VAR_FONT                  12
#define WIN_SYSTEM_FONT                    13
#define WIN_DEVICE_DEFAULT_FONT            14
#define WIN_DEFAULT_PALETTE                15
#define WIN_SYSTEM_FIXED_FONT              16
#define WIN_DEFAULT_GUI_FONT               17
#define WIN_DC_BRUSH                       18
#define WIN_DC_PEN                         19

/* win_ChooseColor() flags */
#define WIN_CC_RGBINIT                     0x00000001
#define WIN_CC_FULLOPEN                    0x00000002
#define WIN_CC_PREVENTFULLOPEN             0x00000004
#define WIN_CC_SHOWHELP                    0x00000008
#define WIN_CC_ENABLEHOOK                  0x00000010
#define WIN_CC_ENABLETEMPLATE              0x00000020
#define WIN_CC_ENABLETEMPLATEHANDLE        0x00000040
#define WIN_CC_SOLIDCOLOR                  0x00000080  /* WINVER >= 0x0400 */
#define WIN_CC_ANYCOLOR                    0x00000100  /* WINVER >= 0x0400 */

/* wapi_GetTextMetrics() "tmPitchAndFamily" values */
#define WIN_TMPF_FIXED_PITCH               0x01
#define WIN_TMPF_VECTOR                    0x02
#define WIN_TMPF_DEVICE                    0x08
#define WIN_TMPF_TRUETYPE                  0x04

/* wapi_GetDeviceCaps() WIN_RASTERCAPS bit positions */
#define WIN_RC_NONE                        0x0000
#define WIN_RC_BITBLT                      0x0001
#define WIN_RC_BANDING                     0x0002
#define WIN_RC_SCALING                     0x0004
#define WIN_RC_BITMAP64                    0x0008
#define WIN_RC_GDI20_OUTPUT                0x0010
#define WIN_RC_GDI20_STATE                 0x0020
#define WIN_RC_SAVEBITMAP                  0x0040
#define WIN_RC_DI_BITMAP                   0x0080
#define WIN_RC_PALETTE                     0x0100
#define WIN_RC_DIBTODEV                    0x0200
#define WIN_RC_BIGFONT                     0x0400
#define WIN_RC_STRETCHBLT                  0x0800
#define WIN_RC_FLOODFILL                   0x1000
#define WIN_RC_STRETCHDIB                  0x2000
#define WIN_RC_OP_DX_OUTPUT                0x4000
#define WIN_RC_DEVBITS                     0x8000

/* wapi_GetUserNameEx() nNameFormats (EXTENDED_NAME_FORMAT) */
#define HB_WIN_ENF_NameUnknown             0
#define HB_WIN_ENF_NameFullyQualifiedDN    1
#define HB_WIN_ENF_NameSamCompatible       2
#define HB_WIN_ENF_NameDisplay             3
#define HB_WIN_ENF_NameUniqueId            6
#define HB_WIN_ENF_NameCanonical           7
#define HB_WIN_ENF_NameUserPrincipal       8
#define HB_WIN_ENF_NameCanonicalEx         9
#define HB_WIN_ENF_NameServicePrincipal    10
#define HB_WIN_ENF_NameDnsDomain           12

/* wapi_LoadLibraryEx() flags */
#define WIN_DONT_RESOLVE_DLL_REFERENCES          0x0001
#define WIN_LOAD_LIBRARY_AS_DATAFILE             0x0002
#define WIN_LOAD_WITH_ALTERED_SEARCH_PATH        0x0008
#define WIN_LOAD_IGNORE_CODE_AUTHZ_LEVEL         0x0010
#define WIN_LOAD_LIBRARY_AS_IMAGE_RESOURCE       0x0020
#define WIN_LOAD_LIBRARY_AS_DATAFILE_EXCLUSIVE   0x0040
#define WIN_LOAD_LIBRARY_SEARCH_DLL_LOAD_DIR     0x0100
#define WIN_LOAD_LIBRARY_SEARCH_APPLICATION_DIR  0x0200
#define WIN_LOAD_LIBRARY_SEARCH_USER_DIRS        0x0400
#define WIN_LOAD_LIBRARY_SEARCH_SYSTEM32         0x0800
#define WIN_LOAD_LIBRARY_SEARCH_DEFAULT_DIRS     0x1000

/* win_PrintDlgDC() flags */
#define WIN_PD_ALLPAGES                          0x00000000
#define WIN_PD_SELECTION                         0x00000001
#define WIN_PD_PAGENUMS                          0x00000002
#define WIN_PD_NOSELECTION                       0x00000004
#define WIN_PD_NOPAGENUMS                        0x00000008
#define WIN_PD_COLLATE                           0x00000010
#define WIN_PD_PRINTTOFILE                       0x00000020
#define WIN_PD_PRINTSETUP                        0x00000040
#define WIN_PD_NOWARNING                         0x00000080
#define WIN_PD_RETURNDC                          0x00000100
#define WIN_PD_RETURNIC                          0x00000200
#define WIN_PD_RETURNDEFAULT                     0x00000400
#define WIN_PD_SHOWHELP                          0x00000800
#define WIN_PD_ENABLEPRINTHOOK                   0x00001000
#define WIN_PD_ENABLESETUPHOOK                   0x00002000
#define WIN_PD_ENABLEPRINTTEMPLATE               0x00004000
#define WIN_PD_ENABLESETUPTEMPLATE               0x00008000
#define WIN_PD_ENABLEPRINTTEMPLATEHANDLE         0x00010000
#define WIN_PD_ENABLESETUPTEMPLATEHANDLE         0x00020000
#define WIN_PD_USEDEVMODECOPIES                  0x00040000
#define WIN_PD_USEDEVMODECOPIESANDCOLLATE        0x00040000
#define WIN_PD_DISABLEPRINTTOFILE                0x00080000
#define WIN_PD_HIDEPRINTTOFILE                   0x00100000
#define WIN_PD_NONETWORKBUTTON                   0x00200000
#define WIN_PD_CURRENTPAGE                       0x00400000
#define WIN_PD_NOCURRENTPAGE                     0x00800000
#define WIN_PD_EXCLUSIONFLAGS                    0x01000000
#define WIN_PD_USELARGETEMPLATE                  0x10000000

/* win_PrinterStatus() return values */
#define HB_WIN_PRINTER_STATUS_ERROR              ( -1 )
#define WIN_PRINTER_STATUS_PAUSED                0x00000001
#define WIN_PRINTER_STATUS_ERROR                 0x00000002
#define WIN_PRINTER_STATUS_PENDING_DELETION      0x00000004
#define WIN_PRINTER_STATUS_PAPER_JAM             0x00000008
#define WIN_PRINTER_STATUS_PAPER_OUT             0x00000010
#define WIN_PRINTER_STATUS_MANUAL_FEED           0x00000020
#define WIN_PRINTER_STATUS_PAPER_PROBLEM         0x00000040
#define WIN_PRINTER_STATUS_OFFLINE               0x00000080
#define WIN_PRINTER_STATUS_IO_ACTIVE             0x00000100
#define WIN_PRINTER_STATUS_BUSY                  0x00000200
#define WIN_PRINTER_STATUS_PRINTING              0x00000400
#define WIN_PRINTER_STATUS_OUTPUT_BIN_FULL       0x00000800
#define WIN_PRINTER_STATUS_NOT_AVAILABLE         0x00001000
#define WIN_PRINTER_STATUS_WAITING               0x00002000
#define WIN_PRINTER_STATUS_PROCESSING            0x00004000
#define WIN_PRINTER_STATUS_INITIALIZING          0x00008000
#define WIN_PRINTER_STATUS_WARMING_UP            0x00010000
#define WIN_PRINTER_STATUS_TONER_LOW             0x00020000
#define WIN_PRINTER_STATUS_NO_TONER              0x00040000
#define WIN_PRINTER_STATUS_PAGE_PUNT             0x00080000
#define WIN_PRINTER_STATUS_USER_INTERVENTION     0x00100000
#define WIN_PRINTER_STATUS_OUT_OF_MEMORY         0x00200000
#define WIN_PRINTER_STATUS_DOOR_OPEN             0x00400000
#define WIN_PRINTER_STATUS_SERVER_UNKNOWN        0x00800000
#define WIN_PRINTER_STATUS_POWER_SAVE            0x01000000

#endif /* HBWIN_CH_ */
