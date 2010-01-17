/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * hbwin header
 *
 * Copyright 2008 Viktor Szakats (harbour.01 syenar.hu)
 * Copyright 2004 Peter Rees <peter@rees.co.nz> Rees Software & Systems Ltd
 * www - http://www.harbour-project.org
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
 * along with this software; see the file COPYING.  If not, write to
 * the Free Software Foundation, Inc., 59 Temple Place, Suite 330,
 * Boston, MA 02111-1307 USA (or visit the web site http://www.gnu.org/).
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

/* WIN_MAPISENDMAIL() address types */
#define WIN_MAPI_TO                 1
#define WIN_MAPI_CC                 2
#define WIN_MAPI_BCC                3

/* Windows registry hives */
#define WIN_HKEY_CLASSES_ROOT       0x80000000
#define WIN_HKEY_CURRENT_USER       0x80000001
#define WIN_HKEY_LOCAL_MACHINE      0x80000002
#define WIN_HKEY_USERS              0x80000003
#define WIN_HKEY_PERFORMANCE_DATA   0x80000004
#define WIN_HKEY_CURRENT_CONFIG     0x80000005
#define WIN_HKEY_DYN_DATA           0x80000006

/* WIN_PORT() related values */

/* WIN_COMOPEN() bit rates */
#define WIN_CBR_110                 110
#define WIN_CBR_300                 300
#define WIN_CBR_600                 600
#define WIN_CBR_1200                1200
#define WIN_CBR_2400                2400
#define WIN_CBR_4800                4800
#define WIN_CBR_9600                9600
#define WIN_CBR_14400               14400
#define WIN_CBR_19200               19200
#define WIN_CBR_38400               38400
#define WIN_CBR_56000               56000
#define WIN_CBR_57600               57600
#define WIN_CBR_115200              115200
#define WIN_CBR_128000              128000
#define WIN_CBR_256000              256000

/* WIN_COMOPEN() parity modes */
#define WIN_NOPARITY                0
#define WIN_ODDPARITY               1
#define WIN_EVENPARITY              2
#define WIN_MARKPARITY              3
#define WIN_SPACEPARITY             4

/* WIN_COMOPEN() stopbit modes */
#define WIN_ONESTOPBIT              0
#define WIN_ONE5STOPBITS            1
#define WIN_TWOSTOPBITS             2

/* WIN_COMDTRFLOW() parameters */
#define WIN_DTR_CONTROL_DISABLE     0x00
#define WIN_DTR_CONTROL_ENABLE      0x01
#define WIN_DTR_CONTROL_HANDSHAKE   0x02

/* WIN_COMRTSFLOW() parameters */
#define WIN_RTS_CONTROL_DISABLE     0x00
#define WIN_RTS_CONTROL_ENABLE      0x01
#define WIN_RTS_CONTROL_HANDSHAKE   0x02
#define WIN_RTS_CONTROL_TOGGLE      0x03

/* WIN_COMDEBUGDCB() debug levels */
#define HB_WIN_COM_DBGBASIC         0x01
#define HB_WIN_COM_DBGFLOW          0x02
#define HB_WIN_COM_DBGXTRAFLOW      0x04
#define HB_WIN_COM_DBGOTHER         0x08
#define HB_WIN_COM_DBGTIMEOUTS      0x10
#define HB_WIN_COM_DBGQUEUE         0x20
#define HB_WIN_COM_DBGALL           0x3F

/* WIN_PRN() related values */

/* WIN_PRINTERLIST() positions for array returned */
#define HB_WINPRN_NAME              1
#define HB_WINPRN_PORT              2
#define HB_WINPRN_TYPE              3
#define HB_WINPRN_DRIVER            4
#define HB_WINPRN_SHARE             5
#define HB_WINPRN_SERVER            6
#define HB_WINPRN_LEN_              6

/* WIN_ENUMFONTS() positions for array returned */
#define HB_WINFONT_NAME             1
#define HB_WINFONT_FIXED            2
#define HB_WINFONT_TRUETYPE         3
#define HB_WINFONT_CHARSET          4
#define HB_WINFONT_LEN_             4

#define WIN_RGB( nR, nG, nB )       ( nR + ( nG * 256 ) + ( nB * 256 * 256 ) )

/* Color constants for convenience */
#define HB_WIN_RGB_BLACK            WIN_RGB( 0x00, 0x00, 0x00 )
#define HB_WIN_RGB_BLUE             WIN_RGB( 0x00, 0x00, 0x85 )
#define HB_WIN_RGB_GREEN            WIN_RGB( 0x00, 0x85, 0x00 )
#define HB_WIN_RGB_CYAN             WIN_RGB( 0x00, 0x85, 0x85 )
#define HB_WIN_RGB_RED              WIN_RGB( 0x85, 0x00, 0x00 )
#define HB_WIN_RGB_MAGENTA          WIN_RGB( 0x85, 0x00, 0x85 )
#define HB_WIN_RGB_BROWN            WIN_RGB( 0x85, 0x85, 0x00 )
#define HB_WIN_RGB_WHITE            WIN_RGB( 0xC6, 0xC6, 0xC6 )

/* WIN_SETDOCUMENTPROPERTIES() paper types */
#define WIN_DMPAPER_LETTER          1
#define WIN_DMPAPER_LEGAL           5
#define WIN_DMPAPER_EXECUTIVE       7
#define WIN_DMPAPER_A3              8
#define WIN_DMPAPER_A4              9
#define WIN_DMPAPER_A5              11
#define WIN_DMPAPER_B4              12
#define WIN_DMPAPER_B5              13
#define WIN_DMPAPER_USER            256

/* WIN_SETDOCUMENTPROPERTIES() bin selections */
#define WIN_DMBIN_UPPER             1
#define WIN_DMBIN_ONLYONE           1
#define WIN_DMBIN_LOWER             2
#define WIN_DMBIN_MIDDLE            3
#define WIN_DMBIN_MANUAL            4
#define WIN_DMBIN_ENVELOPE          5
#define WIN_DMBIN_ENVMANUAL         6
#define WIN_DMBIN_AUTO              7
#define WIN_DMBIN_TRACTOR           8
#define WIN_DMBIN_SMALLFMT          9
#define WIN_DMBIN_LARGEFMT          10
#define WIN_DMBIN_LARGECAPACITY     11
#define WIN_DMBIN_CASSETTE          14
#define WIN_DMBIN_FORMSOURCE        15
#define WIN_DMBIN_USER              256
#define WIN_DMBIN_FIRST             WIN_DMBIN_UPPER
#define WIN_DMBIN_LAST              WIN_DMBIN_FORMSOURCE

/* WIN_SETDOCUMENTPROPERTIES() print qualities */
#define WIN_DMRES_DRAFT             ( -1 )
#define WIN_DMRES_LOW               ( -2 )
#define WIN_DMRES_MEDIUM            ( -3 )
#define WIN_DMRES_HIGH              ( -4 )

/* WIN_SETDOCUMENTPROPERTIES() duplex modes */
#define WIN_DMDUP_SIMPLEX           1
#define WIN_DMDUP_VERTICAL          2
#define WIN_DMDUP_HORIZONTAL        3

/* WIN_SETMAPMODE() parameters */
#define WIN_MM_TEXT                 1
#define WIN_MM_LOMETRIC             2
#define WIN_MM_HIMETRIC             3
#define WIN_MM_LOENGLISH            4
#define WIN_MM_HIENGLISH            5
#define WIN_MM_TWIPS                6
#define WIN_MM_ISOTROPIC            7
#define WIN_MM_ANISOTROPIC          8
#define WIN_MM_MAX_FIXEDSCALE       WIN_MM_TWIPS

/* WIN_CREATEFONT() font weights */
#define WIN_FW_DONTCARE             0
#define WIN_FW_THIN                 100
#define WIN_FW_EXTRALIGHT           200
#define WIN_FW_ULTRALIGHT           WIN_FW_EXTRALIGHT
#define WIN_FW_LIGHT                300
#define WIN_FW_NORMAL               400
#define WIN_FW_REGULAR              WIN_FW_NORMAL
#define WIN_FW_MEDIUM               500
#define WIN_FW_SEMIBOLD             600
#define WIN_FW_DEMIBOLD             WIN_FW_SEMIBOLD
#define WIN_FW_BOLD                 700
#define WIN_FW_EXTRABOLD            800
#define WIN_FW_ULTRABOLD            WIN_FW_EXTRABOLD
#define WIN_FW_HEAVY                900
#define WIN_FW_BLACK                WIN_FW_HEAVY

/* Windows charset values (WIN_CREATEFONT()) */
#define WIN_ANSI_CHARSET            0
#define WIN_DEFAULT_CHARSET         1
#define WIN_SYMBOL_CHARSET          2
#define WIN_MAC_CHARSET             77
#define WIN_SHIFTJIS_CHARSET        128
#define WIN_HANGEUL_CHARSET         129
#define WIN_HANGUL_CHARSET          129
#define WIN_JOHAB_CHARSET           130
#define WIN_GB2312_CHARSET          134
#define WIN_CHINESEBIG5_CHARSET     136
#define WIN_GREEK_CHARSET           161
#define WIN_TURKISH_CHARSET         162
#define WIN_VIETNAMESE_CHARSET      163
#define WIN_HEBREW_CHARSET          177
#define WIN_ARABIC_CHARSET          178
#define WIN_BALTIC_CHARSET          186
#define WIN_RUSSIAN_CHARSET         204
#define WIN_THAI_CHARSET            222
#define WIN_EASTEUROPE_CHARSET      238
#define WIN_OEM_CHARSET             255

/* WIN_GETDEVICECAPS() parameters */
#define WIN_HORZSIZE                4     // Horizontal size in millimeters
#define WIN_VERTSIZE                6     // Vertical size in millimeters
#define WIN_HORZRES                 8     // Horizontal width in pixels
#define WIN_VERTRES                 10    // Vertical height in pixels
#define WIN_NUMBRUSHES              16    // Number of brushes the device has
#define WIN_NUMPENS                 18    // Number of pens the device has
#define WIN_NUMFONTS                22    // Number of fonts the device has
#define WIN_NUMCOLORS               24    // Number of colors the device supports
#define WIN_RASTERCAPS              38    // Bitblt capabilities
#define WIN_LOGPIXELSX              88    // Logical pixels/inch in X
#define WIN_LOGPIXELSY              90    // Logical pixels/inch in Y
#define WIN_PHYSICALWIDTH           110   // Physical Width in device units
#define WIN_PHYSICALHEIGHT          111   // Physical Height in device units
#define WIN_PHYSICALOFFSETX         112   // Physical Printable Area x margin
#define WIN_PHYSICALOFFSETY         113   // Physical Printable Area y margin
#define WIN_SCALINGFACTORX          114   // Scaling factor x
#define WIN_SCALINGFACTORY          115   // Scaling factor y

/* WIN_TEXTOUT() text alignment modes */
#define WIN_TA_NOUPDATECP           0
#define WIN_TA_UPDATECP             1
#define WIN_TA_LEFT                 0
#define WIN_TA_RIGHT                2
#define WIN_TA_CENTER               6
#define WIN_TA_TOP                  0
#define WIN_TA_BOTTOM               8
#define WIN_TA_BASELINE             24

/* WIN_SETPEN() pen styles */
#define WIN_PS_SOLID	            0
#define WIN_PS_DASH	                1
#define WIN_PS_DOT	                2
#define WIN_PS_DASHDOT	            3
#define WIN_PS_DASHDOTDOT	        4
#define WIN_PS_NULL	                5
#define WIN_PS_INSIDEFRAME	        6

/* ------------------------------- */
/* Deprecated constants and macros */
/* ------------------------------- */

#if defined( HB_LEGACY_LEVEL3 ) .AND. ! defined( HB_WIN_NO_LEGACY )

#define HKEY_CLASSES_ROOT           WIN_HKEY_CLASSES_ROOT
#define HKEY_CURRENT_USER           WIN_HKEY_CURRENT_USER
#define HKEY_LOCAL_MACHINE          WIN_HKEY_LOCAL_MACHINE
#define HKEY_USERS                  WIN_HKEY_USERS
#define HKEY_PERFORMANCE_DATA       WIN_HKEY_PERFORMANCE_DATA
#define HKEY_CURRENT_CONFIG         WIN_HKEY_CURRENT_CONFIG
#define HKEY_DYN_DATA               WIN_HKEY_DYN_DATA

#define CBR_110                     WIN_CBR_110
#define CBR_300                     WIN_CBR_300
#define CBR_600                     WIN_CBR_600
#define CBR_1200                    WIN_CBR_1200
#define CBR_2400                    WIN_CBR_2400
#define CBR_4800                    WIN_CBR_4800
#define CBR_9600                    WIN_CBR_9600
#define CBR_14400                   WIN_CBR_14400
#define CBR_19200                   WIN_CBR_19200
#define CBR_38400                   WIN_CBR_38400
#define CBR_56000                   WIN_CBR_56000
#define CBR_57600                   WIN_CBR_57600
#define CBR_115200                  WIN_CBR_115200
#define CBR_128000                  WIN_CBR_128000
#define CBR_256000                  WIN_CBR_256000

#define NOPARITY                    WIN_NOPARITY
#define ODDPARITY                   WIN_ODDPARITY
#define EVENPARITY                  WIN_EVENPARITY
#define MARKPARITY                  WIN_MARKPARITY
#define SPACEPARITY                 WIN_SPACEPARITY

#define ONESTOPBIT                  WIN_ONESTOPBIT
#define ONE5STOPBITS                WIN_ONE5STOPBITS
#define TWOSTOPBITS                 WIN_TWOSTOPBITS

#define DTR_CONTROL_DISABLE         WIN_DTR_CONTROL_DISABLE
#define DTR_CONTROL_ENABLE          WIN_DTR_CONTROL_ENABLE
#define DTR_CONTROL_HANDSHAKE       WIN_DTR_CONTROL_HANDSHAKE

#define RTS_CONTROL_DISABLE         WIN_RTS_CONTROL_DISABLE
#define RTS_CONTROL_ENABLE          WIN_RTS_CONTROL_ENABLE
#define RTS_CONTROL_HANDSHAKE       WIN_RTS_CONTROL_HANDSHAKE
#define RTS_CONTROL_TOGGLE          WIN_RTS_CONTROL_TOGGLE

#define FORM_LETTER                 WIN_DMPAPER_LETTER
#define FORM_A4                     WIN_DMPAPER_A4
#define FORM_CUSTOM                 WIN_DMPAPER_USER

#define PS_SOLID                    WIN_PS_SOLID

#define RGB( nR, nG, nB )           WIN_RGB( nR, nG, nB )

#define RGB_BLACK                   HB_WIN_RGB_BLACK
#define RGB_BLUE                    HB_WIN_RGB_BLUE
#define RGB_GREEN                   HB_WIN_RGB_GREEN
#define RGB_CYAN                    HB_WIN_RGB_CYAN
#define RGB_RED                     HB_WIN_RGB_RED
#define RGB_MAGENTA                 HB_WIN_RGB_MAGENTA
#define RGB_BROWN                   HB_WIN_RGB_BROWN
#define RGB_WHITE                   HB_WIN_RGB_WHITE

#define MM_TEXT                     WIN_MM_TEXT
#define MM_LOMETRIC                 WIN_MM_LOMETRIC
#define MM_HIMETRIC                 WIN_MM_HIMETRIC
#define MM_LOENGLISH                WIN_MM_LOENGLISH
#define MM_HIENGLISH                WIN_MM_HIENGLISH

#define FW_DONTCARE                 WIN_FW_DONTCARE
#define FW_THIN                     WIN_FW_THIN
#define FW_EXTRALIGHT               WIN_FW_EXTRALIGHT
#define FW_ULTRALIGHT               WIN_FW_ULTRALIGHT
#define FW_LIGHT                    WIN_FW_LIGHT
#define FW_NORMAL                   WIN_FW_NORMAL
#define FW_REGULAR                  WIN_FW_REGULAR
#define FW_MEDIUM                   WIN_FW_MEDIUM
#define FW_SEMIBOLD                 WIN_FW_SEMIBOLD
#define FW_DEMIBOLD                 WIN_FW_DEMIBOLD
#define FW_BOLD                     WIN_FW_BOLD
#define FW_EXTRABOLD                WIN_FW_EXTRABOLD
#define FW_ULTRABOLD                WIN_FW_ULTRABOLD
#define FW_HEAVY                    WIN_FW_HEAVY
#define FW_BLACK                    WIN_FW_BLACK

#define TA_NOUPDATECP               WIN_TA_NOUPDATECP
#define TA_UPDATECP                 WIN_TA_UPDATECP
#define TA_LEFT                     WIN_TA_LEFT
#define TA_RIGHT                    WIN_TA_RIGHT
#define TA_CENTER                   WIN_TA_CENTER
#define TA_TOP                      WIN_TA_TOP
#define TA_BOTTOM                   WIN_TA_BOTTOM
#define TA_BASELINE                 WIN_TA_BASELINE

#define HORZSIZE                    WIN_HORZSIZE
#define VERTSIZE                    WIN_VERTSIZE
#define HORZRES                     WIN_HORZRES
#define VERTRES                     WIN_VERTRES
#define NUMBRUSHES                  WIN_NUMBRUSHES
#define NUMPENS                     WIN_NUMPENS
#define NUMFONTS                    WIN_NUMFONTS
#define NUMCOLORS                   WIN_NUMCOLORS
#define RASTERCAPS                  WIN_RASTERCAPS
#define LOGPIXELSX                  WIN_LOGPIXELSX
#define LOGPIXELSY                  WIN_LOGPIXELSY
#define PHYSICALWIDTH               WIN_PHYSICALWIDTH
#define PHYSICALHEIGHT              WIN_PHYSICALHEIGHT
#define PHYSICALOFFSETX             WIN_PHYSICALOFFSETX
#define PHYSICALOFFSETY             WIN_PHYSICALOFFSETY
#define SCALINGFACTORX              WIN_SCALINGFACTORX
#define SCALINGFACTORY              WIN_SCALINGFACTORY

#define DMBIN_UPPER                 WIN_DMBIN_UPPER
#define DMBIN_ONLYONE               WIN_DMBIN_ONLYONE
#define DMBIN_LOWER                 WIN_DMBIN_LOWER
#define DMBIN_MIDDLE                WIN_DMBIN_MIDDLE
#define DMBIN_MANUAL                WIN_DMBIN_MANUAL
#define DMBIN_ENVELOPE              WIN_DMBIN_ENVELOPE
#define DMBIN_ENVMANUAL             WIN_DMBIN_ENVMANUAL
#define DMBIN_AUTO                  WIN_DMBIN_AUTO
#define DMBIN_TRACTOR               WIN_DMBIN_TRACTOR
#define DMBIN_SMALLFMT              WIN_DMBIN_SMALLFMT
#define DMBIN_LARGEFMT              WIN_DMBIN_LARGEFMT
#define DMBIN_LARGECAPACITY         WIN_DMBIN_LARGECAPACITY
#define DMBIN_CASSETTE              WIN_DMBIN_CASSETTE
#define DMBIN_FORMSOURCE            WIN_DMBIN_FORMSOURCE
#define DMBIN_FIRST                 WIN_DMBIN_FIRST
#define DMBIN_LAST                  WIN_DMBIN_LAST

#define DMRES_DRAFT                 WIN_DMRES_DRAFT
#define DMRES_LOW                   WIN_DMRES_LOW
#define DMRES_MEDIUM                WIN_DMRES_MEDIUM
#define DMRES_HIGH                  WIN_DMRES_HIGH

#define DMDUP_SIMPLEX               WIN_DMDUP_SIMPLEX
#define DMDUP_VERTICAL              WIN_DMDUP_VERTICAL
#define DMDUP_HORIZONTAL            WIN_DMDUP_HORIZONTAL

#define ANSI_CHARSET                WIN_ANSI_CHARSET
#define DEFAULT_CHARSET             WIN_DEFAULT_CHARSET
#define SYMBOL_CHARSET              WIN_SYMBOL_CHARSET
#define MAC_CHARSET                 WIN_MAC_CHARSET
#define SHIFTJIS_CHARSET            WIN_SHIFTJIS_CHARSET
#define HANGEUL_CHARSET             WIN_HANGEUL_CHARSET
#define HANGUL_CHARSET              WIN_HANGUL_CHARSET
#define JOHAB_CHARSET               WIN_JOHAB_CHARSET
#define GB2312_CHARSET              WIN_GB2312_CHARSET
#define CHINESEBIG5_CHARSET         WIN_CHINESEBIG5_CHARSET
#define GREEK_CHARSET               WIN_GREEK_CHARSET
#define TURKISH_CHARSET             WIN_TURKISH_CHARSET
#define VIETNAMESE_CHARSET          WIN_VIETNAMESE_CHARSET
#define HEBREW_CHARSET              WIN_HEBREW_CHARSET
#define ARABIC_CHARSET              WIN_ARABIC_CHARSET
#define BALTIC_CHARSET              WIN_BALTIC_CHARSET
#define RUSSIAN_CHARSET             WIN_RUSSIAN_CHARSET
#define THAI_CHARSET                WIN_THAI_CHARSET
#define EASTEUROPE_CHARSET          WIN_EASTEUROPE_CHARSET
#define OEM_CHARSET                 WIN_OEM_CHARSET

#endif

#endif /* HBWIN_CH_ */
