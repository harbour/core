/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * hbwin header
 *
 * Copyright 2008 Viktor Szakats (harbour syenar.net)
 * Copyright 2004 Peter Rees <peter@rees.co.nz> Rees Software & Systems Ltd
 * www - http://harbour-project.org
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

/* win_MAPISendMail() address types */
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

/* WIN_PORT() related values */

/* win_comOpen() bit rates */
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

/* win_comOpen() parity modes */
#define WIN_NOPARITY                0
#define WIN_ODDPARITY               1
#define WIN_EVENPARITY              2
#define WIN_MARKPARITY              3
#define WIN_SPACEPARITY             4

/* win_comOpen() stopbit modes */
#define WIN_ONESTOPBIT              0
#define WIN_ONE5STOPBITS            1
#define WIN_TWOSTOPBITS             2

/* win_comDTRFlow() parameters */
#define WIN_DTR_CONTROL_DISABLE     0x00
#define WIN_DTR_CONTROL_ENABLE      0x01
#define WIN_DTR_CONTROL_HANDSHAKE   0x02

/* win_comRTSFlow() parameters */
#define WIN_RTS_CONTROL_DISABLE     0x00
#define WIN_RTS_CONTROL_ENABLE      0x01
#define WIN_RTS_CONTROL_HANDSHAKE   0x02
#define WIN_RTS_CONTROL_TOGGLE      0x03

/* win_comDebugDCB() debug levels */
#define HB_WIN_COM_DBGBASIC         0x01
#define HB_WIN_COM_DBGFLOW          0x02
#define HB_WIN_COM_DBGXTRAFLOW      0x04
#define HB_WIN_COM_DBGOTHER         0x08
#define HB_WIN_COM_DBGTIMEOUTS      0x10
#define HB_WIN_COM_DBGQUEUE         0x20
#define HB_WIN_COM_DBGALL           0x3F

/* win_Prn() related values */

/* win_printerList() positions for array returned */
#define HB_WINPRN_NAME              1
#define HB_WINPRN_PORT              2
#define HB_WINPRN_TYPE              3
#define HB_WINPRN_DRIVER            4
#define HB_WINPRN_SHARE             5
#define HB_WINPRN_SERVER            6
#define HB_WINPRN_LEN_              6

/* win_EnumFonts()/win_EnumFontFamilies() positions for array returned */
#define HB_WINFONT_NAME             1
#define HB_WINFONT_FIXED            2
#define HB_WINFONT_TRUETYPE         3
#define HB_WINFONT_CHARSET          4
#define HB_WINFONT_LEN_             4

/* win_bitmapType() return values */
#define HB_WIN_BITMAP_UNKNOWN       0
#define HB_WIN_BITMAP_BMP           1
#define HB_WIN_BITMAP_JPEG          2
#define HB_WIN_BITMAP_PNG           3

#define WIN_RGB( nR, nG, nB )       ( nR + ( nG * 256 ) + ( nB * 256 * 256 ) )

/* Color constants for convenience */
#define HB_WIN_RGB_BLACK            WIN_RGB( 0x00, 0x00, 0x00 )
#define HB_WIN_RGB_BLUE             WIN_RGB( 0x00, 0x00, 0xFF )
#define HB_WIN_RGB_GREEN            WIN_RGB( 0x00, 0xFF, 0x00 )
#define HB_WIN_RGB_RED              WIN_RGB( 0xFF, 0x00, 0x00 )
#define HB_WIN_RGB_CYAN             WIN_RGB( 0x00, 0xFF, 0xFF )
#define HB_WIN_RGB_MAGENTA          WIN_RGB( 0xFF, 0x00, 0xFF )
#define HB_WIN_RGB_YELLOW           WIN_RGB( 0xFF, 0xFF, 0x00 )
#define HB_WIN_RGB_WHITE            WIN_RGB( 0xFF, 0xFF, 0xFF )

/* win_SetDocumentProperties() paper types */
#define WIN_DMPAPER_LETTER          1
#define WIN_DMPAPER_LEGAL           5
#define WIN_DMPAPER_EXECUTIVE       7
#define WIN_DMPAPER_A3              8
#define WIN_DMPAPER_A4              9
#define WIN_DMPAPER_A5              11
#define WIN_DMPAPER_B4              12
#define WIN_DMPAPER_B5              13
#define WIN_DMPAPER_USER            256

/* win_SetDocumentProperties() bin selections */
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

/* win_SetDocumentProperties() print qualities */
#define WIN_DMRES_DRAFT             ( -1 )
#define WIN_DMRES_LOW               ( -2 )
#define WIN_DMRES_MEDIUM            ( -3 )
#define WIN_DMRES_HIGH              ( -4 )

/* win_SetDocumentProperties() duplex modes */
#define WIN_DMDUP_SIMPLEX           1
#define WIN_DMDUP_VERTICAL          2
#define WIN_DMDUP_HORIZONTAL        3

/* WAPI DEVMODE "dmOrientation" modes */
#define WIN_DMORIENT_PORTRAIT       1
#define WIN_DMORIENT_LANDSCAPE      2

/* win_SetMapMode() parameters */
#define WIN_MM_TEXT                 1
#define WIN_MM_LOMETRIC             2
#define WIN_MM_HIMETRIC             3
#define WIN_MM_LOENGLISH            4
#define WIN_MM_HIENGLISH            5
#define WIN_MM_TWIPS                6
#define WIN_MM_ISOTROPIC            7
#define WIN_MM_ANISOTROPIC          8
#define WIN_MM_MAX_FIXEDSCALE       WIN_MM_TWIPS

/* win_CreateFont() font weights */
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

/* wapi_CreateFont() font qualities */
#define WIN_DEFAULT_QUALITY         0
#define WIN_DRAFT_QUALITY           1
#define WIN_PROOF_QUALITY           2
#define WIN_NONANTIALIASED_QUALITY  3
#define WIN_ANTIALIASED_QUALITY     4

/* wapi_CreateFont() output precision values */
#define WIN_OUT_DEFAULT_PRECIS      0
#define WIN_OUT_STRING_PRECIS       1
#define WIN_OUT_CHARACTER_PRECIS    2
#define WIN_OUT_STROKE_PRECIS       3
#define WIN_OUT_TT_PRECIS           4
#define WIN_OUT_DEVICE_PRECIS       5
#define WIN_OUT_RASTER_PRECIS       6
#define WIN_OUT_TT_ONLY_PRECIS      7
#define WIN_OUT_OUTLINE_PRECIS      8
#define WIN_OUT_PS_ONLY_PRECIS      10

/* win_SetBkMode() modes */
#define WIN_TRANSPARENT             1
#define WIN_OPAQUE                  2

/* wapi_ExtTextOut() options */
#define WIN_ETO_OPAQUE              0x00002
#define WIN_ETO_CLIPPED             0x00004
#define WIN_ETO_GLYPH_INDEX         0x00010
#define WIN_ETO_RTLREADING          0x00080
#define WIN_ETO_NUMERICSLOCAL       0x00400
#define WIN_ETO_NUMERICSLATIN       0x00800
#define WIN_ETO_IGNORELANGUAGE      0x01000
#define WIN_ETO_PDY                 0x02000
#define WIN_ETO_REVERSE_INDEX_MAP   0x10000

/* wapi_DrawText() format methods */
#define WIN_DT_BOTTOM               0x00008
#define WIN_DT_CALCRECT             0x00400
#define WIN_DT_CENTER               0x00001
#define WIN_DT_EDITCONTROL          0x02000
#define WIN_DT_END_ELLIPSIS         0x08000
#define WIN_DT_PATH_ELLIPSIS        0x04000
#define WIN_DT_WORD_ELLIPSIS        0x40000
#define WIN_DT_EXPANDTABS           0x00040
#define WIN_DT_EXTERNALLEADING      0x00200
#define WIN_DT_LEFT                 0x00000
#define WIN_DT_MODIFYSTRING         0x10000
#define WIN_DT_NOCLIP               0x00100
#define WIN_DT_NOPREFIX             0x00800
#define WIN_DT_RIGHT                0x00002
#define WIN_DT_RTLREADING           0x20000
#define WIN_DT_SINGLELINE           0x00020
#define WIN_DT_TABSTOP              0x00080
#define WIN_DT_TOP                  0x00000
#define WIN_DT_VCENTER              0x00004
#define WIN_DT_WORDBREAK            0x00010
#define WIN_DT_INTERNAL             0x01000

/* wapi_CreateHatchBrush() styles */
#define WIN_HS_BDIAGONAL            3
#define WIN_HS_CROSS                4
#define WIN_HS_DIAGCROSS            5
#define WIN_HS_FDIAGONAL            2
#define WIN_HS_HORIZONTAL           0
#define WIN_HS_VERTICAL             1

/* wapi_AddFontResourceEx()/wapi_RemoveFontResourceEx() flags */
#define WIN_FR_PRIVATE              0x10
#define WIN_FR_NOT_ENUM             0x20

/* Windows charset values (win_CreateFont()) */
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

/* win_GetDeviceCaps() parameters */
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

/* wapi_GetSystemMetrics() parameters */
#define WIN_SM_CXSCREEN             0
#define WIN_SM_CYSCREEN             1
#define WIN_SM_CXVSCROLL            2
#define WIN_SM_CYHSCROLL            3
#define WIN_SM_CYCAPTION            4
#define WIN_SM_CXBORDER             5
#define WIN_SM_CYBORDER             6
#define WIN_SM_CXDLGFRAME           7
#define WIN_SM_CXFIXEDFRAME         7
#define WIN_SM_CYDLGFRAME           8
#define WIN_SM_CYFIXEDFRAME         8
#define WIN_SM_CYVTHUMB             9
#define WIN_SM_CXHTHUMB             10
#define WIN_SM_CXICON               11
#define WIN_SM_CYICON               12
#define WIN_SM_CXCURSOR             13
#define WIN_SM_CYCURSOR             14
#define WIN_SM_CYMENU               15
#define WIN_SM_CXFULLSCREEN         16
#define WIN_SM_CYFULLSCREEN         17
#define WIN_SM_CYKANJIWINDOW        18
#define WIN_SM_MOUSEPRESENT         19
#define WIN_SM_CYVSCROLL            20
#define WIN_SM_CXHSCROLL            21
#define WIN_SM_DEBUG                22
#define WIN_SM_SWAPBUTTON           23
#define WIN_SM_RESERVED1            24
#define WIN_SM_RESERVED2            25
#define WIN_SM_RESERVED3            26
#define WIN_SM_RESERVED4            27
#define WIN_SM_CXMIN                28
#define WIN_SM_CYMIN                29
#define WIN_SM_CXSIZE               30
#define WIN_SM_CYSIZE               31
#define WIN_SM_CXSIZEFRAME          32
#define WIN_SM_CXFRAME              32
#define WIN_SM_CYSIZEFRAME          33
#define WIN_SM_CYFRAME              33
#define WIN_SM_CXMINTRACK           34
#define WIN_SM_CYMINTRACK           35
#define WIN_SM_CXDOUBLECLK          36
#define WIN_SM_CYDOUBLECLK          37
#define WIN_SM_CXICONSPACING        38
#define WIN_SM_CYICONSPACING        39
#define WIN_SM_MENUDROPALIGNMENT    40
#define WIN_SM_PENWINDOWS           41
#define WIN_SM_DBCSENABLED          42
#define WIN_SM_CMOUSEBUTTONS        43
#define WIN_SM_SECURE               44
#define WIN_SM_CXEDGE               45
#define WIN_SM_CYEDGE               46
#define WIN_SM_CXMINSPACING         47
#define WIN_SM_CYMINSPACING         48
#define WIN_SM_CXSMICON             49
#define WIN_SM_CYSMICON             50
#define WIN_SM_CYSMCAPTION          51
#define WIN_SM_CXSMSIZE             52
#define WIN_SM_CYSMSIZE             53
#define WIN_SM_CXMENUSIZE           54
#define WIN_SM_CYMENUSIZE           55
#define WIN_SM_ARRANGE              56
#define WIN_SM_CXMINIMIZED          57
#define WIN_SM_CYMINIMIZED          58
#define WIN_SM_CXMAXTRACK           59
#define WIN_SM_CYMAXTRACK           60
#define WIN_SM_CXMAXIMIZED          61
#define WIN_SM_CYMAXIMIZED          62
#define WIN_SM_NETWORK              63
#define WIN_SM_CLEANBOOT            67
#define WIN_SM_CXDRAG               68
#define WIN_SM_CYDRAG               69
#define WIN_SM_SHOWSOUNDS           70
#define WIN_SM_CXMENUCHECK          71
#define WIN_SM_CYMENUCHECK          72
#define WIN_SM_SLOWMACHINE          73
#define WIN_SM_MIDEASTENABLED       74
#define WIN_SM_MOUSEWHEELPRESENT    75
#define WIN_SM_XVIRTUALSCREEN       76
#define WIN_SM_YVIRTUALSCREEN       77
#define WIN_SM_CXVIRTUALSCREEN      78
#define WIN_SM_CYVIRTUALSCREEN      79
#define WIN_SM_CMONITORS            80
#define WIN_SM_SAMEDISPLAYFORMAT    81
#define WIN_SM_IMMENABLED           82
#define WIN_SM_CXFOCUSBORDER        83
#define WIN_SM_CYFOCUSBORDER        84
#define WIN_SM_TABLETPC             86
#define WIN_SM_MEDIACENTER          87
#define WIN_SM_STARTER              88
#define WIN_SM_SERVERR2             89
#define WIN_SM_REMOTESESSION        4096
#define WIN_SM_SHUTTINGDOWN         8192
#define WIN_SM_REMOTECONTROL        8193
#define WIN_SM_CARETBLINKINGENABLED 8194

/* win_TextOut() text alignment modes */
#define WIN_TA_NOUPDATECP           0
#define WIN_TA_UPDATECP             1
#define WIN_TA_LEFT                 0
#define WIN_TA_RIGHT                2
#define WIN_TA_CENTER               6
#define WIN_TA_TOP                  0
#define WIN_TA_BOTTOM               8
#define WIN_TA_BASELINE             24

/* win_SetPen() pen styles */
#define WIN_PS_SOLID                0
#define WIN_PS_DASH                 1
#define WIN_PS_DOT                  2
#define WIN_PS_DASHDOT              3
#define WIN_PS_DASHDOTDOT           4
#define WIN_PS_NULL                 5
#define WIN_PS_INSIDEFRAME          6

/* win_GetOpenFileName()/win_GetSaveFileName() */
#define WIN_OFN_READONLY                  0x00000001
#define WIN_OFN_OVERWRITEPROMPT           0x00000002
#define WIN_OFN_HIDEREADONLY              0x00000004
#define WIN_OFN_NOCHANGEDIR               0x00000008
#define WIN_OFN_SHOWHELP                  0x00000010
#define WIN_OFN_ENABLEHOOK                0x00000020
#define WIN_OFN_ENABLETEMPLATE            0x00000040
#define WIN_OFN_ENABLETEMPLATEHANDLE      0x00000080
#define WIN_OFN_NOVALIDATE                0x00000100
#define WIN_OFN_ALLOWMULTISELECT          0x00000200
#define WIN_OFN_EXTENSIONDIFFERENT        0x00000400
#define WIN_OFN_PATHMUSTEXIST             0x00000800
#define WIN_OFN_FILEMUSTEXIST             0x00001000
#define WIN_OFN_CREATEPROMPT              0x00002000
#define WIN_OFN_SHAREAWARE                0x00004000
#define WIN_OFN_NOREADONLYRETURN          0x00008000
#define WIN_OFN_NOTESTFILECREATE          0x00010000
#define WIN_OFN_NONETWORKBUTTON           0x00020000
#define WIN_OFN_NOLONGNAMES               0x00040000
#define WIN_OFN_EXPLORER                  0x00080000
#define WIN_OFN_NODEREFERENCELINKS        0x00100000
#define WIN_OFN_LONGNAMES                 0x00200000
#define WIN_OFN_ENABLEINCLUDENOTIFY       0x00400000
#define WIN_OFN_ENABLESIZING              0x00800000
#define WIN_OFN_DONTADDTORECENT           0x02000000
#define WIN_OFN_FORCESHOWHIDDEN           0x10000000

#define WIN_OFN_SHAREFALLTHROUGH          0x00000002
#define WIN_OFN_SHARENOWARN               0x00000001
#define WIN_OFN_SHAREWARN                 0x00000000

/* wapi_LoadImage() <nType> values */
#define WIN_IMAGE_BITMAP            0
#define WIN_IMAGE_ICON              1
#define WIN_IMAGE_CURSOR            2
#define WIN_IMAGE_ENHMETAFILE       3

/* wapi_LoadImage() <nFlags> values */
#define WIN_LR_DEFAULTCOLOR         0x0000
#define WIN_LR_MONOCHROME           0x0001
#define WIN_LR_COLOR                0x0002
#define WIN_LR_COPYRETURNORG        0x0004
#define WIN_LR_COPYDELETEORG        0x0008
#define WIN_LR_LOADFROMFILE         0x0010
#define WIN_LR_LOADTRANSPARENT      0x0020
#define WIN_LR_DEFAULTSIZE          0x0040
#define WIN_LR_VGACOLOR             0x0080
#define WIN_LR_LOADMAP3DCOLORS      0x1000
#define WIN_LR_CREATEDIBSECTION     0x2000
#define WIN_LR_COPYFROMRESOURCE     0x4000
#define WIN_LR_SHARED               0x8000

/* win_ShellNotifyIcon() <nInfoFlags> values */
#define WIN_NIIF_NONE               0x00000000
#define WIN_NIIF_INFO               0x00000001
#define WIN_NIIF_WARNING            0x00000002
#define WIN_NIIF_ERROR              0x00000003
#define WIN_NIIF_USER               0x00000004
#define WIN_NIIF_NOSOUND            0x00000010
#define WIN_NIIF_LARGE_ICON         0x00000020
#define WIN_NIIF_RESPECT_QUIET_TIME 0x00000080
#define WIN_NIIF_ICON_MASK          0x0000000F

/* constants for MENU functions */
#define WIN_MF_ENABLED              0x0000
#define WIN_MF_GRAYED               0x0001
#define WIN_MF_DISABLED             0x0002
#define WIN_MF_BITMAP               0x0004
#define WIN_MF_UNCHECKED            0x0000
#define WIN_MF_CHECKED              0x0008
#define WIN_MF_POPUP                0x0010
#define WIN_MF_MENUBARBREAK         0x0020
#define WIN_MF_MENUBREAK            0x0040
#define WIN_MF_OWNERDRAW            0x0100
#define WIN_MF_SEPARATOR            0x0800
#define WIN_MF_STRING               0x0000
#define WIN_MF_DEFAULT              0x1000
#define WIN_MF_SYSMENU              0x2000
#define WIN_MF_HELP                 0x4000
#define WIN_MF_END                  0x0080
#define WIN_MF_RIGHTJUSTIFY         0x4000
#define WIN_MF_MOUSESELECT          0x8000
#define WIN_MF_INSERT               0x0000
#define WIN_MF_CHANGE               0x0080
#define WIN_MF_APPEND               0x0100
#define WIN_MF_DELETE               0x0200
#define WIN_MF_REMOVE               0x1000
#define WIN_MF_USECHECKBITMAPS      0x0200
#define WIN_MF_UNHILITE             0x0000
#define WIN_MF_HILITE               0x0080
#define WIN_MF_BYCOMMAND            0x0000
#define WIN_MF_BYPOSITION           0x0400

/* wapi_TrackPopupMenu() */
#define WIN_TPM_CENTERALIGN         4
#define WIN_TPM_LEFTALIGN           0
#define WIN_TPM_RIGHTALIGN          8
#define WIN_TPM_LEFTBUTTON          0
#define WIN_TPM_RIGHTBUTTON         2
#define WIN_TPM_HORIZONTAL          0
#define WIN_TPM_VERTICAL            64
#define WIN_TPM_TOPALIGN            0
#define WIN_TPM_VCENTERALIGN        16
#define WIN_TPM_BOTTOMALIGN         32
#define WIN_TPM_NONOTIFY            128
#define WIN_TPM_RETURNCMD           256
#define WIN_TPM_RECURSE             1

/* wapi_PlaySound() flags */
#define WIN_SND_SYNC                0x00000000
#define WIN_SND_ASYNC               0x00000001
#define WIN_SND_NODEFAULT           0x00000002
#define WIN_SND_MEMORY              0x00000004
#define WIN_SND_LOOP                0x00000008
#define WIN_SND_NOSTOP              0x00000010
#define WIN_SND_NOWAIT              0x00002000
#define WIN_SND_ALIAS               0x00010000
#define WIN_SND_ALIAS_ID            0x00110000
#define WIN_SND_FILENAME            0x00020000
#define WIN_SND_RESOURCE            0x00040004
#define WIN_SND_PURGE               0x00000040
#define WIN_SND_APPLICATION         0x00000080
#define WIN_SND_SENTRY              0x00080000
#define WIN_SND_RING                0x00100000
#define WIN_SND_SYSTEM              0x00200000

/* wapi_SetWindowPos() flags */
#define WIN_SWP_NOSIZE              0x0001
#define WIN_SWP_NOMOVE              0x0002
#define WIN_SWP_NOZORDER            0x0004
#define WIN_SWP_NOREDRAW            0x0008
#define WIN_SWP_NOACTIVATE          0x0010
#define WIN_SWP_FRAMECHANGED        0x0020
#define WIN_SWP_SHOWWINDOW          0x0040
#define WIN_SWP_HIDEWINDOW          0x0080
#define WIN_SWP_NOCOPYBITS          0x0100
#define WIN_SWP_NOOWNERZORDER       0x0200
#define WIN_SWP_NOSENDCHANGING      0x0400
#define WIN_SWP_DEFERERASE          0x2000
#define WIN_SWP_ASYNCWINDOWPOS      0x4000
#define WIN_SWP_DRAWFRAME           WIN_SWP_FRAMECHANGED
#define WIN_SWP_NOREPOSITION        WIN_SWP_NOOWNERZORDER

/* Predefined window IDs */
#define WIN_HWND_TOP                win_N2P( 0 )
#define WIN_HWND_BOTTOM             win_N2P( 1 )
#define WIN_HWND_TOPMOST            win_N2P( -1 )
#define WIN_HWND_NOTOPMOST          win_N2P( -2 )

/* window messages */
#define WIN_WM_USER                 1024

/* window styles */
#define WIN_WS_OVERLAPPED           0x00000000
#define WIN_WS_TABSTOP              0x00010000
#define WIN_WS_MAXIMIZEBOX          0x00010000
#define WIN_WS_MINIMIZEBOX          0x00020000
#define WIN_WS_GROUP                0x00020000
#define WIN_WS_THICKFRAME           0x00040000
#define WIN_WS_SYSMENU              0x00080000
#define WIN_WS_HSCROLL              0x00100000
#define WIN_WS_VSCROLL              0x00200000
#define WIN_WS_DLGFRAME             0x00400000
#define WIN_WS_BORDER               0x00800000
#define WIN_WS_CAPTION              0x00C00000
#define WIN_WS_MAXIMIZE             0x01000000
#define WIN_WS_CLIPCHILDREN         0x02000000
#define WIN_WS_CLIPSIBLINGS         0x04000000
#define WIN_WS_DISABLED             0x08000000
#define WIN_WS_VISIBLE              0x10000000
#define WIN_WS_MINIMIZE             0x20000000
#define WIN_WS_CHILD                0x40000000
#define WIN_WS_POPUP                0x80000000
#define WIN_WS_TILED                WIN_WS_OVERLAPPED
#define WIN_WS_ICONIC               WIN_WS_MINIMIZE
#define WIN_WS_SIZEBOX              WIN_WS_THICKFRAME
#define WIN_WS_TILEDWINDOW          WIN_WS_OVERLAPPEDWINDOW
#define WIN_WS_OVERLAPPEDWINDOW     hb_bitOr( WIN_WS_OVERLAPPED, WIN_WS_CAPTION, WIN_WS_SYSMENU, WIN_WS_THICKFRAME, WIN_WS_MINIMIZEBOX, WIN_WS_MAXIMIZEBOX )
#define WIN_WS_POPUPWINDOW          hb_bitOr( WIN_WS_POPUP, WIN_WS_BORDER, WIN_WS_SYSMENU )
#define WIN_WS_CHILDWINDOW          WIN_WS_CHILD

/* win_ReportEvent() event types */
#define WIN_EVENTLOG_SUCCESS              0x0000
#define WIN_EVENTLOG_ERROR_TYPE           0x0001
#define WIN_EVENTLOG_WARNING_TYPE         0x0002
#define WIN_EVENTLOG_INFORMATION_TYPE     0x0004
#define WIN_EVENTLOG_AUDIT_SUCCESS        0x0008
#define WIN_EVENTLOG_AUDIT_FAILURE        0x0010

/* win_serviceGetStatus() return values */
#define WIN_SERVICE_NO_CHANGE             0xFFFFFFFF
#define WIN_SERVICE_STOPPED               1
#define WIN_SERVICE_START_PENDING         2
#define WIN_SERVICE_STOP_PENDING          3
#define WIN_SERVICE_RUNNING               4
#define WIN_SERVICE_CONTINUE_PENDING      5
#define WIN_SERVICE_PAUSE_PENDING         6
#define WIN_SERVICE_PAUSED                7

/* win_serviceInstall() start type */
#define WIN_SERVICE_AUTO_START            0x00000002
#define WIN_SERVICE_BOOT_START            0x00000000
#define WIN_SERVICE_DEMAND_START          0x00000003
#define WIN_SERVICE_DISABLED              0x00000004
#define WIN_SERVICE_SYSTEM_START          0x00000001

/* wapi_MessageBox()/wapi_MessageBeep() flags */
#define WIN_MB_OK                         0x00000000
#define WIN_MB_OKCANCEL                   0x00000001
#define WIN_MB_ABORTRETRYIGNORE           0x00000002
#define WIN_MB_YESNOCANCEL                0x00000003
#define WIN_MB_YESNO                      0x00000004
#define WIN_MB_RETRYCANCEL                0x00000005
#define WIN_MB_CANCELTRYCONTINUE          0x00000006
#define WIN_MB_ICONHAND                   0x00000010
#define WIN_MB_ICONQUESTION               0x00000020
#define WIN_MB_ICONEXCLAMATION            0x00000030
#define WIN_MB_ICONASTERISK               0x00000040
#define WIN_MB_USERICON                   0x00000080
#define WIN_MB_ICONWARNING                WIN_MB_ICONEXCLAMATION
#define WIN_MB_ICONERROR                  WIN_MB_ICONHAND
#define WIN_MB_ICONINFORMATION            WIN_MB_ICONASTERISK
#define WIN_MB_ICONSTOP                   WIN_MB_ICONHAND
#define WIN_MB_DEFBUTTON1                 0x00000000
#define WIN_MB_DEFBUTTON2                 0x00000100
#define WIN_MB_DEFBUTTON3                 0x00000200
#define WIN_MB_DEFBUTTON4                 0x00000300
#define WIN_MB_APPLMODAL                  0x00000000
#define WIN_MB_SYSTEMMODAL                0x00001000
#define WIN_MB_TASKMODAL                  0x00002000
#define WIN_MB_HELP                       0x00004000
#define WIN_MB_NOFOCUS                    0x00008000
#define WIN_MB_SETFOREGROUND              0x00010000
#define WIN_MB_DEFAULT_DESKTOP_ONLY       0x00020000
#define WIN_MB_TOPMOST                    0x00040000
#define WIN_MB_RIGHT                      0x00080000
#define WIN_MB_RTLREADING                 0x00100000
#define WIN_MB_TYPEMASK                   0x0000000F
#define WIN_MB_ICONMASK                   0x000000F0
#define WIN_MB_DEFMASK                    0x00000F00
#define WIN_MB_MODEMASK                   0x00003000
#define WIN_MB_MISCMASK                   0x0000C000

/* win_SHFileOperation() functions */
#define WIN_FO_MOVE                       0x0001
#define WIN_FO_COPY                       0x0002
#define WIN_FO_DELETE                     0x0003
#define WIN_FO_RENAME                     0x0004

/* win_SHFileOperation() flags */
#define WIN_FOF_MULTIDESTFILES            0x0001
#define WIN_FOF_CONFIRMMOUSE              0x0002
#define WIN_FOF_SILENT                    0x0004
#define WIN_FOF_RENAMEONCOLLISION         0x0008
#define WIN_FOF_NOCONFIRMATION            0x0010
#define WIN_FOF_WANTMAPPINGHANDLE         0x0020
#define WIN_FOF_ALLOWUNDO                 0x0040
#define WIN_FOF_FILESONLY                 0x0080
#define WIN_FOF_SIMPLEPROGRESS            0x0100
#define WIN_FOF_NOCONFIRMMKDIR            0x0200
#define WIN_FOF_NOERRORUI                 0x0400
#define WIN_FOF_NOCOPYSECURITYATTRIBS     0x0800
#define WIN_FOF_NORECURSION               0x1000
#define WIN_FOF_NO_CONNECTED_ELEMENTS     0x2000
#define WIN_FOF_WANTNUKEWARNING           0x4000

/* win_SHFileOperation() results */
#define HB_WIN_DE_SAMEFILE                0x71      /* The source and destination files are the same file. */
#define HB_WIN_DE_MANYSRC1DEST            0x72      /* Multiple file paths were specified in the source buffer, but only one destination file path. */
#define HB_WIN_DE_DIFFDIR                 0x73      /* Rename operation was specified but the destination path is a different directory. Use the move operation instead. */
#define HB_WIN_DE_ROOTDIR                 0x74      /* The source is a root directory, which cannot be moved or renamed. */
#define HB_WIN_DE_OPCANCELLED             0x75      /* The operation was cancelled by the user, or silently cancelled if the appropriate flags were supplied to SHFileOperation. */
#define HB_WIN_DE_DESTSUBTREE             0x76      /* The destination is a subtree of the source. */
#define HB_WIN_DE_ACCESSDENIEDSRC         0x78      /* Security settings denied access to the source. */
#define HB_WIN_DE_PATHTOODEEP             0x79      /* The source or destination path exceeded or would exceed MAX_PATH. */
#define HB_WIN_DE_MANYDEST                0x7A      /* The operation involved multiple destination paths, which can fail in the case of a move operation. */
#define HB_WIN_DE_INVALIDFILES            0x7C      /* The path in the source or destination or both was invalid. */
#define HB_WIN_DE_DESTSAMETREE            0x7D      /* The source and destination have the same parent folder. */
#define HB_WIN_DE_FLDDESTISFILE           0x7E      /* The destination path is an existing file. */
#define HB_WIN_DE_FILEDESTISFLD           0x80      /* The destination path is an existing folder. */
#define HB_WIN_DE_FILENAMETOOLONG         0x81      /* The name of the file exceeds MAX_PATH. */
#define HB_WIN_DE_DEST_IS_CDROM           0x82      /* The destination is a read-only CD-ROM, possibly unformatted. */
#define HB_WIN_DE_DEST_IS_DVD             0x83      /* The destination is a read-only DVD, possibly unformatted. */
#define HB_WIN_DE_DEST_IS_CDRECORD        0x84      /* The destination is a writable CD-ROM, possibly unformatted. */
#define HB_WIN_DE_FILE_TOO_LARGE          0x85      /* The file involved in the operation is too large for the destination media or file system. */
#define HB_WIN_DE_SRC_IS_CDROM            0x86      /* The source is a read-only CD-ROM, possibly unformatted. */
#define HB_WIN_DE_SRC_IS_DVD              0x87      /* The source is a read-only DVD, possibly unformatted. */
#define HB_WIN_DE_SRC_IS_CDRECORD         0x88      /* The source is a writable CD-ROM, possibly unformatted. */
#define HB_WIN_DE_ERROR_MAX               0xB7      /* MAX_PATH was exceeded during the operation. */
#define HB_WIN_DE_UNKNOWN_                0x402     /* An unknown error occurred. This is typically due to an invalid path in the source or destination. This error does not occur on Windows Vista and later. */
#define HB_WIN_DE_ERRORONDEST_            0x10000   /* An unspecified error occurred on the destination. */
#define HB_WIN_DE_ROOTDIR_ERRORONDEST_    0x10074   /* Destination is a root directory and cannot be renamed. */

/* RPC status code (win_UuidCreateString() first reference parameter) */
#define HB_RPC_S_ERROR                    ( -1 )
#define RPC_S_OK                          0
#define RPC_S_INVALID_ARG                 87
#define RPC_S_OUT_OF_MEMORY               14
#define RPC_S_OUT_OF_THREADS              164
#define RPC_S_INVALID_LEVEL               87
#define RPC_S_BUFFER_TOO_SMALL            122
#define RPC_S_INVALID_SECURITY_DESC       1338
#define RPC_S_ACCESS_DENIED               5
#define RPC_S_SERVER_OUT_OF_MEMORY        1130
#define RPC_S_ASYNC_CALL_PENDING          997
#define RPC_S_UNKNOWN_PRINCIPAL           1332
#define RPC_S_TIMEOUT                     1460

#endif /* HBWIN_CH_ */
