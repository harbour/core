/*
 * Compatibility header
 *
 * Copyright 2010 Viktor Szakats (vszakats.net/harbour)
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

#ifndef _WIN32PRN_CH
#define _WIN32PRN_CH

#include "hbwin.ch"

/* Color */
#define BLACK                       WIN_RGB( 0x00, 0x00, 0x00 )
#define BLUE                        WIN_RGB( 0x00, 0x00, 0xFF )
#define BROWN                       WIN_RGB( 0x80, 0x0A, 0x0A )
#define CYAN                        WIN_RGB( 0x00, 0xFF, 0xFF )
#define GRAY                        WIN_RGB( 0x80, 0x80, 0x80 )
#define GREEN                       WIN_RGB( 0x00, 0xFF, 0x00 )
#define MAGENTA                     WIN_RGB( 0xFF, 0x00, 0xFF )
#define ORANGE                      WIN_RGB( 0XFF, 0X80, 0X0A )
#define PINK                        WIN_RGB( 0xFF, 0xC0, 0xCB )
#define PURPLE                      WIN_RGB( 0x80, 0x00, 0x80 )
#define RED                         WIN_RGB( 0xFF, 0x00, 0x00 )
#define WHITE                       WIN_RGB( 0xFF, 0xFF, 0xFF )
#define YELLOW                      WIN_RGB( 0xFF, 0xFF, 0x00 )
#define DARKGRAY                    WIN_RGB( 0xA9, 0xA9, 0xA9 )
#define LIGHTGRAY                   WIN_RGB( 0xD3, 0xD3, 0xD3 )

/* Deprecated constants and macros */
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
#define RGB_BROWN                   HB_WIN_RGB_YELLOW
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

#define MM_TO_INCH                  WIN_MM_TO_INCH

/* Background mode */
#define TRANSPARENT                 WIN_TRANSPARENT
#define OPAQUE                      WIN_OPAQUE

/* Brush Styles */
#define BS_SOLID                    WIN_BS_SOLID
#define BS_NULL                     WIN_BS_NULL
#define BS_HOLLOW                   WIN_BS_HOLLOW
#define BS_HATCHED                  WIN_BS_HATCHED
#define BS_PATTERN                  WIN_BS_PATTERN
#define BS_INDEXED                  WIN_BS_INDEXED
#define BS_DIBPATTERN               WIN_BS_DIBPATTERN
#define BS_DIBPATTERNPT             WIN_BS_DIBPATTERNPT
#define BS_PATTERN8X8               WIN_BS_PATTERN8X8
#define BS_DIBPATTERN8X8            WIN_BS_DIBPATTERN8X8
#define BS_MONOPATTERN              WIN_BS_MONOPATTERN

/* Hatch Styles */
#define HS_HORIZONTAL               WIN_HS_HORIZONTAL
#define HS_VERTICAL                 WIN_HS_VERTICAL
#define HS_FDIAGONAL                WIN_HS_FDIAGONAL
#define HS_BDIAGONAL                WIN_HS_BDIAGONAL
#define HS_CROSS                    WIN_HS_CROSS
#define HS_DIAGCROSS                WIN_HS_DIAGCROSS

/* Pen Styles */
#define PS_SOLID                    WIN_PS_SOLID
#define PS_DASH                     WIN_PS_DASH
#define PS_DOT                      WIN_PS_DOT
#define PS_DASHDOT                  WIN_PS_DASHDOT
#define PS_DASHDOTDOT               WIN_PS_DASHDOTDOT
#define PS_NULL                     WIN_PS_NULL
#define PS_INSIDEFRAME              WIN_PS_INSIDEFRAME
#define PS_USERSTYLE                WIN_PS_USERSTYLE
#define PS_ALTERNATE                WIN_PS_ALTERNATE
#define PS_STYLE_MASK               WIN_PS_STYLE_MASK

/* Stock Logical Objects */
#define WHITE_BRUSH                 WIN_WHITE_BRUSH
#define LTGRAY_BRUSH                WIN_LTGRAY_BRUSH
#define GRAY_BRUSH                  WIN_GRAY_BRUSH
#define DKGRAY_BRUSH                WIN_DKGRAY_BRUSH
#define BLACK_BRUSH                 WIN_BLACK_BRUSH
#define NULL_BRUSH                  WIN_NULL_BRUSH
#define HOLLOW_BRUSH                WIN_HOLLOW_BRUSH

#define WHITE_PEN                   WIN_WHITE_PEN
#define BLACK_PEN                   WIN_BLACK_PEN
#define NULL_PEN                    WIN_NULL_PEN

#define OEM_FIXED_FONT              WIN_OEM_FIXED_FONT
#define ANSI_FIXED_FONT             WIN_ANSI_FIXED_FONT
#define ANSI_VAR_FONT               WIN_ANSI_VAR_FONT
#define SYSTEM_FONT                 WIN_SYSTEM_FONT
#define DEVICE_DEFAULT_FONT         WIN_DEVICE_DEFAULT_FONT
#define DEFAULT_PALETTE             WIN_DEFAULT_PALETTE
#define SYSTEM_FIXED_FONT           WIN_SYSTEM_FIXED_FONT

/* Draw Text Styles */
#define DT_TOP                      WIN_DT_TOP
#define DT_LEFT                     WIN_DT_LEFT
#define DT_CENTER                   WIN_DT_CENTER
#define DT_RIGHT                    WIN_DT_RIGHT
#define DT_VCENTER                  WIN_DT_VCENTER
#define DT_BOTTOM                   WIN_DT_BOTTOM
#define DT_WORDBREAK                WIN_DT_WORDBREAK
#define DT_SINGLELINE               WIN_DT_SINGLELINE
#define DT_EXPANDTABS               WIN_DT_EXPANDTABS
#define DT_TABSTOP                  WIN_DT_TABSTOP
#define DT_NOCLIP                   WIN_DT_NOCLIP
#define DT_EXTERNALLEADING          WIN_DT_EXTERNALLEADING
#define DT_CALCRECT                 WIN_DT_CALCRECT
#define DT_NOPREFIX                 WIN_DT_NOPREFIX
#define DT_INTERNAL                 WIN_DT_INTERNAL
#define DT_EDITCONTROL              WIN_DT_EDITCONTROL
#define DT_PATH_ELLIPSIS            WIN_DT_PATH_ELLIPSIS
#define DT_END_ELLIPSIS             WIN_DT_END_ELLIPSIS
#define DT_MODIFYSTRING             WIN_DT_MODIFYSTRING
#define DT_RTLREADING               WIN_DT_RTLREADING
#define DT_WORD_ELLIPSIS            WIN_DT_WORD_ELLIPSIS
#define DT_NOFULLWIDTHCHARBREAK     WIN_DT_NOFULLWIDTHCHARBREAK
#define DT_HIDEPREFIX               WIN_DT_HIDEPREFIX
#define DT_PREFIXONLY               WIN_DT_PREFIXONLY

/* Device mode: Paper Size */
#define DMPAPER_FIRST                          WIN_DMPAPER_FIRST
#define DMPAPER_LETTER                         WIN_DMPAPER_LETTER
#define DMPAPER_LETTERSMALL                    WIN_DMPAPER_LETTERSMALL
#define DMPAPER_TABLOID                        WIN_DMPAPER_TABLOID
#define DMPAPER_LEDGER                         WIN_DMPAPER_LEDGER
#define DMPAPER_LEGAL                          WIN_DMPAPER_LEGAL
#define DMPAPER_STATEMENT                      WIN_DMPAPER_STATEMENT
#define DMPAPER_EXECUTIVE                      WIN_DMPAPER_EXECUTIVE
#define DMPAPER_A3                             WIN_DMPAPER_A3
#define DMPAPER_A4                             WIN_DMPAPER_A4
#define DMPAPER_A4SMALL                        WIN_DMPAPER_A4SMALL
#define DMPAPER_A5                             WIN_DMPAPER_A5
#define DMPAPER_B4                             WIN_DMPAPER_B4
#define DMPAPER_B5                             WIN_DMPAPER_B5
#define DMPAPER_FOLIO                          WIN_DMPAPER_FOLIO
#define DMPAPER_QUARTO                         WIN_DMPAPER_QUARTO
#define DMPAPER_10X14                          WIN_DMPAPER_10X14
#define DMPAPER_11X17                          WIN_DMPAPER_11X17
#define DMPAPER_NOTE                           WIN_DMPAPER_NOTE
#define DMPAPER_ENV_9                          WIN_DMPAPER_ENV_9
#define DMPAPER_ENV_10                         WIN_DMPAPER_ENV_10
#define DMPAPER_ENV_11                         WIN_DMPAPER_ENV_11
#define DMPAPER_ENV_12                         WIN_DMPAPER_ENV_12
#define DMPAPER_ENV_14                         WIN_DMPAPER_ENV_14
#define DMPAPER_CSHEET                         WIN_DMPAPER_CSHEET
#define DMPAPER_DSHEET                         WIN_DMPAPER_DSHEET
#define DMPAPER_ESHEET                         WIN_DMPAPER_ESHEET
#define DMPAPER_ENV_DL                         WIN_DMPAPER_ENV_DL
#define DMPAPER_ENV_C5                         WIN_DMPAPER_ENV_C5
#define DMPAPER_ENV_C3                         WIN_DMPAPER_ENV_C3
#define DMPAPER_ENV_C4                         WIN_DMPAPER_ENV_C4
#define DMPAPER_ENV_C6                         WIN_DMPAPER_ENV_C6
#define DMPAPER_ENV_C65                        WIN_DMPAPER_ENV_C65
#define DMPAPER_ENV_B4                         WIN_DMPAPER_ENV_B4
#define DMPAPER_ENV_B5                         WIN_DMPAPER_ENV_B5
#define DMPAPER_ENV_B6                         WIN_DMPAPER_ENV_B6
#define DMPAPER_ENV_ITALY                      WIN_DMPAPER_ENV_ITALY
#define DMPAPER_ENV_MONARCH                    WIN_DMPAPER_ENV_MONARCH
#define DMPAPER_ENV_PERSONAL                   WIN_DMPAPER_ENV_PERSONAL
#define DMPAPER_FANFOLD_US                     WIN_DMPAPER_FANFOLD_US
#define DMPAPER_FANFOLD_STD_GERMAN             WIN_DMPAPER_FANFOLD_STD_GERMAN
#define DMPAPER_FANFOLD_LGL_GERMAN             WIN_DMPAPER_FANFOLD_LGL_GERMAN
#define DMPAPER_ISO_B4                         WIN_DMPAPER_ISO_B4
#define DMPAPER_JAPANESE_POSTCARD              WIN_DMPAPER_JAPANESE_POSTCARD
#define DMPAPER_9X11                           WIN_DMPAPER_9X11
#define DMPAPER_10X11                          WIN_DMPAPER_10X11
#define DMPAPER_15X11                          WIN_DMPAPER_15X11
#define DMPAPER_ENV_INVITE                     WIN_DMPAPER_ENV_INVITE
#define DMPAPER_RESERVED_48                    WIN_DMPAPER_RESERVED_48
#define DMPAPER_RESERVED_49                    WIN_DMPAPER_RESERVED_49
#define DMPAPER_LETTER_EXTRA                   WIN_DMPAPER_LETTER_EXTRA
#define DMPAPER_LEGAL_EXTRA                    WIN_DMPAPER_LEGAL_EXTRA
#define DMPAPER_TABLOID_EXTRA                  WIN_DMPAPER_TABLOID_EXTRA
#define DMPAPER_A4_EXTRA                       WIN_DMPAPER_A4_EXTRA
#define DMPAPER_LETTER_TRANSVERSE              WIN_DMPAPER_LETTER_TRANSVERSE
#define DMPAPER_A4_TRANSVERSE                  WIN_DMPAPER_A4_TRANSVERSE
#define DMPAPER_LETTER_EXTRA_TRANSVERSE        WIN_DMPAPER_LETTER_EXTRA_TRANSVERSE56
#define DMPAPER_A_PLUS                         WIN_DMPAPER_A_PLUS
#define DMPAPER_B_PLUS                         WIN_DMPAPER_B_PLUS
#define DMPAPER_LETTER_PLUS                    WIN_DMPAPER_LETTER_PLUS
#define DMPAPER_A4_PLUS                        WIN_DMPAPER_A4_PLUS
#define DMPAPER_A5_TRANSVERSE                  WIN_DMPAPER_A5_TRANSVERSE
#define DMPAPER_B5_TRANSVERSE                  WIN_DMPAPER_B5_TRANSVERSE
#define DMPAPER_A3_EXTRA                       WIN_DMPAPER_A3_EXTRA
#define DMPAPER_A5_EXTRA                       WIN_DMPAPER_A5_EXTRA
#define DMPAPER_B5_EXTRA                       WIN_DMPAPER_B5_EXTRA
#define DMPAPER_A2                             WIN_DMPAPER_A2
#define DMPAPER_A3_TRANSVERSE                  WIN_DMPAPER_A3_TRANSVERSE
#define DMPAPER_A3_EXTRA_TRANSVERSE            WIN_DMPAPER_A3_EXTRA_TRANSVERSE
#define DMPAPER_DBL_JAPANESE_POSTCARD          WIN_DMPAPER_DBL_JAPANESE_POSTCARD
#define DMPAPER_A6                             WIN_DMPAPER_A6
#define DMPAPER_JENV_KAKU2                     WIN_DMPAPER_JENV_KAKU2
#define DMPAPER_JENV_KAKU3                     WIN_DMPAPER_JENV_KAKU3
#define DMPAPER_JENV_CHOU3                     WIN_DMPAPER_JENV_CHOU3
#define DMPAPER_JENV_CHOU4                     WIN_DMPAPER_JENV_CHOU4
#define DMPAPER_LETTER_ROTATED                 WIN_DMPAPER_LETTER_ROTATED
#define DMPAPER_A3_ROTATED                     WIN_DMPAPER_A3_ROTATED
#define DMPAPER_A4_ROTATED                     WIN_DMPAPER_A4_ROTATED
#define DMPAPER_A5_ROTATED                     WIN_DMPAPER_A5_ROTATED
#define DMPAPER_B4_JIS_ROTATED                 WIN_DMPAPER_B4_JIS_ROTATED
#define DMPAPER_B5_JIS_ROTATED                 WIN_DMPAPER_B5_JIS_ROTATED
#define DMPAPER_JAPANESE_POSTCARD_ROTATED      WIN_DMPAPER_JAPANESE_POSTCARD_ROTATED
#define DMPAPER_DBL_JAPANESE_POSTCARD_ROTATED  WIN_DMPAPER_DBL_JAPANESE_POSTCARD_ROTATED
#define DMPAPER_A6_ROTATED                     WIN_DMPAPER_A6_ROTATED
#define DMPAPER_JENV_KAKU2_ROTATED             WIN_DMPAPER_JENV_KAKU2_ROTATED
#define DMPAPER_JENV_KAKU3_ROTATED             WIN_DMPAPER_JENV_KAKU3_ROTATED
#define DMPAPER_JENV_CHOU3_ROTATED             WIN_DMPAPER_JENV_CHOU3_ROTATED
#define DMPAPER_JENV_CHOU4_ROTATED             WIN_DMPAPER_JENV_CHOU4_ROTATED
#define DMPAPER_B6_JIS                         WIN_DMPAPER_B6_JIS
#define DMPAPER_B6_JIS_ROTATED                 WIN_DMPAPER_B6_JIS_ROTATED
#define DMPAPER_12X11                          WIN_DMPAPER_12X11
#define DMPAPER_JENV_YOU4                      WIN_DMPAPER_JENV_YOU4
#define DMPAPER_JENV_YOU4_ROTATED              WIN_DMPAPER_JENV_YOU4_ROTATED
#define DMPAPER_P16K                           WIN_DMPAPER_P16K
#define DMPAPER_P32K                           WIN_DMPAPER_P32K
#define DMPAPER_P32KBIG                        WIN_DMPAPER_P32KBIG
#define DMPAPER_PENV_1                         WIN_DMPAPER_PENV_1
#define DMPAPER_PENV_2                         WIN_DMPAPER_PENV_2
#define DMPAPER_PENV_3                         WIN_DMPAPER_PENV_3
#define DMPAPER_PENV_4                         WIN_DMPAPER_PENV_4
#define DMPAPER_PENV_5                         WIN_DMPAPER_PENV_5
#define DMPAPER_PENV_6                         WIN_DMPAPER_PENV_6
#define DMPAPER_PENV_7                         WIN_DMPAPER_PENV_7
#define DMPAPER_PENV_8                         WIN_DMPAPER_PENV_8
#define DMPAPER_PENV_9                         WIN_DMPAPER_PENV_9
#define DMPAPER_PENV_10                        WIN_DMPAPER_PENV_10
#define DMPAPER_P16K_ROTATED                   WIN_DMPAPER_P16K_ROTATED
#define DMPAPER_P32K_ROTATED                   WIN_DMPAPER_P32K_ROTATED
#define DMPAPER_P32KBIG_ROTATED                WIN_DMPAPER_P32KBIG_ROTATED
#define DMPAPER_PENV_1_ROTATED                 WIN_DMPAPER_PENV_1_ROTATED
#define DMPAPER_PENV_2_ROTATED                 WIN_DMPAPER_PENV_2_ROTATED
#define DMPAPER_PENV_3_ROTATED                 WIN_DMPAPER_PENV_3_ROTATED
#define DMPAPER_PENV_4_ROTATED                 WIN_DMPAPER_PENV_4_ROTATED
#define DMPAPER_PENV_5_ROTATED                 WIN_DMPAPER_PENV_5_ROTATED
#define DMPAPER_PENV_6_ROTATED                 WIN_DMPAPER_PENV_6_ROTATED
#define DMPAPER_PENV_7_ROTATED                 WIN_DMPAPER_PENV_7_ROTATED
#define DMPAPER_PENV_8_ROTATED                 WIN_DMPAPER_PENV_8_ROTATED
#define DMPAPER_PENV_9_ROTATED                 WIN_DMPAPER_PENV_9_ROTATED
#define DMPAPER_PENV_10_ROTATED                WIN_DMPAPER_PENV_10_ROTATED
#define DMPAPER_USER                           WIN_DMPAPER_USER

#endif
