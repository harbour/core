/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * hbwin header
 *
 * Copyright 2008 Viktor Szakats (harbour.01 syenar.hu)
 * Copyright 2004 Peter Rees <peter@rees.co.nz>
 *                Rees Software & Systems Ltd
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

#ifndef HBWIN_CH_
#define HBWIN_CH_

/* Registry related values */

#define HKEY_CLASSES_ROOT      0x80000000
#define HKEY_CURRENT_USER      0x80000001
#define HKEY_LOCAL_MACHINE     0x80000002
#define HKEY_USERS             0x80000003
#define HKEY_PERFORMANCE_DATA  0x80000004
#define HKEY_CURRENT_CONFIG    0x80000005
#define HKEY_DYN_DATA          0x80000006

/* win_Port() related values */

/* The following are from winbase.h */

#define CBR_110                110
#define CBR_300                300
#define CBR_600                600
#define CBR_1200               1200
#define CBR_2400               2400
#define CBR_4800               4800
#define CBR_9600               9600
#define CBR_14400              14400
#define CBR_19200              19200
#define CBR_38400              38400
#define CBR_56000              56000
#define CBR_57600              57600
#define CBR_115200             115200
#define CBR_128000             128000
#define CBR_256000             256000

#define NOPARITY               0
#define ODDPARITY              1
#define EVENPARITY             2
#define MARKPARITY             3
#define SPACEPARITY            4

#define ONESTOPBIT             0
#define ONE5STOPBITS           1
#define TWOSTOPBITS            2

/* DTR Control Flow Values. */
#define DTR_CONTROL_DISABLE    0x00
#define DTR_CONTROL_ENABLE     0x01
#define DTR_CONTROL_HANDSHAKE  0x02

/* RTS Control Flow Values */
#define RTS_CONTROL_DISABLE    0x00
#define RTS_CONTROL_ENABLE     0x01
#define RTS_CONTROL_HANDSHAKE  0x02
#define RTS_CONTROL_TOGGLE     0x03

#ifndef INVALID_HANDLE_VALUE
#define INVALID_HANDLE_VALUE    -1
#endif

/* win_Prn() related values */

#define FORM_A4                9

#define PS_SOLID               0

#define RGB( nR, nG, nB )      ( nR + ( nG * 256 ) + ( nB * 256 * 256 ) )

#define RGB_BLACK              RGB( 0x00, 0x00, 0x00 )
#define RGB_BLUE               RGB( 0x00, 0x00, 0x85 )
#define RGB_GREEN              RGB( 0x00, 0x85, 0x00 )
#define RGB_CYAN               RGB( 0x00, 0x85, 0x85 )
#define RGB_RED                RGB( 0x85, 0x00, 0x00 )
#define RGB_MAGENTA            RGB( 0x85, 0x00, 0x85 )
#define RGB_BROWN              RGB( 0x85, 0x85, 0x00 )
#define RGB_WHITE              RGB( 0xC6, 0xC6, 0xC6 )

/* Cut from wingdi.h */

#define MM_TEXT                1
#define MM_LOMETRIC            2
#define MM_HIMETRIC            3
#define MM_LOENGLISH           4
#define MM_HIENGLISH           5

/* Device Parameters for win_GetDeviceCaps() */

#define HORZSIZE               4   // Horizontal size in millimeters
#define VERTSIZE               6   // Vertical size in millimeters
#define HORZRES                8   // Horizontal width in pixels
#define VERTRES                10  // Vertical height in pixels
#define NUMBRUSHES             16  // Number of brushes the device has
#define NUMPENS                18  // Number of pens the device has
#define NUMFONTS               22  // Number of fonts the device has
#define NUMCOLORS              24  // Number of colors the device supports
#define RASTERCAPS             38  // Bitblt capabilities

#define LOGPIXELSX             88  // Logical pixels/inch in X
#define LOGPIXELSY             90  // Logical pixels/inch in Y

#define PHYSICALWIDTH          110 // Physical Width in device units
#define PHYSICALHEIGHT         111 // Physical Height in device units
#define PHYSICALOFFSETX        112 // Physical Printable Area x margin
#define PHYSICALOFFSETY        113 // Physical Printable Area y margin
#define SCALINGFACTORX         114 // Scaling factor x
#define SCALINGFACTORY         115 // Scaling factor y

/* bin selections */
#define DMBIN_FIRST            DMBIN_UPPER
#define DMBIN_UPPER            1
#define DMBIN_ONLYONE          1
#define DMBIN_LOWER            2
#define DMBIN_MIDDLE           3
#define DMBIN_MANUAL           4
#define DMBIN_ENVELOPE         5
#define DMBIN_ENVMANUAL        6
#define DMBIN_AUTO             7
#define DMBIN_TRACTOR          8
#define DMBIN_SMALLFMT         9
#define DMBIN_LARGEFMT         10
#define DMBIN_LARGECAPACITY    11
#define DMBIN_CASSETTE         14
#define DMBIN_FORMSOURCE       15
#define DMBIN_LAST             DMBIN_FORMSOURCE

/* print qualities */
#define DMRES_DRAFT            ( -1 )
#define DMRES_LOW              ( -2 )
#define DMRES_MEDIUM           ( -3 )
#define DMRES_HIGH             ( -4 )

/* duplex enable */
#define DMDUP_SIMPLEX          1
#define DMDUP_VERTICAL         2
#define DMDUP_HORIZONTAL       3

/* Text Alignment Options */
#define TA_NOUPDATECP          0
#define TA_UPDATECP            1

#define TA_LEFT                0
#define TA_RIGHT               2
#define TA_CENTER              6

#define TA_TOP                 0
#define TA_BOTTOM              8
#define TA_BASELINE            24

#define MM_TO_INCH             25.4

#endif /* HBWIN_CH_ */
