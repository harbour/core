/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * hbwin header
 *
 * Copyright 2008 Viktor Szakats <harbour.01 syenar.hu>
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

#define HKEY_CLASSES_ROOT                  0x80000000
#define HKEY_CURRENT_USER                  0x80000001
#define HKEY_LOCAL_MACHINE                 0x80000002
#define HKEY_USERS                         0x80000003
#define HKEY_PERFORMANCE_DATA              0x80000004
#define HKEY_CURRENT_CONFIG                0x80000005
#define HKEY_DYN_DATA                      0x80000006

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

#endif /* HBWIN_CH_ */
