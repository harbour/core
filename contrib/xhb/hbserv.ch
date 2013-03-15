/*
 * $Id$
 */

/*
 * xHarbour Project source code:
 * The Service/Daemon support
 * (Includes also signal/low level error management)
 *
 * Copyright 2003 Giancarlo Niccolai [gian@niccolai.ws]
 * www - http://www.xharbour.org
 *
 * this program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General public License as published by
 * the Free Software Foundation; either version 2, or (at your option)
 * any later version.
 *
 * this program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS for A PARTICULAR PURPOSE.  See the
 * GNU General public License for more details.
 *
 * You should have received a copy of the GNU General public License
 * along with this software; see the file COPYING.txt.  if not, write to
 * the Free Software Foundation, Inc., 59 Temple Place, Suite 330,
 * Boston, MA 02111-1307 USA (or visit the web site http://www.gnu.org/).
 *
 * As a special exception, xHarbour license gives permission for
 * additional uses of the text contained in its release of xHarbour.
 *
 * The exception is that, if you link the xHarbour libraries with other
 * files to produce an executable, this does not by itself cause the
 * resulting executable to be covered by the GNU General public License.
 * Your use of that executable is in no way restricted on account of
 * linking the xHarbour library code into it.
 *
 * this exception does not however invalidate any other reasons why
 * the executable file might be covered by the GNU General public License.
 *
 * this exception applies only to the code released with this xHarbour
 * explicit exception.  if you add/copy code from other sources,
 * as the General public License permits, the above exception does
 * not apply to the code that you add in this way.  To avoid misleading
 * anyone as to the status of such modified files, you must delete
 * this exception notice from them.
 *
 * If you write modifications of your own for xHarbour, it is your choice
 * whether to permit this exception to apply to your modifications.
 * if you do not wish that, delete this exception notice.
 *
 */

#ifndef HB_SERVICE_CH
#define HB_SERVICE_CH

/* Abstract signal types */
#define HB_SIGNAL_QUIT      0x0001
#define HB_SIGNAL_INTERRUPT 0x0002
#define HB_SIGNAL_REFRESH   0x0004
#define HB_SIGNAL_MATHERR   0x0010
#define HB_SIGNAL_FAULT     0x0020
#define HB_SIGNAL_USER1     0x0040
#define HB_SIGNAL_USER2     0x0080
#define HB_SIGNAL_UNKNOWN   0xf000
#define HB_SIGNAL_ALL       0xffff

/* Signal handler return types */
#define HB_SERVICE_CONTINUE  1
#define HB_SERVICE_HANDLED   2
#define HB_SERVICE_QUIT      3


/* Index in the OS dependant signal array that is passed to the
   signal handler as a parameter
   1: low-level signal
   2: low-level subsignal
   3: low-level system error
   4: address that rised the signal
   5: process id of the signal riser
   6: UID of the riser
*/

#define HB_SERVICE_OSSIGNAL  1
#define HB_SERVICE_OSSUBSIG  2
#define HB_SERVICE_OSERROR   3
#define HB_SERVICE_ADDRESS   4
#define HB_SERVICE_PROCESS   5
#define HB_SERVICE_UID       6

#endif
