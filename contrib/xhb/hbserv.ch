/*
 * The Service/Daemon support (Includes also signal/low-level error management)
 *
 * Copyright 2003 Giancarlo Niccolai <gian@niccolai.ws>
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
 * along with this program; see the file LICENSE.txt.  If not, write to
 * the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
 * Boston, MA 02110-1301 USA (or visit https://www.gnu.org/licenses/).
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


/* Index in the OS dependent signal array that is passed to the
   signal handler as a parameter
   1: low-level signal
   2: low-level subsignal
   3: low-level system error
   4: address that rose the signal
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
