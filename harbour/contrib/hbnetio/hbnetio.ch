/*
 * $Id$
 */

/*
 * Harbour Project source code:
 *    HBNETIO header file
 *
 * Copyright 2011 Przemyslaw Czerpak <druzus / at / priv.onet.pl>
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
 * along with this software; see the file COPYING.txt.  If not, write to
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

#ifndef HBNETIO_CH_
#define HBNETIO_CH_

/* netio errors */
#define NETIO_ERR_UNKNOWN_COMMAND   0xff01
#define NETIO_ERR_WRONG_PARAM       0xff02
#define NETIO_ERR_WRONG_FILE_PATH   0xff03
#define NETIO_ERR_WRONG_FILE_HANDLE 0xff04
#define NETIO_ERR_WRONG_FILE_SIZE   0xff05
#define NETIO_ERR_WRONG_STREAMID    0xff06
#define NETIO_ERR_FILES_MAX         0xff07
#define NETIO_ERR_READ              0xff08
#define NETIO_ERR_FILE_IO           0xff09
#define NETIO_ERR_NOT_EXISTS        0xff0a
#define NETIO_ERR_UNSUPPORTED       0xff0b
#define NETIO_ERR_REFUSED           0xff0c

/* netio server connection status */
#define NETIO_SRVSTAT_RUNNING       0
#define NETIO_SRVSTAT_WRONGHANDLE   -1
#define NETIO_SRVSTAT_CLOSED        -2
#define NETIO_SRVSTAT_STOPPED       -3
#define NETIO_SRVSTAT_DATASTREAM    1
#define NETIO_SRVSTAT_ITEMSTREAM    2

/* netio server connection info */
#define NETIO_SRVINFO_FILESCOUNT    -1
#define NETIO_SRVINFO_BYTESSENT     -2
#define NETIO_SRVINFO_BYTESRECEIVED -3
#define NETIO_SRVINFO_PEERADDRESS   -4

#endif /* HBNETIO_CH_ */
