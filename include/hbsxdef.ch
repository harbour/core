/*
 * Harbour Project source code:
 *    SIx3 compatible constants
 *
 * Copyright 2007 Przemyslaw Czerpak <druzus / at / priv.onet.pl>
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

#ifndef HB_SIX_DEF_CH_
#define HB_SIX_DEF_CH_

/*
 * Event Constants for Trigger System
 */
#define EVENT_PREUSE          1
#define EVENT_POSTUSE         2
#define EVENT_UPDATE          3
#define EVENT_APPEND          4
#define EVENT_DELETE          5
#define EVENT_RECALL          6
#define EVENT_PACK            7
#define EVENT_ZAP             8
#define EVENT_PUT             9
#define EVENT_GET             10
#define EVENT_PRECLOSE        11
#define EVENT_POSTCLOSE       12
#define EVENT_PREMEMOPACK     13
#define EVENT_POSTMEMOPACK    14

/*
 * Trigger Toggle Values
 */
#define TRIGGER_ENABLE        1
#define TRIGGER_DISABLE       2
#define TRIGGER_REMOVE        3
#define TRIGGER_INSTALL       4
#define TRIGGER_PENDING       5  /* Internal Use Only */


/*
 * sx_File2Blob() actions
 */
#define BLOB_FILECOMPRESS     1
#define BLOB_FILEENCRYPT      2


#endif /* HB_SIX_DEF_CH_ */
