/*
 * $Id$
 */

/*
 * xHarbour Project source code:
 * GetVolInfo()  - Ready.  This function is new.
 *
 * Copyright 2004-2005 Eduardo Fernandes <modalsist@yahoo.com.br>
 * www - http://www.xharbour.org
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2, or ( at your option )
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
 * Boston, MA 02111-1307 USA ( or visit the web site http://www.gnu.org/ ).
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

#define HB_OS_WIN_USED

#include "hbapi.h"
#include "hbwinuni.h"

/*
 * GetVolInfo() is a new function. It returns the volume name of a Floppy, CD,
 * Hard-disk or mapped network drive.
 * Syntax is: GetVolInfo("X:\")
 * Note that the trailing backslash is required.
 */
HB_FUNC( GETVOLINFO )
{
#if defined( HB_OS_WIN ) && ! defined( HB_OS_WIN_CE )
   void * hDrive = NULL;
   TCHAR lpVolName[ MAX_PATH + 1 ];

   if( GetVolumeInformation( hb_parclen( 1 ) > 0 ? HB_PARSTR( 1, &hDrive, NULL ) : NULL,
                             lpVolName,
                             HB_SIZEOFARRAY( lpVolName ),
                             NULL, NULL, NULL, NULL, 0 ) )
      HB_RETSTR( lpVolName );
   else
      hb_retc_null();

   hb_strfree( hDrive );
#else
   hb_retc_null();
#endif
}
