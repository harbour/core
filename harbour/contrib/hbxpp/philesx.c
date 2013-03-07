/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * CurDrive() function
 *
 * Copyright 1999-2001 Viktor Szakats (harbour syenar.net)
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

#include "hbapi.h"
#include "hbapifs.h"
#include "hbapierr.h"

HB_FUNC( CURDRIVE )
{
#if defined( HB_OS_HAS_DRIVE_LETTER )
   char szCurDrive[ 1 ];
   const char * szDrive;

   szCurDrive[ 0 ] = ( ( char ) hb_fsCurDrv() ) + 'A';
   hb_retclen( szCurDrive, 1 );

   szDrive = hb_parc( 1 );
   if( szDrive )
   {
      int iDrive = -1;

      if( *szDrive >= 'A' && *szDrive <= 'Z' )
         iDrive = *szDrive - 'A';
      else if( *szDrive >= 'a' && *szDrive <= 'z' )
         iDrive = *szDrive - 'a';

      if( iDrive >= 0 )
      {
         while( hb_fsChDrv( iDrive ) != 0 )
         {
            HB_USHORT uiAction = hb_errRT_BASE_Ext1( EG_OPEN, 6001, "Operating system error",
                                                     HB_ERR_FUNCNAME, 0, EF_CANDEFAULT | EF_CANRETRY,
                                                     HB_ERR_ARGS_BASEPARAMS );
            if( uiAction != E_RETRY )
               break;
         }
      }
   }
#else
   hb_retc_null();
#endif
}
