/*
 * Harbour Project source code:
 * Windows functions (event handling)
 *
 * Copyright 2010 Viktor Szakats (vszakats.net/harbour)
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

#include "hbwapi.h"
#include "hbapierr.h"

HB_FUNC( WIN_REPORTEVENT )
{
   HB_BOOL bRetVal = HB_FALSE;

#if ! defined( HB_OS_WIN_CE )
   HANDLE hEventLog;

   void * hServerName;
   void * hSourceName;

   hEventLog = RegisterEventSource( HB_PARSTR( 1, &hServerName, NULL ),
                                    HB_PARSTRDEF( 2, &hSourceName, NULL ) );

   hb_strfree( hServerName );
   hb_strfree( hSourceName );

   if( hEventLog != NULL && hEventLog != ( HANDLE ) ERROR_ACCESS_DENIED )
   {
      WORD wNumStrings = 0;
      LPCTSTR * lpStrings = NULL;
      void ** hStrings = NULL;

      PHB_ITEM pStrings = hb_param( 6, HB_IT_ARRAY );

      if( pStrings && ( wNumStrings = ( WORD ) hb_arrayLen( pStrings ) ) > 0 )
      {
         WORD i;

         lpStrings = ( LPCTSTR * ) hb_xgrab( sizeof( LPCTSTR ) * wNumStrings );
         hStrings = ( void ** ) hb_xgrab( sizeof( void * ) * wNumStrings );

         for( i = 0; i < wNumStrings; ++i )
            lpStrings[ i ] = ( LPCTSTR ) HB_ARRAYGETSTR( pStrings, i + 1, &hStrings[ i ], NULL );
      }
      else if( HB_ISCHAR( 6 ) )
      {
         wNumStrings = 1;

         lpStrings = ( LPCTSTR * ) hb_xgrab( sizeof( LPCTSTR ) );
         hStrings = ( void ** ) hb_xgrab( sizeof( void * ) );

         lpStrings[ 0 ] = ( LPCTSTR ) HB_ITEMGETSTR( hb_param( 6, HB_IT_STRING ), &hStrings[ 0 ], NULL );
      }

      if( ReportEvent( hEventLog,
                       ( WORD ) hb_parni( 3 ) /* wType */,
                       ( WORD ) hb_parni( 4 ) /* wCategory */,
                       ( DWORD ) hb_parnint( 5 ) /* dwEventID */,
                       NULL /* lpUserSid */,
                       wNumStrings,
                       ( DWORD ) hb_parclen( 7 ),
                       lpStrings,
                       ( LPVOID ) hb_parc( 7 ) ) )
         bRetVal = HB_TRUE;

      if( lpStrings )
      {
         while( wNumStrings )
            hb_strfree( hStrings[ --wNumStrings ] );

         hb_xfree( hStrings );
         hb_xfree( ( void * ) lpStrings );
      }

      DeregisterEventSource( hEventLog );
   }
#endif

   hb_retl( bRetVal );
}
