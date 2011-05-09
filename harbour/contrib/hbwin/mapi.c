/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Simple MAPI wrapper
 *
 * Copyright 2009 Viktor Szakats (harbour.01 syenar.hu)
 * Copyright 2009 Toninho (toninhofwi yahoo.com.br)
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

#include "hbapi.h" /* for HB_OS_* detection */

#if ! defined( HB_OS_WIN_CE )
#  if defined( UNICODE )
#     undef UNICODE
#  endif
#endif

#include "hbwapi.h"
#if defined( HB_OS_WIN_CE )
#  include "hbwince.h"
#endif

#include <mapi.h>

#if !defined( MAPI_RECEIPT_REQUESTED )
#  if defined( MAPI_RECIPIENT_REQUESTED )
#     define MAPI_RECEIPT_REQUESTED MAPI_RECIPIENT_REQUESTED
#  else
#     define MAPI_RECEIPT_REQUESTED 0x00000002L
#  endif
#endif

#if defined( __WATCOMC__ ) || defined( __CYGWIN__ )
typedef ULONG ( PASCAL * LPMAPISENDMAIL ) ( LHANDLE, ULONG, lpMapiMessage, FLAGS, ULONG );
#endif

HB_FUNC( WIN_MAPISENDMAIL )
{
   HINSTANCE hMapiDll;

   /* Set default return value */
   hb_retnl( -1 );

   if( ( hMapiDll = hbwapi_LoadLibrarySystem( TEXT( "mapi32.dll" ) ) ) >= ( HINSTANCE ) 32 )
   {
      LPMAPISENDMAIL MAPISendMail = ( LPMAPISENDMAIL ) GetProcAddress( hMapiDll, "MAPISendMail" );

      if( MAPISendMail )
      {
         PHB_ITEM pFrom = hb_param( 8, HB_IT_ARRAY );
         PHB_ITEM pRecpList = hb_param( 9, HB_IT_ARRAY );
         PHB_ITEM pFileList = hb_param( 10, HB_IT_ARRAY );

         HB_SIZE nRecpCount = pRecpList ? hb_arrayLen( pRecpList ) : 0;
         HB_SIZE nFileCount = pFileList ? hb_arrayLen( pFileList ) : 0;
         HB_SIZE i;

         void ** hString;
         int iString = 0;

         MapiMessage note;
         MapiRecipDesc origin;
         FLAGS flags;

         memset( &note, 0, sizeof( note ) );
         memset( &origin, 0, sizeof( origin ) );

         hString = ( void ** ) hb_xgrab( ( 4 + 2 + ( 2 * nRecpCount ) + ( 2 * nFileCount ) ) * sizeof( void * ) );

         note.lpszSubject      = ( LPSTR ) HB_PARSTR( 1, &hString[ iString++ ], NULL );
         note.lpszNoteText     = ( LPSTR ) HB_PARSTR( 2, &hString[ iString++ ], NULL );
         note.lpszMessageType  = ( LPSTR ) HB_PARSTR( 3, &hString[ iString++ ], NULL );
         note.lpszDateReceived = ( LPSTR ) HB_PARSTRDEF( 4, &hString[ iString++ ], NULL );

         if( nRecpCount )
         {
            note.lpRecips = ( MapiRecipDesc * ) hb_xgrab( nRecpCount * sizeof( MapiRecipDesc ) );
            memset( note.lpRecips, 0, nRecpCount * sizeof( MapiRecipDesc ) );
         }

         if( nFileCount )
         {
            note.lpFiles = ( MapiFileDesc * ) hb_xgrab( nFileCount * sizeof( MapiFileDesc ) );
            memset( note.lpFiles , 0, nFileCount * sizeof( MapiFileDesc  ) );
         }

         if( hb_parl( 6 ) )
            note.flFlags |= MAPI_RECEIPT_REQUESTED;

         flags = MAPI_LOGON_UI;

         if( hb_parl( 7 ) )
            flags |= MAPI_DIALOG;

         if( pFrom && hb_arrayLen( pFrom ) >= 2 )
         {
            origin.lpszName     = ( LPSTR ) HB_ARRAYGETSTR( pFrom, 1, &hString[ iString++ ], NULL );
            origin.lpszAddress  = ( LPSTR ) HB_ARRAYGETSTR( pFrom, 2, &hString[ iString++ ], NULL ); /* optional */
            origin.ulRecipClass = MAPI_ORIG;

            note.lpOriginator = &origin;
         }
         else if( HB_ISCHAR( 8 ) )
         {
            origin.lpszName     = ( LPSTR ) HB_PARSTR( 8, &hString[ iString++ ], NULL );
            origin.ulRecipClass = MAPI_ORIG;

            note.lpOriginator = &origin;
         }

         for( i = 0; i < nRecpCount; ++i )
         {
            PHB_ITEM pItem = hb_arrayGetItemPtr( pRecpList, i + 1 );

            if( HB_IS_ARRAY( pItem ) && hb_arrayLen( pItem ) >= 2 )
            {
               if( hb_arrayGetCLen( pItem, 1 ) > 0 )
               {
                  note.lpRecips[ note.nRecipCount ].lpszName = ( LPSTR ) HB_ARRAYGETSTR( pItem, 1, &hString[ iString++ ], NULL );

                  if( hb_arrayGetCLen( pItem, 2 ) > 0 )
                     note.lpRecips[ note.nRecipCount ].lpszAddress = ( LPSTR ) HB_ARRAYGETSTR( pItem, 2, &hString[ iString++ ], NULL );
               }
               else if( hb_arrayGetCLen( pItem, 2 ) > 0 )
                  note.lpRecips[ note.nRecipCount ].lpszName = ( LPSTR ) HB_ARRAYGETSTR( pItem, 2, &hString[ iString++ ], NULL );
               else
                  continue;

               if( hb_arrayLen( pItem ) >= 3 && HB_IS_NUMERIC( hb_arrayGetItemPtr( pItem, 3 ) ) )
                  note.lpRecips[ note.nRecipCount ].ulRecipClass = ( ULONG ) hb_arrayGetNL( pItem, 3 );
               else
                  note.lpRecips[ note.nRecipCount ].ulRecipClass = MAPI_TO;

               ++note.nRecipCount;
            }
            else if( HB_IS_STRING( pItem ) )
            {
               note.lpRecips[ note.nRecipCount ].lpszName = ( LPSTR ) HB_ITEMGETSTR( pItem, &hString[ iString++ ], NULL );
               note.lpRecips[ note.nRecipCount ].ulRecipClass = MAPI_TO;

               ++note.nRecipCount;
            }
         }

         for( i = 0; i < nFileCount; ++i )
         {
            PHB_ITEM pItem = hb_arrayGetItemPtr( pFileList, i + 1 );

            if( HB_IS_ARRAY( pItem ) &&
                hb_arrayLen( pItem ) >= 1 &&
                hb_arrayGetCLen( pItem, 1 ) > 0 )
            {
               note.lpFiles[ note.nFileCount ].lpszPathName = ( LPSTR ) HB_ARRAYGETSTR( pItem, 1, &hString[ iString++ ], NULL );
               note.lpFiles[ note.nFileCount ].lpszFileName = ( LPSTR ) HB_ARRAYGETSTR( pItem, 2, &hString[ iString++ ], NULL ); /* optional */
               note.lpFiles[ note.nFileCount ].nPosition    = ( ULONG ) -1;
               ++note.nFileCount;
            }
            else if( HB_IS_STRING( pItem ) )
            {
               note.lpFiles[ note.nFileCount ].lpszPathName = ( LPSTR ) HB_ITEMGETSTR( pItem, &hString[ iString++ ], NULL );
               note.lpFiles[ note.nFileCount ].nPosition    = ( ULONG ) -1;
               ++note.nFileCount;
            }
         }

         hb_retnint( ( *MAPISendMail )( 0, ( ULONG_PTR ) GetActiveWindow(), &note, flags, 0 ) );

         if( nRecpCount > 0 )
            hb_xfree( note.lpRecips );

         if( nFileCount > 0 )
            hb_xfree( note.lpFiles );

         while( --iString >= 0 )
            hb_strfree( hString[ iString ] );

         hb_xfree( hString );
      }

      FreeLibrary( hMapiDll );
   }
}
