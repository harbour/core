/*
 * $Id$
 */


// INI file interface

/*
 * hbwhat Project source code:
 * Windows Profile functions (Windows .ini rounines)
 *
 * Copyright 2001-2002 Luiz Rafael Culik<culikr@uol.com.br>
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

#define HB_OS_WIN_USED
#undef _WIN32_WINNT
#define _WIN32_WINNT   0x0400

#include "hbwhat.h"

#include <windows.h>
#include "hbapi.h"


//-----------------------------------------------------------------------------
HB_FUNC( VWN_GETPROFILESTRING )
{
   DWORD nSize = 1024 ;
   LPTSTR bBuffer = (LPTSTR) hb_xgrab( nSize );
   DWORD dwLen ;
   char * lpSection = ISNIL( 1 ) ? NULL : hb_parcx( 1 );
   char * lpEntry   = ISNIL( 2 ) ? NULL : hb_parcx( 2 );
   char * lpDefault = hb_parc ( 3 );

   for( ;; )
   {
      dwLen = GetProfileString( lpSection , lpEntry ,lpDefault , bBuffer, nSize );
      if ( ( ( ( lpSection == NULL ) || ( lpEntry == NULL ) ) && ( nSize - dwLen == 2 ) ) || ( ( lpSection && lpEntry ) && ( nSize - dwLen == 1 ) ) )
      {
        hb_xfree( bBuffer );
        nSize *= 2 ;
        bBuffer = (LPTSTR) hb_xgrab( nSize );
      }
      else
        break ;
   }

   if( dwLen )
     hb_retclen( ( char * ) bBuffer, dwLen );
   else
      hb_retc( lpDefault );

   hb_xfree( bBuffer );

}

//-----------------------------------------------------------------------------
HB_FUNC( VWN_GETPRIVATEPROFILESTRING )
{
   DWORD nSize = 1024 ;
   LPTSTR bBuffer = (LPTSTR) hb_xgrab( nSize );
   DWORD dwLen ;
   char * lpSection  = ISNIL( 1 ) ? NULL : hb_parcx( 1 );
   char * lpEntry    = ISNIL( 2 ) ? NULL : hb_parcx( 2 );
   char * lpDefault  = hb_parcx( 3 );
   char * lpFileName = hb_parcx( 4 );

   for( ;; )
   {
      dwLen = GetPrivateProfileString( lpSection , lpEntry ,lpDefault , bBuffer, nSize , lpFileName);
      if ( ( ( ( lpSection == NULL ) || ( lpEntry == NULL ) ) && ( nSize - dwLen == 2 ) ) || ( ( lpSection && lpEntry ) && ( nSize - dwLen == 1 ) ) )
      {
        hb_xfree( bBuffer );
        nSize *= 2 ;
        bBuffer = (LPTSTR) hb_xgrab( nSize );
      }
      else
        break ;

   }

   if( dwLen )
     hb_retclen( ( char * ) bBuffer, dwLen );
   else
      hb_retc( lpDefault );

   hb_xfree( bBuffer );

}

//-----------------------------------------------------------------------------
HB_FUNC( VWN_WRITEPROFILESTRING )
{
   char * lpSection = hb_parcx( 1 );
   char * lpEntry = ISCHAR(2) ? hb_parcx( 2 ) : NULL ;
   char * lpData = ISCHAR(3) ? hb_parcx( 3 ) : NULL ;

   if ( WriteProfileString( lpSection , lpEntry , lpData) )
      hb_retl( TRUE );
   else
      hb_retl(FALSE);
}

//-----------------------------------------------------------------------------
HB_FUNC( VWN_WRITEPRIVATEPROFILESTRING )
{
   char * lpSection = hb_parcx( 1 );
   char * lpEntry = ISCHAR(2) ? hb_parcx( 2 ) : NULL ;
   char * lpData = ISCHAR(3) ? hb_parcx( 3 ) : NULL ;
   char * lpFileName= hb_parcx( 4 );

   if ( WritePrivateProfileString( lpSection , lpEntry , lpData , lpFileName ) )
      hb_retl( TRUE );
   else
      hb_retl(FALSE);
}


//-----------------------------------------------------------------------------
// WINBASEAPI UINT WINAPI GetPrivateProfileInt( IN LPCSTR lpAppName, IN LPCSTR lpKeyName, IN INT nDefault, IN LPCSTR lpFileName );


HB_FUNC( VWN_GETPRIVATEPROFILEINT )
{
   hb_retni( GetPrivateProfileIntA( (LPCSTR) hb_parcx( 1 ),
                                    (LPCSTR) hb_parcx( 2 ),
                                    hb_parni( 3 )        ,
                                    (LPCSTR) hb_parcx( 4 )
                                    ) );
}


//-----------------------------------------------------------------------------
// WINBASEAPI UINT WINAPI GetProfileInt( IN LPCSTR lpAppName, IN LPCSTR lpKeyName, IN INT nDefault );


HB_FUNC( VWN_GETPROFILEINT )
{
   hb_retni( GetProfileIntA( (LPCSTR) hb_parcx( 1 ),
                             (LPCSTR) hb_parcx( 2 ),
                             hb_parni( 3 )
                             ) );
}

//-----------------------------------------------------------------------------
// WINBASEAPI DWORD WINAPI GetProfileSection( IN LPCSTR lpAppName, OUT LPSTR lpReturnedString, IN DWORD nSize );

/*
HB_FUNC( VWN_GETPROFILESECTION )
{
   hb_retnl( (LONG) GetProfileSectionA( (LPCSTR) hb_parcx( 1 ),
                                        (LPSTR) hb_parcx( 2 ) ,
                                        (DWORD) hb_parnl( 3 )
                                        ) );
}
*/

//-----------------------------------------------------------------------------
// WINBASEAPI BOOL WINAPI WriteProfileSection( IN LPCSTR lpAppName, IN LPCSTR lpString );


HB_FUNC( VWN_WRITEPROFILESECTION )
{
   hb_retl( WriteProfileSectionA( (LPCSTR) hb_parcx( 1 ), (LPCSTR) hb_parcx( 2 ) ) );
}

/*
//-----------------------------------------------------------------------------
// WINBASEAPI DWORD WINAPI GetPrivateProfileSection( IN LPCSTR lpAppName, OUT LPSTR lpReturnedString, IN DWORD nSize, IN LPCSTR lpFileName );


HB_FUNC( VWN_GETPRIVATEPROFILESECTION )
{
   hb_retnl( (LONG) GetPrivateProfileSectionA( (LPCSTR) hb_parcx( 1 ),
                                               (LPSTR) hb_parcx( 2 ) ,
                                               (DWORD) hb_parnl( 3 ),
                                               (LPCSTR) hb_parcx( 4 )
                                               ) );
}

*/
//-----------------------------------------------------------------------------
// WINBASEAPI BOOL WINAPI WritePrivateProfileSectionA( IN LPCSTR lpAppName, IN LPCSTR lpString, IN LPCSTR lpFileName );


HB_FUNC( VWN_WRITEPRIVATEPROFILESECTION )
{
   hb_retl( WritePrivateProfileSectionA( (LPCSTR) hb_parcx( 1 ),
                                         (LPCSTR) hb_parcx( 2 ),
                                         (LPCSTR) hb_parcx( 3 )
                                         ) );
}

/*
//-----------------------------------------------------------------------------
// WINBASEAPI DWORD WINAPI GetPrivateProfileSectionNamesA( OUT LPSTR lpszReturnBuffer, IN DWORD nSize, IN LPCSTR lpFileName );


HB_FUNC( VWN_GETPRIVATEPROFILESECTIONNAMES )
{
   hb_retnl( (LONG) GetPrivateProfileSectionNames( (LPSTR) hb_parcx( 1 ) ,
                                                    (DWORD) hb_parnl( 2 ),
                                                    (LPCSTR) hb_parcx( 3 )
                                                    ) );
}
*/
//-----------------------------------------------------------------------------
// WINBASEAPI BOOL WINAPI GetPrivateProfileStruct( IN LPCSTR lpszSection, IN LPCSTR lpszKey, OUT LPVOID lpStruct, IN UINT uSizeStruct, IN LPCSTR szFile );

/*

HB_FUNC( VWN_GETPRIVATEPROFILESTRUCT )
{
   LPVOID lpStruct    ;

   // Your code goes here

   hb_retl( GetPrivateProfileStructA( (LPCSTR) hb_parcx( 1 ),
                                      (LPCSTR) hb_parcx( 2 ),
                                      lpStruct             ,
                                      (UINT) hb_parni( 4 ) ,
                                      (LPCSTR) hb_parcx( 5 )
                                      ) );
}

*/

//-----------------------------------------------------------------------------
// WINBASEAPI BOOL WINAPI WritePrivateProfileStruct( IN LPCSTR lpszSection, IN LPCSTR lpszKey, IN LPVOID lpStruct, IN UINT uSizeStruct, IN LPCSTR szFile );

/*

HB_FUNC( VWN_WRITEPRIVATEPROFILESTRUCT )
{
   LPVOID lpStruct    ;

   // Your code goes here

   hb_retl( WritePrivateProfileStructA( (LPCSTR) hb_parcx( 1 ),
                                        (LPCSTR) hb_parcx( 2 ),
                                        lpStruct             ,
                                        (UINT) hb_parni( 4 ) ,
                                        (LPCSTR) hb_parcx( 5 )
                                        ) );
}

*/
