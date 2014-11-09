/*
 * Windows OS version information
 *
 * Copyright 2014 Viktor Szakats (vszakats.net/harbour) (win_osIs81(), win_osIs10())
 * Copyright 2004 Peter Rees <peter@rees.co.nz> Rees Software and Systems Ltd
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
#include "hbapiitm.h"

static HB_BOOL getwinver( OSVERSIONINFO * pOSvi )
{
   pOSvi->dwOSVersionInfoSize = sizeof( OSVERSIONINFO );
   return ( GetVersionEx( pOSvi ) == TRUE );
}

HB_FUNC( WIN_OSISNT )
{
   hb_retl( hb_iswinnt() );
}

HB_FUNC( WIN_OSISNT351 )
{
   OSVERSIONINFO osvi;

   hb_retl( hb_iswinnt() && getwinver( &osvi ) && osvi.dwMajorVersion == 3 && osvi.dwMinorVersion == 51 );
}

HB_FUNC( WIN_OSISNT4 )
{
   OSVERSIONINFO osvi;

   hb_retl( hb_iswinnt() && getwinver( &osvi ) && osvi.dwMajorVersion == 4 && osvi.dwMinorVersion == 0 );
}

HB_FUNC( WIN_OSIS2000 )
{
   hb_retl( hb_iswinver( 5, 0, 0, HB_FALSE ) );
}

HB_FUNC( WIN_OSIS2000ORUPPER )
{
   hb_retl( hb_iswin2k() );
}

HB_FUNC( WIN_OSISXP )
{
   hb_retl( hb_iswinver( 5, 1, 0, HB_FALSE ) );
}

HB_FUNC( WIN_OSISWINXPORUPPER )
{
   hb_retl( hb_iswinver( 5, 1, 0, HB_TRUE ) );
}

HB_FUNC( WIN_OSIS2003 )
{
   hb_retl( hb_iswinver( 5, 2, 0, HB_FALSE ) );
}

HB_FUNC( WIN_OSISVISTA )
{
   hb_retl( hb_iswinver( 6, 0, 0, HB_FALSE ) );
}

HB_FUNC( WIN_OSISVISTAORUPPER )
{
   hb_retl( hb_iswinvista() );
}

HB_FUNC( WIN_OSIS7 )
{
   hb_retl( hb_iswinver( 6, 1, 0, HB_FALSE ) );
}

HB_FUNC( WIN_OSIS8 )
{
   hb_retl( hb_iswinver( 6, 2, 0, HB_FALSE ) );
}

HB_FUNC( WIN_OSIS81 )
{
   hb_retl( hb_iswinver( 6, 3, 0, HB_FALSE ) );
}

HB_FUNC( WIN_OSIS10 )
{
   hb_retl( hb_iswinver( 6, 4, 0, HB_FALSE ) );
}

HB_FUNC( WIN_OSIS9X )
{
   hb_retl( hb_iswin9x() );
}

HB_FUNC( WIN_OSIS95 )
{
   OSVERSIONINFO osvi;

   hb_retl( hb_iswin9x() && getwinver( &osvi ) && osvi.dwMajorVersion == 4 && osvi.dwMinorVersion == 0 );
}

HB_FUNC( WIN_OSIS98 )
{
   OSVERSIONINFO osvi;

   hb_retl( hb_iswin9x() && getwinver( &osvi ) && osvi.dwMajorVersion == 4 && osvi.dwMinorVersion == 10 );
}

HB_FUNC( WIN_OSISME )
{
   OSVERSIONINFO osvi;

   hb_retl( hb_iswin9x() && getwinver( &osvi ) && osvi.dwMajorVersion == 4 && osvi.dwMinorVersion == 90 );
}

HB_FUNC( WIN_OSISTSCLIENT )
{
   /* Only supported on NT 4.0 SP3 & higher */
   #ifndef SM_REMOTESESSION
      #define SM_REMOTESESSION  0x1000
   #endif

   hb_retl( GetSystemMetrics( SM_REMOTESESSION ) != 0 );
}

HB_FUNC( WIN_OSVERSIONINFO )
{
   PHB_ITEM pArray = hb_itemArrayNew( 5 );
   OSVERSIONINFO osvi;

   if( ! getwinver( &osvi ) )
      memset( &osvi, 0, sizeof( osvi ) );

   hb_arraySetNL( pArray, 1, osvi.dwMajorVersion );
   hb_arraySetNL( pArray, 2, osvi.dwMinorVersion );
   if( hb_iswin9x() )
      osvi.dwBuildNumber = LOWORD( osvi.dwBuildNumber );
   hb_arraySetNL( pArray, 3, osvi.dwBuildNumber );
   hb_arraySetNL( pArray, 4, osvi.dwPlatformId );
   HB_ARRAYSETSTR( pArray, 5, osvi.szCSDVersion );
   hb_itemReturnRelease( pArray );
}
