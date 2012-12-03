/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Windows OS version information
 *
 * Copyright 2004 Peter Rees <peter@rees.co.nz> Rees Software & Systems Ltd
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

#include "hbwin.h"
#include "hbapiitm.h"

static void getwinver( OSVERSIONINFO * pOSvi )
{
   pOSvi->dwOSVersionInfoSize = sizeof( OSVERSIONINFO );
   GetVersionEx( pOSvi );
}

HB_FUNC( WIN_OSISNT )
{
   OSVERSIONINFO osvi;

   getwinver( &osvi );
   hb_retl( osvi.dwPlatformId == VER_PLATFORM_WIN32_NT );
}

HB_FUNC( WIN_OSISNT351 )
{
   OSVERSIONINFO osvi;

   getwinver( &osvi );
   hb_retl( osvi.dwPlatformId == VER_PLATFORM_WIN32_NT &&
            osvi.dwMajorVersion == 3 && osvi.dwMinorVersion == 51 );
}

HB_FUNC( WIN_OSISNT4 )
{
   OSVERSIONINFO osvi;

   getwinver( &osvi );
   hb_retl( osvi.dwPlatformId == VER_PLATFORM_WIN32_NT &&
            osvi.dwMajorVersion == 4 && osvi.dwMinorVersion == 0 );
}

HB_FUNC( WIN_OSIS2000ORUPPER )
{
   OSVERSIONINFO osvi;

   getwinver( &osvi );
   hb_retl( osvi.dwMajorVersion >= 5 );
}

HB_FUNC( WIN_OSIS2000 )
{
   OSVERSIONINFO osvi;

   getwinver( &osvi );
   hb_retl( osvi.dwMajorVersion == 5 && osvi.dwMinorVersion == 0 );
}

HB_FUNC( WIN_OSISXP )
{
   OSVERSIONINFO osvi;

   getwinver( &osvi );
   hb_retl( osvi.dwMajorVersion == 5 && osvi.dwMinorVersion == 1 );
}

HB_FUNC( WIN_OSISWINXPORUPPER )
{
   OSVERSIONINFO osvi;

   getwinver( &osvi );
   hb_retl( osvi.dwMajorVersion > 5 || ( osvi.dwMajorVersion == 5 && osvi.dwMinorVersion >= 1 ) );
}

HB_FUNC( WIN_OSIS2003 )
{
   OSVERSIONINFO osvi;

   getwinver( &osvi );
   hb_retl( osvi.dwMajorVersion == 5 && osvi.dwMinorVersion == 2 );
}

HB_FUNC( WIN_OSISVISTA )
{
   OSVERSIONINFO osvi;

   getwinver( &osvi );
   hb_retl( osvi.dwMajorVersion == 6 && osvi.dwMinorVersion == 0 );
}

HB_FUNC( WIN_OSISVISTAORUPPER )
{
   OSVERSIONINFO osvi;

   getwinver( &osvi );
   hb_retl( osvi.dwMajorVersion >= 6 );
}

HB_FUNC( WIN_OSIS7 )
{
   OSVERSIONINFO osvi;

   getwinver( &osvi );
   hb_retl( osvi.dwMajorVersion == 6 && osvi.dwMinorVersion == 1 );
}

HB_FUNC( WIN_OSIS8 )
{
   OSVERSIONINFO osvi;

   getwinver( &osvi );
   hb_retl( osvi.dwMajorVersion == 6 && osvi.dwMinorVersion == 2 );
}

HB_FUNC( WIN_OSIS9X )
{
   OSVERSIONINFO osvi;

   getwinver( &osvi );
   hb_retl( osvi.dwPlatformId == VER_PLATFORM_WIN32_WINDOWS );
}

HB_FUNC( WIN_OSIS95 )
{
   OSVERSIONINFO osvi;

   getwinver( &osvi );
   hb_retl( osvi.dwPlatformId == VER_PLATFORM_WIN32_WINDOWS &&
            osvi.dwMajorVersion == 4 && osvi.dwMinorVersion == 0 );
}

HB_FUNC( WIN_OSIS98 )
{
   OSVERSIONINFO osvi;

   getwinver( &osvi );
   hb_retl( osvi.dwPlatformId == VER_PLATFORM_WIN32_WINDOWS &&
            osvi.dwMajorVersion == 4 && osvi.dwMinorVersion == 10 );
}

HB_FUNC( WIN_OSISME )
{
   OSVERSIONINFO osvi;

   getwinver( &osvi );
   hb_retl( osvi.dwPlatformId == VER_PLATFORM_WIN32_WINDOWS &&
            osvi.dwMajorVersion == 4 && osvi.dwMinorVersion == 90 );
}

HB_FUNC( WIN_OSISTSCLIENT )
{
   HB_BOOL bResult = HB_FALSE;
   OSVERSIONINFO osvi;

   getwinver( &osvi );
   if( osvi.dwPlatformId == VER_PLATFORM_WIN32_NT && osvi.dwMajorVersion >= 4 )
   {
      /* Only supported on NT 4.0 SP3 & higher */
      #ifndef SM_REMOTESESSION
         #define SM_REMOTESESSION  0x1000
      #endif
      bResult = ( GetSystemMetrics( SM_REMOTESESSION ) != 0 );
   }
   hb_retl( bResult );
}

HB_FUNC( WIN_OSVERSIONINFO )
{
   PHB_ITEM pArray = hb_itemArrayNew( 5 );
   OSVERSIONINFO osvi;

   getwinver( &osvi );
   hb_arraySetNL( pArray, 1, osvi.dwMajorVersion );
   hb_arraySetNL( pArray, 2, osvi.dwMinorVersion );
   if( osvi.dwPlatformId == VER_PLATFORM_WIN32_WINDOWS )
      osvi.dwBuildNumber = LOWORD( osvi.dwBuildNumber );
   hb_arraySetNL( pArray, 3, osvi.dwBuildNumber );
   hb_arraySetNL( pArray, 4, osvi.dwPlatformId );
   HB_ARRAYSETSTR( pArray, 5, osvi.szCSDVersion );
   hb_itemReturnRelease( pArray );
}
