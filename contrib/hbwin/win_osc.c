/*
 * Windows OS version information
 *
 * Copyright 2014 Viktor Szakats (vszakats.net/harbour)
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

#include "hbwin.h"
#include "hbapiitm.h"

HB_FUNC( WIN_OSISNT )
{
   hb_retl( hb_iswinnt() != 0 );
}

HB_FUNC( WIN_OSISNT351 )
{
   hb_retl( hb_iswinnt() == 3 );
}

HB_FUNC( WIN_OSISNT4 )
{
   hb_retl( hb_iswinnt() == 4 );
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
   hb_retl( hb_iswinver( 10, 0, 0, HB_FALSE ) );
}

HB_FUNC( WIN_OSIS9X )
{
   hb_retl( hb_iswin9x() );
}

HB_FUNC( WIN_OSIS95 )
{
   hb_retl( hb_iswin9x() == 5 );
}

HB_FUNC( WIN_OSIS98 )
{
   hb_retl( hb_iswin9x() == 8 );
}

HB_FUNC( WIN_OSISME )
{
   hb_retl( hb_iswin9x() == 9 );
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

   int iMajor = 4;
   int iMinor = 0;
   int pos;

   typedef struct
   {
      int iMajor;
      int iMinor;
   } HB_ISWINVER;

   static const HB_ISWINVER s_vers[] = {
      { 10, 0 },
      { 6, 3 },
      { 6, 2 },
      { 6, 1 },
      { 6, 0 },
      { 5, 2 },
      { 5, 1 },
      { 5, 0 } };

   for( pos = 0; pos < ( int ) HB_SIZEOFARRAY( s_vers ); ++pos )
   {
      if( hb_iswinver( s_vers[ pos ].iMajor, s_vers[ pos ].iMinor, 0, ( pos == 0 ) ) )
      {
         iMajor = s_vers[ pos ].iMajor;
         iMinor = s_vers[ pos ].iMinor;
         break;
      }
   }

   hb_arraySetNL( pArray, 1, iMajor );
   hb_arraySetNL( pArray, 2, iMinor );
   hb_arraySetNL( pArray, 3, 0 );
#if defined( HB_OS_WIN_CE )
   hb_arraySetNL( pArray, 4, VER_PLATFORM_WIN32_CE );
#else
   hb_arraySetNL( pArray, 4, hb_iswinnt() ? VER_PLATFORM_WIN32_NT : VER_PLATFORM_WIN32_WINDOWS );
#endif
   hb_arraySetC( pArray, 5, NULL );

   if( hb_iswin2k() )
   {
      int tmp;

      for( tmp = 5; tmp > 0; --tmp )
      {
         if( hb_iswinsp( tmp, HB_TRUE ) )
         {
            char szServicePack[ 8 ];
            hb_snprintf( szServicePack, sizeof( szServicePack ), "SP%u", tmp );
            hb_arraySetC( pArray, 5, szServicePack );
            break;
         }
      }
   }

   hb_itemReturnRelease( pArray );
}
