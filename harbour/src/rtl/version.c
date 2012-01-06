/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Version related functions
 *
 * Copyright 1999-2008 Viktor Szakats (harbour syenar.net)
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

#include "hbapi.h"
#include "hbvm.h"

#include "hbver.ch"

HB_FUNC( OS )
{
   hb_retc_buffer( hb_verPlatform() );
}

HB_FUNC( VERSION )
{
   hb_retc_buffer( hb_verHarbour() );
}

HB_FUNC( HB_VERSION )
{
   switch( hb_parni( 1 ) )
   {
   case HB_VERSION_HARBOUR:        hb_retc_buffer( hb_verHarbour() ); break;
   case HB_VERSION_COMPILER:       hb_retc_buffer( hb_verCompiler() ); break;
   case HB_VERSION_MAJOR:          hb_retni( HB_VER_MAJOR ); break;
   case HB_VERSION_MINOR:          hb_retni( HB_VER_MINOR ); break;
   case HB_VERSION_RELEASE:        hb_retni( HB_VER_RELEASE ); break;
   case HB_VERSION_STATUS:         hb_retc_const( HB_VER_STATUS ); break;
   case HB_VERSION_REVISION:       hb_retni( hb_verSvnID() ); break;
   case HB_VERSION_CHANGELOG_LAST: hb_retc_const( hb_verSvnLastEntry() ); break;
   case HB_VERSION_CHANGELOG_ID:   hb_retc_const( hb_verSvnChangeLogID() ); break;
   case HB_VERSION_PCODE_VER:      hb_retni( HB_PCODE_VER ); break;
   case HB_VERSION_PCODE_VER_STR:  hb_retc_buffer( hb_verPCode() ); break;
   case HB_VERSION_BUILD_PLAT:     hb_retc_const( hb_verHB_PLAT() ); break;
   case HB_VERSION_BUILD_COMP:     hb_retc_const( hb_verHB_COMP() ); break;
   case HB_VERSION_BUILD_DATE_STR: hb_retc_buffer( hb_verBuildDate() ); break;
   case HB_VERSION_BUILD_DATE:
   {
      char * pszBuildDate = hb_verBuildDate();

      if( strlen( pszBuildDate ) >= 11 )
      {
         static const char * s_months[] = { "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec" };
         char szDate[ 9 ];
         int iMonth;

         szDate[ 4 ] = szDate[ 5 ] = '0';

         for( iMonth = 11; iMonth >= 0; iMonth-- )
         {
            if( memcmp( pszBuildDate, s_months[ iMonth ], 3 ) == 0 )
            {
               hb_snprintf( szDate + 4, 3, "%02d", iMonth + 1 );
               break;
            }
         }

         memcpy( szDate, pszBuildDate + 7, 4 );
         szDate[ 6 ] = pszBuildDate[ 4 ] == ' ' ? '0' : pszBuildDate[ 4 ];
         szDate[ 7 ] = pszBuildDate[ 5 ];
         szDate[ 8 ] = '\0';

         hb_retds( szDate );
      }
      else
         hb_retds( NULL );

      hb_xfree( pszBuildDate );
      break;
   }
   case HB_VERSION_BUILD_TIME:
   {
      char * pszBuildDate = hb_verBuildDate();
      hb_retc( strlen( pszBuildDate ) >= 20 ? pszBuildDate + 12 : NULL );
      hb_xfree( pszBuildDate );
      break;
   }
   case HB_VERSION_FLAG_PRG:       hb_retc_const( hb_verFlagsPRG() ); break;
   case HB_VERSION_FLAG_C:         hb_retc_const( hb_verFlagsC() ); break;
   case HB_VERSION_FLAG_LINKER:    hb_retc_const( hb_verFlagsL() ); break;
   case HB_VERSION_BITWIDTH:       hb_retni( ( int ) sizeof( void * ) * 8 ); break;
   case HB_VERSION_MT:             hb_retl( hb_vmIsMt() );

   case HB_VERSION_UNIX_COMPAT:
      #if defined( HB_OS_UNIX )
         hb_retl( HB_TRUE );
      #else
         hb_retl( HB_FALSE );
      #endif
      break;

   case HB_VERSION_COMPILER_CPP:
      #if defined( __cplusplus )
         hb_retl( HB_TRUE );
      #else
         hb_retl( HB_FALSE );
      #endif
      break;

   case HB_VERSION_PLATFORM:
      #if defined( HB_OS_WIN_CE ) /* NOTE: Must precede HB_OS_WIN */
         hb_retc_const( "WCE" );
      #elif defined( HB_OS_WIN )
         hb_retc_const( "WIN" );
      #else
         hb_retc_const( hb_verPlatformMacro() );
      #endif
      break;

   case HB_VERSION_CPU:
      hb_retc_const( hb_verCPU() );
      break;

   case HB_VERSION_ENDIANNESS:
      #if defined( HB_LITTLE_ENDIAN )
         hb_retni( HB_VERSION_ENDIAN_LITTLE );
      #elif defined( HB_BIG_ENDIAN )
         hb_retni( HB_VERSION_ENDIAN_BIG );
      #elif defined( HB_PDP_ENDIAN )
         hb_retni( HB_VERSION_ENDIAN_PDP );
      #else
         hb_retni( 0 );
      #endif
      break;
   }
}

HB_FUNC( HB_OSCPU )
{
   hb_retc_const( hb_verHostCPU() );
}

HB_FUNC( HB_OSIS64BIT )
{
   hb_retl( hb_verHostBitWidth() >= 64 );
}

HB_FUNC( HB_OSISWIN9X )
{
   hb_retl( hb_iswin9x() );
}

HB_FUNC( HB_OSISWINNT )
{
   hb_retl( hb_iswinnt() );
}

HB_FUNC( HB_OSISWIN2K )
{
   hb_retl( hb_iswin2k() );
}

HB_FUNC( HB_OSISWINVISTA )
{
   hb_retl( hb_iswinvista() );
}

HB_FUNC( HB_OSISWINCE )
{
   hb_retl( hb_iswince() );
}

/* Legacy functions */

HB_FUNC( HB_COMPILER )
{
   hb_retc_buffer( hb_verCompiler() );
}

HB_FUNC( HB_PCODEVER )
{
   hb_retc_buffer( hb_verPCode() );
}

HB_FUNC( HB_BUILDDATE )
{
   hb_retc_buffer( hb_verBuildDate() );
}
