/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * OS(), VERSION(), HB_COMPILER() functions
 *
 * Copyright 1999 {list of individual authors and e-mail addresses}
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

/*
 * The following parts are Copyright of the individual authors.
 * www - http://www.harbour-project.org
 *
 * Copyright 1999-2008 Viktor Szakats <viktor.szakats@syenar.hu>
 *    HB_VERSION(), HB_COMPILER()
 *
 * See doc/license.txt for licensing terms.
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
   case HB_VERSION_MICRO:          hb_retni( HB_VER_REVISION ); break;
   case HB_VERSION_STATUS:         hb_retc( HB_VER_STATUS ); break;
   case HB_VERSION_REVISION:       hb_retni( hb_verSvnID() ); break;
   case HB_VERSION_BLD_DATE_STR:   hb_retc_buffer( hb_verBuildDate() ); break;
   case HB_VERSION_BLD_DATE:       hb_retds( NULL ); break; /* TODO */
   case HB_VERSION_BLD_TIME:       hb_retc( NULL ); break; /* TODO */
   case HB_VERSION_PCODE_VER:      hb_retni( HB_PCODE_VER ); break;
   case HB_VERSION_PCODE_VER_STR:  hb_retc_buffer( hb_verPCode() ); break;
   case HB_VERSION_CHANGELOG_LAST: hb_retc_const( hb_verSvnLastEntry() ); break;
   case HB_VERSION_CHANGELOG_REV:  hb_retc_const( hb_verSvnChangeLogID() ); break;
   case HB_VERSION_FLAG_PRG:       hb_retc_const( hb_verFlagsPRG() ); break;
   case HB_VERSION_FLAG_C:         hb_retc_const( hb_verFlagsC() ); break;
   case HB_VERSION_FLAG_LINKER:    hb_retc_const( hb_verFlagsL() ); break;
   case HB_VERSION_BITWIDTH:       hb_retni( ( int ) sizeof( void * ) * 8 ); break;
   case HB_VERSION_MT:             hb_retl( hb_vmIsMt() );

   case HB_VERSION_UNIX_COMPAT:
      #if defined( HB_OS_UNIX_COMPATIBLE )
         hb_retl( TRUE );
      #else
         hb_retl( FALSE );
      #endif
      break;

   case HB_VERSION_PLATFORM:
      #if defined( HB_OS_DOS )
         hb_retc_const( "DOS" );
      #elif defined( HB_OS_OS2 )
         hb_retc_const( "OS2" );
      #elif defined( HB_OS_WIN_32 )
         hb_retc_const( "WINDOWS" );
      #elif defined( HB_OS_LINUX )
         hb_retc_const( "LINUX" );
      #elif defined( HB_OS_SUNOS )
         hb_retc_const( "SUNOS" );
      #elif defined( HB_OS_HPUX )
         hb_retc_const( "HPUX" );
      #elif defined( HB_OS_DARWIN )
         hb_retc_const( "DARWIN" );
      #elif defined( HB_OS_BSD )
         hb_retc_const( "BSD" );
      #else
         hb_retc_null();
      #endif
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

HB_FUNC( HB_OSISWINNT )
{
   hb_retl( hb_iswinnt() );
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
