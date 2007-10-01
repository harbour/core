/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Display build information - code moved from hb_ver.c
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
 *
 * See doc/license.txt for licensing terms.
 *
 */

#include "hbapi.h"
#include "hbver.h"
#include "hbmemory.ch"

void hb_verBuildInfo( void )
{
   hb_conOutErr( "Harbour Build Info", 0 );
   hb_conOutErr( hb_conNewLine(), 0 );
   hb_conOutErr( "---------------------------", 0 );
   hb_conOutErr( hb_conNewLine(), 0 );

   {
      char * pszVersion = hb_verHarbour();
      hb_conOutErr( "Version: ", 0 );
      hb_conOutErr( pszVersion, 0 );
      hb_conOutErr( hb_conNewLine(), 0 );
      hb_xfree( pszVersion );
   }

   {
      char * pszPCode = hb_verPCode();
      hb_conOutErr( pszPCode, 0 );
      hb_conOutErr( hb_conNewLine(), 0 );
      hb_xfree( pszPCode );
   }

   {
      char * pszVersion = hb_verCompiler();
      hb_conOutErr( "Compiler: ", 0 );
      hb_conOutErr( pszVersion, 0 );
      hb_conOutErr( hb_conNewLine(), 0 );
      hb_xfree( pszVersion );
   }

   {
      char * pszVersion = hb_verPlatform();
      hb_conOutErr( "Platform: ", 0 );
      hb_conOutErr( pszVersion, 0 );
      hb_conOutErr( hb_conNewLine(), 0 );
      hb_xfree( pszVersion );
   }

   hb_conOutErr( hb_conNewLine(), 0 );

   hb_conOutErr( "Built on: ", 0 );
   hb_conOutErr( __DATE__, 0 );
   hb_conOutErr( " ", 0 );
   hb_conOutErr( __TIME__, 0 );
   hb_conOutErr( hb_conNewLine(), 0 );

   hb_conOutErr( "Last ChangeLog entry: ", 0 );
   hb_conOutErr( HB_VER_LENTRY, 0 );
   hb_conOutErr( hb_conNewLine(), 0 );

   hb_conOutErr( "ChangeLog CVS version: ", 0 );
   hb_conOutErr( HB_VER_CHLCVS, 0 );
   hb_conOutErr( hb_conNewLine(), 0 );

   if( strlen( HB_VER_C_USR ) )
   {
      hb_conOutErr( "Harbour compiler switches: ", 0 );
      hb_conOutErr( HB_VER_C_USR, 0 );
      hb_conOutErr( hb_conNewLine(), 0 );
   }

   if( strlen( HB_VER_L_USR ) )
   {
      hb_conOutErr( "C compiler switches: ", 0 );
      hb_conOutErr( HB_VER_L_USR, 0 );
      hb_conOutErr( hb_conNewLine(), 0 );
   }

   if( strlen( HB_VER_PRG_USR ) )
   {
      hb_conOutErr( "Linker switches: ", 0 );
      hb_conOutErr( HB_VER_PRG_USR, 0 );
      hb_conOutErr( hb_conNewLine(), 0 );
   }

   hb_conOutErr( hb_conNewLine(), 0 );

   hb_conOutErr( "Harbour extensions: ", 0 );
#if defined( HB_EXTENSION )
   hb_conOutErr( "Yes", 0 );
#else
   hb_conOutErr( "No", 0 );
#endif
   hb_conOutErr( hb_conNewLine(), 0 );

   hb_conOutErr( "CA-Clipper 5.2e undocumented: ", 0 );
#if defined( HB_C52_UNDOC )
   hb_conOutErr( "Yes", 0 );
#else
   hb_conOutErr( "No", 0 );
#endif
   hb_conOutErr( hb_conNewLine(), 0 );

   hb_conOutErr( "CA-Clipper 5.2e strict compatibility: ", 0 );
#if defined( HB_C52_STRICT )
   hb_conOutErr( "Yes", 0 );
#else
   hb_conOutErr( "No", 0 );
#endif
   hb_conOutErr( hb_conNewLine(), 0 );

   hb_conOutErr( "CA-Clipper 5.3x compatible extensions: ", 0 );
#if defined( HB_COMPAT_C53 )
   hb_conOutErr( "Yes", 0 );
#else
   hb_conOutErr( "No", 0 );
#endif
   hb_conOutErr( hb_conNewLine(), 0 );

   hb_conOutErr( "Alaska Xbase++ compatible extensions: ", 0 );
#if defined( HB_COMPAT_XPP )
   hb_conOutErr( "Yes", 0 );
#else
   hb_conOutErr( "No", 0 );
#endif
   hb_conOutErr( hb_conNewLine(), 0 );

   hb_conOutErr( "CA-Visual Objects compatible extensions: ", 0 );
#if defined( HB_COMPAT_VO )
   hb_conOutErr( "Yes", 0 );
#else
   hb_conOutErr( "No", 0 );
#endif
   hb_conOutErr( hb_conNewLine(), 0 );

   hb_conOutErr( "Multisoft Flagship compatible extensions: ", 0 );
#if defined( HB_COMPAT_FLAGSHIP )
   hb_conOutErr( "Yes", 0 );
#else
   hb_conOutErr( "No", 0 );
#endif
   hb_conOutErr( hb_conNewLine(), 0 );

   hb_conOutErr( "CLIP compatible extensions: ", 0 );
#if defined( HB_COMPAT_CLIP )
   hb_conOutErr( "Yes", 0 );
#else
   hb_conOutErr( "No", 0 );
#endif
   hb_conOutErr( hb_conNewLine(), 0 );

   hb_conOutErr( "Microsoft FoxPro compatible extensions: ", 0 );
#if defined( HB_COMPAT_FOXPRO )
   hb_conOutErr( "Yes", 0 );
#else
   hb_conOutErr( "No", 0 );
#endif
   hb_conOutErr( hb_conNewLine(), 0 );

   hb_conOutErr( "dBase compatible extensions: ", 0 );
#if defined( HB_COMPAT_DBASE )
   hb_conOutErr( "Yes", 0 );
#else
   hb_conOutErr( "No", 0 );
#endif
   hb_conOutErr( hb_conNewLine(), 0 );

   hb_conOutErr( "Object file generation support: ", 0 );
#if defined( HARBOUR_OBJ_GENERATION )
   hb_conOutErr( "Yes", 0 );
#else
   hb_conOutErr( "No", 0 );
#endif
   hb_conOutErr( hb_conNewLine(), 0 );

   hb_conOutErr( "ANSI C usage: ", 0 );
#if defined( HARBOUR_STRICT_ANSI_C )
   hb_conOutErr( "Strict", 0 );
#else
   hb_conOutErr( "Non strict", 0 );
#endif
   hb_conOutErr( hb_conNewLine(), 0 );

   hb_conOutErr( "C++ mode: ", 0 );
#if defined(__cplusplus)
   hb_conOutErr( "On", 0 );
#else
   hb_conOutErr( "Off", 0 );
#endif
   hb_conOutErr( hb_conNewLine(), 0 );

   hb_conOutErr( "Compiler YACC debug mode: ", 0 );
#if defined( HARBOUR_YYDEBUG )
   hb_conOutErr( "On", 0 );
#else
   hb_conOutErr( "Off", 0 );
#endif
   hb_conOutErr( hb_conNewLine(), 0 );

   hb_conOutErr( "Memory tracing and statistics: ", 0 );
   hb_conOutErr( hb_xquery( HB_MEM_USEDMAX ) != 0 ? "On" : "Off", 0 );
/*
#if defined( HB_FM_STATISTICS )
   hb_conOutErr( "On", 0 );
#else
   hb_conOutErr( "Off", 0 );
#endif
*/
   hb_conOutErr( hb_conNewLine(), 0 );

   {
      char buffer[ 64 ];
      snprintf( buffer, sizeof( buffer ), "Maximum symbol name length: %i", HB_SYMBOL_NAME_LEN );
      hb_conOutErr( buffer, 0 );
      hb_conOutErr( hb_conNewLine(), 0 );
   }

   hb_conOutErr( "---------------------------", 0 );
   hb_conOutErr( hb_conNewLine(), 0 );
}
