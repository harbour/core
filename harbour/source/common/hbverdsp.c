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

#include "hbapi.h"
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

   {
      const char * pszLastEntry = hb_verSvnLastEntry();
      hb_conOutErr( "Last ChangeLog entry: ", 0 );
      hb_conOutErr( pszLastEntry, 0 );
      hb_conOutErr( hb_conNewLine(), 0 );
   }

   {
      const char * pszLogID = hb_verSvnChangeLogID();
      hb_conOutErr( "ChangeLog SVN version: ", 0 );
      hb_conOutErr( pszLogID, 0 );
      hb_conOutErr( hb_conNewLine(), 0 );
   }

   {
      const char * pszFlags = hb_verFlagsPRG();
      if( pszFlags && *pszFlags )
      {
         hb_conOutErr( "Extra Harbour compiler switches: ", 0 );
         hb_conOutErr( pszFlags, 0 );
         hb_conOutErr( hb_conNewLine(), 0 );
      }
   }

   {
      const char * pszFlags = hb_verFlagsC();
      if( pszFlags && *pszFlags )
      {
         hb_conOutErr( "Extra C compiler switches: ", 0 );
         hb_conOutErr( pszFlags, 0 );
         hb_conOutErr( hb_conNewLine(), 0 );
      }
   }

   {
      const char * pszFlags = hb_verFlagsL();
      if( pszFlags && *pszFlags )
      {
         hb_conOutErr( "Extra linker switches: ", 0 );
         hb_conOutErr( pszFlags, 0 );
         hb_conOutErr( hb_conNewLine(), 0 );
      }
   }

   hb_conOutErr( "Other build settings: ", 0 );
#if defined(__cplusplus)
   hb_conOutErr( "(C++ mode) ", 0 );
#else
   hb_conOutErr( "(C mode) ", 0 );
#endif
#if defined( HB_STRICT_ANSI_C )
   hb_conOutErr( "(ANSI C symbol initialization) ", 0 );
#endif
   hb_conOutErr( hb_conNewLine(), 0 );

   hb_conOutErr( hb_conNewLine(), 0 );

   hb_conOutErr( "Clipper 5.3b compatible extensions: ", 0 );
#if defined( HB_COMPAT_C53 )
   hb_conOutErr( "yes", 0 );
#else
   hb_conOutErr( "no", 0 );
#endif
   hb_conOutErr( hb_conNewLine(), 0 );

   hb_conOutErr( "Clipper 5.2e/5.3b compatible undocumented: ", 0 );
#if defined( HB_C52_UNDOC )
   hb_conOutErr( "yes", 0 );
#else
   hb_conOutErr( "no", 0 );
#endif
   hb_conOutErr( hb_conNewLine(), 0 );

   hb_conOutErr( "Clipper 5.2e/5.3b strict compatibility: ", 0 );
#if defined( HB_C52_STRICT )
   hb_conOutErr( "yes", 0 );
#else
   hb_conOutErr( "no", 0 );
#endif
   hb_conOutErr( hb_conNewLine(), 0 );

   hb_conOutErr( "Xbase++ compatible extensions: ", 0 );
#if defined( HB_COMPAT_XPP )
   hb_conOutErr( "yes", 0 );
#else
   hb_conOutErr( "no", 0 );
#endif
   hb_conOutErr( hb_conNewLine(), 0 );

   hb_conOutErr( "FlagShip compatible extensions: ", 0 );
#if defined( HB_COMPAT_FLAGSHIP )
   hb_conOutErr( "yes", 0 );
#else
   hb_conOutErr( "no", 0 );
#endif
   hb_conOutErr( hb_conNewLine(), 0 );

   hb_conOutErr( "Visual Objects compatible extensions: ", 0 );
#if defined( HB_COMPAT_VO )
   hb_conOutErr( "yes", 0 );
#else
   hb_conOutErr( "no", 0 );
#endif
   hb_conOutErr( hb_conNewLine(), 0 );

   hb_conOutErr( "FoxPro compatible extensions: ", 0 );
#if defined( HB_COMPAT_FOXPRO )
   hb_conOutErr( "yes", 0 );
#else
   hb_conOutErr( "no", 0 );
#endif
   hb_conOutErr( hb_conNewLine(), 0 );

   hb_conOutErr( "dBase compatible extensions: ", 0 );
#if defined( HB_COMPAT_DBASE )
   hb_conOutErr( "yes", 0 );
#else
   hb_conOutErr( "no", 0 );
#endif
   hb_conOutErr( hb_conNewLine(), 0 );

   hb_conOutErr( "CLIP compatible extensions: ", 0 );
#if defined( HB_COMPAT_CLIP )
   hb_conOutErr( "yes", 0 );
#else
   hb_conOutErr( "no", 0 );
#endif
   hb_conOutErr( hb_conNewLine(), 0 );

   hb_conOutErr( "Non-portable Harbour extensions: ", 0 );
#if defined( HB_EXTENSION )
   hb_conOutErr( "yes", 0 );
#else
   hb_conOutErr( "no", 0 );
#endif
   hb_conOutErr( hb_conNewLine(), 0 );

   hb_conOutErr( "Profiler: ", 0 );
#if defined( HB_NO_PROFILER )
   hb_conOutErr( "off", 0 );
#else
   hb_conOutErr( "on", 0 );
#endif
   hb_conOutErr( hb_conNewLine(), 0 );

   hb_conOutErr( "Memory tracing and statistics: ", 0 );
   hb_conOutErr( hb_xquery( HB_MEM_USEDMAX ) != 0 ? "on" : "off", 0 );
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
