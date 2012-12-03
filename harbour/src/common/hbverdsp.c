/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Display build information
 *
 * Copyright 1999-2010 Viktor Szakats (harbour syenar.net)
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

   {
      char * pszPCode = hb_verPCode();
      hb_conOutErr( pszPCode, 0 );
      hb_conOutErr( hb_conNewLine(), 0 );
      hb_xfree( pszPCode );
   }

   hb_conOutErr( "ChangeLog last entry: ", 0 );
   hb_conOutErr( hb_verChangeLogLastEntry(), 0 );
   hb_conOutErr( hb_conNewLine(), 0 );

   hb_conOutErr( "ChangeLog ID: ", 0 );
   hb_conOutErr( hb_verChangeLogID(), 0 );
   hb_conOutErr( hb_conNewLine(), 0 );

   {
      char * pszBuildDate = hb_verBuildDate();
      hb_conOutErr( "Built on: ", 0 );
      hb_conOutErr( pszBuildDate, 0 );
      hb_conOutErr( hb_conNewLine(), 0 );
      hb_xfree( pszBuildDate );
   }

   {
      const char * pszFlags = hb_verFlagsPRG();
      if( pszFlags && *pszFlags )
      {
         hb_conOutErr( "Extra Harbour compiler options: ", 0 );
         hb_conOutErr( pszFlags, 0 );
         hb_conOutErr( hb_conNewLine(), 0 );
      }
   }

   {
      const char * pszFlags = hb_verFlagsC();
      if( pszFlags && *pszFlags )
      {
         hb_conOutErr( "Extra C compiler options: ", 0 );
         hb_conOutErr( pszFlags, 0 );
         hb_conOutErr( hb_conNewLine(), 0 );
      }
   }

   {
      const char * pszFlags = hb_verFlagsL();
      if( pszFlags && *pszFlags )
      {
         hb_conOutErr( "Extra linker options: ", 0 );
         hb_conOutErr( pszFlags, 0 );
         hb_conOutErr( hb_conNewLine(), 0 );
      }
   }

   hb_conOutErr( "Build options: ", 0 );
   if( hb_xquery( HB_MEM_USEDMAX ) != 0 )
      hb_conOutErr( "(memory tracking) ", 0 );
#if defined( HB_TR_INFO ) && ( HB_TR_LEVEL == HB_TR_INFO || HB_TR_LEVEL == HB_TR_DEBUG )
   hb_conOutErr( "(tracing) ", 0 );
#endif
#if ! defined( HB_NO_PROFILER )
   hb_conOutErr( "(profiler) ", 0 );
#endif
#if defined( __cplusplus )
   hb_conOutErr( "(C++ mode) ", 0 );
#endif
#if defined( HB_COMPAT_C53 )
   hb_conOutErr( "(Clipper 5.3b) ", 0 );
#endif
#if defined( HB_CLP_UNDOC )
   hb_conOutErr( "(Clipper 5.x undoc) ", 0 );
#endif
#if defined( HB_CLP_STRICT )
   hb_conOutErr( "(Clipper 5.x strict) ", 0 );
#endif
   hb_conOutErr( hb_conNewLine(), 0 );

   hb_conOutErr( "---------------------------", 0 );
   hb_conOutErr( hb_conNewLine(), 0 );
}
