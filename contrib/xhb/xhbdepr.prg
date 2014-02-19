/*
 * Harbour Project source code:
 * Functions deprecated from core Harbour by HB_LEGACY_LEVELn
 *   but kept in xHarbour, so they are made available here as well.
 *   Do not add internal functions (names starting with '__')
 *
 * Copyright 2013 Viktor Szakats (vszakats.net/harbour)
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

#ifndef HB_LEGACY_LEVEL4

#include "hbver.ch"

FUNCTION hb_osPathSeparator()
   RETURN hb_ps()

FUNCTION hb_osNewLine()
   RETURN hb_eol()

#if 0

FUNCTION hb_ANSIToOEM( s )
#if defined( __PLATFORM__WINDOWS )
   RETURN win_ANSIToOEM( s )
#else
   RETURN s
#endif

FUNCTION hb_OEMToANSI( s )
#if defined( __PLATFORM__WINDOWS )
   RETURN win_OEMToANSI( s )
#else
   RETURN s
#endif

FUNCTION hb_PCodeVer()
   RETURN hb_Version( HB_VERSION_PCODE_VER_STR )

FUNCTION hb_BuildDate()
   RETURN hb_Version( HB_VERSION_BUILD_DATE_STR )

FUNCTION hb_regexMatch( ... )
   RETURN iif( HB_ISLOGICAL( hb_PValue( 5 ) ) .AND. hb_PValue( 5 ), ;
      hb_regexLike( ... ), ;
      hb_regexHas( ... ) )

#endif

#endif
