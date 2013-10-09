/*
 * Harbour Project source code:
 * CALLDLL compatibility library. EXPERIMENTAL CODE. USE AT YOUR OWN RISK. NO GUARANTEES.
 *
 * Copyright 2010 Viktor Szakats (vszakats.net/harbour)
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

#include "hbdyn.ch"

STATIC s_hDLL := { => }
STATIC s_mutex := hb_mutexCreate()

PROCEDURE UnloadAllDll()

   hb_mutexLock( s_mutex )
   s_hDLL := { => }
   hb_mutexUnlock( s_mutex )

   RETURN

FUNCTION CallDll32( cFunction, cLibrary, ... )
   RETURN hb_DynaCall1( cFunction, cLibrary, NIL, ... )

#if define( __PLATFORM__WINDOWS )
   /* Use Windows system .dll calling convention on Windows systems,
      like in original lib. Original .lib was a Windows-only solution.
      [vszakats] */
#  define _DEF_CALLCONV_ HB_DYN_CALLCONV_STDCALL
#else
#  define _DEF_CALLCONV_ HB_DYN_CALLCONV_CDECL
#endif

FUNCTION hb_DynaCall1( cFunction, cLibrary, nCount, ... )

   LOCAL aParams
   LOCAL hHandle

   IF HB_ISSTRING( cFunction ) .AND. ;
      HB_ISSTRING( cLibrary )

      hb_mutexLock( s_mutex )

      IF !( cLibrary $ s_hDLL )
         s_hDLL[ cLibrary ] := hb_libLoad( cLibrary )
      ENDIF

      hHandle := s_hDLL[ cLibrary ]

      hb_mutexUnlock( s_mutex )

      IF HB_ISNUMERIC( nCount ) .AND. nCount >= 0 .AND. nCount < PCount() - 3
         aParams := ASize( hb_AParams(), nCount )
         RETURN hb_DynCall( { cFunction, hHandle, _DEF_CALLCONV_ }, hb_ArrayToParams( aParams ) )
      ELSE
         RETURN hb_DynCall( { cFunction, hHandle, _DEF_CALLCONV_ }, ... )
      ENDIF
   ENDIF

   RETURN NIL

FUNCTION StrPtr( x )
   RETURN x

FUNCTION PtrStr( x )
   RETURN x
