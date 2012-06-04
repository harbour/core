/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Calling function from dynamic library (FOX___DYNCALL())
 *
 * Copyright 2010 Viktor Szakats (harbour syenar.net)
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

#include "hbdyn.ch"

/*
DECLARE [cFunctionType] FunctionName IN LibraryName [AS AliasName]
   [cParamType1 [@] ParamName1, cParamType2 [@] ParamName2, ...]
*/

FUNCTION FOX___DYNCALL( cCommand, ... )
   LOCAL aParam

   LOCAL cFunction
   LOCAL cLibrary
   LOCAL nFuncFlags := hb_bitOr( HB_DYN_CALLCONV_CDECL, HB_DYN_ENC_RAW )

   LOCAL aCommand := hb_ATokens( cCommand )
   LOCAL nPos := 1

   LOCAL aType := {;
      "SHORT"   => HB_DYN_CTYPE_SHORT ,;
      "INTEGER" => HB_DYN_CTYPE_INT ,;
      "SINGLE"  => HB_DYN_CTYPE_FLOAT ,;
      "DOUBLE"  => HB_DYN_CTYPE_DOUBLE ,;
      "LONG"    => HB_DYN_CTYPE_LONG ,;
      "STRING"  => HB_DYN_CTYPE_CHAR_PTR ,;
      "OBJECT"  => HB_DYN_CTYPE_VOID_PTR }

   IF nPos <= Len( aCommand ) .AND. Upper( aCommand[ nPos ] ) == "DECLARE"
      ++nPos
   ENDIF

   IF nPos <= Len( aCommand ) .AND. Upper( aCommand[ nPos ] ) $ aType
      nFuncFlags := hb_bitOr( nFuncFlags, aType[ Upper( aCommand[ nPos ] ) ] )
      ++nPos
   ELSE
      RETURN NIL
   ENDIF

   IF nPos <= Len( aCommand )
      cFunction := aCommand[ nPos ]
      ++nPos
   ELSE
      RETURN NIL
   ENDIF

   IF nPos <= Len( aCommand ) .AND. Upper( aCommand[ nPos ] ) == "IN"
      ++nPos
   ELSE
      RETURN NIL
   ENDIF

   IF nPos <= Len( aCommand )
      cLibrary := aCommand[ nPos ]
      ++nPos
   ELSE
      RETURN NIL
   ENDIF

   IF nPos <= Len( aCommand ) .AND. Upper( aCommand[ nPos ] ) == "AS"
      nPos += 2
   ENDIF

   aParam := { cFunction, cLibrary, nFuncFlags }

   DO WHILE nPos <= Len( aCommand )
      IF Upper( Upper( aCommand[ nPos ] ) ) $ aType
         AAdd( aParam, hb_bitOr( HB_DYN_ENC_RAW, aType[ Upper( aCommand[ nPos ] ) ] ) )
         ++nPos
      ENDIF
      IF nPos <= Len( aCommand ) .AND. aCommand[ nPos ] == "@"
         ++nPos
      ENDIF
      /* ignore parameter name */
      IF nPos <= Len( aCommand )
         ++nPos
      ENDIF
      IF nPos <= Len( aCommand ) .AND. aCommand[ nPos ] == ","
         ++nPos
      ENDIF
   ENDDO

   RETURN hb_dynCall( aParam, ... )
