/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Misc. string functions
 *
 * Copyright 1999 Eddie Runia <eddie@runia.com>
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

#include "hboo.ch"

/*
 * Convert to character
 *
 * ToChar( <xTxt>, [cSeparator], [lDebug] ) --> <cOut>
 *   <xTxt>       : Item to write
 *   [cSeparator] : Separator for arrays
 *   [lDebug]     : .T. -> Write debug output
 *
 * In DEBUG mode :
 *
 * It will show the xItem according to the following format :
 *
 * <num>                        Numerical
 * yyyy-mm-dd                   Date
 * "<chr>"                      Character
 * {<el1>, <el2>, ...}          Array
 * NIL                          NIL
 * .T. / .F.                    Boolean
 * <ClassName>(<ClassH>):{<DataSymbol1>:<val1>, ... }
 *                              Object
 */

FUNCTION ToChar( xTxt, cSeparator, lDebug )

   LOCAL cOut
   LOCAL n
   LOCAL nLen
   LOCAL aData

   hb_default( @cSeparator, " " )
   hb_default( @lDebug, .F. )

   SWITCH ValType( xTxt )
   CASE "C"
   CASE "M" ; RETURN iif( lDebug, '"' + xTxt + '"', xTxt )
   CASE "N" ; RETURN hb_ntos( xTxt )
   CASE "U" ; RETURN iif( lDebug, "NIL", "" )
   CASE "D" ; RETURN hb_DToC( xTxt, "yyyy-mm-dd" )
   CASE "L" ; RETURN iif( lDebug, iif( xTxt, ".T.", ".F." ), iif( xTxt, "True", "False" ) )
   CASE "B" ; RETURN iif( lDebug, "Block", Eval( xTxt ) )

   CASE "A"
      IF lDebug
         cOut := "{"
      ELSE
         cOut := ""
      ENDIF
      nLen := Len( xTxt )
      FOR n := 1 TO nLen                     // For each item : Recurse !
         cOut += ToChar( xTxt[ n ], cSeparator, lDebug )
         IF n != nLen
            cOut += cSeparator
         ENDIF
      NEXT
      IF lDebug
         cOut += "}"
      ENDIF
      EXIT

   CASE "O"
      IF lDebug
         cOut  := xTxt:ClassName() + "(#" + ToChar( xTxt:ClassH() ) + "):{"
         aData := __objGetValueList( xTxt )
         nLen  := Len( aData )
         FOR n := 1 TO nLen                     // For each item : Recurse !
            cOut += aData[ n ][ HB_OO_DATA_SYMBOL ] + ":" + ;
               ToChar( aData[ n ][ HB_OO_DATA_VALUE ], cSeparator, lDebug )
            IF n != nLen
               cOut += cSeparator
            ENDIF
         NEXT
         cOut += "}"
      ELSE
         cOut := ToChar( xTxt:Run(), cSeparator, lDebug )
      ENDIF
      EXIT

   ENDSWITCH

   RETURN cOut

// Non-volatile debugging function showing contents of xItem and returing
// passed argument.

FUNCTION Debug( xItem )

   ? ToChar( xItem, ", ", .T. )

   RETURN xItem
