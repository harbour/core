/*
 * $Id$
 */

/*
 * Harbour Project source code:
 *    Decode function
 *
 * Copyright 2006 Francesco Saverio Giudice <info / at / fsgiudice / dot / com>
 * www - http://harbour-project.org
 * www - http://www.xharbour.org
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

#include "common.ch"

/*
PROCEDURE Main()
   LOCAL aArray

   ? "DECODE FUNCTION TESTS"

   // Single, return empty value
   ? Decode( 10 )
   // A list
   ? Decode( 3, 1, "A", 2, "B", 3, "C" )
   // A list with default
   ? Decode( 4, 1, "A", 2, "B", 3, "C", "X" )
   // Using an array as list of values to check
   ? Decode( 2, { 1, "A", 2, "B", 3, "C" } )
   // Using an array with default as list of values to check
   ? Decode( 2, { 1, "A", 2, "B", 3, "C", "X" } )
   // Using an hash as list
   ? Decode( 2, { 1 => "A", 2 => "B", 3 => "C" } )

   // Returning a codeblock
   ? cStr( Decode( 2, 1, {|| 1 }, 2, {|| 2 }, 3, {|| 3 } ) )

   // Checking an array
   aArray := { 1 }
   ? Decode( aArray, aArray, "A", { 2 }, "B", { 3 }, "C" )

   RETURN
*/

/******************
* Function .......: hb_Decode( <var>, [ <case1,ret1 [,...,caseN,retN] ] [, <def> ]> ) ---> <xRet>
* Author .........: Francesco Saverio Giudice
* Date of creation: 25/01/1991
* Last revision ..: 24/01/2006 1.13 - rewritten for xHarbour and renamed in hb_Decode()
*
*                   Decode a value from a list.
*******************/
FUNCTION HB_Decode(...)

   LOCAL aParams, nParams, xDefault
   LOCAL xVal, cKey, xRet
   LOCAL aValues, aResults, n, i, nPos, nLen

   aParams  := hb_aParams()
   nParams  := PCount()
   xDefault := NIL

   DO CASE
   CASE nParams > 1     // More parameters, real case

      xVal := aParams[ 1 ]

      hb_ADel( aParams, 1, .T. ) // Resize params
      nParams := Len( aParams )

      // if I have a odd number of members, last is default
      IF ( nParams % 2 != 0 )
         xDefault := aTail( aParams )
         // Resize again deleting last
         hb_ADel( aParams, nParams, .T. )
         nParams := Len( aParams )
      ENDIF

      // Ok because I have no other value than default, I will check if it is a complex value
      // like an array or an hash, so I can get it to decode values
      IF xDefault != NIL .AND. ;
         ( HB_ISARRAY( xDefault ) .OR. ;
           HB_ISHASH( xDefault ) )

         // If it is an array I will restart this function creating a linear call
         IF HB_ISARRAY( xDefault ) .AND. Len( xDefault ) > 0

            // I can have a linear array like { 1, "A", 2, "B", 3, "C" }
            // or an array of array couples like { { 1, "A" }, { 2, "B" }, { 3, "C" } }
            // first element tell me what type is

            // couples of values
            IF HB_ISARRAY( xDefault[ 1 ] )

               //// If i have an array as default, this contains couples of key / value
               //// so I have to convert in a linear array

               nLen := Len( xDefault )

               // Check if array has a default value, this will be last value and has a value
               // different from an array
               IF ! HB_ISARRAY( xDefault[ nLen ] )

                  aParams := Array( ( nLen - 1 ) * 2 )

                  n := 1
                  FOR i := 1 TO nLen - 1
                     aParams[ n++ ] := xDefault[ i ][ 1 ]
                     aParams[ n++ ] := xDefault[ i ][ 2 ]
                  NEXT

                  aAdd( aParams, xDefault[ nLen ] )

               ELSE

                  // I haven't a default

                  aParams := Array( Len( xDefault ) * 2 )

                  n := 1
                  FOR i := 1 TO Len( xDefault )
                     aParams[ n++ ] := xDefault[ i ][ 1 ]
                     aParams[ n++ ] := xDefault[ i ][ 2 ]
                  NEXT

               ENDIF
            ELSE
               // I have a linear array

               aParams := xDefault
            ENDIF


         // If it is an hash, translate it in an array
         ELSEIF HB_ISHASH( xDefault )

            aParams := Array( Len( xDefault ) * 2 )

            i := 1
            FOR EACH cKey IN xDefault:Keys
                aParams[ i++ ] := cKey
                aParams[ i++ ] := xDefault[ cKey ]
            NEXT

         ENDIF

         // Then add Decoding value at beginning
         hb_AIns( aParams, 1, xVal, .T. )

         // And run decode() again
         xRet := hb_ExecFromArray( @hb_Decode(), aParams )

      ELSE

         // Ok let's go ahead with real function

         // Combine in 2 lists having elements as { value } and { decode }
         aValues  := Array( nParams / 2 )
         aResults := Array( nParams / 2 )

         i := 1
         FOR n := 1 TO nParams - 1 STEP 2
            aValues[ i ]  := aParams[ n ]
            aResults[ i ] := aParams[ n + 1 ]
            i++
         NEXT

         // Check if value exists (valtype of values MUST be same of xVal,
         // otherwise I will get a runtime error)
         // TODO: Have I to check also between different valtypes, jumping different ?
         nPos := aScan( aValues, {| e | e == xVal } )

         IF nPos == 0 // Not Found, returning default
            xRet := xDefault   // it could be also nil because not present
         ELSE
            xRet := aResults[ nPos ]
         ENDIF
      ENDIF

   CASE nParams == 0    // No parameters
      xRet := NIL

   CASE nParams == 1    // Only value to decode as parameter, return an empty value of itself
      xRet := EmptyValue( aParams[ 1 ] )

   ENDCASE

   RETURN xRet

FUNCTION HB_DecodeOrEmpty(...)
   LOCAL aParams := hb_aParams()
   LOCAL xVal    := hb_ExecFromArray( @hb_decode(), aParams )
   RETURN iif( xVal == NIL, EmptyValue( aParams[ 1 ] ), xVal )

STATIC FUNCTION EmptyValue( xVal )
   LOCAL xRet
   LOCAL cType := ValType( xVal )

   SWITCH cType
   CASE "C"  // Char
   CASE "M"  // Memo
        xRet := ""
        EXIT
   CASE "D"  // Date
        xRet := hb_SToD()
        EXIT
   CASE "L"  // Logical
        xRet := .F.
        EXIT
   CASE "N"  // Number
        xRet := 0
        EXIT
   CASE "B"  // code block
        xRet := {|| NIL }
        EXIT
   CASE "A"  // array
        xRet := {}
        EXIT
   CASE "H"  // hash
        xRet := { => }
        EXIT
   CASE "U"  // undefined
        xRet := NIL
        EXIT
   CASE "O"  // Object
        xRet := NIL   // Or better another value ?
        EXIT

   OTHERWISE
        // Create a runtime error for new datatypes
        xRet := ""
        IF xRet == 0 // BANG!
        ENDIF
   ENDSWITCH

   RETURN xRet
