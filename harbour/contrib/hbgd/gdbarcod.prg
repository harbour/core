/*
 * $Id$
 */

/*
 * xHarbour Project source code:
 * CodeBar engine library class
 *
 * Copyright 2005-2005 Laverson Espindola <laverson.espindola@gmail.com>
 * www - http://www.xharbour.org http://harbour-project.org
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

#include "hbclass.ch"

#define CODEC            100
#define CODEB            101
#define CODEA            102
#define FNC1             103
#define STARTA           104
#define STARTB           105
#define STARTC           106

CREATE CLASS TCode FROM TBarCode

   DATA nType

   // EAN-13 ISBN

   METHOD New( nTypeCode ) CONSTRUCTOR
   METHOD Draw( cText )
   METHOD Draw13( cText )
   METHOD DrawText13()

   // EAN-8

   METHOD Draw8( cText )
   METHOD DrawText8()

   // EAN-128

   METHOD Draw128( cText, cModeCode )

   // I25

   METHOD DrawI25( cText )
   METHOD GenCodei25()

   // Utils

   METHOD FindCharCode( cstring, cchar )
   METHOD MixCode( value )
   METHOD Findcode( uval )

ENDCLASS

METHOD New( nTypeCode ) CLASS TCode

   LOCAL ii

   IF nTypeCode == 13 .OR. ;
      nTypeCode ==  8

      ::LeftHand_Odd  := { "0011001", "0010011", "0111101", "0100011", "0110001", "0101111", "0111011", "0110111", "0001011", "0001101" }
      ::LeftHand_Even := { "0110011", "0011011", "0100001", "0011101", "0111001", "0000101", "0010001", "0001001", "0010111", "0100111" }
      ::Right_Hand    := { "1100110", "1101100", "1000010", "1011100", "1001110", "1010000", "1000100", "1001000", "1110100", "1110010" }
      ::Parity        := { "OOEOEE",  "OOEEOE",  "OOEEEO",  "OEOOEE",  "OEEOOE",  "OEEEOO",  "OEOEOE",  "OEOEEO",  "OEEOEO", "OOOOOO"  }
      ::keys          := { "1", "2", "3", "4", "5", "6", "7", "8", "9", "0" }

   ELSEIF nTypeCode == 128

      ::aCode := {;
         "212222", "222122", "222221", "121223", "121322", "131222", "122213", "122312", "132212", "221213", ;
         "221312", "231212", "112232", "122132", "122231", "113222", "123122", "123221", "223211", "221132", ;
         "221231", "213212", "223112", "312131", "311222", "321122", "321221", "312212", "322112", "322211", ;
         "212123", "212321", "232121", "111323", "131123", "131321", "112313", "132113", "132311", "211313", ;
         "231113", "231311", "112133", "112331", "132131", "113123", "113321", "133121", "313121", "211331", ;
         "231131", "213113", "213311", "213131", "311123", "311321", "331121", "312113", "312311", "332111", ;
         "314111", "221411", "431111", "111224", "111422", "121124", "121421", "141122", "141221", "112214", ;
         "112412", "122114", "122411", "142112", "142211", "241211", "221114", "213111", "241112", "134111", ;
         "111242", "121142", "121241", "114212", "124112", "124211", "411212", "421112", "421211", "212141", ;
         "214121", "412121", "111143", "111341", "131141", "114113", "114311", "411113", "411311", "113141", ;
         "114131", "311141", "411131", "211412", "211214", "211232", "2331112";
         }

      ::KeysmodeA := " " + [!"#$%&\()*+-.,/0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ] + "[\]^_"
      ::KeysmodeB := " " + [!"#$%&\()*+-.,/0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ] + "[\]^_abcdefghijklmnopqrstuvwxyz{|}~"

      ::KeysModeC := Array( 99 )

      FOR ii := 1 TO 99
         ::KeysmodeC[ ii ] := StrZero( ii, 2 )
      NEXT

   ELSEIF nTypeCode == 25

      ::keys          := { "1", "2", "3", "4", "5", "6", "7", "8", "9", "0" }

      ::aCode := Array( 12 )

      ::aCode[ 1 ]  := "10001"          // 1 digit
      ::aCode[ 2 ]  := "01001"          // 2 digit
      ::aCode[ 3 ]  := "11000"          // 3 digit
      ::aCode[ 4 ]  := "00101"          // 4 digit
      ::aCode[ 5 ]  := "10100"          // 5 digit
      ::aCode[ 6 ]  := "01100"          // 6 digit
      ::aCode[ 7 ]  := "00011"          // 7 digit
      ::aCode[ 8 ]  := "10010"          // 8 digit
      ::aCode[ 9 ]  := "01010"          // 9 digit
      ::aCode[ 10 ] := "00110"          // 0 digit
      ::acode[ 11 ] := "10000"          // pre-amble
      ::acode[ 12 ] := "100"            // post-amble

   ELSE

      ::DrawError( "Invalid type to barcode." )

      RETURN NIL

   ENDIF

   ::nType := nTypeCode

   RETURN Self

METHOD Draw( cText ) CLASS TCode

   IF ::nType == 13
      ::Draw13( cText )
   ELSEIF ::nType == 8
      ::Draw8( cText )
   ELSEIF ::nType == 128
      ::Draw128( cText )
   ELSEIF ::nType == 25
      ::DrawI25( cText )
   ENDIF

   RETURN NIL

METHOD Draw13( cText )  CLASS TCode

   LOCAL lerror := .F.
   LOCAL nchkSum := 0
   LOCAL nChk    := 0
   LOCAL ii, jj
   LOCAL xParity

   ::Settext( ctext )

   // Valid characters
   IF ! ::CheckCode()
      lerror := .T.
   ENDIF

   IF !lerror

      IF ::book .AND. Len( ::text ) != 10
         ::DrawError( "Must contains 10 chars if ISBN is true." )
         lerror := .T.
      ENDIF

      // book, we changed the code to the right
      IF ::book .AND. Len( ::text ) == 10
         ::text := "978" + SubStr( ::text, 1, Len( ::text ) - 1 )
      ENDIF

      //  contain only 12 characters ?
      IF Len( ::text ) != 12
         ::DrawError( "Must contains 12 chars, the 13th digit is automatically added." )
         lerror := .T.
      ENDIF

      IF !lerror

         // If we have to write text, we moved the barcode to the right to have space to put digit
         ::positionX := iif( ::textfont == 0 , 0, 10 )

         xParity := ::Parity[ Val( SubStr( ::text, 1, 1 ) ) ]

         // First Bar
         ::positionX := 10
         ::maxHeight := ::maxHeight + 9
         ::DrawSingleBar( "101" )

         // start code
         ::maxHeight := ::maxHeight - 9

         FOR ii := 1 TO Len( ::text )

            // Calculate check digit
            IF Mod( ( Len( ::text ) + 1 ) - ii, 2 ) == 0
               nchkSum := nchkSum + Int( Val( SubStr(::text, ii, 1 ) ) )
            ELSE
               nchkSum := nchkSum + Int( Val( SubStr( ::text, ii, 1 ) ) ) * 3
            ENDIF

            // ANow, the bar of the middle
            IF ii == 8
               ::positionX += 1
               ::maxHeight := ::maxHeight + 9
               ::DrawSingleBar( "101" )
               ::maxHeight := ::maxHeight - 9
               ::positionX += 1
            ENDIF

            jj := Val( SubStr( ::text, ii, 1 ) )

            IF jj == 0
               jj := 10
            ENDIF

            IF ii > 1 .AND. ii < 8

               ::DrawSingleBar( iif( SubStr( xParity, ii - 1, 1 ) == "E",;
                  ::LeftHand_Even[ jj ], ;
                  ::LeftHand_Odd[ jj ] ) )
            ELSEIF ii > 1 .AND. ii >= 8

               ::DrawSingleBar( ::Right_Hand[ jj ] )

            ENDIF

         NEXT

         jj := Mod( nchkSum, 10 )

         IF jj != 0
            nChk := 10 - jj
         ENDIF

         IF nChk == 0
            nChk := 10
         ENDIF

         ::DrawSingleBar( ::Right_Hand[ nChk ] )

         // Now,  finish bar
         ::maxHeight := ::maxHeight + 9
         ::DrawSingleBar( "101" )

         ::lastX := ::positionX
         ::lastY := ::maxHeight

         ctext += Str( nChk, 1 ) // TOFIX? nChk can be 10 at this point, is this intended?

         // Draw Text
         IF ::lDrawValue
            ::Settext( ctext )
            ::DrawText13()
         ENDIF

      ENDIF

   ENDIF

   RETURN NIL

METHOD DrawText13()  CLASS TCode

   IF ::textfont != 0

      ::Say( 2, ::maxHeight - ( ::GetFontHeight() / 2 ), SubStr( ::text, 1, 1 ) , ::FillColor )
      ::Say( ( 10 + ( 3 * ::res + 48 * ::res ) / 2 ) - ( ::GetFontWidth() * ( 6 / 2 ) ), ::maxHeight + 1, SubStr( ::text, 2, 6 ), ::FillColor )
      ::Say( 10 + 46 * ::res + ( 3 * ::res + 46 * ::res ) / 2 - ::GetFontWidth() * ( 6 / 2 ), ::maxHeight + 1, SubStr( ::text, 8, 6 ), ::FillColor )

   ENDIF

   ::lastY := ::maxHeight + ::GetFontHeight()

   RETURN NIL

METHOD Draw8( cText ) CLASS TCode

   LOCAL lerror := .F.
   LOCAL ii, jj

   //LOCAL xParity
   LOCAL nchkSum := 0
   LOCAL nChk    := 0

   ::Settext( ctext )

   // Valid characters
   IF !::CheckCode()
      lerror := .T.
   ENDIF

   IF !lerror

      ::positionX := iif( ::textfont == 0, 0, 10 )

      //xParity := ::Parity[ 7 ]

      // First Bar
      ::positionX := 10
      ::maxHeight := ::maxHeight + 9
      ::DrawSingleBar( "101" )

      // Start Code
      ::maxHeight := ::maxHeight - 9

      FOR ii := 1 TO Len( ::text )

         IF Mod( ( Len( ::text ) + 1 ) - ii, 2 ) == 0
            nchkSum := nchkSum +  Int( Val( SubStr( ::text, ii, 1 ) ) )
         ELSE
            nchkSum := nchkSum +  Int( Val( SubStr( ::text, ii, 1 ) ) ) * 3
         ENDIF

         IF ii == 5
            ::positionX += 1
            ::maxHeight := ::maxHeight + 9
            ::DrawSingleBar( "01010" )
            ::maxHeight := ::maxHeight - 9
            ::positionX += 1
         ENDIF

         jj := Val( SubStr( ::text, ii, 1 ) )

         IF jj == 0
            jj := 10
         ENDIF

         IF ii < 5
            ::DrawSingleBar( ::LeftHand_Odd[ jj ] )
         ELSEIF ii >= 5
            ::DrawSingleBar( ::Right_Hand[ jj ] )
         ENDIF

      NEXT

      jj := Mod( nchkSum, 10 )

      IF jj != 0
         nChk := 10 - jj
      ENDIF

      ::DrawSingleBar( ::Right_Hand[ nChk ] )

      // Now, finish bar
      ::maxHeight := ::maxHeight + 9
      ::DrawSingleBar( "101" )

      ::lastX := ::positionX
      ::lastY := ::maxHeight

      ctext += Str( nChk, 1 )

      // Draw text
      IF ::lDrawValue
         ::Settext( ctext )
         ::DrawText8()
      ENDIF

   ENDIF

   RETURN NIL

METHOD DrawText8() CLASS TCode

   ::say( 10 + ( ( 3 * ::res + 34 * ::res ) / 2 - ::GetFontWidth() * ( 4 / 2 ) ), ::maxHeight + 1, SubStr( ::text, 1, 4 ), ::fillcolor )
   ::say( 10 + ( 32 * ::res + ( 3 * ::res + 32 * ::res ) / 2 - ::GetFontWidth() * ( 4 / 2 ) ), ::maxHeight + 1, SubStr( ::text, 5, 4 ), ::fillcolor )

   ::lastY := ::maxHeight + ::GetFontHeight()

   RETURN NIL

METHOD FindCharCode( cstring, cchar ) CLASS TCode

   LOCAL i
   LOCAL nC   := 0
   LOCAL nret := 0

   FOR i := 1 TO Len( cstring )

      IF SubStr( cstring, i, 1 ) == cchar
         ++nC
         nRet := nC
         EXIT
      ENDIF

      ++nC

   NEXT

   RETURN nret

METHOD Draw128( cText, cModeCode ) CLASS TCode

   LOCAL cchar, nvalchar, n, i

   LOCAL nSum       := 0
   LOCAL nC         := 0
   LOCAL npos

   //LOCAL value_test := 0
   LOCAL lTypeCodeC := .F.
   LOCAL lTypeCodeA := .F.
   LOCAL lerror     := .F.
   LOCAL cBarCode   := ""
   LOCAL cconc      := ""

   hb_default( @cModeCode, "B" )

   ::settext( cText )

   IF ! Empty( cModeCode )
      IF HB_ISSTRING( cModeCode ) .AND. Upper( cModeCode ) $ "ABC"
         cModeCode := Upper( cModeCode )
      ELSE
         ::DrawError( "Code 128 Modes are A,B o C. Character values." )
         lerror := .T.
      ENDIF
   ENDIF

   // Checking if all chars are allowed
   FOR i := 1 TO Len( ::text )

      IF cModeCode == "C"

         npos := AScan( ::KeysmodeC, {| x | x == SubStr( ::Text, i, 1 ) + SubStr( ::Text, i + 1, 1 ) } )

         IF npos == 0
            ::DrawError( "With Code C, you must provide always pair of two integers. Char " + SubStr( ::text, i, 1 ) + SubStr( ::text, i + 1, 1 ) + " not allowed." )
            lerror := .T.
         ENDIF

      ELSEIF cModeCode == "B"

         IF ::FindCharCode( ::KeysmodeB, SubStr( ::Text, i, 1 ) ) == 0
            ::DrawError( "Char " + SubStr( ::text, i, 1 ) + " not allowed." )
            lerror := .T.
         ENDIF

      ELSEIF cModeCode == "A"

         IF ::FindCharCode( ::KeysmodeA, SubStr( ::text, i, 1 ) ) == 0
            ::DrawError( "Char " + SubStr( ::text, i, 1 ) + " not allowed." )
            lerror := .T.
         ENDIF

      ENDIF

   NEXT

   IF !lerror

      IF Empty( cModeCode )

         IF Str( Val( ::text ), Len( ::text ) ) == ::text

            lTypeCodeC :=  .T.
            cconc  := ::aCode[ STARTC ]
            nSum   := STARTB

         ELSE

            FOR n := 1 TO Len( ::text )
               nC += iif( SubStr( ::text, n, 1 ) > 31, 1, 0 )
            NEXT

            IF nC < Len( ::text ) / 2
               lTypeCodeA := .T.
               cconc := ::aCode[ STARTA ]
               nSum  := FNC1
            ELSE
               cconc := ::aCode[ STARTB ]
               nSum := STARTA
            ENDIF

         ENDIF

      ELSE

         IF cModeCode == "C"

            lTypeCodeC  := .T.
            cconc       := ::aCode[ STARTC ]
            nSum        := STARTB

         ELSEIF cModeCode == "A"

            lTypeCodeA := .T.
            cconc      := ::aCode[ STARTB ]
            nSum       := FNC1

         ELSE

            cconc := ::aCode[ STARTB ]
            nSum := STARTA

         ENDIF

      ENDIF

      nC := 0

      FOR n := 1 TO Len( ::text )

         ++nC

         cchar := SubStr( ::text, n, 1 )

         IF lTypeCodeC

            IF Len( ::TEXT ) == n
               cconc += ::aCode[ CODEB ]
               nvalchar := Asc( cchar ) - 31
            ELSE
               nvalchar := Val( SubStr( ::text, n, 2 ) ) + 1
               ++n
            ENDIF

         ELSEIF lTypeCodeA

            IF cchar > "_"
               cconc += ::aCode[ CODEB ]
               nvalchar  := Asc( cchar ) - 31
            ELSEIF cchar <= " "
               nvalchar  := Asc( cchar ) + 64
            ELSE
               nvalchar  := Asc( cchar ) - 31
            ENDIF

         ELSE

            IF cchar < " "
               cconc += ::aCode[ CODEA ]
               nvalchar := Asc( cchar ) + 64
            ELSE
               nvalchar := Asc( cchar ) - 31
            ENDIF

         ENDIF
         nSum += ( nvalchar - 1 ) * nC
         cconc := cconc + ::aCode[ nvalchar ]

      NEXT

      nSum  := nSum % 103 + 1
      cconc := cconc + ::aCode[ nSum ] + ::aCode[ 107 ]

      FOR n := 1 TO Len( cconc ) STEP 2
         cBarCode += Replicate( "1", Val( SubStr( cconc, n, 1 ) ) )
         cBarCode += Replicate( "0", Val( SubStr( cconc, n + 1, 1 ) ) )
      NEXT

      ::DrawSingleBar( cBarCode )

      ::lastX := ::positionX
      ::lastY := ::maxHeight

      // Draw Text
      IF ::lDrawValue
         ::Settext( ctext )
         ::DrawText()
      ENDIF

   ENDIF

   RETURN NIL

METHOD DrawI25( cText ) CLASS TCode

   ::settext( cText )

   ::GenCodei25()

   RETURN NIL

METHOD GenCodei25() CLASS TCode

   LOCAL lError   := .F.
   LOCAL bc_string // := ::text

// LOCAL new_string := ""

   IF ( Len( ::text ) % 2 ) != 0
      ::DrawError( "Invalid barcode lenght" )
      lError := .T.
   ENDIF

   IF !lError

      bc_string := Upper( ::text )

      // encode itemId to I25 barcode standard.

      bc_string := ::MixCode( bc_string )

      ///////////////////////////////
      //Adding Start and Stop Pattern

      ::DrawSingleI25( ::acode[ 11 ] + bc_string + ::acode[ 12 ]  )

      ::lastY := ::maxHeight

      // Draw Text
      IF ::lDrawValue
         ::DrawText( .T. )
      ENDIF

   ENDIF

   RETURN NIL

/*
   It makes mixe of the value to be codified by the Bar code I25
*/

METHOD MixCode( value ) CLASS TCode

   LOCAL l, i, k
   LOCAL s
   LOCAL bar_string := ""
   LOCAL cfirst
   LOCAL cnext

   l := Len( value )

   IF ( l % 2 ) != 0
      ::DrawError( "Code cannot be intercalated:  Invalid length (mix)" )
   ELSE

      i := 1
      s := ""

      DO WHILE i < l

         cFirst := ::Findcode( SubStr( value, i, 1 ) )
         cnext  := ::Findcode( SubStr( value, i + 1, 1 ) )

         // Mix of the codes
         // NNNNWNNWWW
         //  N N N W W
         FOR k := 1 TO 5
            s += SubStr( cFirst, k, 1 ) + SubStr( cnext, k, 1 )
         NEXT

         i += 2

      ENDDO

      bar_string :=  s

   ENDIF

   RETURN bar_string

METHOD Findcode( uval ) CLASS TCode

   LOCAL npos
   LOCAL cretc

   npos  := AScan( ::keys, {| x | Left( x, 1 ) == uval } )
   cretc := ::acode[ npos ]

   RETURN cretc
