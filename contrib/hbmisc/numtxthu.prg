/*
 * Harbour Project source code:
 * NumToTxtHU() function to convert a number to Hungarian text
 *
 * Copyright 1999-2008 Viktor Szakats (harbour syenar.net)
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

FUNCTION NumToTxtHU( nValue )

   LOCAL aTort := { "tized", "század", "ezred", "tízezred", "százezred", "milliomod", "milliárdod" }
   LOCAL cRetVal
   LOCAL tmp, tmp1, tmp2

   IF nValue < 0
      nValue := -nValue
      cRetVal := "mínusz "
   ELSE
      cRetVal := ""
   ENDIF

   IF Int( nValue ) == 0
      cRetVal += "nulla"
   ENDIF

   cRetVal += NumToTxtRaw( tmp := Int( nValue ) )

   IF ( tmp := ( nValue - tmp ) ) > 0 .AND. tmp < 1

      tmp1 := Len( tmp2 := SubStr( Str( tmp, 8, 6 ), 3 ) )

      WHILE SubStr( tmp2, tmp1, 1 ) == "0" .AND. tmp1 > 0
         tmp1--
      ENDDO

      cRetVal += " egész " + NumToTxtRaw( tmp * ( 10 ^ tmp1 ) ) + iif( tmp1 >= 1 .AND. tmp1 <= Len( aTort ), " " + aTort[ tmp1 ], "" )
   ENDIF

   RETURN hb_UTF8ToStr( cRetVal )

STATIC FUNCTION NumToTxtRaw( nValue )

   LOCAL aEgesz  := { "", "ezer" , "millió", "milliárd", "billió" , "trillió", "kvadrillió", "kvintillió" } // , "szextillió", "szeptillió", "oktillió", "nontillió" }
   LOCAL aEgyes  := { "", "egy"  , "kettő" , "három"   , "négy"   , "öt"     , "hat"       , "hét"       , "nyolc"     , "kilenc" }
   LOCAL aTizes1 := { "", "tíz"  , "húsz"  , "harminc" , "negyven", "ötven"  , "hatvan"    , "hetven"    , "nyolcvan"  , "kilencven" }
   LOCAL aTizes2 := { "", "tizen", "huszon", "harminc" , "negyven", "ötven"  , "hatvan"    , "hetven"    , "nyolcvan"  , "kilencven" }

   LOCAL aDigit
   LOCAL nLen
   LOCAL cValue
   LOCAL tmp

   cValue := hb_ntos( nValue )
   cValue := PadL( cValue, ( Int( Max( Len( cValue ) - 1, 0 ) / 3 ) + 1 ) * 3, "0" )

   aDigit := Array( nLen := Len( cValue ) )
   FOR tmp := 1 TO nLen
      aDigit[ tmp ] := Val( SubStr( cValue, tmp, 1 ) )
   NEXT

   cValue := ""
   FOR tmp := 1 TO nLen - 2 STEP 3

      IF aDigit[ tmp     ] != 0 .OR. ;
         aDigit[ tmp + 1 ] != 0 .OR. ;
         aDigit[ tmp + 2 ] != 0

         cValue += ;
            iif( Empty( cValue ), "", "-" ) + ;
            iif( aDigit[ tmp ] != 0, aEgyes[ aDigit[ tmp ] + 1 ] + "száz", "" ) + ;
            iif( aDigit[ tmp + 2 ] == 0, aTizes1[ aDigit[ tmp + 1 ] + 1 ], aTizes2[ aDigit[ tmp + 1 ] + 1 ] ) + ;
            aEgyes[ aDigit[ tmp + 2 ] + 1 ] + ;
            aEgesz[ ( Int( ( nLen - tmp ) / 3 ) ) + 1 ]
      ENDIF
   NEXT

   RETURN cValue
