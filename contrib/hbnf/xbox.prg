/*
 * Author....: Don Opperthauser
 * CIS ID....: ?
 *
 * This is an original work by Don Opperthauser and is placed in the
 * public domain.
 *
 * Modification history:
 * ---------------------
 *
 *    Rev 1.3   17 Aug 1991 15:47:06   GLENN
 * Don Caton fixed some spelling errors in the doc
 *
 *    Rev 1.2   15 Aug 1991 23:05:12   GLENN
 * Forest Belt proofread/edited/cleaned up doc
 *
 *    Rev 1.1   14 Jun 1991 17:55:50   GLENN
 * Fixed bug where extra blank line was displayed in the box.
 *
 *    Rev 1.0   01 Apr 1991 01:02:34   GLENN
 * Nanforum Toolkit
 *
 */

#include "box.ch"

/* NOTE: In original NF, flag parameters were also accepted when
         having extra characters (f.e. "DOUBLE" instead of "D"),
         but only if _SET_EXACT was set to .F., Harbour accepts them
         that way regardless of _SET_EXACT setting. [vszakats] */

PROCEDURE ft_XBox( ;
      cJustType, ; // "L" -> left, otherwise centered
      cRetWait, ; // "W" -> wait for keypress before continuing
      cBorType, ; // "D" -> double, anything else single border
      cBorColor, ; // color string for border
      cBoxColor, ; // color string for text
      nStartRow, ; // upper row of box.  99=center vertically
      nStartCol ) // left edge of box.  99=center horizontally

   LOCAL nLLen
   LOCAL nLCol
   LOCAL nRCol
   LOCAL nTRow
   LOCAL nBRow
   LOCAL nNumRows
   LOCAL aLines
   LOCAL tmp

   hb_default( @cJustType, "" )
   hb_default( @cRetWait, "" )
   hb_default( @cBorType, "" )
   hb_default( @cBorColor, "N/W" )
   hb_default( @cBoxColor, "W/N" )
   hb_default( @nStartRow, 99 )
   hb_default( @nStartCol, 99 )

   cJustType := Upper( cJustType )
   cRetWait  := Upper( cRetWait )
   cBorType  := Upper( cBorType )

   // establish array of strings to be displayed
   aLines := {}
   FOR tmp := 8 TO PCount()
      AAdd( aLines, AllTrim( Left( hb_defaultValue( hb_PValue( tmp ), "" ), MaxCol() - 5 ) ) )
   NEXT
   nNumRows := Len( aLines )

   // determine longest line
   nLLen := 0
   AEval( aLines, {| cSayStr | nLLen := Max( nLLen, Len( cSayStr ) ) } )

   // calculate corners
   nLCol := iif( nStartCol == 99, Int( ( MaxCol() - 3 - nLLen ) / 2 ), Min( nStartCol, MaxCol() - 5 - nLLen ) )
   nRCol := nLCol + nLLen + 3
   nTRow := iif( nStartRow == 99, Int( ( MaxRow() - nNumRows ) / 2 ), Min( nStartRow, MaxRow() - 2 - nNumRows ) )
   nBRow := nTRow + nNumRows + 1

   // form box and border

   // save screen color and set new color
   hb_Scroll( nTRow, nLCol, nBRow, nRCol )

   // draw border
   SetColor( cBorColor )
   hb_DispBox( nTRow, nLCol, nBRow, nRCol, ;
      iif( hb_LeftIs( cBorType, "D" ), HB_B_DOUBLE_UNI, HB_B_SINGLE_UNI ) )

   // write shadow
   hb_Shadow( nTRow, nLCol, nBRow, nRCol )

   // print text in box
   SetColor( cBoxColor )
   AEval( aLines, {| cSayStr, nLoop | ;
      hb_DispOutAt( ;
         nTRow + nLoop, ;
         nLCol + 2 + iif( hb_LeftIs( cJustType, "L" ), 0, ( nLLen - Int( Len( cSayStr ) ) ) / 2 ), ;
         cSayStr ) } )

   // wait for keypress if desired
   IF hb_LeftIs( cRetWait, "W" )
      Inkey( 0 )
   ENDIF

   RETURN
