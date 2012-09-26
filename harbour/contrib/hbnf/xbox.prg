/*
 * $Id$
 */

/*
 * File......: xbox.prg
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

#ifdef FT_TEST

PROCEDURE Main()

   LOCAL i

   SetColor( "W/B" )
// clear screen
   FOR i := 1 TO 24
      @ i, 0 SAY Replicate( "@", 80 )
   NEXT

   FT_XBOX( , , , , , , , "This is a test", "of the XBOX() function" )
   FT_XBOX( "L", "W", "D", "GR+/R", "W/B", 1, 10, "It is so nice", ;
      "to not have to do the messy chore", ;
      "of calculating the box size!" )
   FT_XBOX( , "W", "D", "GR+/R", "W/B", 16, 10, "It is so nice", ;
      "to not have to do the messy chore", ;
      "of calculating the box size!", ;
      "Even though this line is way too long, and is in fact more than 80 characters long, if you care to check!" )

   RETURN

#endif

/* NOTE: In original NF, flag parameters were also accepted when
         having extra characters (f.e. "DOUBLE" instead of "D"),
         but only if _SET_EXACT was set to .F., Harbour accepts them
         that way regardless of _SET_EXACT setting. [vszakats] */

FUNCTION FT_XBOX( cJustType, ; // "L" -> left, otherwise centered
   cRetWait, ; // "W" -> wait for keypress before continuing
   cBorType, ; // "D" -> double, anything else single border
   cBorColor, ; // color string for border
   cBoxColor, ; // color string for text
   nStartRow, ; // upper row of box.  99=center vertically
   nStartCol, ; // left edge of box.  99=center horizontally
   cLine1, cLine2, cLine3, cLine4, cLine5, cLine6, cLine7, cLine8 )

   LOCAL nLLen := 0
// LOCAL cOldColor
   LOCAL nLCol
   LOCAL nRCol
   LOCAL nTRow
   LOCAL nBRow
   LOCAL nLoop
   LOCAL nSayRow
   LOCAL nSayCol
   LOCAL nNumRows
   LOCAL aLines_[ 8 ]

   IF cJustType == NIL
      cJustType := ""
   ENDIF
   IF cRetWait == NIL
      cRetWait := ""
   ENDIF
   IF cBorType == NIL
      cBorType := ""
   ENDIF

   // validate parameters
   cJustType := iif( HB_ISSTRING( cJustType ), Upper( cJustType ), "" )
   cRetWait  := iif( HB_ISSTRING( cRetWait ) , Upper( cRetWait ), "" )
   cBorType  := iif( HB_ISSTRING( cBorType ) , Upper( cBorType ), "" )
   cBorColor := iif( HB_ISSTRING( cBoxColor ), cBorColor, "N/W" )
   cBoxColor := iif( HB_ISSTRING( cBoxColor ), cBoxColor, "W/N" )
   nStartRow := iif( HB_ISNUMERIC( nStartRow ), nStartRow, 99 )
   nStartCol := iif( HB_ISNUMERIC( nStartCol ), nStartCol, 99 )

   nNumRows := Min( PCount() - 7, 8 )

   // establish array of strings to be displayed
   aLines_[ 1 ] := iif( HB_ISSTRING( cLine1 ), AllTrim( SubStr( cLine1, 1, 74 ) ), "" )
   aLines_[ 2 ] := iif( HB_ISSTRING( cLine2 ), AllTrim( SubStr( cLine2, 1, 74 ) ), "" )
   aLines_[ 3 ] := iif( HB_ISSTRING( cLine3 ), AllTrim( SubStr( cLine3, 1, 74 ) ), "" )
   aLines_[ 4 ] := iif( HB_ISSTRING( cLine4 ), AllTrim( SubStr( cLine4, 1, 74 ) ), "" )
   aLines_[ 5 ] := iif( HB_ISSTRING( cLine5 ), AllTrim( SubStr( cLine5, 1, 74 ) ), "" )
   aLines_[ 6 ] := iif( HB_ISSTRING( cLine6 ), AllTrim( SubStr( cLine6, 1, 74 ) ), "" )
   aLines_[ 7 ] := iif( HB_ISSTRING( cLine7 ), AllTrim( SubStr( cLine7, 1, 74 ) ), "" )
   aLines_[ 8 ] := iif( HB_ISSTRING( cLine8 ), AllTrim( SubStr( cLine8, 1, 74 ) ), "" )
   ASize( aLines_, Min( nNumRows, 8 ) )

   // determine longest line
   nLoop := 1
   AEval( aLines_, {|| nLLen := Max( nLLen, Len( aLines_[ nLoop ] ) ), nLoop++ } )

   // calculate corners
   nLCol := iif( nStartCol == 99, Int( ( 76 - nLLen ) / 2 ), Min( nStartCol, 74 - nLLen ) )
   nRCol := nLCol + nLLen + 3
   nTRow := iif( nStartRow == 99, Int( ( 24 - nNumRows ) / 2 ), Min( nStartRow, 22 - nNumRows ) )
   nBRow := nTRow + nNumRows + 1

   // form box and border

   // save screen color and set new color
// cOldColor := SetColor( cBoxColor )
   @ nTRow, nLCol CLEAR TO nBRow, nRCol

   // draw border
   SetColor( cBorColor )
   IF Left( cBorType, 1 ) == "D"
      @ nTRow, nLCol TO nBRow, nRCol double
   ELSE
      @ nTRow, nLCol TO nBRow, nRCol
   ENDIF

   // write shadow
   FT_SHADOW( nTRow, nLCol, nBRow, nRCol )

   // print text in box
   SetColor( cBoxColor )
   nLoop := 1
   AEval( aLines_, {| cSayStr |;
      nSayRow := nTRow + nLoop, ;
      nSayCol := iif( Left( cJustType, 1 ) == "L", ;
      nLCol + 2, ;
      nLCol + 2 + ( nLLen - Int( Len( aLines_[ nLoop ] ) ) ) / 2 ), ;
      nLoop++, ;
      _FTSAY( nSayRow, nSayCol, cSayStr );
      } )

   // wait for keypress if desired
   IF Left( cRetWait, 1 ) == "W"
      Inkey( 0 )
   ENDIF

   RETURN NIL

STATIC FUNCTION _FTSAY( nSayRow, nSayCol, cSayStr )

   @ nSayRow, nSayCol SAY cSayStr

   RETURN NIL
