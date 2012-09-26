/*
 * $Id$
 */

/*
 * File......: dispmsg.prg
 * Author....: Paul Ferrara, ColumbuSoft
 * CIS ID....: 76702,556
 *
 * This function is an original work by Paul Ferrara and is placed in the
 * public domain.
 *
 * Modification history:
 * ---------------------
 *
 *    Rev 2.0   12 Aug 1994 23:05:14   PAUL
 * Added ablilty to highlight individual characters and cleaned up code
 *
 *    Rev 1.2   15 Aug 1991 23:05:14   GLENN
 * Forest Belt proofread/edited/cleaned up doc
 *
 *    Rev 1.1   14 Jun 1991 19:51:36   GLENN
 * Minor edit to file header
 *
 *    Rev 1.0   01 Apr 1991 01:01:12   GLENN
 * Nanforum Toolkit
 *
 */

#include "inkey.ch"
#include "setcurs.ch"

// beginning of demo program
#ifdef FT_TEST

// color variables
STATIC cNormH, cNormN, cNormE
STATIC cWindH, cWindN, cWindE
STATIC cErrH, cErrN, cErrE

PROCEDURE Main( cCmdLine )

   LOCAL cDosScrn
   LOCAL nDosRow
   LOCAL nDosCol
   LOCAL lColor
   LOCAL nMaxRow
   LOCAL nType

// main routine starts here
   SET SCOREBOARD OFF

   lColor := .T.

   cNormH := iif( lColor, "W+/BG", "W+/N" )
   cNormN := iif( lColor, "N/BG" , "W/N"  )
   cNormE := iif( lColor, "N/W" , "N/W"  )
   cWindH := iif( lColor, "W+/B", "W+/N" )
   cWindN := iif( lColor, "W/B" , "W/N"  )
   cWindE := iif( lColor, "N/W" , "N/W"  )
   cErrH  := iif( lColor, "W+/R", "W+/N" )
   cErrN  := iif( lColor, "W/R" , "W/N"  )
   cErrE  := iif( lColor, "N/W" , "N/W"  )

   cDosScrn := SaveScreen()
   nDosRow := Row()
   nDosCol := Col()
   SetColor( "W/N" )
   CLS
   nMaxRow := MaxRow()
   SetBlink( .F. )
   SetColor( cWindN + "*" )
   CLS
   SetColor( cNormN )

   FT_DispMsg( { { "[Esc] To Abort Changes   [PgDn] To Continue" }, { cNormN, , cNormH } }, , nMaxRow - 5 )

   FT_DispMsg( { { "[E]dit     [P]rint    [D]elete",     ;
      "[Esc]ape       [Alt-Q]" },           ;
      { cErrN, cErrN, cErrH } }, , 2 )

   nType := FT_DispMsg( { { "Create Or Edit [I]nvoice",    ;
      "Create Or Edit [O]rder",      ;
      "Create Or Edit [B]ack Order", ;
      "Create Or Edit [Q]uote",      ;
      "[Esc] To Exit" },             ;
      { cWindN, , , , , cWindH } }, "BIOQ" + Chr( 27 ) )

   SetColor( "W/N" )
   SetCursor( SC_NORMAL )
   SetBlink( .T. )
   RestScreen( , , , , cDosScrn )
   SetPos( nDosRow, nDosCol )
   QUIT

#endif
// end of demo program

FUNCTION FT_DispMsg( aInfo, cKey, nBoxTop, nBoxLeft, cnBoxString, lShadow )

   LOCAL xRtnVal := .F.
   LOCAL nWidest := 0
   LOCAL nBoxRight
   LOCAL nBoxBottom
   LOCAL cOldScreen
   LOCAL cOldCursor
   LOCAL cOldColor
   LOCAL i
   LOCAL j
   LOCAL nOption
   LOCAL x
   LOCAL y
   LOCAL aPos := {}
   LOCAL nLeft
   LOCAL aLeft

   FOR i := 1 TO Len( aInfo[ 1 ] )
      AAdd( aPos, {} )
   NEXT

   FOR i := 1 TO Len( aInfo[1] )

      DO WHILE At( "[", aInfo[ 1, i ] ) > 0
         x := At( "[", aInfo[ 1, i ] )
         y := At( "]", aInfo[ 1, i ] ) - 2
         AAdd( aPos[ i ], { x, y } )
         aInfo[ 1, i ] := StrTran( aInfo[ 1, i ], "[", "", 1, 1 )
         aInfo[ 1, i ] := StrTran( aInfo[ 1, i ], "]", "", 1, 1 )
      ENDDO

   NEXT

   AEval( aInfo[ 1 ], {| x | nWidest := Max( nWidest, Len( x ) ) } )

   /* calculate location of data */
   IF nBoxLeft == NIL
      nLeft := Round( ( MaxCol() - nWidest ) / 2, 0 )
   ELSE
      nLeft := nBoxLeft + 2
   ENDIF

/*
   IF nBoxTop == NIL
      nTop := ( MAXROW() - LEN( aInfo[ 1 ] ) - 2 ) / 2 + 2
   ENDIF
*/

   /* calculate location of box */
   IF nBoxLeft == NIL
      nBoxLeft := nLeft - 2
   ENDIF
   nBoxRight := nBoxLeft + nWidest + 3

   IF nBoxTop == NIL
      nBoxTop := ( MaxRow() - Len( aInfo[ 1 ] ) - 2 ) / 2 + 1
   ENDIF
   nBoxBottom := nBoxTop + Len( aInfo[ 1 ] ) + 1

// following is to keep from breaking old code and to be
// consistent with DISPBOX()

   IF cnBoxString == NIL .OR. cnBoxString == 2
      cnBoxString := hb_UTF8ToStr( "╔═╗║╝═╚║ " )
   ELSEIF cnBoxString == 1
      cnBoxString := hb_UTF8ToStr( "┌─┐│┘─└│ " )
   ENDIF

   lShadow := iif( lShadow == NIL, .T. , lShadow )

   cOldScreen := SaveScreen( nBoxTop, nBoxLeft, nBoxBottom + 1, nBoxRight + 2 )

   cOldCursor := SetCursor( SC_NONE )

// draw box
   cOldColor := SetColor( aInfo[ 2, LEN( aInfo[ 2 ] ) ] )

   DispBox( nBoxTop, nBoxLeft, nBoxBottom, nBoxRight, cnBoxString, ;
      aInfo[ 2, LEN( aInfo[ 2 ] ) ] )
   IF lShadow
      FT_Shadow( nBoxTop, nBoxLeft, nBoxBottom, nBoxRight )
   ENDIF

   /* fill array with left positions for each row */
   aLeft := Array( Len( aInfo[ 1 ] ) )
   FOR i := 1 TO Len( aInfo[ 1 ] )
      IF Len( aInfo[ 1, i ] ) == nWidest
         aLeft[ i ] := nLeft
      ELSE
         aLeft[ i ] := nLeft + Round( ( nWidest - Len( aInfo[ 1, i ] ) ) / 2, 0 )
      ENDIF
   NEXT

   /* fill array of colors */
   FOR i := 2 TO Len( aInfo[ 2 ] )
      IF aInfo[ 2, i ] == NIL
         aInfo[ 2, i ] := aInfo[ 2, i - 1 ]
      ENDIF
   NEXT

   /* display messages */
   FOR i := 1 TO Len( aInfo[ 1 ] )
      @ nBoxTop + i, aLeft[ i ] SAY aInfo[ 1, i ] COLOR aInfo[ 2, i ]
   NEXT

   /* highlight characters */
   FOR i := 1 TO Len( aPos )
      FOR j := 1 TO Len( aPos[ i ] )

         FT_SetAttr( nBoxTop + i,                           ;
            aPos[ i, j, 1 ] + aLeft[ i ] - 1,               ;
            nBoxTop + i,                                    ;
            aPos[ i, j, 2 ] + aLeft[ i ] - 1,               ;
            FT_Color2N( aInfo[ 2, LEN( aInfo[ 2 ] ) ] ) )
      NEXT
   NEXT

   IF cKey != NIL
      IF Len( cKey ) == 1
         nOption := FT_SInkey( 0 )
         IF Upper( Chr( nOption ) ) == cKey
            xRtnVal := .T.
         ENDIF
      ELSE
         nOption := 0
         DO WHILE At( Upper( Chr( nOption ) ), Upper( cKey ) ) == 0
            nOption := FT_SInkey( 0 )
         ENDDO
         xRtnVal := nOption
      ENDIF
      RestScreen( nBoxTop, nBoxLeft, nBoxBottom + 1, nBoxRight + 2, cOldScreen )
   ENDIF

   SetColor( cOldColor )
   SetCursor( cOldCursor )

   RETURN xRtnVal
