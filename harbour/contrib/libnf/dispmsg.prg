/*
 * File......: DISPMSG.PRG
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


/*  $DOC$
 *  $FUNCNAME$
 *     FT_DISPMSG()
 *  $CATEGORY$
 *     Menus/Prompts
 *  $ONELINER$
 *     Display a message and optionally waits for a keypress
 *  $SYNTAX$
 *     FT_DISPMSG( <aMessageArray>, [ <cKey2Check> ],
 *                 [ <nTopBoxRow> ], [ <nLeftBoxColumn> ],
 *                 [ <cnBoxType> ], [ <lShadow> ] ) -> lKeyMatch
 *  $ARGUMENTS$
 *     <aMessageArray> is a multidimensional array of messages to be
 *     displayed and the color attributes for each message.
 *
 *     The first dimension of the array contains one or more elements,
 *     each representing one line in the message box, up to the maximum
 *     number of rows on the screen.
 *
 *     Within each line of the message individual characters or groups
 *     of characters may be delimited with braces ([]).  The braces will
 *     be stripped out and the character(s) inside those braces will be
 *     highlighted.
 *
 *     The second dimension of the array contains a color attribute for
 *     the corresponding element in dimension one, plus one additional
 *     element for the color of the box border.  Dimension two will
 *     always contain one more element than dimension one.  If an
 *     attribute is omitted, the last color selected will be used.
 *
 *     <Key2Check> is a character string of one or more keys to check
 *     for.  If omitted, the message is displayed and control is returned
 *     to the calling procedure.  If one character is specified,
 *     FT_DISPMSG() waits for one keypress, restores the screen and
 *     returns.  If multiple characters are specified, FT_DISPMSG()
 *     remains in a loop until one of the specified keys has been
 *     pressed, then restores the screen and returns.
 *
 *     <nTopBoxRow> is the upper row for the message box.  If omitted, the
 *     box is centered vertically.
 *
 *     <nLeftBoxColumn> is the leftmost column for the box.  If omitted, the
 *     box is centered horizontally.
 *
 *     <cnBoxType> is a string of characters or a variable for the box
 *     border.  See the DISPBOX() function.  If omitted, a double box is
 *     drawn.
 *
 *     <lShadow> is a logical variable.  If true (.T.) or omitted, it
 *     uses FT_SHADOW() to add a transparent shadow to the box.  If
 *     false (.F.), the box is drawn without the shadow.
 *  $RETURNS$
 *     If <Key2Check> is not specified, FT_DISPMSG() will return false
 *     (.F.).
 *
 *     If <Key2Check> is a one-character string, FT_DISPMSG() will return
 *     true (.T.) if the user presses that key, or false (.F.) if any
 *     other key is pressed.
 *
 *     If <Key2Check> consists of multiple characters, it will lock the
 *     user in a loop until one of those keys are pressed and return the
 *     INKEY() value of the keypress.
 *  $DESCRIPTION$
 *     FT_DISPMSG() is a multi-purpose pop-up for user messages.
 *     Multiple lines may be displayed, each with a different attribute.
 *     The box will be automatically centered on the screen, or the row
 *     and/or column can be specified by the programmer.  It also centers
 *     each line of the message within the box.
 *  $EXAMPLES$
 *     The following example displays a simple two-line message
 *     and returns immediately to the calling routine.
 *
 *        FT_DISPMSG( { { "Printing Report"                    , ;
 *                        "Press [ESC] To Interrupt" }         , ;
 *                      { "W+/B*", "W/B", "GR+/B" } } )
 *
 *     The next example displays a message and waits for a key press.
 *
 *        FT_DISPMSG( { { "Press [D] To Confirm Deletion"      , ;
 *                        "Or Any Other Key To Abort" }        , ;
 *                      { "W+/B", "W+/B", "GR+/B" } }          , ;
 *                      "D" )
 *
 *     The next example displays a one-line message centered on row 5
 *     and returns to the calling procedure.
 *
 *        FT_DISPMSG( { { "Please Do Not Interrupt" }   , ;
 *                      { "W+/B", "GR+/B" } }          , ;
 *                      , 5, )
 *  $END$
 */


#include "inkey.ch"

// beginning of demo program
#ifdef FT_TEST

// color variables
STATIC cNormH, cNormN, cNormE, ;
       cWindH, cWindN, cWindE, ;
       cErrH, cErrN, cErrE

PROCEDURE Main( cCmdLine )
   LOCAL cDosScrn,   ;
         nDosRow,    ;
         nDosCol,    ;
         lColor,     ;
         nMaxRow,    ;
         nType


   // main routine starts here
   SET SCOREBOARD OFF

   lColor := .T.

   cNormH := IIF( lColor, "W+/BG","W+/N" )
   cNormN := IIF( lColor, "N/BG" ,"W/N"  )
   cNormE := IIF( lColor, "N/W" , "N/W"  )
   cWindH := IIF( lColor, "W+/B", "W+/N" )
   cWindN := IIF( lColor, "W/B" , "W/N"  )
   cWindE := IIF( lColor, "N/W" , "N/W"  )
   cErrH  := IIF( lColor, "W+/R", "W+/N" )
   cErrN  := IIF( lColor, "W/R" , "W/N"  )
   cErrE  := IIF( lColor, "N/W" , "N/W"  )

   cDosScrn := SAVESCREEN()
   nDosRow=ROW()
   nDosCol=COL()
   SETCOLOR( "W/N" )
   CLS
   nMaxRow := MAXROW()
   SETBLINK(.F.)
   SETCOLOR( cWindN + "*" )
   CLS
   SETCOLOR( cNormN )

   FT_DispMsg( { { "[Esc] To Abort Changes   [PgDn] To Continue" }, { cNormN, , cNormH } }, , nMaxRow - 5 )

   FT_DispMsg( { { "[E]dit     [P]rint    [D]elete",     ;
                   "[Esc]ape       [Alt-Q]" },           ;
                 { cErrN, cErrN, cErrH } },, 2 )

      nType := FT_DispMsg( { { "Create Or Edit [I]nvoice",    ;
                               "Create Or Edit [O]rder",      ;
                               "Create Or Edit [B]ack Order", ;
                               "Create Or Edit [Q]uote",      ;
                               "[Esc] To Exit" },             ;
                             { cWindN,,,,, cWindH } }, "BIOQ" + CHR(27) )

   SETCOLOR( "W/N" )
   SETCURSOR( 1 )
   SETBLINK( .T.)
   RESTSCREEN(,,,, cDosScrn )
   SETPOS(nDosRow, nDosCol)
   QUIT

#endif
// end of demo program




FUNCTION FT_DispMsg( aInfo, cKey, nBoxTop, nBoxLeft, cnBoxString, lShadow )

   LOCAL xRtnVal := .F.,   ;
         nWidest := 0,     ;
         nBoxRight,        ;
         nBoxBottom,       ;
         cOldScreen,       ;
         cOldCursor,       ;
         cOldColor,        ;
         i,                ;
         j,                ;
         nOption,          ;
         x,                ;
         y,                ;
         aPos := {},       ;
         nLeft,            ;
         nTop,             ;
         aLeft,            ;
         cLeftMarker,      ;
         cRightMarker

   FOR i := 1 TO LEN( aInfo[1] )
      AADD( aPos, {} )
   NEXT

   FOR i := 1 TO LEN( aInfo[1] )

      DO WHILE AT( "[", aInfo[1,i] ) > 0
         x := AT( "[", aInfo[1,i] )
         y := AT( "]", aInfo[1,i] ) - 2
         AADD( aPos[i], { x, y } )
         aInfo[1,i] := STRTRAN( aInfo[1,i], "[", "", 1, 1 )
         aInfo[1,i] := STRTRAN( aInfo[1,i], "]", "", 1, 1 )
      ENDDO

   NEXT

   AEVAL( aInfo[1], {|x| nWidest := MAX( nWidest, LEN( x ) ) } )

   /* calculate location of data */
   IF nBoxLeft == NIL
      nLeft := ROUND( ( MAXCOL() - nWidest ) / 2, 0 )
   ELSE
      nLeft := nBoxLeft + 2
   ENDIF

   IF nBoxTop == NIL
      nTop := ( MAXROW() - LEN( aInfo[1] ) - 2 ) / 2 + 2
   ENDIF


   /* calculate location of box */
   IF nBoxLeft == NIL
      nBoxLeft := nLeft - 2
   ENDIF
   nBoxRight := nBoxLeft + nWidest + 3

   IF nBoxTop == NIL
      nBoxTop := (MAXROW() - LEN( aInfo[1] ) - 2) / 2 + 1
   ENDIF
   nBoxBottom := nBoxTop + LEN( aInfo[1] ) + 1

   // following is to keep from breaking old code and to be
   // consistent with DISPBOX()

   IF cnBoxString == NIL .OR. cnBoxString == 2
      cnBoxString := "ÉÍ»º¼ÍÈº "
   ELSEIF cnBoxString == 1
      cnBoxString := "ÚÄ¿³ÙÄÀ³ "
   ENDIF

   lShadow := IIF( lShadow == NIL, .T., lShadow )

   cOldScreen := SAVESCREEN( nBoxTop, nBoxLeft, nBoxBottom+1, nBoxRight+2 )

   cOldCursor := SETCURSOR( 0 )

   // draw box
   cOldColor := SETCOLOR( aInfo[ 2, LEN( aInfo[2] ) ] )

   DISPBOX( nBoxTop, nBoxLeft, nBoxBottom, nBoxRight, cnBoxString, ;
            aInfo[ 2, LEN( aInfo[2] ) ] )
   IF lShadow
      FT_Shadow( nBoxTop, nBoxLeft, nBoxBottom, nBoxRight )
   ENDIF


   /* fill array with left positions for each row */
   aLeft := ARRAY( LEN( aInfo[1] ) )
   FOR i := 1 TO LEN( aInfo[1] )
      IF LEN( aInfo[1,i] ) = nWidest
         aLeft[i] := nLeft
      ELSE
         aLeft[i] := nLeft + ROUND( ( nWidest - LEN( aInfo[1,i] ) ) / 2, 0 )
      ENDIF
   NEXT

   /* fill array of colors */
   FOR i := 2 TO LEN( aInfo[2] )
      IF aInfo[2,i] == NIL
         aInfo[2,i] := aInfo[2,i-1]
      ENDIF
   NEXT


   /* display messages */
   FOR i := 1 TO LEN( aInfo[1] )
      @ nBoxTop+i, aLeft[i] SAY aInfo[1,i] COLOR aInfo[2,i]
   NEXT


   /* highlight characters */
   FOR i := 1 TO LEN( aPos )
      FOR j := 1 TO LEN( aPos[i] )

         FT_SetAttr( nBoxTop + i,                              ;
                     aPos[i,j,1] + aLeft[i] - 1,               ;
                     nBoxTop + i,                              ;
                     aPos[i,j,2] + aLeft[i] - 1,               ;
                     FT_Color2N( aInfo[ 2, LEN( aInfo[2] ) ] ) )
      NEXT
   NEXT


   IF cKey != NIL
      IF LEN( cKey ) == 1
         nOption := FT_SInkey(0)
         IF UPPER( CHR( nOption) ) == cKey
            xRtnVal := .t.
         ENDIF
      ELSE
         nOption := 0
         DO WHILE AT( UPPER( CHR( nOption ) ), UPPER( cKey ) ) == 0
            nOption := FT_SInkey(0)
         ENDDO
         xRtnVal := nOption
      ENDIF
      RESTSCREEN( nBoxTop, nBoxLeft, nBoxBottom+1, nBoxRight+2, cOldScreen )
   ENDIF

   SETCOLOR( cOldColor )
   SETCURSOR( cOldCursor )
   RETURN xRtnVal

