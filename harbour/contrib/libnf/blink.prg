/*
 * File......: BLINK.PRG
 * Author....: Terry Hackett
 * CIS ID....: 76662,2035
 *
 * This is an original work by Terry Hackett and is placed in the
 * public domain.
 *
 * Modification history:
 * ---------------------
 *
 *    Rev 1.2   15 Aug 1991 23:02:56   GLENN
 * Forest Belt proofread/edited/cleaned up doc
 *
 *    Rev 1.1   14 Jun 1991 19:51:06   GLENN
 * Minor edit to file header
 *
 *    Rev 1.0   01 Apr 1991 01:00:46   GLENN
 * Nanforum Toolkit
 *
 */


/*  $DOC$
 *  $FUNCNAME$
 *     FT_BLINK()
 *  $CATEGORY$
 *     Menus/Prompts
 *  $ONELINER$
 *     Display a blinking message on the screen
 *  $SYNTAX$
 *     FT_BLINK( <cMsg>, [ <nRow> ], [ <nCol> ] ) -> NIL
 *  $ARGUMENTS$
 *     <cMsg> is the string to blink.
 *
 *     <nRow> is an optional screen row for @...SAY, default current.
 *
 *     <nCol> is an optional screen col for @...say, default current.
 *  $RETURNS$
 *     NIL
 *  $DESCRIPTION$
 *     A quick way to blink a msg on screen in the CURRENT colors.
 *     Restores colors on return.
 *  $EXAMPLES$
 *     FT_BLINK( "WAIT", 5, 10 )   // Blinks "WAIT" in current colors @ 5,10
 *
 *     @5,10 SAY "WAIT - Printing Report"
 *     FT_BLINK( "..." )           //  Blink "..." after wait message...
 *  $END$
 */

#ifdef FT_TEST
  FUNCTION MAIN()
     FT_BLINK( "WAIT", 5, 10 )
     return ( nil )
#endif

FUNCTION FT_BLINK( cMsg, nRow, nCol )

  * Declare color restore var.
  LOCAL cSavColor

  * Return if no msg.
  IF (cMsg == NIL) ; RETURN NIL; ENDIF

  * Set default row and col to current.
  nRow := IF( nRow == NIL, ROW(), nRow )
  nCol := IF( nCol == NIL, COL(), nCol )

  cSavColor := SETCOLOR()                // Save colors to restore on exit.

  * IF blink colors not already set, add blink to current foreground color.
  SETCOLOR( IF( ("*" $ LEFT(cSavColor,4)), cSavColor, "*" + cSavColor ) )

  @ nRow, nCol SAY cMsg                  // Say the dreaded blinking msg.
  SETCOLOR( cSavColor )                  // It's a wrap, restore colors & exit.

RETURN NIL

