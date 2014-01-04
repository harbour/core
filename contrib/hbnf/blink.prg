/*
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

/* NOTE: In its original version, this function changed cursor position,
         in Harbour it doesn't. */

PROCEDURE ft_Blink( cMsg, nRow, nCol )

   LOCAL cSavColor

   IF cMsg != NIL

      cSavColor := SetColor()

      // If blink colors not already set, add blink to current foreground color.
      hb_DispOutAt( ;
         iif( nRow == NIL, Row(), nRow ), ;
         iif( nCol == NIL, Col(), nCol ), ;
         cMsg, ;
         iif( "*" $ Left( cSavColor, 4 ), cSavColor, "*" + cSavColor ) )
   ENDIF

   RETURN
