/*
 * Author....: Isa Asudeh
 * CIS ID....: 76477,647
 *
 * This is an original work by Isa Asudeh and is placed in the
 * public domain.
 *
 * Modification History
 * --------------------
 *
 *    Rev 1.1   15 Aug 1991 23:05:20   GLENN
 * Forest Belt proofread/edited/cleaned up doc
 *
 *    Rev 1.0   31 May 1991 21:18:04   GLENN
 * Initial revision.
 *
 */

// cMsg    Message to display
// nRow    Row of displayed message
// nCol    Col of displayed message
// nWait   Wait in seconds between messages
// cColor  Color of displayed message

FUNCTION ft_Pending( cMsg, nRow, nCol, nWait, cColor )

   THREAD STATIC t_nRow1 := 24
   THREAD STATIC t_nCol1 := 0
   THREAD STATIC t_nWait1 := 5
   THREAD STATIC t_cColor1 := "W+/R,X"

   THREAD STATIC t_nLast_Time := 0

   LOCAL nThis_Time

   IF cMsg != NIL

      t_nRow1   := iif( nRow != NIL, nRow, t_nRow1 )        // reset display row
      t_nCol1   := iif( nCol != NIL, nCol, t_nCol1 )        // reset display col
      t_nWait1  := iif( nWait != NIL, nWait, t_nWait1 )     // reset display wait
      t_cColor1 := iif( cColor != NIL, cColor, t_cColor1 )  // reset display color

      nThis_Time := Seconds()                       // time of current message

      IF t_nLast_Time == 0
         t_nLast_Time := nThis_Time - t_nWait1      // for first time round.
      ENDIF

      IF ( nThis_Time - t_nLast_Time ) < 0.1        // if messages are coming too fast,
         t_nLast_Time := nThis_Time + t_nWait1      // set time counter and then
         Inkey( t_nWait1 )                          // wait a few seconds.
      ELSE
         t_nLast_Time := nThis_Time                 // set time counter for next message.
      ENDIF

      hb_Scroll( t_nRow1, 0, t_nRow1, 80 )          // clear the display line

      hb_DispOutAt( t_nRow1, t_nCol1, cMsg, t_cColor1 ) // display message

   ENDIF

   RETURN NIL
