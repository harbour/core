/*
 * File......: PENDING.PRG
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


/*  $DOC$
 *  $FUNCNAME$
 *     FT_PENDING()
 *  $CATEGORY$
 *     Menus/Prompts
 *  $ONELINER$
 *     Display same-line pending messages after a wait.
 *  $SYNTAX$
 *     FT_PENDING ( <cMsg>, [ <nRow> ], [ <nCol> ], ;
 *                       [ <nWait> ], [ <cColor> ] ) -> NIL
 *  $ARGUMENTS$
 *     <cMsg> is the message string to display.
 *
 *     <nRow> is an optional screen row for message display, default row 24.
 *
 *     <nCol> is an optional screen col for message display, default col 0.
 *
 *     <nWait> is an optional wait (sec) between messages, default 5 sec.
 *
 *     <cColor> is an optional color string for displayed messages, default
 *              is white text over red background.
 *  $RETURNS$
 *     NIL
 *  $DESCRIPTION$
 *     A good way to display information messages during the running
 *     of an application is to send them all to the SAME line on the
 *     screen where users are expected to look for them. In order to
 *     give users a chance to read the current message before the next one
 *     is displayed we may need to insert a delay after each message.
 *
 *     FT_PENDING() function displays messages by keeping track of
 *     the time of the last message and providing a delay ONLY if the next
 *     pending message is issued much too soon after the current one.
 *
 *  $EXAMPLES$
 *     FT_PENDING("Message one",20,0,3,"W+/G") // Displays "Message one."
 *                                             // sets row to 20, col to 0.
 *                                             // wait to 3 and color to
 *                                             // bright white over green.
 *     FT_PENDING("Message two")   // Displays "Message two", after 5 sec.
 *     FT_PENDING("Message three") // Displays "Message three", after 5 sec.
 *
 *
 *     Note that default row, col, wait time and color need to be set only
 *     once in the very first call to FT_PENDING() and only if the internal
 *     default values are not appropriate.
 *
 *  $END$
 */

#ifdef FT_TEST
  FUNCTION MAIN()
     @0,0 CLEAR
     FT_PENDING("Message one",20,0,3,"W+/G") // Displays "Message one."
                                             // sets row to 20, col to 0.
                                             // wait to 3 and color to
                                             // bright white over green.
     FT_PENDING("Message two")   // Displays "Message two", after 5 sec.
     FT_PENDING("Message three") // Displays "Message three", after 5 sec.
     return ( nil )
#endif

FUNCTION FT_PENDING (cMsg, nRow, nCol, nWait, cColor)
 STATIC nLast_Time := 0, nRow1 := 24, nCol1 := 0
 STATIC nWait1 := 5, cColor1 := 'W+/R,X'
 LOCAL  nThis_Time, nTiny := 0.1, cSavColor

*
* cMsg        Message to display
* nRow        Row of displayed message
* nCol        Col of displayed message
* nWait       Wait in seconds between messages
* cColor      Color of displayed message
*

 IF (cMsg == NIL )                       //if no message, no work
    RETURN NIL
 ENDIF

 nRow1 := IIF( nRow <> NIL, nRow, nRow1 )  //reset display row
 nCol1 := IIF( nCol <> NIL, nCol, nCol1 )  //reset display col

 nWait1 := IIF ( nWait <> NIL, nWait, nWait1)     //reset display wait
 cColor1 := IIF (cColor <> NIL, cColor, cColor1)  //reset display color

 nThis_Time := SECONDS()                //time of current message

 IF nLast_Time == 0
    nLast_Time := nThis_Time - nWait1   //for first time round.
 ENDIF

 IF (nThis_Time - nLast_Time) < nTiny   //if messages are coming too fast,
    nLast_Time := nThis_Time + nWait1   //set time counter and then
    INKEY (nWait1)                      //wait a few seconds.
 ELSE
    nLast_Time := nThis_Time            //set time counter for next message.
 ENDIF

 @nRow1,0 clear to nRow1,80             //clear the display line

 cSavColor := SETCOLOR(cColor1)         //save current and set display color

 @nRow1,nCol1 SAY cMsg                  //display message

 SETCOLOR( cSavColor )                  //restore colors.

 RETURN NIL
