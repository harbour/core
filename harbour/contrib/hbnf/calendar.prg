/*
 * $Id$
 */

/*
 * File......: calendar.prg
 * Author....: Isa Asudeh
 * CIS ID....: 76477,647
 *
 * This is an original work by Isa Asudeh and is placed in the
 * public domain.
 *
 * Modification history
 * --------------------
 *
 *    Rev 1.1   15 Aug 1991 23:05:24   GLENN
 * Forest Belt proofread/edited/cleaned up doc
 *
 *    Rev 1.0   31 May 1991 21:07:26   GLENN
 * Initial revision.
 *
 */

#include "setcurs.ch"

#ifdef FT_TEST
  FUNCTION MAIN()
   local aRet[8], i
   setcolor ('w+/b')
   cls
   if ft_numlock()
     ft_numlock( .f. )
   endif
   keyboard chr (28)
   aRet := ft_calendar (10,40,'w+/rb',.t.,.t.) //display calendar, return all.
   @1,0 say 'Date        :'+dtoc(aRet[1])
   @2,0 say 'Month Number:'+str(aRet[2],2,0)
   @3,0 say 'Day Number  :'+str(aRet[3],2,0)
   @4,0 say 'Year Number :'+str(aRet[4],4,0)
   @5,0 say 'Month       :'+aRet[5]
   @6,0 say 'Day         :'+aRet[6]
   @7,0 say 'Julian Day  :'+str(aRet[7],3,0)
   @8,0 say 'Current Time:'+aRet[8]
   RETURN NIL
#endif

#include "inkey.ch"

FUNCTION FT_CALENDAR (nRow, nCol, cColor, lShadow, lShowHelp)

 LOCAL  nJump :=0, nKey :=0, cSavColor, cSaveScreen, cSaveCursor
 LOCAL  aRetVal[8]
 LOCAL  nHelpRow, cSaveHelp, lHelpIsDisplayed :=.F.

 nRow    := IIF ( nRow != NIL, nRow, 1 )           //check display row
 nCol    := IIF ( nCol != NIL, nCol, 63)           //check display col
 cColor  := IIF ( cColor != NIL, cColor, 'W+/G' )  //check display color
 lShadow := IIF ( lShadow == NIL , .F., lShadow )  //check shadow switch
 lShowHelp := IIF ( lShowHelp == NIL , .F., lShowHelp )//check help switch

 nRow := IIF ( nRow <1 .OR. nRow >21,  1, nRow )   //check row bounds
 nCol := IIF ( nCol <1 .OR. nCol >63, 63, nCol )   //check col bounds

 cSavColor   := SETCOLOR(cColor)  //save current and set display color
 cSaveScreen := SAVESCREEN( nRow-1, nCol-1, nRow+3, nCol+17 ) //save screen
 cSaveCursor := SETCURSOR( SC_NONE )     // save current and turn off cursor

 IF lShadow
    @nRow-1,nCol-1 to nRow+2, nCol+15
    FT_SHADOW( nRow-1, nCol-1, nRow+2, nCol+15 )
 ENDIF

 IF lShowHelp
   nHelpRow := IIF (nRow > 10 , nRow - 10 , nRow + 6 )
 ENDIF

 DO WHILE nKey != K_ESC

    DO CASE
    CASE nKey == K_HOME
       nJump := nJump - 1

    CASE nKey == K_END
       nJump := nJump + 1

    CASE nKey == K_UP
       nJump := nJump - 30

    CASE nKey == K_DOWN
       nJump := nJump + 30

    CASE nKey == K_PGUP
       nJump := nJump - 365

    CASE nKey == K_PGDN
       nJump := nJump + 365

    CASE nKey == K_RIGHT
       nJump := nJump - 7

    CASE nKey == K_LEFT
       nJump := nJump + 7

    CASE nKey == K_INS
       nJump := 0

    CASE nKey == K_F1
       IF lShowHelp .AND. .NOT. lHelpIsDisplayed
          lHelpIsDisplayed := .T.
          cSaveHelp := SAVESCREEN ( nHelpRow-1, 1, nHelpRow+7, 80)
          FT_XBOX('L',,,cColor,cColor,nHelpRow,1,;
 "Home, Up_Arrow or PgUp keys page by day, month or year to a past date.",;
 "End, Dn_Arrow or PgDn keys page by day, month or year to a future date.",;
 "Left_Arrow or Right_Arrow keys page by week to a past or future date.",;
 "Hit Ins to reset to today's date, F1 to get this help, ESC to quit.")
       ENDIF

    OTHERWISE
    ENDCASE

 aRetVal[1] :=         DATE() + nJump
 aRetVal[2] :=  MONTH( DATE() + nJump )
 aRetVal[3] :=    DAY( DATE() + nJump )
 aRetVal[4] :=   YEAR( DATE() + nJump )
 aRetVal[5] := CMONTH( DATE() + nJump )
 aRetVal[6] :=   CDOW( DATE() + nJump )
 aRetVal[7] :=   JDOY( aRetVal[4], aRetVal[2], aRetVal[3] )

 @nRow, nCol SAY SUBSTR(aRetval[6],1,3)+' '+;
                    STR(aRetVal[3],2,0)+' '+;
                 SUBSTR(aRetVal[5],1,3)+' '+;
                    STR(aRetVal[4],4,0)
 @nRow+1,nCol SAY   STR(aRetVal[7],3,0)

 nKey := 0
 DO WHILE nKey == 0
    @nRow+1,nCol+3 SAY '    '+TIME()
    nKey := INKEY(1)
 ENDDO
 aRetVal[8] :=   TIME()
 ENDDO

 SETCOLOR ( cSavColor )                 //restore colors.
 SETCURSOR ( cSaveCursor )              //restore cursor.
 RESTSCREEN ( nRow-1, nCol-1, nRow+3, nCol+17, cSaveScreen ) //restore screen.
 IF lHelpIsDisplayed
    RESTSCREEN (nHelpRow-1, 1, nHelpRow+7, 80, cSaveHelp)
 ENDIF
 RETURN aRetVal

 STATIC FUNCTION JDOY (nYear, nMonth, nDay)
  LOCAL cString :='000031059090120151181212243273304334'
  RETURN VALS(cString,(nMonth-1)*3+1,3) + nDay +;
             IIF( nYear%4==0.AND.nMonth>2, 1, 0)

 STATIC FUNCTION VALS (cString, nOffset, nChar)
 RETURN VAL(SUBSTR(cString,nOffset,nChar))

* end of calendar.prg
