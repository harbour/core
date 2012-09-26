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

PROCEDURE Main()

   LOCAL aRet[ 8 ], i

   SetColor( "w+/b" )
   cls
   IF ft_numlock()
      ft_numlock( .F. )
   ENDIF
   KEYBOARD Chr( 28 )
   aRet := ft_calendar( 10, 40, "w+/rb", .T. , .T. ) //display calendar, return all.
   @ 1, 0 SAY "Date        :" + DToC( aRet[ 1 ] )
   @ 2, 0 SAY "Month Number:" + Str( aRet[ 2 ], 2, 0 )
   @ 3, 0 SAY "Day Number  :" + Str( aRet[ 3 ], 2, 0 )
   @ 4, 0 SAY "Year Number :" + Str( aRet[ 4 ], 4, 0 )
   @ 5, 0 SAY "Month       :" + aRet[ 5 ]
   @ 6, 0 SAY "Day         :" + aRet[ 6 ]
   @ 7, 0 SAY "Julian Day  :" + Str( aRet[ 7 ], 3, 0 )
   @ 8, 0 SAY "Current Time:" + aRet[ 8 ]

   RETURN

#endif

#include "inkey.ch"

FUNCTION FT_CALENDAR( nRow, nCol, cColor, lShadow, lShowHelp )

   LOCAL  nJump := 0, nKey := 0, cSavColor, cSaveScreen, cSaveCursor
   LOCAL  aRetVal[8]
   LOCAL  nHelpRow, cSaveHelp, lHelpIsDisplayed := .F.

   nRow    := iif( nRow != NIL, nRow, 1 )           //check display row
   nCol    := iif( nCol != NIL, nCol, 63 )           //check display col
   cColor  := iif( cColor != NIL, cColor, "W+/G" )  //check display color
   lShadow := iif( lShadow == NIL , .F. , lShadow )  //check shadow switch
   lShowHelp := iif( lShowHelp == NIL , .F. , lShowHelp )//check help switch

   nRow := iif( nRow < 1 .OR. nRow > 21,  1, nRow )   //check row bounds
   nCol := iif( nCol < 1 .OR. nCol > 63, 63, nCol )   //check col bounds

   cSavColor   := SetColor( cColor )  //save current and set display color
   cSaveScreen := SaveScreen( nRow - 1, nCol - 1, nRow + 3, nCol + 17 ) //save screen
   cSaveCursor := SetCursor( SC_NONE )     // save current and turn off cursor

   IF lShadow
      @nRow - 1, nCol - 1 TO nRow + 2, nCol + 15
      FT_SHADOW( nRow - 1, nCol - 1, nRow + 2, nCol + 15 )
   ENDIF

   IF lShowHelp
      nHelpRow := iif( nRow > 10 , nRow - 10 , nRow + 6 )
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
         IF lShowHelp .AND. ! lHelpIsDisplayed
            lHelpIsDisplayed := .T.
            cSaveHelp := SaveScreen ( nHelpRow - 1, 1, nHelpRow + 7, 80 )
            FT_XBOX( "L", , , cColor, cColor, nHelpRow, 1, ;
               "Home, Up_Arrow or PgUp keys page by day, month or year to a past date.", ;
               "End, Dn_Arrow or PgDn keys page by day, month or year to a future date.", ;
               "Left_Arrow or Right_Arrow keys page by week to a past or future date.", ;
               "Hit Ins to reset to today's date, F1 to get this help, ESC to quit." )
         ENDIF

      OTHERWISE
      ENDCASE

      aRetVal[ 1 ] :=         Date() + nJump
      aRetVal[ 2 ] :=  Month( Date() + nJump )
      aRetVal[ 3 ] :=    Day( Date() + nJump )
      aRetVal[ 4 ] :=   Year( Date() + nJump )
      aRetVal[ 5 ] := CMonth( Date() + nJump )
      aRetVal[ 6 ] :=   CDOW( Date() + nJump )
      aRetVal[ 7 ] :=   JDOY( aRetVal[ 4 ], aRetVal[ 2 ], aRetVal[ 3 ] )

      @ nRow, nCol SAY SubStr( aRetval[ 6 ], 1, 3 ) + " " + ;
         Str( aRetVal[ 3 ], 2, 0 ) + " " + ;
         SubStr( aRetVal[ 5 ], 1, 3 ) + " " + ;
         Str( aRetVal[ 4 ], 4, 0 )
      @ nRow + 1, nCol SAY Str( aRetVal[ 7 ], 3, 0 )

      nKey := 0
      DO WHILE nKey == 0
         @ nRow + 1, nCol + 3 SAY "    " + Time()
         nKey := Inkey( 1 )
      ENDDO
      aRetVal[ 8 ] := Time()
   ENDDO

   SetColor( cSavColor )                 //restore colors.
   SetCursor( cSaveCursor )              //restore cursor.
   RestScreen( nRow - 1, nCol - 1, nRow + 3, nCol + 17, cSaveScreen ) //restore screen.
   IF lHelpIsDisplayed
      RestScreen( nHelpRow - 1, 1, nHelpRow + 7, 80, cSaveHelp )
   ENDIF

   RETURN aRetVal

STATIC FUNCTION JDOY( nYear, nMonth, nDay )

   LOCAL cString := "000031059090120151181212243273304334"

   RETURN VALS( cString, ( nMonth - 1 ) * 3 + 1, 3 ) + nDay + ;
      iif( nYear % 4 == 0 .AND. nMonth > 2, 1, 0 )

STATIC FUNCTION VALS( cString, nOffset, nChar )
   RETURN Val( SubStr( cString, nOffset, nChar ) )
