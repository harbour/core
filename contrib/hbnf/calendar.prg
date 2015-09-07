/* This is an original work by Isa Asudeh and is placed in the public domain.

   Modification history

      Rev 1.1   15 Aug 1991 23:05:24   GLENN
   Forest Belt proofread/edited/cleaned up doc

      Rev 1.0   31 May 1991 21:07:26   GLENN
   Initial revision.
 */

#include "box.ch"
#include "inkey.ch"
#include "setcurs.ch"

FUNCTION ft_Calendar( nRow, nCol, cColor, lShadow, lShowHelp )

   LOCAL nJump := 0, nKey := 0, cSavColor, cSaveScreen, cSaveCursor
   LOCAL aRetVal[ 8 ]
   LOCAL nHelpRow, cSaveHelp, lHelpIsDisplayed := .F.

   __defaultNIL( @nRow, 1 )         // check display row
   __defaultNIL( @nCol, 63 )        // check display col
   __defaultNIL( @cColor, "W+/G" )  // check display color
   __defaultNIL( @lShadow, .F. )    // check shadow switch
   __defaultNIL( @lShowHelp, .F. )  // check help switch

   nRow := iif( nRow < 1 .OR. nRow > 21,  1, nRow )  // check row bounds
   nCol := iif( nCol < 1 .OR. nCol > 63, 63, nCol )  // check col bounds

   cSavColor   := SetColor( cColor )
   cSaveScreen := SaveScreen( nRow - 1, nCol - 1, nRow + 3, nCol + 17 )
   cSaveCursor := SetCursor( SC_NONE )

   IF lShadow
      hb_DispBox( nRow - 1, nCol - 1, nRow + 2, nCol + 15, HB_B_SINGLE_UNI )
      hb_Shadow( nRow - 1, nCol - 1, nRow + 2, nCol + 15 )
   ENDIF

   IF lShowHelp
      nHelpRow := iif( nRow > 10, nRow - 10, nRow + 6 )
   ENDIF

   DO WHILE nKey != K_ESC

      SWITCH nKey
      CASE K_HOME  ; nJump-- ; EXIT
      CASE K_END   ; nJump++ ; EXIT
      CASE K_UP    ; nJump -= 30 ; EXIT
      CASE K_DOWN  ; nJump += 30 ; EXIT
      CASE K_PGUP  ; nJump -= 365 ; EXIT
      CASE K_PGDN  ; nJump += 365 ; EXIT
      CASE K_RIGHT ; nJump -= 7 ; EXIT
      CASE K_LEFT  ; nJump += 7 ; EXIT
      CASE K_INS   ; nJump := 0 ; EXIT
      CASE K_F1
         IF lShowHelp .AND. ! lHelpIsDisplayed
            lHelpIsDisplayed := .T.
            cSaveHelp := SaveScreen( nHelpRow - 1, 1, nHelpRow + 7, MaxCol() + 1 )
            ft_XBox( "L",,, cColor, cColor, nHelpRow, 1, ;
               "Home, Up_Arrow or PgUp keys page by day, month or year to a past date.", ;
               "End, Dn_Arrow or PgDn keys page by day, month or year to a future date.", ;
               "Left_Arrow or Right_Arrow keys page by week to a past or future date.", ;
               "Hit Ins to reset to today's date, F1 to get this help, ESC to quit." )
         ENDIF
         EXIT
      ENDSWITCH

      aRetVal[ 1 ] := Date() + nJump
      aRetVal[ 2 ] :=  Month( aRetVal[ 1 ] )
      aRetVal[ 3 ] :=    Day( aRetVal[ 1 ] )
      aRetVal[ 4 ] :=   Year( aRetVal[ 1 ] )
      aRetVal[ 5 ] := CMonth( aRetVal[ 1 ] )
      aRetVal[ 6 ] :=   CDoW( aRetVal[ 1 ] )
      aRetVal[ 7 ] :=   JDoY( aRetVal[ 4 ], aRetVal[ 2 ], aRetVal[ 3 ] )

      hb_DispOutAt( nRow, nCol, ;
         Left( aRetval[ 6 ], 3 ) + " " + ;
         Str( aRetVal[ 3 ], 2 ) + " " + ;
         Left( aRetVal[ 5 ], 3 ) + " " + ;
         StrZero( aRetVal[ 4 ], 4 ) )

      hb_DispOutAt( nRow + 1, nCol, ;
         Str( aRetVal[ 7 ], 3 ) )

      nKey := 0
      DO WHILE nKey == 0
         hb_DispOutAt( nRow + 1, nCol + 3, "    " + Time() )
         nKey := hb_keyStd( Inkey( 1 ) )
      ENDDO

      aRetVal[ 8 ] := Time()
   ENDDO

   SetColor( cSavColor )
   SetCursor( cSaveCursor )
   RestScreen( nRow - 1, nCol - 1, nRow + 3, nCol + 17, cSaveScreen )
   IF lHelpIsDisplayed
      RestScreen( nHelpRow - 1, 1, nHelpRow + 7, MaxCol() + 1, cSaveHelp )
   ENDIF

   RETURN aRetVal

STATIC FUNCTION JDoY( nYear, nMonth, nDay )
   RETURN Val( hb_BSubStr( "000031059090120151181212243273304334", ( nMonth - 1 ) * 3 + 1, 3 ) ) + nDay + ;
      iif( nMonth > 2 .AND. ( ( nYear % 4 == 0 .AND. nYear % 100 != 0 ) .OR. nYear % 400 == 0 ), 1, 0 )
