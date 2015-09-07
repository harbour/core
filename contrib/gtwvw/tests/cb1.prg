/* Copyright 2005 Budyanto Dj. <budyanto@centrin.net.id>
   A simple example on how to make use of GTWVW's combobox. */

#require "gtwvw"

#include "inkey.ch"
#include "setcurs.ch"

STATIC s_cQuestion := "Quick survey: What do you think about GTWVW?"
STATIC s_cHint     := "Hint: Please be positive..."

STATIC s_aAnswers  := { ;
   "1-GTWVW is a great library!", ;
   "2-GT who? I never heard about him", ;
   "3-Sorry, I don't like it" }

STATIC s_cYourAnswer := "Your response is:"
STATIC s_cThankYou := "Thanks for participating in our survey :-)"

#define _SECRET_KEY  101010

ANNOUNCE HB_NOSTARTUPWINDOW

PROCEDURE Main()

   LOCAL nMaxWidth, nCBid, nPBid, nPos
   LOCAL nKeyStd, ncursor

#if defined( __HBSCRIPT__HBSHELL ) .AND. defined( __PLATFORM__WINDOWS )
   hbshell_gtSelect( "GTWVW" )
#endif

   SetColor( "N/W" )
   wvw_SetTitle( , "Quick Survey" )
   wvw_NoClose()
   wvw_SetAltF4Close( .F. )
   wvw_SetLineSpacing( , 4 )
#if 0
   wvw_SetLSpaceColor( , 7 )
#endif
   wvw_cbSetFont( , "Arial", 16 )
   wvw_pbSetFont( , "Arial", 16 )

   nMaxWidth := 0
   AEval( s_aAnswers, {| x | nMaxWIdth := Max( nMaxWidth, Len( x ) ) } )

   SetMode( 11, nMaxWidth + 1 + 10 + 2 )

   CLS
   nCursor := SetCursor( SC_NONE )
   @ 1, 1 SAY s_cQuestion
   @ 2, 1 SAY s_cHint
   nCBid := wvw_cbCreate( , 4, 1, nMaxWidth, s_aAnswers, ;
      {| nWinNum, nId, nEvent, nIndex | ;
      CBhandler( nWinNum, nId, nEvent, nIndex, nPBid ) } )

   nPBid := wvw_pbCreate( , 4, 1 + nMaxWidth + 1, 4, 1 + nMaxWidth + 1 + 10 - 1, "OK", , ;
      {|| hb_keyPut( _SECRET_KEY ) }, { 0, 0, + 2, 0 } )

   wvw_cbSetFocus( , nCBid )

   wvw_ShowWindow()

   nPos := 1
   DO WHILE ( nKeyStd := hb_keyStd( Inkey( 0 ) ) ) != _SECRET_KEY

      DO CASE
      CASE nKeyStd == K_TAB .OR. nKeyStd == K_ENTER
         IF nPos == 2 .AND. nKeyStd == K_ENTER .AND. wvw_pbEnable( , nPBid )
            hb_keyPut( _SECRET_KEY )
            LOOP
         ELSE
            ++nPos
         ENDIF
      CASE nKeyStd == K_SH_TAB
         --nPos
      ENDCASE

      IF nPos > 2
         nPos := 1
      ELSEIF nPos < 1
         nPos := 2
      ENDIF

      DO CASE
      CASE nPos == 1
         wvw_cbSetFocus( , nCBid )
      CASE nPos == 2
         wvw_pbSetFocus( , nPBid )
         wvw_pbSetStyle( , nPBid, 1 ) // BS_DEFPUSHBUTTON
      ENDCASE
   ENDDO

   wvw_cbEnable( , nCBid, .F. )
   wvw_pbEnable( , nPBid, .F. )
   @ 6, 1 SAY "Your response is:"
   @ 7, 1 SAY wvw_cbGetCurText( , nCBid )
   @ 9, 1 SAY s_cThankYou
   Inkey( 0 )
   SetCursor( nCursor )

   RETURN

STATIC PROCEDURE CBhandler( nWinNum, nId, nEvent, nIndex, nPBid )

   SWITCH nEvent
   CASE 3  // CBN_SETFOCUS
      // none
      EXIT
   CASE 4  // CBN_KILLFOCUS
      wvw_pbEnable( nWinNum, nPBid, nIndex == 0 )
      EXIT
   CASE 1  // CBN_SELCHANGE
      IF ! wvw_cbIsDropped( nWinNum, nId )
         wvw_pbEnable( nWinNum, nPBid, nIndex == 0 )  // nIndex is 0-based
         wvw_cbSetFocus( nWinNum, nId )
      ENDIF
      EXIT
   ENDSWITCH

   RETURN
