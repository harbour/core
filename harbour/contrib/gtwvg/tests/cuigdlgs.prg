/*
 * $Id$
 */

/*
 *    Pritpal Bedi <bedipritpal@hotmail.com>
 */

//

#include "inkey.ch"
#include "wvtwin.ch"
#include "hbgtinfo.ch"
#include "hbgtwvg.ch"
#include "wvgparts.ch"

//
//
//                    Simplified Console with GUI Look
//
//

PROCEDURE ExecGCUI()

   IF hb_mtvm()
      hb_threadStart( {| oCrt | oCrt := WvgCrt():New( , , { 2, 4 }, { 20, 81 }, , .T. ), ;
         oCrt:icon := GetResource( "dia_excl.ico" ), ;
         oCrt:create(), ;
         GCUIConsole( oCrt ), ;
         oCrt:destroy() } )
   ENDIF

   RETURN

//

#xtranslate Alert( => MyAlert(

PROCEDURE GCUIConsole( oCrt )

   LOCAL dDate      := Date()
   LOCAL cName      := PadR( 'Some Usefule Name', 35 )
   LOCAL cAdd1      := PadR( 'Linda Goldman Avenue', 35 )
   LOCAL cAdd2      := PadR( 'Excellent Street'    , 35 )
   LOCAL cAdd3      := PadR( 'Suit #415'           , 35 )
   LOCAL nSlry      := 9000
   LOCAL nColGet    := 8
   LOCAL GetList    := {}
   LOCAL hBoxR, hTxt

   SET SCOREBOARD OFF

   SetColor( "N/W,N/GR*,,,N/W*" )
   CLS
   hb_gtInfo( HB_GTI_WINTITLE, "WVG Simplified yet Powerful CUI-GUI Console!" )

   @ MaxRow(), 0 SAY PadC( "Navigate the Gets", MaxCol() + 1 ) COLOR "W+/B"

   @  2, nColGet SAY "< Date >"
   @  5, nColGet SAY "<" + PadC( "Name"   , 33 ) + ">"
   @  8, nColGet SAY "<" + PadC( "Address", 33 ) + ">"
   @ 15, nColGet SAY "< Salary >"

   @  3, nColGet GET dDate  ;
      WHEN  {|| Wvg_SetGObjData( hTxt, 1, FetchText( 1 ) ) } ;
      VALID {|| Wvg_SetGObjData( hTxt, 6, RGB( 255, 0, 0 ) ), .T. }
   @  6, nColGet GET cName  ;
      WHEN  {|| Wvg_SetGObjData( hTxt, 1, FetchText( 2 ) ) } ;
      VALID {|| Wvg_SetGObjData( hTxt, 6, RGB( 255, 255, 0 ) ), ;
      Wvg_SetGObjState( hBoxR, 3 ), .T. }
   @  9, nColGet GET cAdd1  ;
      WHEN  {|| Wvg_SetGObjData( hTxt, 1, FetchText( 3 ) ) } ;
      VALID {|| Wvg_SetGObjData( hTxt, 6, RGB( 255, 0, 255 ) ), .T. }
   @ 11, nColGet GET cAdd2  ;
      WHEN  {|| Wvg_SetGObjData( hTxt, 1, FetchText( 4 ) ) } ;
      VALID {|| Wvg_SetGObjData( hTxt, 6, RGB( 255, 255, 255 ) ), ;
      Wvg_SetGObjState( hBoxR, 1 ), .T. }
   @ 13, nColGet GET cAdd3  ;
      WHEN  {|| Wvg_SetGObjData( hTxt, 6, RGB( 198, 21, 140 ) ), .T. }
   @ 16, nColGet GET nSlry PICTURE "@Z 9999999.99" ;
      WHEN  {|| Wvg_SetGObjData( hTxt, 6, RGB( 0, 0, 0 ) ), .T. }

   // The only additional calls to render your console GUI
   //
   // The GETLIST  : This can be embedded via  @ GET preprocessor command
   AEval( GetList, {| oGet | Wvg_BoxGet( oGet:Row, oGet:Col, Len( Transform( oGet:VarGet(), oGet:Picture ) ) ) } )
   //
   hBoxR := Wvg_BoxRaised( 1, 2, 18, 49, { -5, -5, 5, 5 } )
   //
   Wvg_BoxRecessed( 1, 2, 18, 49 )
   //
   // Wvg_BoxGroup( 2, 4, 17, 47 )
   //
   Wvg_BoxGroupRaised( 2, 4, 17, 47, { -7, -7, 7, 7 } )
   //
   hTxt := Wvg_TextBox( 3, 57, 16, 75, { 10, 10, -10, -10 }, 'This is first TextBox Line!', 2, 2 )
   //
   Wvg_Image( 15, 36, 16, 42, { -3, -3, 3, 3 }, GOBJ_IMAGESOURCE_FILE, GetResource( 'Vouch1.bmp' ) )
   Wvg_BoxRaised( 15, 36, 16, 42, { -2, -2, 2, 2 } )
   //
   Wvg_ShadedRect( 1, 54, 18, 79, { -5, -5, 5, 5 }, 0, { 65000, 21000, 7000, 56000 }, { 255, 32255, 16000, 32500 } )
   //
   Wvg_BoxRaised( 1, 54, 18, 79, { -5, -5, 5, 5 } )

   // Instruct GT to Repaint the Screen with GUI elements.
   oCrt:refresh()

   // Issue the read
   READ

   // Alert( 'How did you like the "Alert" replacement?', { 'WOW','OK','OOps'} )
   My_Alert( 'How did you like the "Alert" replacement?', { 'WOW', 'OK', 'OOps' } )

   RETURN

//

STATIC FUNCTION FetchText( nMode )

   LOCAL cText

   DO CASE
   CASE nMode == 1
      cText := 'Do you know Harbour is gaining a popularity what Clipper enjoyed at one time! '
      cText += 'Enjoy it.'
   CASE nMode == 2
      cText := 'Do you know Harbour can host pure console, cui+gui console, pure gui consoles applications? '
      cText += 'This demonstration is a proof of that.'
   CASE nMode == 3
      cText := 'Do you know Harbour is a multi-gt, multi-window, multi-thread compiler far superior than others in the market! '
      cText += 'And is FREE.'
   CASE nMode == 4
      cText := 'Enjoy and contribute to the project any way you can. Develop, Debug, Support, and spread a word of mouth!'
   ENDCASE

   RETURN cText

//
