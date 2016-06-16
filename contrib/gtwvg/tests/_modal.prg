/* Pritpal Bedi <bedipritpal@hotmail.com> */

#include "inkey.ch"
#include "hbgtinfo.ch"
#include "setcurs.ch"

FUNCTION Just_Alert( cMsg, aOpt )
   RETURN Alert( cMsg, aOpt )

FUNCTION My_Alert( cMessage, aOptions, cCaption, nInit, nTime )
   RETURN DialogAlert( cCaption, cMessage, aOptions, nInit,, nTime )

FUNCTION MyAlert( cMsg, aOpt )

   LOCAL nSel, oCrt

   oCrt := WvgCrt():New( , , { -1, -1 }, { 9, MaxCol() - 6 }, , .T. )
   oCrt:lModal := .T.
   oCrt:icon   := "dia_excl.ico"
   oCrt:create()
   oCrt:resizable := .T.

   SetColor( "N/W" )
   CLS
   hb_gtInfo( HB_GTI_WINTITLE, cMsg )

   nSel := Alert( cMsg, aOpt )

   oCrt:destroy()

   RETURN nSel

#define DLG_CLR_MOUSE              1
#define DLG_CLR_CAPT               2
#define DLG_CLR_TEXT               3
#define DLG_CLR_BTN                4
#define DLG_CLR_TRG                5
#define DLG_CLR_SHADOW             6
#define DLG_CLR_HILITE             7
#define DLG_CLR_HISEL              8

#define K_MOVING                   1001
#define K_LEFT_DOWN                1002
#define K_LEFT_DBLCLICK            1006
#define K_LEFT_UP                  1003
#define K_RIGHT_DOWN               1004
#define K_RIGHT_DBLCLICK           1007
#define K_RIGHT_UP                 1005

#xtranslate B_CRT <nTop>,<nLeft>,<nBottom>,<nRight> ;
      [ TITLE    <ttl>     ] ;
      [ ICON    <icon>     ] ;
      [ <lModal:MODAL>     ] ;
      [ <lRowCols:RESIZEROWCOLS> ] ;
      [ <lHidden:HIDDEN>   ] ;
      [ <lCenter:CENTER>   ] ;
      [ AT <nRow>,<nCol>   ] ;
      INTO <oCrt> ;
      => ;
      <oCrt > := CreateOCrt( <nTop>, <nLeft>, <nBottom>, <nRight>, <ttl>, <icon>, ;
      <.lModal.>, <.lRowCols.>, <.lHidden.>, <.lCenter.>, ;
      <nRow>, <nCol> )

STATIC FUNCTION DialogAlert( cCaption, aText_, aButtons_, sel, nTop, nTime )

   LOCAL nLinesRqd, nColRqd, nLeft, nBottom, nRight, oCrt
   LOCAL nColTxt, nColCap, nColBut, nBtnRow
   LOCAL i, nTopReq, lGo, nKey, nKeyStd, nMCol, nMRow, nTrg
   LOCAL maxCol  := MaxCol()
   LOCAL maxRow  := MaxRow()
   LOCAL nBtnCol_
   LOCAL pal_    := { "w+/n", "w/r", "n/w", "n/bg", "r/bg", "N/W", "n/B", "w+/B" }
   LOCAL aTrg_, x_ := {}

   hb_default( @cCaption, "Your Attention Please!" )
   hb_default( @sel     , 1 )
   hb_default( @nTime   , 10 )

   IF nTime == 0
      nTime := 10000   //  Seconds
   ENDIF

   IF HB_ISSTRING( aText_ )
      aText_ := { aText_ }
   ENDIF

   hb_default( @aText_, {} )

   IF HB_ISSTRING( aButtons_ )
      aButtons_ := { aButtons_ }
   ENDIF

   hb_default( @aButtons_, { "OK" } )

   nLinesRqd := Len( aText_ ) + iif( Len( aText_ ) == 0, 4, 5 )
   nTopReq   := Int( ( maxRow - nLinesRqd ) / 2 )
   nTop      := iif( HB_ISNUMERIC( nTop ), iif( nTop > nTopReq, nTop, nTopReq ), nTopReq )
   nBottom   := nTop + nLinesRqd - 1   // 1 for shadow

   // check for columns
   // place 2 spaces before and after the buttons
   nColCap   := Len( cCaption ) + 7  // " - " + "  " + cCaption + "  "
   nColTxt   := 0
   IF ! Empty( aText_ )
      AEval( aText_, {| e | nColTxt := Max( nColTxt, Len( e ) ) } )
   ENDIF
   nColTxt   += 6                   // for two spaces at both sides
   nColBut   := 0
   AEval( aButtons_, {| e | nColBut += Len( e ) + 7 } )
   nColBut   += 3

   nColRqd   := 0
   AEval( { nColCap, nColTxt, nColBut }, {| e | nColRqd := Max( nColRqd, e ) } )

   nLeft     := iif( maxCol > nColRqd, Int( ( maxCol - nColRqd ) / 2 ), 0 )
   nRight    := nLeft + nColRqd

   aTrg_ := Array( Len( aButtons_ ) )
   FOR i := 1 TO Len( aButtons_ )
      aTrg_[ i ] := Left( aButtons_[ i ], 1 )
   NEXT

   // Create a new Window
   B_CRT nTop, nLeft, nBottom - 1, nRight MODAL ICON "dia_excl.ico" TITLE "  " + cCaption INTO oCrt

   nTop    := -1
   nLeft   := 0
   nBottom := nTop + nLinesRqd - 1
   nRight  := nLeft + nColRqd
   nBtnRow := nTop + 1 + Len( aText_ ) + iif( Len( aText_ ) == 0, 1, 2 )

   nBtnCol_  := Array( Len( aButtons_ ) )

   nBtnCol_[ 1 ] := Int( ( nColRqd - nColBut ) / 2 ) + 3
   IF Len( aButtons_ ) > 1
      FOR i := 2 TO Len( aButtons_ )
         nBtnCol_[ i ] := nBtnCol_[ i - 1 ] + Len( aButtons_[ i - 1 ] ) + 3 + 4
      NEXT
   ENDIF

   SetCursor( SC_NONE )
   SetColor( "N/W" )
   CLS

   DispBegin()
   SetColor( pal_[ DLG_CLR_TEXT ] )

   wvg_BoxRaised( nTop, nLeft, nBottom, nRight )

   SetColor( pal_[ DLG_CLR_TEXT ] )
   IF ! Empty( aText_ )
      FOR i := 1 TO Len( aText_ )
         @ nTop + 1 + i, nLeft SAY PadC( aText_[ i ], nRight - nLeft + 1 )
      NEXT
   ENDIF

   // display buttons
   FOR i := 1 TO Len( aButtons_ )
      SetColor( pal_[ DLG_CLR_BTN ] )
      @ nBtnRow, nBtnCol_[ i ] SAY "  " + aButtons_[ i ] + "  "
      SetColor( pal_[ DLG_CLR_TRG ] )
      @ nBtnRow, nBtnCol_[ i ] + 2 SAY Left( aButtons_[ i ], 1 )

      AAdd( x_, { nBtnRow, nBtnCol_[ i ], nBtnRow, nBtnCol_[ i ] + Len( aButtons_[ i ] ) + 3 } )
   NEXT

   SetColor( pal_[ DLG_CLR_HILITE ] )
   @ nBtnRow, nBtnCol_[ sel ] SAY "  " + aButtons_[ sel ] + "  "

   SetColor( pal_[ DLG_CLR_HISEL ] )
   @ nBtnRow, nBtnCol_[ sel ] + 2 SAY Left( aButtons_[ sel ], 1 )

   AEval( x_, {| e_ | wvg_BoxRaised( e_[ 1 ], e_[ 2 ], e_[ 3 ], e_[ 4 ] ) } )

   DispEnd()

   lGo := .T.
   DO WHILE lGo

      nKeyStd := hb_keyStd( nKey := Inkey( 0, hb_bitOr( Set( _SET_EVENTMASK ), HB_INKEY_EXT ) ) )

      IF nKey == 0
         LOOP
      ENDIF

      IF nKeyStd == K_ESC
         sel := 0
         EXIT
      ENDIF
      nMRow := MRow()
      nMCol := MCol()

      DO CASE
      CASE nKeyStd == K_RIGHT_DOWN
         sel := 0
         lGo := .F.
      CASE nKeyStd == K_LEFT_DOWN
         IF nMRow == nTop
            IF nMCol >= nLeft .AND. nMCol <= nLeft + 3
               sel := 0
               lGo := .F.
            ENDIF
         ELSEIF nMRow == nBtnRow
            FOR i := 1 TO Len( nBtnCol_ )
               IF nMCol >= nBtnCol_[ i ] .AND. nMCol <= nBtnCol_[ i ] + Len( aButtons_[ i ] ) + 4
                  sel := i
                  lGo := .F.
               ENDIF
            NEXT
         ENDIF
      CASE nKeyStd == K_ESC
         sel := 0
         lGo := .F.
      CASE nKeyStd == K_ENTER
         lGo := .F.
      CASE nKeyStd == K_LEFT .OR. nKeyStd == K_DOWN
         --sel
      CASE nKeyStd == K_RIGHT .OR. nKeyStd == K_UP
         ++sel
      CASE ( nTrg := hb_AScanI( aTrg_, hb_keyChar( nKey ), , , .T. ) ) > 0
         sel := nTrg
         lGo := .F.
      CASE SetKey( nKey ) != NIL
         Eval( SetKey( nKey ) )
      CASE SetKey( nKeyStd ) != NIL
         Eval( SetKey( nKeyStd ) )
      ENDCASE

      IF sel > Len( aButtons_ )
         sel := 1
      ELSEIF sel < 1
         sel := Len( aButtons_ )
      ENDIF

      DispBegin()
      FOR i := 1 TO Len( aButtons_ )
         SetColor( pal_[ DLG_CLR_BTN ] )
         @ nBtnRow, nBtnCol_[ i ] SAY "  " + aButtons_[ i ] + "  "
         SetColor( pal_[ DLG_CLR_TRG ] )
         @ nBtnRow, nBtnCol_[ i ] + 2 SAY Left( aButtons_[ i ], 1 )
      NEXT
      IF sel > 0
         SetColor( pal_[ DLG_CLR_HILITE ] )
         @ nBtnRow, nBtnCol_[ sel ] SAY "  " + aButtons_[ sel ] + "  "
         SetColor( pal_[ DLG_CLR_HISEL ] )
         @ nBtnRow, nBtnCol_[ sel ] + 2 SAY Left( aButtons_[ sel ], 1 )
      ENDIF

      DispEnd()
   ENDDO

   oCrt:destroy()

   RETURN sel

STATIC FUNCTION CreateOCrt( nT, nL, nB, nR, cTitle, xIcon, lModal, lRowCols, lHidden, ;
      lCenter, nRow, nCol )

   LOCAL aPos := iif( hb_defaultValue( lCenter, .F. ), { -1, -1 }, iif( HB_ISNUMERIC( nRow ), { nRow, nCol }, { nT, nL } ) )

   LOCAL oCrt := WvgCrt():new( ,, aPos, { nB - nT, nR - nL },, ! hb_defaultValue( lHidden, .F. ) )

   oCrt:lModal := hb_defaultValue( lModal, .T. )
   IF lRowCols
      oCrt:resizeMode := HB_GTI_RESIZEMODE_ROWS
   ENDIF
   oCrt:create()
   SetCursor( SC_NONE )

   IF HB_ISNUMERIC( xIcon )
      hb_gtInfo( HB_GTI_ICONRES, xIcon )
   ELSE
      hb_default( @xIcon, "VW_DFT" )
      IF ".bmp" $ Lower( xIcon )
         xIcon := "VW_DFT"
      ENDIF
      hb_gtInfo( HB_GTI_ICONRES, xIcon )
   ENDIF

   hb_gtInfo( HB_GTI_WINTITLE, hb_defaultValue( cTitle, "Info" ) )

   SetColor( "N/W" )
   CLS

   RETURN oCrt

PROCEDURE DoModalWindow()

   LOCAL oCrt, nSel, pGT
   LOCAL aLastPaint := WvtSetBlocks( {} )

   /* This part can be clubbed in a separate prg for different dialogs
      OR can be loaded from a data dictionary. */

   oCrt := WvgCrt():New( , , { 4, 8 }, { 12, 49 }, , .T. )

   oCrt:lModal      := .T.
   oCrt:resizable   := .F.
   oCrt:closable    := .F.
   oCrt:title       := "Information! [R:4 C:8]"

   oCrt:rbUp        := {|| hb_DispOutAt( MaxRow(), 0, PadC( "rbUp", MaxCol() + 1 ), "W+/R*" ) }
   oCrt:lbUp        := {|| hb_DispOutAt( MaxRow(), 0, PadC( "lbUp", MaxCol() + 1 ), "W+/B*" ) }
   oCrt:leave       := {|| hb_DispOutAt( MaxRow(), 0, PadC( "Leaving", MaxCol() + 1 ), "W+/RB" ) }
   oCrt:enter       := {|| hb_DispOutAt( MaxRow(), 0, PadC( "Entering", MaxCol() + 1 ), "W+/B" ) }

   oCrt:Create()
   oCrt:show()

   pGT := SetGT( 3, hb_gtSelect() )

   // Here goes the Cl*pper Code
   SetColor( "N/W" )
   CLS
   DO WHILE .T.
      nSel := Just_Alert( "I am in modal window !;< Try: MMove LBUp RBUp >;Click Parent Window", { "OK" } )

      IF nSel == 0 .OR. nSel == 1
         EXIT
      ENDIF
   ENDDO

   SetGT( 3, pGT )
   oCrt:Destroy()

   WvtSetBlocks( aLastPaint )

   RETURN
