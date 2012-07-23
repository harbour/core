/*
 * $Id$
 */
/*
 *    Pritpal Bedi <bedipritpal@hotmail.com>
 */
/*----------------------------------------------------------------------*/

#include "inkey.ch"
#include "common.ch"
#include "wvtwin.ch"
#include "hbgtinfo.ch"
#include "hbgtwvg.ch"
#include "wvgparts.ch"

/*----------------------------------------------------------------------*/

#xUntranslate alert( =>

FUNCTION MyAlert( cMsg, aOpt )
   LOCAL nSel, oCrt

   oCrt := WvgCrt():New( , , { -1,-1 }, { 9, MaxCol()-6 }, , .t. )
   oCrt:lModal := .t.
   oCrt:icon   := "dia_excl.ico"
   oCrt:create()
   oCrt:resizable := .t.

   SetColor( 'N/W' )
   CLS
   hb_gtInfo( HB_GTI_WINTITLE, cMsg )

   nSel := Alert( cMsg, aOpt )

   oCrt:destroy()

   RETURN nSel

#xTranslate Alert( => MyAlert(

/*----------------------------------------------------------------------*/

FUNCTION My_Alert( cMessage, aOptions, cCaption, nInit, nTime )
   RETURN DialogAlert( cCaption, cMessage, aOptions, nInit, , ,nTime )

/*----------------------------------------------------------------------*/

#xUntranslate alert( =>
FUNCTION Just_Alert( cMsg, aOpt )
   RETURN Alert( cMsg, aOpt )
#xTranslate Alert( => MyAlert(

/*----------------------------------------------------------------------*/

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
            [ <lNoTitleBar:NOTITLEBAR> ] ;
            INTO <oCrt> ;
            => ;
   <oCrt> := CreateOCrt( <nTop>, <nLeft>, <nBottom>, <nRight>, <ttl>, <icon>, ;
                         <.lModal.>, <.lRowCols.>, <.lHidden.>, <.lCenter.>, ;
                         <nRow>, <nCol>, <.lNoTitleBar.> )

/*----------------------------------------------------------------------*/

FUNCTION DialogAlert( cCaption, aText_, aButtons_, sel, aMessage_, nTop, nTime )
   LOCAL nLinesRqd, nColRqd, nLeft, nBottom, nRight, oCrt
   LOCAL nColTxt, nColCap, nColBut, nBtnRow
   LOCAL i, nTopReq, lGo, nKey, nMCol, nMRow, nTrg
   LOCAL maxCol  := maxcol()
   LOCAL maxRow  := maxrow()
   LOCAL nBtnCol_
   LOCAL pal_    := {"w+/n","w/r","n/w","n/bg","r/bg","N/W","n/B","w+/B"}
   LOCAL aTrg_   , x_:={}

   DEFAULT cCaption  TO "Your Attention Please!"
   DEFAULT aButtons_ TO {"OK"}
   DEFAULT aText_    TO {}
   DEFAULT aMessage_ TO {}
   DEFAULT sel       TO 1
   DEFAULT nTime     TO 10

   if nTime == 0
      nTime := 10000   //  Seconds
   endif

   if valtype( aText_ ) == "C"
      aText_:= {aText_}
   endif

   if valtype(aButtons_) == "C"
      aButtons_:= {aButtons_}
   endif

   nLinesRqd := len( aText_ )+ iif( len( aText_ )== 0, 4, 5 )
   nTopReq   := int( ( maxRow - nLinesRqd ) / 2 )
   nTop      := iif( nTop == nil, nTopReq, iif( nTop >  nTopReq, nTop, nTopReq ) )
   nBottom   := nTop + nLinesRqd - 1   // 1 for shadow

   // check for columns
   // place 2 spaces before and after the buttons
   nColCap   := len( cCaption )+ 7  // " - "+"  "+caption+"  "
   nColTxt   := 0
   if !empty(aText_)
      aeval(aText_, {|e| nColTxt := max( nColTxt, len( e ) ) } )
   endif
   nColTxt   += 6                   // for two spaces at both sides
   nColBut   := 0
   aeval( aButtons_, {|e| nColBut += len( e ) + 7 } )
   nColBut   += 3

   nColRqd   := 0
   aeval( { nColCap, nColTxt, nColBut }, {|e| nColRqd := max( nColRqd, e ) } )

   nLeft     := iif( maxCol > nColRqd, int( ( maxCol - nColRqd ) / 2 ), 0 )
   nRight    := nLeft+nColRqd

   aTrg_:= array( len( aButtons_ ) )
   for i := 1 to len( aButtons_ )
      aTrg_[i] := upper( substr( aButtons_[ i ], 1, 1 ) )
   next

   //                        Create a new Window
   //
   B_CRT nTop, nLeft, nBottom-1, nRight MODAL ICON "dia_excl.ico" TITLE '  '+cCaption INTO oCrt

   nTop    := -1
   nLeft   := 0
   nBottom := nTop + nLinesRqd - 1
   nRight  := nLeft + nColRqd
   nBtnRow := nTop + 1 + len( aText_ ) + iif( len( aText_ ) == 0, 1, 2 )

   nBtnCol_  := array( len( aButtons_ ) )

   nBtnCol_[ 1 ] := int( ( nColRqd - nColBut ) / 2 ) + 3
   if len( aButtons_ ) > 1
      for i := 2 to len( aButtons_ )
         nBtnCol_[ i ] := nBtnCol_[ i-1 ] + len( aButtons_[ i-1 ] ) + 3 + 4
      next
   ENDIF

   setcursor( 0 )
   SetColor( 'N/W' )
   CLS

   DispBegin()
   SetColor( pal_[ DLG_CLR_TEXT ] )

   Wvg_BoxRaised( nTop, nLeft, nBottom, nRight )

   SetColor( pal_[ DLG_CLR_TEXT ] )
   if !empty( aText_ )
      FOR  i := 1 to len( aText_ )
         @ nTop+1+i, nLeft SAY padc( aText_[ i ], nRight-nLeft+1 )
      NEXT
   ENDIF

   // display buttons
   //
   for i := 1 to len( aButtons_ )
      SetColor( pal_[ DLG_CLR_BTN ] )
      @ nBtnRow, nBtnCol_[ i ] SAY "  "+aButtons_[ i ]+"  "
      SetColor( pal_[ DLG_CLR_TRG ] )
      @ nBtnRow, nBtnCol_[ i ]+2 say substr( aButtons_[ i ],1,1 )

      aadd( x_, { nBtnRow, nBtnCol_[ i ], nBtnRow, nBtnCol_[ i ] + len( aButtons_[ i ] ) + 3 } )
   next

   setColor( pal_[ DLG_CLR_HILITE ] )
   @ nBtnRow, nBtnCol_[ sel ] SAY "  "+aButtons_[sel]+"  "

   setColor( pal_[ DLG_CLR_HISEL ] )
   @ nBtnRow, nBtnCol_[ sel ]+2 SAY substr( aButtons_[ sel ],1,1 )

   aeval( x_, {|e_| Wvg_BoxRaised( e_[ 1 ], e_[ 2 ], e_[ 3 ], e_[ 4 ] ) } )

   dispend()

   lGo := .t.
   do while lGo
      IF ( nKey := Inkey() ) == 0
         LOOP
      ENDIF

      if nKey == K_ESC
         sel := 0
         exit
      endif
      nMRow := MRow()
      nMCol := MCol()

      do case
      case nKey == K_RIGHT_DOWN
         sel := 0
         lGo := .f.
      case nKey == K_LEFT_DOWN
         if nMRow == nTop
            if nMCol >= nLeft .and. nMCol <= nLeft+3
               sel := 0
               lGo := .f.
            endif
         elseif nMRow == nBtnRow
            for i := 1 to len( nBtnCol_ )
               if nMCol >= nBtnCol_[ i ] .and. nMCol <= nBtnCol_[ i ] + len( aButtons_[ i ] )+4
                  sel := i
                  lGo := .f.
               endif
            next
         endif
      case nKey == K_ESC
         sel := 0
         lGo := .f.
      case nKey == K_ENTER
         lGo := .f.
      case nKey == K_LEFT  .or. nKey == K_DOWN
         sel--
      case nKey == K_RIGHT .or. nKey == K_UP
         sel++
      case ( nTrg := ascan( aTrg_, upper( chr( nKey ) ) ) ) > 0
         sel := nTrg
         lGo := .f.
      otherwise
         if setkey( nKey ) != nil
            eval( setKey( nKey ) )
         endif
      endcase

      if sel > len( aButtons_ )
         sel := 1
      elseif sel < 1
         sel := len( aButtons_ )
      endif

      dispbegin()
      for i := 1 to len ( aButtons_ )
         setColor( pal_[ DLG_CLR_BTN ] )
         @ nBtnRow, nBtnCol_[ i ] SAY "  "+aButtons_[i]+"  "
         setColor( pal_[ DLG_CLR_TRG])
         @ nBtnRow, nBtnCol_[i]+2 say substr(aButtons_[i],1,1)
      next
      if sel > 0
         setColor( pal_[ DLG_CLR_HILITE ] )
         @ nBtnRow, nBtnCol_[sel] SAY "  "+aButtons_[ sel ]+"  "
         setColor( pal_[ DLG_CLR_HISEL ] )
         @ nBtnRow, nBtnCol_[ sel ]+2 SAY substr( aButtons_[ sel ], 1, 1 )
      endif

      dispend()
   enddo

   oCrt:destroy()

   return sel

//----------------------------------------------------------------------//

FUNCTION CreateOCrt( nT, nL, nB, nR, cTitle, xIcon, lModal, lRowCols, lHidden, ;
                                                  lCenter, nRow, nCol, lNoTitleBar )
   LOCAL oCrt, aPos

   DEFAULT cTitle        TO 'Info'
   DEFAULT xIcon         TO 'VW_DFT'
   DEFAULT lModal        TO .T.
   DEFAULT lHidden       TO .F.
   DEFAULT lCenter       TO .F.
   DEFAULT lNoTitleBar   TO .F.

   aPos := iif( lCenter, {-1,-1}, iif( nRow == NIL, { nT, nL }, { nRow,nCol } ) )

   oCrt := WvgCrt():new( , , aPos, { nB - nT, nR - nL }, , !lHidden )
   oCrt:lModal := lModal
   IF lRowCols
      oCrt:resizeMode := HB_GTI_RESIZEMODE_ROWS
   ENDIF
   oCrt:create()
   SetCursor( 0 )

   IF HB_ISNUMERIC( xIcon )
      hb_gtInfo( HB_GTI_ICONRES, xIcon )
   ELSE
      IF ( '.ico' $ lower( xIcon ) )
         hb_gtInfo( HB_GTI_ICONFILE, xIcon )
      ELSE
         IF '.bmp' $ lower( xIcon )
            xIcon := 'VW_DFT'
         ENDIF
         hb_gtInfo( HB_GTI_ICONRES, xIcon )
      ENDIF
   ENDIF

   hb_gtInfo( HB_GTI_WINTITLE, cTitle )

   SetColor( 'N/W' )
   CLS

   RETURN oCrt

/*----------------------------------------------------------------------*/

FUNCTION DoModalWindow()
   LOCAL oCrt, nSel, pGT
   LOCAL aLastPaint := WvtSetBlocks( {} )

   /* This part can be clubbed in a separate prg for different dialogs
    * OR can be loaded from a data dictionary.
    */
   oCrt := WvgCrt():New( , , { 4,8 }, { 12,49 }, , .T. )

   oCrt:lModal      := .t.
   oCrt:resizable   := .f.
   oCrt:closable    := .f.
   oCrt:title       := 'Information! [R:4 C:8]'

   oCrt:rbUp        := {|| DispOutAt( maxrow(), 0, padc( 'rbUp', maxcol()+1 ),'W+/R*' ) }
   oCrt:lbUp        := {|| DispOutAt( maxrow(), 0, padc( 'lbUp', maxcol()+1 ),'W+/B*' ) }
   oCrt:leave       := {|| DispOutAt( maxrow(), 0, padc( 'Leaving', maxcol()+1 ), 'W+/RB' ) }
   oCrt:enter       := {|| DispOutAt( maxrow(), 0, padc( 'Entering', maxcol()+1 ), 'W+/B' ) }

   oCrt:Create()
   oCrt:show()

   pGT := SetGT( 3, hb_gtSelect() )

   // Here goes the Clipper Code
   //
   SetColor( 'N/W' )
   CLS
   do while .t.
      nSel := Just_Alert( 'I am in modal window !;< Try: MMove LBUp RBUp >;Click Parent Window', { 'OK' } )

      if nSel == 0  .or. nSel == 1
         exit

      endif
   enddo

   SetGT( 3, pGT )
   oCrt:Destroy()

   WvtSetBlocks( aLastPaint )
   Return NIL

//----------------------------------------------------------------------//
