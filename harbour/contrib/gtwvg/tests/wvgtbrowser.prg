/*
 * $Id$
 */

/*
 *    TBrowse Demonstration with GUI Elements
 *
 *    This protocol can be clubbed with pure console implementation
 *    AND can be called IN a separate thread as well as modal TO
 *    current window.
 *
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

#define K_SBLINEUP              1051
#define K_SBLINEDOWN            1052
#define K_SBPAGEUP              1053
#define K_SBPAGEDOWN            1054

#define K_SBLINELEFT            1055
#define K_SBLINERIGHT           1056
#define K_SBPAGELEFT            1057
#define K_SBPAGERIGHT           1058

#define K_SBTHUMBTRACKVERT      1059
#define K_SBTHUMBTRACKHORZ      1060

/*----------------------------------------------------------------------*/

FUNCTION WvtMyBrowse()

   IF hb_mtvm()
      Hb_ThreadStart( {|oCrt|  oCrt := WvgCrt():New( , , { -1,-2 }, { 34,69 }, , .T. ), ;
                            oCrt:resizeMode := HB_GTI_RESIZEMODE_ROWS,;
                            oCrt:icon := GetResource( "dia_excl.ico" ),;
                            oCrt:create(),;
                            Wvt_SetGui( .t. ),;
                            ExecBrowser( oCrt ),;
                            oCrt:destroy();
                  } )

   ELSE
      ExecBrowser()
   ENDIF

   Return NIL

//----------------------------------------------------------------------//

FUNCTION ExecBrowser( oCrt )
   LOCAL nKey, bBlock, oBrowse , aLastPaint, i, pGT
   LOCAL cFileIndex, cFileDbf, cRDD, nIndex, oTBar, cScr, info_
   LOCAL lEnd       := .f.
   LOCAL aBlocks    := {}
   LOCAL nTop       :=  4
   LOCAL nLeft      :=  3
   LOCAL nBottom    := maxrow() - 2
   LOCAL nRight     := maxcol() - 3
   LOCAL nCursor    := setCursor( 0 )
   LOCAL nRow       := row()
   LOCAL nCol       := col()
   LOCAL cColor     := SetColor( "N/W*,N/GR*,,,N/W*" )
   LOCAL aObjects   := WvtSetObjects( {} )
   LOCAL hPopup     := Wvt_SetPopupMenu()

   STATIC nStyle := 0
   THREAD STATIC nFactor := 200

   IF oCrt == NIL
      cScr    := SaveScreen( 0,0,maxrow(),maxcol() )
   ENDIF

   BrwBuildMenu( oCrt )
   oTBar := BuildWvgToolBar( oCrt )

   pGT := SetGT( 2, hb_gtSelect() )

   cRDD       := "DBFCDX"
   cFileDbf   := hb_DirBase() + ".." + hb_ps() + ".." + hb_ps() + ".." + hb_ps() + "tests" + hb_ps() + "test.dbf"
   cFileIndex := "test.z01"

   USE ( cFileDbf ) NEW SHARED VIA ( cRDD )
   if NetErr()
      return nil
   endif
   if fLock()
      INDEX ON Test->FIRST TAG "001" TO ( cFileIndex )
      INDEX ON Test->LAST  TAG "002" TO ( cFileIndex )
      INDEX ON Test->CITY  TAG "003" TO ( cFileIndex )
      dbUnlock()
   endif
   SET INDEX TO
   SET INDEX TO ( cFileIndex )
   SET ORDER TO 1
   DbGoTo( 50 )

   info_:= DbStruct()

   Popups( 2 )

   oBrowse := TBrowseWVG():New( nTop + 3, nLeft + 2, nBottom - 1, nRight - 2 )

   oBrowse:ColSep        := "  "
   oBrowse:HeadSep       := "__"
   oBrowse:GoTopBlock    := { || dbGoTop() }
   oBrowse:GoBottomBlock := { || dbGoBottom() }
   oBrowse:SkipBlock     := { | nSkip | dbSkipBlock( nSkip,oBrowse ) }

   for i := 1 to len( info_ )
      bBlock := VouBlockField( i )
      oBrowse:AddColumn( TBColumnNew( info_[ i,1 ], bBlock ) )
   next

   oBrowse:configure()

   if nStyle > 5
      nStyle := 0
   endif

   Wvt_SetPen( nStyle, 0, rgb( 210,1210,210 ) )

   nStyle++

   //hb_gtInfo( HB_GTI_ICONFILE, "dia_excl.ico" )
   hb_gtInfo( HB_GTI_WINTITLE, "WVT Gui TBrowse()" )

   aAdd( aBlocks, {|| Wvt_DrawBoxRaised( oBrowse:nTop-3, oBrowse:nLeft-2, oBrowse:nBottom+1, oBrowse:nRight+2 ) } )
   aAdd( aBlocks, {|| Wvt_DrawBoxRecessed( oBrowse:nTop, oBrowse:nLeft, oBrowse:nBottom, oBrowse:nRight ) } )
   aAdd( aBlocks, {|| Wvt_DrawGridHorz( oBrowse:nTop+3, oBrowse:nLeft, oBrowse:nRight, oBrowse:nBottom - oBrowse:nTop - 2 ) } )
   aAdd( aBlocks, {|| Wvt_DrawGridVert( oBrowse:nTop, oBrowse:nBottom, oBrowse:aColumnsSep, len( oBrowse:aColumnsSep ) ) } )

   aLastPaint := WvtSetBlocks( aBlocks )

   DispBox( 0, 0, maxrow(), maxcol(), "         ", "N/W" )
   DispOutAt( oBrowse:nTop-2, oBrowse:nleft, padc( cFileDbf, oBrowse:nRight-oBrowse:nLeft+1 ), "W+/W" )
   DispOutAt( maxrow(), 0, padc( '<F3 Modal Window> <F4 Maximize> <F11 Transp++> <F12 Transp--> <Thread'+str(Hb_ThreadID(),3)+'>',maxcol()+1), 'B/W' )

   oTBar:buttonClick := {|oBtn| IF( oBtn:caption=='Show',__keyboard( chr( K_DOWN ) ),nil ) }

   While !lEnd
      oBrowse:ForceStable()

      nKey := InKey( 0, INKEY_ALL  + HB_INKEY_GTEVENT )

      do case
      case nKey == K_F12
         nFactor--
         hb_gtInfo( HB_GTI_SPEC, HB_GTS_FACTOR, nFactor )

      case nKey == K_F11
         nFactor++
         hb_gtInfo( HB_GTI_SPEC, HB_GTS_FACTOR, nFactor )

      case nKey == K_F6
         hb_gtInfo( HB_GTI_RESIZABLE, .f. )

      case nKey == K_F7
         hb_gtInfo( HB_GTI_RESIZABLE, .t. )

      case BrwHandleKey( oBrowse, nKey, @lEnd )

      case nKey == HB_K_RESIZE
         oBrowse:nBottom := maxrow() - 3
         oBrowse:nRight  := maxcol() - 5

         DispBox( 0, 0, maxrow(), maxcol(), "         ", "N/W" )
         DispOutAt( oBrowse:nTop-2, oBrowse:nleft, padc( cFileDbf, oBrowse:nRight - oBrowse:nLeft + 1 ), "W+/W" )
         DispOutAt( maxrow(), 0, padc( '<F3 Modal Window> <F4 Maximize> <F11 Transp++> <F12 Transp--> <Thread'+str(Hb_ThreadID(),3)+'>',maxcol()+1), 'B/W' )
         oBrowse:configure()

      case nKey == K_F2
         nIndex := IndexOrd()
         nIndex++
         if nIndex > 3
            nIndex := 1
         endif
         Set Order To ( nIndex )
         oBrowse:RefreshAll()

      case nKey == K_F3
         DoModalWindow()

      case nKey == K_F4
         hb_gtInfo( HB_GTI_SPEC, HB_GTS_WNDSTATE, HB_GTS_WS_MAXIMIZED )

      endcase
   end

   Wvt_SetPen( 0 )
   WvtSetBlocks( aLastPaint )
   WvtSetObjects( aObjects )

   DevPos( nRow, nCol )
   SetColor( cColor )
   SetCursor( nCursor )

   DBCloseArea()
   IF oCrt == NIL
      RestScreen( 0, 0, maxrow(), maxcol(), cScr )
   ENDIF
   Wvt_setPopupMenu( hPopup )
   SetGT( 2, pGT )

   RETURN NIL

//-------------------------------------------------------------------//

STATIC FUNCTION DbSkipBlock( n )

   LOCAL nSkipped := 0

   if n == 0
      DBSkip( 0 )

   elseif n > 0
      do while nSkipped != n .and. TBNext()
         nSkipped++
      enddo
   else
      do while nSkipped != n .and. TBPrev()
         nSkipped--
      enddo
   endif

   RETURN  nSkipped

//-------------------------------------------------------------------//

STATIC FUNCTION TBNext()

   LOCAL nSaveRecNum := recno()
   LOCAL lMoved := .T.

   if Eof()
      lMoved := .F.
   else
      DBSkip( 1 )
      if Eof()
         lMoved := .F.
         DBGoTo( nSaveRecNum )
      endif
   endif

   RETURN lMoved

//-------------------------------------------------------------------//

STATIC FUNCTION TBPrev()
   LOCAL nSaveRecNum := Recno()
   LOCAL lMoved := .T.

   DBSkip( -1 )

   if Bof()
      DBGoTo( nSaveRecNum )
      lMoved := .F.
   endif

   RETURN lMoved

//-------------------------------------------------------------------//

STATIC FUNCTION VouBlockField( i )

   RETURN  {|| fieldget( i ) }

//-------------------------------------------------------------------//

STATIC FUNCTION BrwHandleKey( oBrowse, nKey, lEnd )
   LOCAL lRet := .t.

   do case
   case nKey == K_ESC
      lEnd := .t.

   case nKey == K_ENTER
      lEnd := .t.

   case nKey == K_DOWN
      oBrowse:Down()

   case nKey == K_UP
      oBrowse:Up()

   case nKey == K_LEFT
      oBrowse:Left()

   case nKey == K_RIGHT
      oBrowse:Right()

   case nKey == K_PGDN
      oBrowse:pageDown()

   case nKey == K_PGUP
      oBrowse:pageUp()

   case nKey == K_CTRL_PGUP
      oBrowse:goTop()

   case nKey == K_CTRL_PGDN
      oBrowse:goBottom()

   case nKey == K_HOME
      oBrowse:home()

   case nKey == K_END
      oBrowse:end()

   case nKey == K_CTRL_LEFT
      oBrowse:panLeft()

   case nKey == K_CTRL_RIGHT
      oBrowse:panRight()

   case nKey == K_CTRL_HOME
      oBrowse:panHome()

   case nKey == K_CTRL_END
      oBrowse:panEnd()

   case nKey == K_MWBACKWARD
      oBrowse:down()

   case nKey == K_MWFORWARD
      oBrowse:up()

   otherwise
      lRet := .f.

   endcase

   RETURN lRet

//-------------------------------------------------------------------//

STATIC FUNCTION BrwOnEvent( oWvtBrw, cPaintID, oBrowse, nKey )
   LOCAL lRet := .t., lRefAll := .f.

   HB_SYMBOL_UNUSED( cPaintID )

   do case
   case nKey == K_DOWN
      oBrowse:Down()

   case nKey == K_UP
      oBrowse:Up()

   case nKey == K_LEFT
      oBrowse:Left()

   case nKey == K_RIGHT
      oBrowse:Right()

   case nKey == K_PGDN
      oBrowse:pageDown()
      lRefAll := .t.

   case nKey == K_PGUP
      oBrowse:pageUp()
      lRefAll := .t.

   case nKey == K_CTRL_PGUP
      oBrowse:goTop()
      lRefAll := .t.

   case nKey == K_CTRL_PGDN
      oBrowse:goBottom()
      lRefAll := .t.

   case nKey == K_HOME
      oBrowse:home()

   case nKey == K_END
      oBrowse:end()

   case nKey == K_CTRL_LEFT
      oBrowse:panLeft()

   case nKey == K_CTRL_RIGHT
      oBrowse:panRight()

   case nKey == K_CTRL_HOME
      oBrowse:panHome()

   case nKey == K_CTRL_END
      oBrowse:panEnd()

   case nKey == K_MWBACKWARD
      oBrowse:down()

   case nKey == K_MWFORWARD
      oBrowse:up()

   case nKey == K_SBTHUMBTRACKVERT
      OrdKeyGoTo( oWvtBrw:oVBar:GetPos() )
      lRefAll := .t.

   case nKey == K_SBTHUMBTRACKHORZ
      oBrowse:ColPos := oWvtBrw:oHBar:GetPos()

   case nKey == K_SBLINEUP
      oBrowse:up()

   case nKey == K_SBLINEDOWN
      oBrowse:down()

   case nKey == K_SBPAGEUP
     oBrowse:PageUp()

   case nKey == K_SBPAGEDOWN
      oBrowse:PageDown()

   case nKey == K_SBLINELEFT
      oBrowse:Left()

   case nKey == K_SBLINERIGHT
      oBrowse:Right()

   case nKey == K_SBPAGELEFT
      oBrowse:Left()

   case nKey == K_SBPAGERIGHT
      oBrowse:right()

   otherwise
      lRet := .f.

   endcase

   if lRet
      if lRefAll
         oBrowse:RefreshAll()
      endif
      oBrowse:ForceStable()

      oWvtBrw:oVBar:SetPos( OrdKeyCount(),OrdKeyNo() )
      oWvtBrw:oHBar:SetPos( oBrowse:ColCount, oBrowse:ColPos )
   endif

   RETURN lRet

//-------------------------------------------------------------------//

FUNCTION ConfigBrowser( aFields, cUseAlias, aTLBR, cDesc, oParent, cColorSpec, nID )
   LOCAL info_, oWvtBrw, oBrowse, i, bBlock
   LOCAL aPopup := {}

   aadd( aPopup, { "Down"     , {|| oBrowse:Down()    , oBrowse:ForceStable() } } )
   aadd( aPopup, { "Up"       , {|| oBrowse:Up()      , oBrowse:ForceStable() } } )
   aadd( aPopup, { "Page Down", {|| oBrowse:PageDown(), oBrowse:ForceStable() } } )
   aadd( aPopup, { "Page Up"  , {|| oBrowse:PageUp()  , oBrowse:ForceStable() } } )
   aadd( aPopup, { "Top"      , {|| oBrowse:GoTop()   , oBrowse:ForceStable() } } )
   aadd( aPopup, { "Bottom"   , {|| oBrowse:GoBottom(), oBrowse:ForceStable() } } )

   Select( cUseAlias )
   info_:= DbStruct()

   //oBrowse := TBrowseNew( aTLBR[ 1 ], aTLBR[ 2 ], aTLBR[ 3 ], aTLBR[ 4 ] )
   oBrowse := TBrowseWVG():New( aTLBR[ 1 ], aTLBR[ 2 ], aTLBR[ 3 ], aTLBR[ 4 ] )

   oBrowse:ColSep        := "  "
   oBrowse:HeadSep       := "__"
   oBrowse:ColorSpec     := cColorSpec
   oBrowse:GoTopBlock    := { || dbGoTop() }
   oBrowse:GoBottomBlock := { || dbGoBottom() }
   oBrowse:SkipBlock     := { | nSkip | dbSkipBlock( nSkip,oBrowse ) }

   for i := 1 to len( aFields )
      bBlock := VouBlockField( aFields[ i ] )
      oBrowse:AddColumn( TBColumnNew( info_[ aFields[ i ],1 ], bBlock ) )
   next

   oBrowse:configure()

   oWvtBrw := WvtBrowse():New( oParent,nID )

   oWvtBrw:nTop         := aTLBR[ 1 ]
   oWvtBrw:nLeft        := aTLBR[ 2 ]
   oWvtBrw:nBottom      := aTLBR[ 3 ]
   oWvtBrw:nRight       := aTLBR[ 4 ]
   oWvtBrw:cAlias       := cUseAlias
   oWvtBrw:oBrw         := oBrowse
   oWvtBrw:cDesc        := cDesc
   oWvtBrw:nPointer     := WVT_IDC_HAND
   oWvtBrw:cColorHilite := "W+/B*"
   oWvtBrw:Tooltip      := cDesc
   oWvtBrw:aPopup       := aPopup

   oWvtBrw:bHandleEvent := {|oWvtBrw,cPaintID,oBrowse,nKey| BrwOnEvent( oWvtBrw,cPaintID,oBrowse,nKey ) }

   RETURN oWvtBrw

//-------------------------------------------------------------------//

STATIC FUNCTION BrwBuildMenu( oCrt )
   Local oMenu, oSMenu

   oMenu := WvgMenuBar():new( oCrt, , .t. ):create()

   oSMenu := WvgMenu():new( oMenu ):create()
   oSMenu:addItem( { '~First' , {|| alert( 'First'  ) } } )
   oSMenu:addItem( { '~Second', {|| alert( 'Second' ) } } )
   oSMenu:addItem()
   oSMenu:addItem( { '~Third' , {|| alert( 'Third'  ) } } )
   oMenu:addItem( { oSMenu, '~Hello' } )

   oSMenu := WvgMenu():new( oMenu ):create()
   oSMenu:addItem( { '~First' , {|| alert( 'First'  ) } } )
   oSMenu:addItem( '-' )
   oSMenu:addItem( { '~Second', {|| alert( 'Second' ) } } )
   oSMenu:addItem( { '~Third' , {|| alert( 'Third'  ) } } )
   oMenu:addItem( { oSMenu, '~MyFriends' } )

   oSMenu := WvgMenu():new( oMenu ):create()
   oSMenu:title := "~Procedural"
   oSMenu:addItem( { "Procedure ~1", } )
   oSMenu:addItem( { "Procedure ~2", } )
   oSMenu:itemSelected := {|mp1| MyMenuProcedure( 100+mp1 ) }
   oSMenu:checkItem( 2 )

   oMenu:addItem( { oSMenu, NIL } )

   Return oMenu

//----------------------------------------------------------------------//
