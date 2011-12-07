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

#define K_MOVING                1001

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
      Hb_ThreadStart( {|oCrt|  oCrt := WvgCrt():new( , , { -1,-2 }, { 34,69 }, , .T. ), ;
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

   RETURN NIL

//----------------------------------------------------------------------//

FUNCTION ExecBrowser( oCrt )
   LOCAL nKey, bBlock, oBrowse , aLastPaint, i, pGT, oVBar, oHBar
   LOCAL cFileIndex, cFileDbf, cRDD, nIndex, oTBar, cScr, info_ //, oLB
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
      cScr := SaveScreen( 0,0,maxrow(),maxcol() )
   ENDIF

   BrwBuildMenu( oCrt )
   oTBar := BrwBuildToolBar( oCrt )
   SetMode( maxrow()+1, maxCol()+1 )  /* Neccessary because adding menu has reduced the overall size of window */

   pGT := SetGT( 2, hb_gtSelect() )

   cRDD       := "DBFCDX"
   cFileDbf   := hb_DirBase() + ".." + hb_ps() + ".." + hb_ps() + ".." + hb_ps() + "tests" + hb_ps() + "test.dbf"
   cFileIndex := "test.z01"

   USE ( cFileDbf ) NEW SHARED VIA ( cRDD )
   IF NetErr()
      RETURN NIL
   ENDIF
   IF fLock()
      INDEX ON Test->FIRST TAG "001" TO ( cFileIndex )
      INDEX ON Test->LAST  TAG "002" TO ( cFileIndex )
      INDEX ON Test->CITY  TAG "003" TO ( cFileIndex )
      dbUnlock()
   ENDIF
   SET INDEX TO
   SET INDEX TO ( cFileIndex )
   SET ORDER TO 1
   DbGoTo( 50 )

   info_:= DbStruct()

   Popups( 2 )

   oBrowse := TBrowseWVG():New( nTop + 3, nLeft + 12, nBottom - 1, nRight - 2 )

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
   hb_gtInfo( HB_GTI_WINTITLE, "WVT Gui TBrowse()" )

   aAdd( aBlocks, {|| Wvt_DrawBoxRaised( oBrowse:nTop-2, oBrowse:nLeft-2, oBrowse:nBottom+1, oBrowse:nRight+2 ) } )
   aAdd( aBlocks, {|| Wvt_DrawBoxRecessed( oBrowse:nTop, oBrowse:nLeft, oBrowse:nBottom, oBrowse:nRight ) } )
   aAdd( aBlocks, {|| Wvt_DrawGridHorz( oBrowse:nTop+3, oBrowse:nLeft, oBrowse:nRight, oBrowse:nBottom - oBrowse:nTop - 2 ) } )
   aAdd( aBlocks, {|| Wvt_DrawGridVert( oBrowse:nTop, oBrowse:nBottom, oBrowse:aColumnsSep, len( oBrowse:aColumnsSep ) ) } )

   Vou_BrwAddScrollBars( oCrt, oBrowse, @oVBar, @oHBar )

   aLastPaint := WvtSetBlocks( aBlocks )

   BrwBuildCheckBox( oCrt, oBrowse )
   BrwBuildSLE( oCrt, oBrowse )
   BrwBuildListBox( oCrt, oBrowse )
   BrwBuildMLE( oCrt, oBrowse )
   BrwBuildButtons( oCrt, oBrowse )
   BrwBuildTree( oCrt, oBrowse )

   DispBox( 0, 0, maxrow(), maxcol(), "         ", "N/W" )
   DispOutAt( oBrowse:nTop-2, oBrowse:nleft-2, padc( cFileDbf, oBrowse:nRight-oBrowse:nLeft+5 ), "W+/B*" )

   oTBar:buttonClick := {|oBtn| Vou_ExecTBarAction( oBtn ) }

   WHILE ! lEnd
      dispbegin()
      DO WHILE ( ( nKey := inkey( , INKEY_ALL + HB_INKEY_GTEVENT ) ) == 0 .or. nKey == K_MOVING ) .and. ! oBrowse:stabilize()
      ENDDO
      dispend()

      IF nKey == 0
         oVBar:setData( OrdKeyNo() )
         oHBar:setData( oBrowse:colPos )
         DO WHILE ( ( nKey := inkey( , INKEY_ALL + HB_INKEY_GTEVENT ) ) == 0 .or. nKey == K_MOVING )
         ENDDO
      ENDIF

      DO CASE
      CASE nKey == K_F12
         nFactor--
         hb_gtInfo( HB_GTI_SPEC, HB_GTS_FACTOR, nFactor )

      CASE nKey == K_F11
         nFactor++
         hb_gtInfo( HB_GTI_SPEC, HB_GTS_FACTOR, nFactor )

      CASE nKey == K_F6
         hb_gtInfo( HB_GTI_RESIZABLE, .f. )

      CASE nKey == K_F7
         hb_gtInfo( HB_GTI_RESIZABLE, .t. )

      CASE nKey == K_F2
         nIndex := IndexOrd()
         nIndex++
         IF nIndex > 3
            nIndex := 1
         ENDIF
         SET ORDER TO ( nIndex )
         oBrowse:RefreshAll()
         oBrowse:ForceStable()

      CASE nKey == K_F3
         DoModalWindow()

      CASE nKey == K_F4
         hb_gtInfo( HB_GTI_SPEC, HB_GTS_WNDSTATE, HB_GTS_WS_MAXIMIZED )

      CASE BrwHandleKey( oBrowse, nKey, @lEnd )

      CASE nKey == HB_K_RESIZE
         oBrowse:nBottom := maxrow() - 3
         oBrowse:nRight  := maxcol() - 5

         BrwReposButtons( oCrt ) /* Because we are repositioning at the center of console width */

         DispBox( 0, 0, maxrow(), maxcol(), "         ", "N/W" )
         DispOutAt( oBrowse:nTop-2, oBrowse:nleft-2, padc( cFileDbf, oBrowse:nRight - oBrowse:nLeft + 5 ), "W+/B*" )
         oBrowse:configure()

      ENDCASE
   END

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

STATIC FUNCTION BrwShowColumn( oBrw, cHeading )
   LOCAL i, j, nCur

   nCur := oBrw:colPos
   FOR i := 1 TO oBrw:colCount
      IF oBrw:getColumn( i ):heading == cHeading
         EXIT
      ENDIF
   NEXT
   IF i < nCur
      FOR j := nCur-1 TO i STEP -1
         oBrw:left()
         oBrw:refreshCurrent()
         oBrw:forceStable()
      NEXT
   ELSEIF i > nCur
      FOR j := nCur+1 TO i
         oBrw:right()
         oBrw:refreshCurrent()
         oBrw:forceStable()
      NEXT
   ENDIF

   RETURN NIL

/*----------------------------------------------------------------------*/

STATIC FUNCTION BrwBuildTree( oCrt /*, oBrw*/ )
   LOCAL oTree, oItem1, oItem2

   oTree := WvgTreeView():new( oCrt )
   oTree:hasLines   := .T.
   oTree:hasButtons := .T.
   oTree:alwaysShowSelection := .T.
   oTree:create( , , { -25, -1 }, { -8, -10 } )
   oTree:setColorFG( "W+" )
   oTree:setColorBG( "R*" )
   oTree:itemSelected := {|oItem| WVG_MessageBox( , iif( oItem <> NIL, oItem:caption, "Some Problem" ) ) }

   oItem1 := oTree:rootItem:addItem( "First level A" )

   oTree:rootItem:addItem( "First level B" )

   oItem2 := oItem1:addItem( "Second level 1" )
   oItem1:addItem( "Second level 2" )

   oItem2:addItem( "Third level x" )
   oItem2:addItem( "Third level y" )
   oItem2:addItem( "Third level z" )

   oTree:showExpanded( .t., 2 )
   oTree:setData( oItem2 )

   oTree:tooltipText := "Treeview embedded onto CUI window"

   RETURN NIL

/*----------------------------------------------------------------------*/

STATIC FUNCTION BrwBuildMLE( oCrt /*, oBrw*/ )
   LOCAL oXbp

   oXbp := WvgMLE():new( oCrt )
   oXbp:create( , , { -16,-1 }, { -8, -10 }, , .t. )
   oXbp:setColorFG( "B" )
   oXbp:setData( "Harbour's implementation of GUI controls onto CUI console." )
   oXbp:tooltipText := "Multi-line Edit Control"

   RETURN NIL

/*----------------------------------------------------------------------*/

STATIC FUNCTION BrwBuildSLE( oCrt, oBrw )
   LOCAL oXbp

   oXbp := WvgStatic():new( oCrt )
   oXbp:type    := WVGSTATIC_TYPE_TEXT
   oXbp:options := WVGSTATIC_TEXT_LEFT
   oXbp:caption := "Field Name:"
   oXbp:create( , , { -3, -13 }, { -1, -7 } )
   oXbp:setColorFG( "W+" )
   oXbp:setColorBG( "BG" )

   oXbp := WvgSLE():new( oCrt )
   oXbp:create( , , { -3, -21 }, { -1, -20 } )
   oXbp:setColorFG( "N"  )
   oXbp:setColorBG( "BG+"  )
   oXbp:returnPressed := {|m1,m2,o| m1:=m1, m2:=m2, BrwShowColumn( oBrw, upper( trim( o:getData() ) ) ) }
   oXbp:tooltipText := "Typein a FieldName to focus it!"

   RETURN NIL

/*----------------------------------------------------------------------*/

STATIC FUNCTION BrwBuildListBox( oCrt, oBrw )
   LOCAL oXbp, i

   oXbp := WvgListBox():new( oCrt )
   oXbp:create( , , { -5,-1 }, { -10, -10 }, , .t. )
   oXbp:setColorFG( "W+" )
   oXbp:setColorBG( "B*" )
   oXbp:itemMarked := {|m1,m2,o| m1 := m1, m2 := m2, BrwShowColumn( oBrw, o:getCurItem() ) }
   FOR i := 1 TO oBrw:colCount
      oXbp:addItem( oBrw:getColumn( i ):heading )
   NEXT
   oXbp:setData( 3 )
   oXbp:tooltipText := "Click on a FieldName to make it active!"

   RETURN oXbp

/*----------------------------------------------------------------------*/

STATIC FUNCTION BrwBuildCheckBox( oCrt, oBrw )
   LOCAL oXbp

   oXbp := WvgCheckBox():new( oCrt )
   oXbp:caption     := "Navigate"
   oXbp:selected    := {|x,y,o| x := x, y := y, iif( o:getData(), oBrw:goTop():forceStable(), oBrw:goBottom():forceStable() ) }
   oXbp:selection   := .t.
   oXbp:create( , , { -3,-1 }, { -1,-7 } )
   oXbp:setColorFG( "R+" )
   oXbp:setColorBG( "W" )
   oXbp:tooltipText := "Checked: GoBottom, Unchecked: GoTop"

   RETURN NIL

/*----------------------------------------------------------------------*/

STATIC FUNCTION BrwReposButtons( oCrt )
   LOCAL oXbp, nOff, nTtl, nG, i
   LOCAL aW   := { 10, 10, 10, 10, 10 }

   nG   := 2
   nTtl := 0
   aeval( aW, {|e| nTtl += e } )
   nTtl += ( len( aW ) - 1 ) * nG

   nOff := ( ( maxcol()+1 ) - nTtl ) / 2
   i := 0
   FOR EACH oXbp IN oCrt:childList()
      IF __ObjGetClsName( oXbp ) == "WVGPUSHBUTTON"
         i++
         oXbp:setPosAndSize( { -maxrow(), -nOff }, { -1, -aW[ i ] } )
         nOff += aW[ i ] + nG
      ENDIF
   NEXT

   RETURN NIL

/*----------------------------------------------------------------------*/

STATIC FUNCTION BrwBuildButtons( oCrt, oBrw )
   LOCAL oPB, nOff, nTtl, nG, i
   LOCAL aPmt := { "Modal Window", "Maximize", "Go Top", "Go Bottom", "Right Most" }
   LOCAL aAct := { {|| Wvt_Keyboard( K_F3 ) }, ;
                   {|| Wvt_Keyboard( K_F4 ) }, ;
                   {|| oBrw:goTop(), oBrw:forceStable() }, ;
                   {|| oBrw:goBottom(), oBrw:forceStable() }, ;
                   {|| oBrw:panEnd(), oBrw:forceStable() } }
   LOCAL aW   := { 10, 10, 10, 10, 10 }

   nG := 2
   nTtl := 0
   aeval( aW, {|e| nTtl += e } )
   nTtl += ( len( aW ) - 1 ) * nG

   nOff := ( ( maxcol()+1 ) - nTtl ) / 2

   FOR i := 1 TO len( aPmt )
      oPB := WvgPushButton():new( oCrt )
      IF i == len( aPmt )
         oPB:caption := hb_dirBase() + "\" + "v_lock.bmp"
         oPB:create( , , { {|| -maxrow() }, -nOff }, { -1,-aW[ i ] } )
      ELSE
         oPB:caption := aPmt[ i ]
         oPB:create( , , { {|| -maxrow() }, -nOff }, { -1,-aW[ i ] } )
      ENDIF
      oPB:activate := aAct[ i ]
      oPB:setColorFG( RGB( 0,255,0 ) )
      oPB:tooltipText := aPmt[ i ]

      nOff += aW[ i ] + nG
   NEXT

   RETURN NIL

/*----------------------------------------------------------------------*/

STATIC FUNCTION BrwHandleKey( oBrowse, nKey, lEnd )
   LOCAL lVMove := .f.
   LOCAL lHMove := .f.
   LOCAL lRet   := .t.

   DO CASE
   CASE nKey == K_ESC
      lEnd := .t.

   CASE nKey == K_ENTER
      lEnd := .t.

   CASE nKey == K_DOWN
      lVMove := .t.
      oBrowse:Down()

   CASE nKey == K_UP
      lVMove := .t.
      oBrowse:Up()

   CASE nKey == K_PGDN
      lVMove := .t.
      oBrowse:pageDown()

   CASE nKey == K_PGUP
      lVMove := .t.
      oBrowse:pageUp()

   CASE nKey == K_CTRL_PGUP
      lVMove := .t.
      oBrowse:goTop()

   CASE nKey == K_CTRL_PGDN
      lVMove := .t.
      oBrowse:goBottom()

   CASE nKey == K_LEFT
      lHMove := .t.
      oBrowse:Left()

   CASE nKey == K_RIGHT
      lHMove := .t.
      oBrowse:Right()

   CASE nKey == K_HOME
      lHMove := .t.
      oBrowse:home()

   CASE nKey == K_END
      lHMove := .t.
      oBrowse:end()

   CASE nKey == K_CTRL_LEFT
      lHMove := .t.
      oBrowse:panLeft()

   CASE nKey == K_CTRL_RIGHT
      lHMove := .t.
      oBrowse:panRight()

   CASE nKey == K_CTRL_HOME
      lHMove := .t.
      oBrowse:panHome()

   CASE nKey == K_CTRL_END
      lHMove := .t.
      oBrowse:panEnd()

   CASE nKey == K_MWBACKWARD
      lVMove := .t.
      oBrowse:down()

   CASE nKey == K_MWFORWARD
      lVMove := .t.
      oBrowse:up()

   CASE Vou_NavigateToCell( oBrowse, nKey )

   OTHERWISE
      lRet := .f.

   ENDCASE

   IF lHMove .or. lVMove
      oBrowse:forceStable()
   ENDIF

   RETURN lRet

/*----------------------------------------------------------------------*/

STATIC FUNCTION DbSkipBlock( n )
   LOCAL nSkipped := 0

   IF n == 0
      DBSkip( 0 )

   ELSEIF n > 0
      DO WHILE nSkipped != n .AND. TBNext()
         nSkipped++
      enddo
   else
      DO WHILE nSkipped != n .AND. TBPrev()
         nSkipped--
      ENDDO
   ENDIF

   RETURN  nSkipped

//-------------------------------------------------------------------//

STATIC FUNCTION TBNext()
   LOCAL nSaveRecNum := recno()
   LOCAL lMoved := .T.

   IF Eof()
      lMoved := .F.
   ELSE
      DBSkip( 1 )
      IF Eof()
         lMoved := .F.
         DBGoTo( nSaveRecNum )
      ENDIF
   ENDIF

   RETURN lMoved

//-------------------------------------------------------------------//

STATIC FUNCTION TBPrev()
   LOCAL nSaveRecNum := Recno()
   LOCAL lMoved := .T.

   DBSkip( -1 )

   IF Bof()
      DBGoTo( nSaveRecNum )
      lMoved := .F.
   ENDIF

   RETURN lMoved

//-------------------------------------------------------------------//

STATIC FUNCTION VouBlockField( i )
   RETURN  {|| fieldget( i ) }

/*----------------------------------------------------------------------*/

STATIC FUNCTION Vou_ExecTBarAction( oBtn )

   SWITCH oBtn:caption
   CASE "New"
      Wvt_Keyboard( K_DOWN      ); EXIT
   CASE "Select"
      Wvt_Keyboard( K_UP        ); EXIT
   CASE "Calendar"
      Wvt_Keyboard( K_RIGHT     ); EXIT
   CASE "Tools"
      Wvt_Keyboard( K_LEFT      ); EXIT
   CASE "Index"
      Wvt_Keyboard( K_PGDN      ); EXIT
   CASE "Show"
      Wvt_Keyboard( K_PGUP      ); EXIT
   CASE "Hide"
      Wvt_Keyboard( K_CTRL_HOME ); EXIT
   ENDSWITCH

   RETURN NIL

/*----------------------------------------------------------------------*/

FUNCTION Vou_NavigateToCell( oBrowse )
   LOCAL nCount
   LOCAL nHitWhere := oBrowse:HitTest( mrow(), mcol() )

   IF nHitWhere ==  -5121   // on a cell
      nCount := oBrowse:mRowPos - oBrowse:RowPos
      DispBegin()
      WHILE ( nCount < 0 )
         nCount++
         oBrowse:Up()
         oBrowse:ForceStable()
      ENDDO

      WHILE ( nCount > 0 )
         nCount --
         oBrowse:Down()
         oBrowse:ForceStable()
      ENDDO

      nCount := oBrowse:mColPos - oBrowse:ColPos
      WHILE ( nCount < 0 )
         nCount++
         oBrowse:Left()
      ENDDO

      WHILE ( nCount > 0 )
         nCount--
         oBrowse:Right()
      ENDDO
      DispEnd()
      RETURN .t.
   ENDIF

   RETURN .f.

/*----------------------------------------------------------------------*/

FUNCTION Vou_BrwAddScrollBars( oCrt, oBrowse, oVBar, oHBar )

   oHBar := WvgScrollBar():new( oCrt, , { {|| -( maxrow()-2 ) }, {|| -( oBrowse:nLeft ) } }, ;
                                         { -1, {|| -( maxcol() - 5 - oBrowse:nLeft + 1 ) } } )
   oHBar:range := { 1, oBrowse:colCount }
   oHBar:type  := WVGSCROLL_HORIZONTAL
   oHBar:create()
   oHBar:scroll := {|mp1| oBrowse:colPos := mp1[ 1 ], oBrowse:refreshAll(), oBrowse:forceStable() }

   oVBar := WvgScrollBar():new( oCrt, , { {|| -( oBrowse:nTop ) }, {|| -( maxcol()-4 ) } }, ;
                                     { {|| -( maxrow()-3-oBrowse:nTop+1 ) }, {|| -( 2 ) } } )
   oVBar:range := { 1, LastRec() }
   oVBar:type  := WVGSCROLL_VERTICAL
   oVBar:create()
   oVBar:scroll := {|mp1| Vou_BrwSetVScroll( mp1, oBrowse ) }

   RETURN NIL

/*----------------------------------------------------------------------*/

FUNCTION Vou_BrwSetVScroll( mp1, oBrowse )
   LOCAL nCmd, nPos

   nCmd := mp1[ 2 ]
   nPos := mp1[ 1 ]

   SWITCH nCmd

   CASE WVGSB_NEXTPOS
      oBrowse:down()
      EXIT

   CASE WVGSB_PREVPOS
      oBrowse:up()
      EXIT

   CASE WVGSB_NEXTPAGE
      oBrowse:pageDown()
      EXIT

   CASE WVGSB_PREVPAGE
      oBrowse:pageUp()
      EXIT

   CASE WVGSB_SLIDERTRACK
      OrdKeyGoTo( nPos )
      oBrowse:refreshAll()
      EXIT

   ENDSWITCH

   oBrowse:forceStable()

   RETURN nil

/*----------------------------------------------------------------------*/
/*                   For brosers inside WvtDialog()                     */
/*----------------------------------------------------------------------*/

STATIC FUNCTION BrwOnEvent( oWvtBrw, cPaintID, oBrowse, nKey )
   LOCAL lRet := .t., lRefAll := .f.

   HB_SYMBOL_UNUSED( cPaintID )

   DO CASE
   CASE nKey == K_DOWN
      oBrowse:Down()

   CASE nKey == K_UP
      oBrowse:Up()

   CASE nKey == K_LEFT
      oBrowse:Left()

   CASE nKey == K_RIGHT
      oBrowse:Right()

   CASE nKey == K_PGDN
      oBrowse:pageDown()
      lRefAll := .t.

   CASE nKey == K_PGUP
      oBrowse:pageUp()
      lRefAll := .t.

   CASE nKey == K_CTRL_PGUP
      oBrowse:goTop()
      lRefAll := .t.

   CASE nKey == K_CTRL_PGDN
      oBrowse:goBottom()
      lRefAll := .t.

   CASE nKey == K_HOME
      oBrowse:home()

   CASE nKey == K_END
      oBrowse:end()

   CASE nKey == K_CTRL_LEFT
      oBrowse:panLeft()

   CASE nKey == K_CTRL_RIGHT
      oBrowse:panRight()

   CASE nKey == K_CTRL_HOME
      oBrowse:panHome()

   CASE nKey == K_CTRL_END
      oBrowse:panEnd()

   CASE nKey == K_MWBACKWARD
      oBrowse:down()

   CASE nKey == K_MWFORWARD
      oBrowse:up()

   CASE nKey == K_SBTHUMBTRACKVERT
      OrdKeyGoTo( oWvtBrw:oVBar:GetPos() )
      lRefAll := .t.

   CASE nKey == K_SBTHUMBTRACKHORZ
      oBrowse:ColPos := oWvtBrw:oHBar:GetPos()

   CASE nKey == K_SBLINEUP
      oBrowse:up()

   CASE nKey == K_SBLINEDOWN
      oBrowse:down()

   CASE nKey == K_SBPAGEUP
     oBrowse:pageUp()

   CASE nKey == K_SBPAGEDOWN
      oBrowse:pageDown()

   CASE nKey == K_SBLINELEFT
      oBrowse:left()

   CASE nKey == K_SBLINERIGHT
      oBrowse:right()

   CASE nKey == K_SBPAGELEFT
      oBrowse:left()

   CASE nKey == K_SBPAGERIGHT
      oBrowse:right()

   OTHERWISE
      lRet := .f.

   ENDCASE

   IF lRet
      IF lRefAll
         oBrowse:refreshAll()
      ENDIF
      oBrowse:forceStable()

      oWvtBrw:oVBar:setPos( OrdKeyCount(),OrdKeyNo() )
      oWvtBrw:oHBar:setPos( oBrowse:ColCount, oBrowse:ColPos )
   ENDIF

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

   oBrowse := TBrowseWVG():New( aTLBR[ 1 ], aTLBR[ 2 ], aTLBR[ 3 ], aTLBR[ 4 ] )

   oBrowse:ColSep        := "  "
   oBrowse:HeadSep       := "__"
   oBrowse:ColorSpec     := cColorSpec
   oBrowse:GoTopBlock    := { || dbGoTop() }
   oBrowse:GoBottomBlock := { || dbGoBottom() }
   oBrowse:SkipBlock     := { | nSkip | dbSkipBlock( nSkip,oBrowse ) }

   FOR i := 1 TO len( aFields )
      bBlock := VouBlockField( aFields[ i ] )
      oBrowse:AddColumn( TBColumnNew( info_[ aFields[ i ],1 ], bBlock ) )
   NEXT

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
   LOCAL oMenu, oSMenu

   oMenu := oCrt:menubar()

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

   RETURN oMenu

//----------------------------------------------------------------------//

STATIC FUNCTION BrwBuildToolBar( oCrt )
   LOCAL oTBar

   oTBar := WvgToolBar():new( oCrt, , { -0.1,-0.1 }, { -3, {|| -( maxcol()+1 ) } } )

   oTBar:style        := WVGTOOLBAR_STYLE_FLAT
   oTBar:borderStyle  := WVGFRAME_RECT

   oTBar:buttonWidth  := 40 //28
   oTBar:buttonHeight := 26

   oTBar:imageWidth   := 26
   oTBar:imageHeight  := 24

   oTBar:showToolTips := .t.

   // After setting properties, create toolbar.
   oTBar:create()

   oTBar:addItem( "New"       , hb_DirBase() + 'v_new.bmp'    )
   oTBar:addItem( "Select"    , hb_DirBase() + 'v_selct1.bmp' )
   oTBar:addItem( "Calendar"  , hb_DirBase() + 'v_calend.bmp' )
   oTBar:addItem( "Tools"     , hb_DirBase() + 'v_lock.bmp'   )
   oTBar:addItem( "Index"     , hb_DirBase() + 'v_index.bmp'  )
   oTBar:addItem( "Show"      , hb_DirBase() + 'v_clclt.bmp'  )
   oTBar:addItem( "Hide"      , hb_DirBase() + 'v_notes1.bmp' )

   RETURN oTBar

//----------------------------------------------------------------------//

