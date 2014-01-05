/*
 *    Wvt*Classes Demonstration
 *
 *    This protocol can be clubbed with pure console implementation
 *    AND can be called IN a separate thread as well as modal TO
 *    current window.
 *
 *    Pritpal Bedi <bedipritpal@hotmail.com>
 */

#include "inkey.ch"
#include "hbgtinfo.ch"

PROCEDURE DialogWvgClassesOne( nMode )

   LOCAL bBlock

   IF hb_mtvm()
      IF nMode == 2
         MyDialogOne()
      ELSE
         bBlock := {| oCrt | ;
            oCrt := WvgCrt():New( , , { -1, -1 }, { 54, 184 }, , .F. ), ;
            oCrt:fontName   := "Courier", ;
            oCrt:fontHeight := 13, ;
            oCrt:fontWidth  := 0, ;
            oCrt:Create(), ;
            MyDialogOne( oCrt ), ;
            oCrt:destroy() ;
            }
         hb_threadStart( bBlock )
      ENDIF
   ELSE
      MyDialogOne()
   ENDIF

   RETURN

STATIC PROCEDURE MyDialogOne( oCrt )

   LOCAL aObjects := WvtSetBlocks( {} )
   LOCAL nWinRows, nWinCols, cWinTitle, cFont, nHeight
   LOCAL oDlg, oBar, cUseAlias
   LOCAL oText, oTBar, aImg_, oImg, oLine, oBox, oBtn, oBtn2
   LOCAL oBBox, oCon, oGet, oBBox2, oBnr, oTBx
   LOCAL oBRsd, cTxt, oRct, nGetCol, nSayCol, bBlock, bBlock1
   LOCAL oWvtBrw, oWvtBrw1, lOpen, lOpen1, cUseAlias1, oGetArea, oGet1
   LOCAL hPopup, nGetRow, aGets_, lChkMouse
   LOCAL g_oMenuBar, oPBar2, oPBar3, oMenu

   HB_SYMBOL_UNUSED( oCrt )

   WvtSetKeys( .F. )
   lChkMouse := SetMouseCheck( .F. )

   hPopup := Wvt_SetPopupMenu()
   Popups()

   cTxt := "GTWVG is capable of designing virtually any preceivable control "
   cTxt := cTxt + "Windows offers."
   cTxt := cTxt + hb_eol() + hb_eol()
   cTxt := cTxt + "This text is placed in a WvtTextBox() control with "
   cTxt := cTxt + "font and alignment adjustments!"
   cTxt := cTxt + hb_eol() + hb_eol()
   cTxt := cTxt + "Enjoy - Pritpal Bedi, INDIA"

   aImg_ := {}
   AAdd( aImg_, GetResource( "v_lock.bmp"   ) )
   AAdd( aImg_, GetResource( "v_new.bmp"    ) )
   AAdd( aImg_, GetResource( "v_clclt.bmp"  ) )
   AAdd( aImg_, GetResource( "v_calend.bmp" ) )
   AAdd( aImg_, GetResource( "v_index.bmp"  ) )
   AAdd( aImg_, GetResource( "v_notes1.bmp" ) )
   AAdd( aImg_, GetResource( "v_selct1.bmp" ) )
   ? "."
   Wvt_ShowWindow( 1 )
   nWinRows  := 55
   nWinCols  := 185
   cWinTitle := "WvtGui Dialog One"
   cFont     := "Courier New"
   nHeight   := 13

   oDlg := WvtDialog():New( nWinRows, nWinCols, cWinTitle, cFont, nHeight )
   oDlg:nTooltipWidth     := 300
   oDlg:nTooltipTextColor := RGB( 255, 0, 0 )

   oBar := WvtStatusBar():New( oDlg, 201 )
   oBar:SetPanels( { 50, 100 } )
   oBar:SetText( 1, "Tab.SH_Tab.Left_Click - Select a Browse" )
   oBar:SetText( 2, "GtWvt is Fantastic", "w+/W" )
   oBar:SetText( 3, "WOW" )
   oBar:nPointer := WVT_IDC_HAND
   oBar:Tooltip  := "GtWvt Statusbar with 3 panels"
   oDlg:AddObject( oBar )

   oBox := WvtStatic():New( oDlg, 110, 4, oDlg:MaxCol() - 40, 7, oDlg:MaxCol() - 2 )
   oBox:nStatic := WVT_STATIC_BOXRECESSED
   oDlg:AddObject( oBox )

   oText := WvtLabel():New( oDlg, 101, 4, oDlg:MaxCol() - 40, 7, oDlg:MaxCol() - 2 )
   oText:Text              := "Harbour"
   oText:nFontHeight       := 36
   oText:nAlignHorz        := 2
   oText:nAlignVert        := 2
   oText:nFontWeight       := 700
   oText:nTextColor        := RGB( 100, 255,  12 )
   oText:nBackColor        := RGB(   0,   0, 255 )
   oText:nTextColorHoverOn := RGB( 255, 255,   0 )
   oText:nBackColorHoverOn := RGB( 255, 100,  12 )
   oText:lItalic           := .T.
   oText:ToolTip           := "Software that GROWS with you"
   oText:bOnSelect         := {|| .T. }
   oDlg:AddObject( oText )

   oImg := WvtImage():New( oDlg, 102, 20, oDlg:MaxCol() - 40, 37, oDlg:MaxCol() - 2 )
   oImg:cImage  := aImg_[ 5 ]
   oImg:Tooltip := "WvtImage():New()"
   oDlg:AddObject( oImg )

   oTBar := WvtToolBar():New( oDlg, 103, 0, 0, 2 )
   oTBar:lFloating := .F.
   oTBar:Tooltip   := "Toolbar"
   oTBar:AddButton( aImg_[ 1 ], {|| oImg:SetImage( aImg_[ 1 ] ) }, "Lock" )
   oTBar:AddButton( aImg_[ 2 ], {|| oImg:SetImage( aImg_[ 2 ] ), oText:SetText( "Harbour" ) }, "New" )
   oTBar:AddButton( aImg_[ 3 ], {|| oImg:SetImage( aImg_[ 3 ] ) }, "Calculator" )
   oTBar:AddButton()
   oTBar:AddButton( aImg_[ 5 ], {|| oImg:SetImage( aImg_[ 5 ] ) }, "Restore" )
   oTBar:AddButton( aImg_[ 4 ], {|| oImg:SetImage( aImg_[ 4 ] ), oText:SetText( "Vouch" )    }, "Calendar" )
   oTBar:AddButton( aImg_[ 6 ], {|| oImg:SetImage( aImg_[ 6 ] ) }, "Notes" )
   oTBar:AddButton( aImg_[ 7 ], {|| oImg:SetImage( aImg_[ 7 ] ) }, "Press to Send Browse on Top" )
   oTBar:AddButton()
   oDlg:AddObject( oTBar )

   oLine := WvtStatic():New( oDlg, 105, 39, 0, 39, oDlg:MaxCol() )
   oLine:nStatic := WVT_STATIC_LINE
   oDlg:AddObject( oLine )

   oBBox := WvtStatic():New( oDlg, 125, 4, 127, 37, 139 )
   oBBox:nStatic := WVT_STATIC_BOXGROUP
   oDlg:AddObject( oBBox )

   oBtn := WvtPushButton():New( oDlg, 124, 6, 129, 7, 137 )
   oBtn:cCaption  := "Print"
   oBtn:bOnLeftUp := {|| Wvt_Keyboard( 379 ) }
   oBtn:Tooltip   := "Open Printing Dialog for the Browser in Focus"
   oDlg:AddObject( oBtn )

   oBtn2 := WvtPushButton():New( oDlg, 124, 9, 129, 12, 137 )
   oBtn2:cFileImage := aImg_[ 3 ]
   oBtn2:block      := {|| ExeProgressBar( oPBar2, oPBar3 ) }
   oBtn2:Tooltip    := "Execute Progress Bar"
   oDlg:AddObject( oBtn2 )

   oPBar2 := WvtProgressBar():New( oDlg, , 14, 129, 25, 137 )
   oPBar2:nBarColor  := RGB( 240, 240, 0 )
   oPBar2:cBackColor := "W/N*"
   oPBar2:lVertical  := .T.
   oPBar2:nDirection := 0
   oPBar2:cImage     := GetResource( "vouch1.bmp" )
   oDlg:AddObject( oPBar2 )

   oPBar3 := WvtProgressBar():New( oDlg, , 26, 129, 36, 137 )
   oPBar3:nBarColor  := RGB( 240, 240, 0 )
   oPBar3:cBackColor := "W/N*"
   oPBar3:lVertical  := .T.
   oPBar3:nDirection := 1
   oPBar3:cImage     := GetResource( "vouch1.bmp" )
   oDlg:AddObject( oPBar3 )

   oBBox2 := WvtStatic():New( oDlg, , 9, oDlg:MaxCol() - 40, 18, oDlg:MaxCol() - 2 )
   oBBox2:nStatic := WVT_STATIC_BOXGROUP
   oDlg:AddObject( oBBox2 )

   oCon := WvtConsole():New( oDlg )
   oDlg:AddObject( oCon )

   nGetCol := 158
   bBlock  := {|| oCon:Say( 12, 148, "Name", "N/W" ), ;
      oCon:Say( 14, 148, "Date", "N/W" ), ;
      oCon:Say( 16, 148, "Amount", "N/W" ) }

   oGet := WvtGets():New( oDlg, 210, 9, oDlg:MaxCol() - 40, 18, oDlg:MaxCol() - 2 )
   oGet:AddGets( 12, nGetCol, "GTWvt               ", "@! ", "W+/B*,N/W*" )
   oGet:AddGets( 14, nGetCol, Date() )
   oGet:AddGets( 16, nGetCol, 2122.57, "@Z 99999999.99", "w+/R,GR+/B" )
   oGet:Tooltip   := "WvtGets():New() - ReadModal() like Clipper"
   oGet:cDesc     := "Normal Get Box"
   oGet:bOnCreate := bBlock
   oDlg:AddObject( oGet )

   oBnr := WvtBanner():New( oDlg, 101, 0, 127, 1, oDlg:MaxCol() - 2 )
   oBnr:nTimeDelay        := 0.25
   oBnr:cText             := "the compiler that EXTENDS with you"
   oBnr:nFontHeight       := 24
   oBnr:nFontWeight       := 0
   oBnr:nDirection        := 0
   oBnr:nAlignVert        := 2
   oBnr:nTextColor        := RGB( 253, 251, 170 )
   oBnr:nBackColor        := RGB( 128, 227, 142 )
   oBnr:nTextColorHoverOn := RGB( 255, 255,  0 )
   oBnr:nBackColorHoverOn := RGB( 255, 100, 12 )
   oBnr:Tooltip           := "WvtBanner():New()"
   oDlg:AddObject( oBnr )

   oBRsd := WvtStatic():New( oDlg, , 41, 127, 52, oDlg:MaxCol() - 2 )
   oBRsd:nStatic := WVT_STATIC_BOXGROUPRAISED
   oDlg:AddObject( oBRsd )

   oRct := WvtStatic():New( oDlg, , 41, 127, 52, oDlg:MaxCol() - 2 )
   oRct:nStatic := WVT_STATIC_SHADEDRECT
   oRct:aRGBb   := { 0xffff, 0x0000, 0x0000, 0x0000 }
   oRct:aRGBe   := { 0x0000, 0xffff, 0xffff, 0x0000 }
   oDlg:AddObject( oRct )

   oTBx := WvtTextBox():New( oDlg, , 42, 129, 51, oDlg:MaxCol() - 4 )
   oTBx:cText       := cTxt
   oTBx:Tooltip     := "WvtTextBox():New()"
   oTBx:nFontHeight := 16
   oTBx:lItalic     := .T.
   oTBx:lUnderline  := .T.
   oTBx:nAlignHorz  := 2
   oTBx:nTextColor  := RGB( 255, 255, 255 )
   oTBx:nTextColorHoverOn := RGB( 0, 0, 255 )
   oTBx:aPopup      := {}
   AAdd( oTBx:aPopup, { "Getsome", {|| .T. } } )
   AAdd( oTBx:aPopup, { "Getsome2", {|| .T. } } )
   oDlg:AddObject( oTBx )

   oGetArea := WvtStatic():New( oDlg, , 4, 2, 37, 62 )
   oGetArea:nStatic := WVT_STATIC_BOXRAISED
   oDlg:AddObject( oGetArea )

   nGetCol := 20
   nSayCol := 7
   nGetRow := 7
   bBlock1 := {|| ;
      oCon:Say( nGetRow +  0, nSayCol, "First Name"  , "N/W" ), ;
      oCon:Say( nGetRow +  2, nSayCol, "Last Name "  , "N/W" ), ;
      oCon:Say( nGetRow +  4, nSayCol, "Street"      , "N/W" ), ;
      oCon:Say( nGetRow +  6, nSayCol, "City"        , "W+/W" ), ;
      oCon:Say( nGetRow +  8, nSayCol, "State"       , "N/W" ), ;
      oCon:Say( nGetRow + 10, nSayCol, "Zip"         , "B+/W" ), ;
      oCon:Say( nGetRow + 12, nSayCol, "Date Hired"  , "B+/W" ), ;
      oCon:Say( nGetRow + 14, nSayCol, "Married"     , "B+/W" ), ;
      oCon:Say( nGetRow + 16, nSayCol, "Age"         , "B+/W" ), ;
      oCon:Say( nGetRow + 18, nSayCol, "Salary"      , "B+/W" ), ;
      oCon:Say( nGetRow + 20, nSayCol, "Notes",      , "B+/W" ) ;
      }

   aGets_ := { ;
      PadR( "Pritpal", 20 ), ;
      PadR( "Bedi", 20 ), ;
      PadR( "60, New Professor Colony", 30 ), ;
      PadR( "Ludhiana, INDIA", 30 ), ;
      "PB", ;
      PadR( "141004", 10 ), ;
      hb_SToD( "20040622" ), ;
      .T., ;
      48, ;
      17000, ;
      PadR( "Wvtgui is a classical example of Harbour capabilities...", 65 ) }

   oGet1 := WvtGets():New( oDlg, , 4, 2, 37, 62 )
   oGet1:AddGets( nGetRow +  0, nGetCol, aGets_[ 1 ], "@ "       , "N/W*,N/GR*" )
   oGet1:AddGets( nGetRow +  2, nGetCol, aGets_[ 2 ], "@ "       , "N/W*,N/GR*" )
   oGet1:AddGets( nGetRow +  4, nGetCol, aGets_[ 3 ], "@ "       , "N/W*,N/GR*" )
   oGet1:AddGets( nGetRow +  6, nGetCol, aGets_[ 4 ], "@ "       , "N/W*,N/GR*" )
   oGet1:AddGets( nGetRow +  8, nGetCol, aGets_[ 5 ], "@ "       , "N/W*,N/GR*" )
   oGet1:AddGets( nGetRow + 10, nGetCol, aGets_[ 6 ], "@ "       , "N/W*,N/GR*" )
   oGet1:AddGets( nGetRow + 12, nGetCol, aGets_[ 7 ], "@ "       , "N/W*,N/GR*" )
   oGet1:AddGets( nGetRow + 14, nGetCol, aGets_[ 8 ], "@Y"       , "N/W*,N/GR*" )
   oGet1:AddGets( nGetRow + 16, nGetCol, aGets_[ 9 ], "@Z 99"    , "N/W*,N/GR*" )
   oGet1:AddGets( nGetRow + 18, nGetCol, aGets_[ 10], "@Z 999999", "N/W*,N/GR*" )
   oGet1:AddGets( nGetRow + 20, nGetCol, aGets_[ 11], "@S35"     , "N/W*,N/GR*" )
   oGet1:cDesc     := "test.dbf Fields"
   oGet1:Tooltip   := "Double Click to Activate ReadModal()"
   oGet1:bOnCreate := bBlock1
   oDlg:AddObject( oGet1 )

   g_oMenuBar := WvtMenu():new():create()
   oMenu      := WvtMenu():new():create()
   oMenu:Caption := "Other Dialogs"
   oMenu:AddItem( "Dialog Two", {|| DialogWvgClassesTwo() } )
   oMenu:AddItem( "-" )
   oMenu:AddItem( "Exit",       {|| Wvt_Keyboard( K_ESC ) } )
   g_oMenuBar:addItem( "", oMenu )

   oDlg:oMenu := g_oMenuBar

   lOpen := .F.
   cUseAlias := "TEST"
   USE ( hb_DirBase() + hb_DirSepToOS( "../../../tests/test.dbf" ) ) NEW ALIAS ( cUseAlias ) SHARED
   IF ! NetErr()
      lOpen := .T.
      oWvtBrw := ConfigBrowser( { 1, 7, 9, 10, 8 }, cUseAlias, { 6, 67, 36, 120 }, "test.dbf - 1,7,9,10,8", oDlg, "N/W*,N/GR*", 1001 )
      oDlg:AddObject( oWvtBrw )
   ENDIF

   lOpen1 := .F.
   cUseAlias1 := "TEST1"
   USE ( hb_DirBase() + hb_DirSepToOS( "../../../tests/test.dbf" ) ) NEW ALIAS ( cUseAlias1 ) SHARED
   IF ! NetErr()
      lOpen1 := .T.
      oWvtBrw1 := ConfigBrowser( { 1, 2, 3, 4, 5, 6 }, cUseAlias1, { 43, 4, 51, 120 }, "test.dbf - 1,2,3,4,5,6", oDlg, "N/BG*,N/W*", 1002 )
      oDlg:AddObject( oWvtBrw1 )
   ENDIF

   SetKey( K_F12, {|| hb_gtInfo( HB_GTI_SPEC, HB_GTS_FACTOR, 200 ) } )

   oDlg:Create()
   oDlg:Execute()
   oDlg:Destroy()

   IF lOpen
      Select( cUseAlias )
      USE
   ENDIF
   IF lOpen1
      Select( cUseAlias1 )
      USE
   ENDIF

   WvtSetBlocks( aObjects )
   WvtSetKeys( .T. )
   Wvt_SetPopupMenu( hPopup )
   SetMouseCheck( lChkMouse )

   RETURN

PROCEDURE DialogWvgClassesTwo()

   LOCAL aObjects := WvtSetBlocks( {} )
   LOCAL oDlg     := WvtDialog():New( 30, 90, "My Dialog Two" )
   LOCAL g_oMenuBar, oMenu, oPBar
   LOCAL oPBar1, oPBar2, oPBar3, oPBar4

   g_oMenuBar    := WvtMenu():new():create()
   oMenu         := WvtMenu():new():create()
   oMenu:Caption := "Miscellaneous"
   oMenu:AddItem( "Progressbar", {|| ExeProgBar( oPBar, oPBar1, oPBar2, oPBar3, oPBar4 ) } )
   oMenu:AddItem( "-" )
   oMenu:AddItem( "Exit",        {|| Wvt_Keyboard( K_ESC ) } )
   g_oMenuBar:addItem( "", oMenu )

   oDlg:oMenu := g_oMenuBar

   oPBar := WvtProgressBar():New( oDlg, , 3, 10, 5, 80 )
   oPBar:nBarColor   := RGB( 0, 240, 240 )
   oPBar:cBackColor  := "W/N*"
   oPBar:nDirection  := 1
   oPBar:cImage      := "vouch1.bmp"
   oDlg:AddObject( oPBar )

   oPBar1 := WvtProgressBar():New( oDlg, , 7, 10, 8, 80 )
   oPBar1:nBarColor  := RGB( 11, 255, 196 )
   oPBar1:cBackColor := "W/N*"
   oPBar1:nDirection := 0
   oDlg:AddObject( oPBar1 )

   oPBar2 := WvtProgressBar():New( oDlg, , 11, 10, 28, 19 )
   oPBar2:nBarColor  := RGB( 240, 240, 0 )
   oPBar2:cBackColor := "W/N*"
   oPBar2:lVertical  := .T.
   oPBar2:nDirection := 0
   oPBar2:cImage     := "v_notes.ico"
   oDlg:AddObject( oPBar2 )

   oPBar3 := WvtProgressBar():New( oDlg, , 11, 77, 28, 80 )
   oPBar3:nBarColor  := RGB( 0, 0, 255 )
   oPBar3:cBackColor := "W/N*"
   oPBar3:lVertical  := .T.
   oPBar3:nDirection := 1
   oDlg:AddObject( oPBar3 )

   oPBar4 := WvtProgressBar():New( oDlg, , 22, 22, 28, 74 )
   oPBar4:nBarColor  := RGB( 255, 255, 0 )
   oPBar4:cBackColor := "W/N*"
   oPBar4:lVertical  := .T.
   oPBar4:nDirection := 0
   oDlg:AddObject( oPBar4 )

   oDlg:Create()
   oDlg:Execute()
   oDlg:Destroy()

   WvtSetBlocks( aObjects )

   RETURN

STATIC PROCEDURE ExeProgBar( oPBar, oPBar1, oPBar2, oPBar3, oPBar4 )

   LOCAL i

   oPBar:Activate()
   oPBar1:Activate()
   oPBar2:Activate()
   oPBar3:Activate()
   oPBar4:Activate()
   FOR i := 1 TO 100
      oPBar:Display( i, 100 )
      oPBar1:Display( i, 100 )
      oPBar2:Display( i, 100 )
      oPBar3:Display( i, 100 )
      oPBar4:Display( i, 100 )
      Inkey( 0.3 )
   NEXT
   Inkey( 0 )
   oPBar:DeActivate()
   oPBar1:DeActivate()
   oPBar2:DeActivate()
   oPBar3:DeActivate()
   oPBar4:DeActivate()

   RETURN

STATIC PROCEDURE ExeProgressBar( oPBar, oPBar3 )

   LOCAL i

   oPBar:Activate()
   oPBar3:Activate()
   FOR i := 1 TO 100
      oPBar:Display( i, 100 )
      oPBar3:Display( i, 100 )
      Inkey( 0.3 )
   NEXT
   oPBar:DeActivate()
   oPBar3:DeActivate()

   RETURN
