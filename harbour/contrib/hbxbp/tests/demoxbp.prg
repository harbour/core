/*
 * $Id$
 */

/*
 * Harbour Project source code:
 *
 * Copyright 2009 Pritpal Bedi <pritpal@vouchcac.com>
 * www - http://www.harbour-project.org
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2, or (at your option)
 * any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this software; see the file COPYING.  If not, write to
 * the Free Software Foundation, Inc., 59 Temple Place, Suite 330,
 * Boston, MA 02111-1307 USA (or visit the web site http://www.gnu.org/).
 *
 * As a special exception, the Harbour Project gives permission for
 * additional uses of the text contained in its release of Harbour.
 *
 * The exception is that, if you link the Harbour libraries with other
 * files to produce an executable, this does not by itself cause the
 * resulting executable to be covered by the GNU General Public License.
 * Your use of that executable is in no way restricted on account of
 * linking the Harbour library code into it.
 *
 * This exception does not however invalidate any other reasons why
 * the executable file might be covered by the GNU General Public License.
 *
 * This exception applies only to the code released by the Harbour
 * Project under the name Harbour.  If you copy code from other
 * Harbour Project or Free Software Foundation releases into a copy of
 * Harbour, as the General Public License permits, the exception does
 * not apply to the code that you add in this way.  To avoid misleading
 * anyone as to the status of such modified files, you must delete
 * this exception notice from them.
 *
 * If you write modifications of your own for Harbour, it is your choice
 * whether to permit this exception to apply to your modifications.
 * If you do not wish that, delete this exception notice.
 *
 */
/*----------------------------------------------------------------------*/

#include "common.ch"
#include "xbp.ch"
#include "appevent.ch"
#include "inkey.ch"
#include "gra.ch"

/*----------------------------------------------------------------------*/

PROCEDURE Main()

   BuildADialog()

   RETURN

/*----------------------------------------------------------------------*/

PROCEDURE BuildADialog()
   LOCAL oDlg, mp1, mp2, oXbp, nEvent, aSize, aTabs

   /* Create Application Window */
   oDlg := GuiStdDialog( 'Harbour - Xbase++ - QT Dialog [ Press "Q" to Exit ]' )
   oDlg:close := {|| MsgBox( 'You can also close me by Pressing "Q"' ), .T. }

   SetAppWindow( oDlg )

   /* Obtain desktop dimensions */
   aSize := AppDesktop():currentSize()
   /* Place on the center of desktop */
   oDlg:setPos( { ( aSize[ 1 ] - oDlg:currentSize()[ 1 ] ) / 2, ;
                  ( aSize[ 2 ] - oDlg:currentSize()[ 2 ] ) / 2 } )

   /* Make background color of :drawingArea different */
   //oDlg:drawingArea:setColorBG( GraMakeRGBColor( { 134,128,164 } ) )
   //oDlg:drawingArea:setColorFG( GraMakeRGBColor( { 40,120,100 } ) )

   /* Install menu system */
   Build_MenuBar()

   /* Install Statusbar */
   Build_StatusBar( oDlg )

   /* Install Toolbar */
   Build_ToolBar( oDlg:drawingArea )

   /* Install Tab Pages */
   aTabs := Build_TabPages( oDlg )

   /* Install checkboxes */
   Build_CheckBox( aTabs[ 3 ] )

   /* Install 3state checkboxes */
   Build_3State( aTabs[ 3 ] )

   /* Install Radio Buttons */
   Build_RadioButton( aTabs[ 3 ] )

   /* Install ListBox */
   Build_ListBox( aTabs[ 1 ] )

   /* Install Push Buttons */
   Build_PushButton( oDlg:drawingArea )

   /* Install Single Line Edits */
   Build_SLEs( oDlg:drawingArea )

   /* Install Multi-Line Edit */
   Build_MLE( aTabs[ 2 ] )

   /* Install ScrollBar */
   Build_ScrollBar( aTabs[ 1 ] )

   /* Install Spin Buttons */
   Build_SpinButtons( aTabs[ 3 ] )

   /* Install Combo Box */
   Build_ComboBox( oDlg:drawingArea )

   /* Install TreeView */
   Build_TreeView( aTabs[ 4 ] )

   /* Present the dialog on the screen */
   oDlg:Show()

   /* Enter Xbase++ Event Loop - still with limited functionality but working */
   DO WHILE .t.
      nEvent := AppEvent( @mp1, @mp2, @oXbp )
      IF nEvent == xbeP_Close .or. ( nEvent == xbeP_Keyboard .and. ( mp1 == 81 .or. mp1 == 113 ) )
hb_outDebug( "WOW" )
         EXIT
      ENDIF
      oXbp:handleEvent( nEvent, mp1, mp2 )
   ENDDO

   /* Very important - destroy resources */
   oDlg:destroy()
hb_outDebug( "WOWXXXXXXXXXX" )
   RETURN

/*----------------------------------------------------------------------*/

PROCEDURE AppSys()
   RETURN

/*----------------------------------------------------------------------*/
#ifdef __XPP__
FUNCTION Hb_OutDebug();RETURN nil
FUNCTION Hb_Symbol_Unused();RETURN nil
FUNCTION Hb_NtoS( n );RETURN ltrim( str( n ) )
#endif
/*----------------------------------------------------------------------*/

STATIC FUNCTION GuiStdDialog( cTitle )
   LOCAL oDlg

   DEFAULT cTitle TO "Standard Dialog Window"

   oDlg          := XbpDialog():new( , , {10,10}, {900,500}, , .f. )

   /* NOTE: method to install the windows icon is bit different than Windows */
   /* So curretly we can only place disk icon file only */
   oDlg:icon     :=  "test"

   /* TODO: still not implemented*/
   oDlg:taskList := .T.

   oDlg:title    := cTitle
   oDlg:create()

   RETURN oDlg

/*----------------------------------------------------------------------*/

STATIC FUNCTION Build_MenuBar()
   LOCAL oMenuBar, oSubMenu

   //oMenuBar := XbpMenuBar():new( oDlg ):create()
   oMenuBar := SetAppWindow():MenuBar()

   /* Define submenu in procedural style.
    * The numeric index of the selected menu item
    * is passed to the Callback code block -> mp1
    */
   oSubMenu := XbpMenu():new( oMenuBar ):create()
   //
   oSubMenu:title := "~Procedural"
   oSubMenu:addItem( { "Play Charge ~1",   } )
   oSubMenu:addItem( { "Play Nannyboo ~2", } )
   oSubMenu:itemSelected := {|mp1| MyFunctionXbp( 100+mp1 ) }
   //
   oMenuBar:addItem( { oSubMenu, NIL } )
   //
   oSubMenu:disableItem( 2 )

   /* Define submenu in the functional style:
    * A menu item executes a code block that calls a function
    */
   oSubMenu := XbpMenu():new( oMenuBar ):create()
   oSubMenu:title := "~Functional"
   oSubMenu:addItem( { "Play Opening ~1"+chr(K_TAB)+"Ctrl+U", {|| MyFunctionXbp( 1 ) } } )
   oSubMenu:addItem( { "Play Closing ~2"                    , {|| MyFunctionXbp( 2 ) } } )
   oSubMenu:addItem( { NIL, NIL, XBPMENUBAR_MIS_SEPARATOR, NIL } )
   oSubMenu:addItem( { "new.png|~MessageBox"                , {|| MyFunctionXbp( 3 ) }  , , XBPMENUBAR_MIA_HILITED } )
   oMenuBar:addItem( { oSubMenu, NIL } )
   //
   oSubMenu:insItem( 2, { "This executes MsgBox()"          , {|| MyFunctionXbp( 103 ) }, , XBPMENUBAR_MIA_CHECKED } )
   oSubMenu:itemMarked := {|mp1| IF( mp1 == 5, MsgBox( "WOW - ::itemMarked - Activated" ), NIL ) }

   /* Menu colors are being honored in Harbour only */
   oSubMenu:setColorBG( GraMakeRGBColor( { 134,128,250 } ) )
   oSubMenu:setColorFG( GraMakeRGBColor( { 255,  1,  1 } ) )

   #if 1
   oSubMenu := XbpMenu():new( oMenuBar ):create()
   oSubMenu:title := "~Dialogs"
   #if 0
   oSubMenu:addItem( { "~One More Instance"+chr(K_TAB)+"Ctrl+M", ;
                                 {|| hb_threadStart( {|| BuildADialog() } ) } } )
   #endif
   oSubMenu:addItem( { "~One More Instance"+chr(K_TAB)+"Ctrl+M", {|| BuildADialog() } } )
   oMenuBar:addItem( { oSubMenu, NIL } )
   #endif

   Return nil

/*----------------------------------------------------------------------*/

STATIC FUNCTION MyFunctionXbp( nMode )

   DO CASE
   CASE nMode == 1
      MsgBox( "Play Opening" + STR( GraMakeRGBColor( { 134,128,164 } ) ) )

   CASE nMode == 2
      MsgBox( "Play Closing ~2" )

   CASE nMode == 3
      MsgBox( "new.png|~MessageBox" )

   CASE nMode == 101
      MsgBox( "101 - Play Charge" )

   CASE nMode == 102
      MsgBox( "102 - Play Nanyboo" )

   CASE nMode == 103
      MsgBox( "This executes MsgBox()" )

   ENDCASE

   RETURN nil

/*----------------------------------------------------------------------*/

FUNCTION Build_ToolBar( oDA )
   LOCAL oTBar

   // Create an XbpToolBar object and
   // add it at the top of the dialog
   //
   oTBar := XbpToolBar():new( oDA )
   oTBar:create( , , { 0, oDA:currentSize()[ 2 ]-60 }, ;
                     { oDA:currentSize()[ 1 ], 60 } )
   //
   // Add two tool bar buttons, each with a
   // caption and an image. Constrict the
   // button image sizes to 32 pixels and
   // ensure transparency is turned off.
   //
   oTBar:imageWidth  := 32
   oTBar:imageHeight := 32

   /* Harbour does not support resource IDs so giving bitmap files */
   #ifdef __HARBOUR__
   oTBar:addItem( "Button #1", "new.png"  )
   oTBar:addItem( "Button #2", "open.png" )
   #else
   oTBar:addItem( "Button #1" )//, 100 )
   oTBar:addItem( "Button #2" )//, 101 )
   #endif

   oTBar:transparentColor := GRA_CLR_INVALID
   oTBar:buttonClick := {|oButton| MsgBox( "Button [" + oButton:caption + "] clicked!" ) }

   RETURN nil

/*----------------------------------------------------------------------*/

FUNCTION Build_PushButton( oDA )
   LOCAL oXbp

    oXbp := XbpPushButton():new( oDA )
    oXbp:caption := "A"
    oXbp:create( , , {30,370}, {100,40} )
    oXbp:activate:= {|| MsgBox( "Pushbutton A" ) }
    /* Harbour supports presentation colors */
    oXbp:setColorBG( GraMakeRGBColor( {133,240,90} ) )

    oXbp := XbpPushButton():new( oDA )
    oXbp:caption := "new.png"
    oXbp:create( , , {140,370}, {100,40} )
    oXbp:activate:= {|| MsgBox( "Pushbutton B" ) }
    /* Harbour supports presentation colors */
    oXbp:setColorBG( GraMakeRGBColor( {0,255,255} ) )

    RETURN nil

/*----------------------------------------------------------------------*/

FUNCTION Build_CheckBox( oWnd )
   LOCAL oXbp

   oXbp := XbpCheckbox():new()
   oXbp:caption := "A"
   oXbp:create( oWnd, , {30,30}, {100,30} )

   // Determine state using mp1
   oXbp:selected := ;
      {| mp1, mp2, oChk| MsgBox( "Checkbox A", ;
                       IIf( mp1, "selected" , ;
                                 "not selected" ) ) }

   // Create second checkbox, specify position using :new()
   oXbp := XbpCheckbox():new( oWnd, , {30,70}, {100,30} )
   oXbp:caption := "B"
   oXbp:create()

   // Determine state using :getData()
   oXbp:selected := ;
      {| mp1, mp2, oChk| MsgBox( "Checkbox B", ;
            IIf( oChk:getData(), "selected", ;
                                 "not selected" ) ) }

   RETURN nil

/*----------------------------------------------------------------------*/

FUNCTION Build_3State( oWnd )
   LOCAL oXbp
   LOCAL aState := { "not selected", "selected", "undefined" }

   // Create first 3State button, passing the position to :create()
   //
   oXbp := Xbp3State():new()
   oXbp:caption := "3State A"
   oXbp:create( oWnd, , {130,30}, {100,30} )
   // Determine current state using mp1
   oXbp:selected := {| mp1, mp2, oBtn| MsgBox( "3State A", aState[ mp1+1 ] ) }

   // Create second 3State Button, passing the position to :new()
   //
   oXbp := Xbp3State():new( oWnd, , {130,70}, {100,30} )
   oXbp:caption := "3State B"
   oXbp:create()
   // Determine current state using :getData()
   oXbp:selected := {| mp1, mp2, oBtn|  MsgBox( "3State B", aState[ oBtn:getData()+1 ] ) }

   RETURN nil

/*----------------------------------------------------------------------*/

FUNCTION Build_RadioButton( oStatic )
   LOCAL bSelected, oRadio

   // Display which radiobutton is selected
   bSelected := {|mp1,mp2,obj| MsgBox( obj:caption ) }

   // Create four radiobuttons
   oRadio := XbpRadioButton():new( oStatic,, {30,110}, {80,30} )
   oRadio:caption   := "COM 1"
   oRadio:selection :=.T.
   oRadio:selected  := bSelected
   oRadio:create()

   oRadio := XbpRadioButton():new( oStatic,, {30,150}, {80,30} )
   oRadio:caption   := "COM 2"
   oRadio:selected  := bSelected
   oRadio:create()

   oRadio := XbpRadioButton():new( oStatic,, {30,190}, {80,30} )
   oRadio:caption   := "COM 3"
   oRadio:selected  := bSelected
   oRadio:create()

   oRadio := XbpRadioButton():new( oStatic,, {30,230}, {80,30} )
   oRadio:caption   := "COM 4"
   oRadio:selected  := bSelected
   oRadio:create()

   RETURN nil

/*----------------------------------------------------------------------*/

FUNCTION Build_TabPages( oDlg )
   LOCAL oTab1, oTab2, oTab3, oTab4
   LOCAL nHeight := 380

   // First tab page is maximized

   oTab1 := XbpTabPage():new( oDlg:drawingArea, , { 510, 20 }, { 360, nHeight } )
   /* comment our following line to position tabs at the bottom */
   //oTab1:type := XBPTABPAGE_TAB_BOTTOM
   oTab1:minimized := .F.
   oTab1:caption   := "ListView"
   oTab1:create()
   oTab1:TabActivate := {|| oTab2:minimize(), oTab3:minimize(), oTab4:minimize(), oTab1:maximize() }
   #if 0
   oTab1:setColorBG( GraMakeRGBColor( {198,198,198} ) )
   #endif

   // Second tab page is minimized
   oTab2 := XbpTabPage():new( oDlg:drawingArea, , { 510, 20 }, { 360, nHeight } )
   oTab2:caption    := "XbpMLE"
   oTab2:preOffset  := 20
   oTab2:postOffset := 60
   oTab2:create()
   oTab2:TabActivate := {|| oTab1:minimize(), oTab3:minimize(), oTab4:minimize(), oTab2:maximize() }

   // Third tab page is minimized
   oTab3 := XbpTabPage():new( oDlg:drawingArea, , { 510, 20 }, { 360, nHeight } )
   oTab3:caption    := "CheckBoxes"
   oTab3:preOffset  := 40
   oTab3:postOffset := 40
   oTab3:create()
   oTab3:TabActivate := ;
       {|x,y,oTab| x := y, oTab1:minimize(), oTab2:minimize(), oTab4:minimize(), oTab3:maximize() }

   // Third tab page is minimized
   oTab4 := XbpTabPage():new( oDlg:drawingArea, , { 510, 20 }, { 360, nHeight } )
   oTab4:caption    := "TreeView"
   oTab4:preOffset  := 60
   oTab4:postOffset := 20
   oTab4:create()
   oTab4:TabActivate := ;
       {|x,y,oTab| x := y, oTab1:minimize(), oTab2:minimize(), oTab3:minimize(), oTab4:maximize() }

   RETURN { oTab1, oTab2, oTab3, oTab4 }

/*----------------------------------------------------------------------*/

FUNCTION Build_ListBox( oWnd )
   LOCAL oListBox, aItems

   aItems := { "India", "United States", "England", "Japan", "Hungary", "Argentina", "China" }

   // Create list box that allows multiple selections

   oListBox := XbpListBox():new()
   //oListBox:markMode := XBPLISTBOX_MM_MULTIPLE
   oListBox:create( oWnd, , {10,10}, {150,200} )

   // Copy field names from the DbStruct() array to the list box
   aeval( aItems, {|e| oListBox:addItem( e ) } )

   // Code block for list box selection:
   oListBox:ItemSelected := {|mp1, mp2, obj| mp1:=oListBox:getData(), ;
                              mp2:=oListBox:getItem(mp1), MsgBox( "itemSelected:"+mp2 ) }

   #if 0
   oListBox:setColorBG( GraMakeRGBColor( {227,12,110} ) )
   oListBox:setColorBG( GraMakeRGBColor( { 27,12, 45} ) )
   #endif

   RETURN nil

/*----------------------------------------------------------------------*/

FUNCTION Build_StatusBar( oWnd )
   LOCAL oSBar, oPanel

   oSBar := XbpStatusBar():new( oWnd )
   oSBar:create( oWnd, , { 0,0 }, { oWnd:currentSize()[1],30 } )

   oPanel := oSBar:getItem( 1 )
   oPanel:caption  := "Harbour-QT-Xbase++ is Ready"
   oPanel:autosize := XBPSTATUSBAR_AUTOSIZE_SPRING

   RETURN nil

/*----------------------------------------------------------------------*/

FUNCTION Build_ScrollBar( oWnd )
   LOCAL oXbpH, oXbpV
   LOCAL aSize   := oWnd:currentSize()
   LOCAL nWidth  := aSize[ 1 ]
   LOCAL nHeight := aSize[ 2 ]
   LOCAL nFat    := 20

   oXbpH := XbpScrollbar():new()
   oXbpH:type  := XBPSCROLL_HORIZONTAL
   oXbpH:range := { 1, 100 }
   oXbpH:create( oWnd, , { 10,nHeight-50 }, { nWidth-40,nFat } )
   oXbpH:scroll := {|| oXbpV:setData( oXbpH:getData() ) }

   oXbpV := XbpScrollbar():new()
   oXbpV:type  := XBPSCROLL_VERTICAL
   oXbpV:range := { 1, 100 }
   oXbpV:create( oWnd, , { nWidth-30,10 }, { nFat,nHeight-60 } )
   oXbpV:scroll := {|| oXbpH:setData( oXbpV:getData() ) }

   RETURN nil

/*----------------------------------------------------------------------*/

FUNCTION Build_SLEs( oWnd )
   LOCAL oXbp
   LOCAL cVarA := "Test A", cVarB := "Test B"

   // Create second SLE, specify position using :new()
   oXbp              := XbpSLE():new( oWnd, , {30,150}, {100,30} )
   oXbp:tabStop      := .T.
   oXbp:bufferLength := 15
   oXbp:dataLink     := {|x| IIf( x==NIL, cVarA, cVarA := x ) }
   oXbp:create()
   oXbp:setData()
   //oXbp:setInputFocus  := { |x,y,oSLE| oSLE:getData(), Qt_QDebug( "Var A =" + cVarA ) }
   //oXbp:setInputFocus  := { |x,y,oSLE| oSLE:getData() }

   oXbp              := XbpSLE():new()
   oXbp:autoTab      := .T.
   oXbp:bufferLength := 20
   // Data code block containing assignment to LOCAL variable
   oXbp:dataLink     := {|x| IIf( x==NIL, cVarB, cVarB := x ) }
   oXbp:create( oWnd, , {30,200}, {100,30} )
   oXbp:setData()
   // Assign the value of the edit buffer to a LOCAL variable
   // when the input focus is lost
   oXbp:killInputFocus := { |x,y,oSLE| oSLE:getData(), MsgBox( "Var B =" + cVarB ) }

   RETURN nil

/*----------------------------------------------------------------------*/

FUNCTION Build_MLE( oWnd )
   LOCAL oMLE
   LOCAL cText := 'This is Xbase++ compatible implementation of XbpMLE()'

   // Create MLE, specify position using :create() and
   // assign data code block accessing LOCAL variable
   oMLE          := XbpMLE():new()
   oMLE:wordWrap := .t.
   oMLE:dataLink := {|x| IIf( x==NIL, cText, cText := x ) }
   oMLE:create( oWnd, , {10,10}, {oWnd:currentSize()[1]-25,oWnd:currentSize()[2]-45} )

   // Copy text from LOCAL variable into edit buffer
   // via :dataLink
   oMLE:setData()

   RETURN nil

/*----------------------------------------------------------------------*/

FUNCTION Build_SpinButtons( oWnd )
   LOCAL oSpinRed, oSpinGreen, oSpinBlue, bCallBack
   LOCAL nGreen := 5, nBlue := 12, nRed := 35
   LOCAL nX := 230, nY := 190

   // Callback code block
   bCallback := {|mp1, mp2, oXbp| nRed := oXbp:getData(), ;
                               RGB( nRed, nGreen, nBlue ) }

   // Create spinbutton for red (without using :dataLink)
   oSpinRed := XbpSpinButton():new( oWnd,, {nX,nY+00}, {100,40} )
   oSpinRed:align := 2
   oSpinRed:fastSpin := .T.
   oSpinRed:create()
   oSpinRed:setNumLimits( 0, 255 )
   oSpinRed:endSpin  := bCallback
   oSpinRed:keyboard := bCallback
   oSpinRed:setData( 121 )

   // Callback code block
   bCallback := {|mp1, mp2, oXbp| oXbp:getData(), ;
                       RGB( nRed, nGreen, nBlue ) }

   // Create spinbutton for green (using :dataLink)
   oSpinGreen := XbpSpinButton():new( oWnd,, {nX,nY+50}, {100,40} )
   oSpinGreen:align := 3
   oSpinGreen:create()
   oSpinGreen:setNumLimits( 0, 255 )
   oSpinGreen:dataLink := {|x| IIf( x==NIL, nGreen, nGreen := x ) }
   oSpinGreen:endSpin  := bCallback
   oSpinGreen:keyboard := bCallback
   oSpinGreen:setData()

   // Create spinbutton for blue (using :dataLink)
   // (Master is oSpinGreen)
   oSpinBlue := XbpSpinButton():new( oWnd,, {nX,nY+100}, {100,40} )
   oSpinBlue:master := oSpinGreen
   oSpinBlue:create()
   oSpinBlue:setNumLimits( 0, 255 )
   oSpinBlue:dataLink := {|x| IIf( x==NIL, nBlue, nBlue := x ) }
   oSpinBlue:endSpin  := bCallback
   oSpinBlue:keyboard := bCallback

   RETURN nil

/*----------------------------------------------------------------------*/

STATIC FUNCTION RGB( r, g, b )

   HB_SYMBOL_UNUSED( r )
   HB_SYMBOL_UNUSED( g )
   HB_SYMBOL_UNUSED( b )

   // Display a static window with flashing color of rgb

   RETURN nil

/*----------------------------------------------------------------------*/

STATIC FUNCTION Build_ComboBox( oWnd )
   LOCAL oCombo, i, bAction
   LOCAL cDay  := "< Monday >"
   LOCAL aDays := { "Monday" , "Tuesday"  , "Wednesday", "Thursday", ;
                    "Friday" , "Saturday" , "Sunday" }
   LOCAL aPNG  := { 'copy','cut','new','open','paste','save','new' }

   // Create combo box with drop down list box
   oCombo      := XbpCombobox():new()
   oCombo:type := XBPCOMBO_DROPDOWN
   //oCombo:editable := .f.
   oCombo:create( oWnd,, {30, 20}, {200, 30} )

   // Link data from entry field to LOCAL variable
   oCombo:XbpSLE:dataLink := {|x| IIf( x==NIL, cDay, cDay := x ) }
   oCombo:XbpSLE:setData()

   // Code block for selection:
   //  - assign to LOCAL variable using :getData()
   //  - display LOCAL variable using DispoutAt()
   bAction := {|mp1, mp2, obj| obj:XbpSLE:getData(), hb_outDebug( 'Highlighted: '+cDay ) }

   // Assign code block for selection with Up and Down keys
   oCombo:ItemMarked := bAction

   // Assign code block for selection by left mouse click in list box
   oCombo:ItemSelected := {|mp1, mp2, obj| obj:XbpSLE:getData(), hb_outDebug( 'Selected: '+cDay ) }

   // Copy data from array to combo box, then discard array
   FOR i := 1 TO 7
      oCombo:addItem( aDays[ i ] )
      #ifdef __HARBOUR__
      /*  the command below is not Xbase++ compatible - will be documented while extended */
      oCombo:setIcon( i, aPNG[ i ]+'.png' )
      #endif
   NEXT

   oCombo:XbpSLE:setData()
   RETURN nil

/*----------------------------------------------------------------------*/

FUNCTION Build_TreeView( oWnd )
   LOCAL i
   LOCAL oTree := XbpTreeView():new( oWnd, , {10,10}, {oWnd:currentSize()[1]-25,oWnd:currentSize()[2]-45} )

   oTree:hasLines   := .T.
   oTree:hasButtons := .T.
   oTree:create()
   oTree:itemCollapsed := {|oItem,aRect,oSelf| MsgBox( oItem:caption ) }

   FOR i := 1 TO 5
      WorkAreaInfo( oTree, i )
   NEXT

   RETURN nil

   ** Build the tree structure for a work area

PROCEDURE WorkAreaInfo( oTree, iIndex )
   LOCAL oArea, oStatus, oStruct

   // First level in the tree starts with oTree:rootItem
   oArea := oTree:rootItem:addItem( 'Alias '+hb_ntos( iIndex ) )

   // Second level in the tree begins with a XbpTreeViewItem
   // Create XbpTreeViewItem explicitly (1st possibility)
   oStatus         := XbpTreeViewItem():new()
   oStatus:caption := "STATUS"
   oStatus:create()

   oArea:addItem( oStatus )
   #ifdef __HARBOUR__
   oArea:setImage( 'copy.png' )
   #endif

   // Create XbpTreeViewItem implicitly (2nd possibility)
   oStruct := oArea:addItem( "STRUCTURE" )

   // Create third level in the tree
   WAStatus( oStatus, iIndex )
   WAStruct( oStruct, iIndex )

   RETURN

   ** Third level -> status information

PROCEDURE WAStatus( oItem, iIndex )
   oItem:addItem( "Bof() = "    + IIf( iIndex%2 == 0, ".T.", ".F." ) )
   oItem:addItem( "Eof() = "    + IIf( iIndex%3 == 0, ".T.", ".F." ) )
   oItem:addItem( "Found() = "  + IIf( iIndex%4 == 0, ".T.", ".F." ) )
   oItem:addItem( "Recno() = "  + Ltrim( Str( iIndex  ) ) )
   oItem:addItem( "LastRec() =" + Ltrim( Str( iIndex )+'....PPP' ) )

   RETURN

   ** Third level -> field information

PROCEDURE WAStruct( oItem, iIndex )
   LOCAL aStr := {}

   aadd( aStr, { 'Name__'+hb_ntos( iIndex ), 'C', 20, 0 } )
   aadd( aStr, { 'Birth' , 'D',  8, 0 } )
   aadd( aStr, { 'Salary', 'N', 10, 2 } )

   AEval( aStr, ;
     {|a,i,oSub| oSub := oItem:addItem( "FIELD_NAME = " + a[1] ), FieldStruct( oSub, a ) } )

   RETURN

   ** Create fourth level in the tree

PROCEDURE FieldStruct( oItem, aField )

   oItem:addItem( "FIELD_TYPE = " + aField[2] )
   oItem:addItem( "FIELD_LEN  = " + Str( aField[3], 3 ) )
   oItem:addItem( "FIELD_DEC  = " + Str( aField[4], 3 ) )

   RETURN

/*----------------------------------------------------------------------*/
