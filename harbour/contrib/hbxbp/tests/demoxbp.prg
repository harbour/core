/*
 * $Id$
 */

/*
 * Harbour Project source code:
 *
 * Copyright 2009 Pritpal Bedi <pritpal@vouchcac.com>
 * www - http://harbour-project.org
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
#include "set.ch"

#ifdef __XPP__
   #pragma library("XppUi2")
#endif

#ifndef __HARBOUR__
   #define HB_SYMBOL_UNUSED( x ) ( x := x )
#endif

/*----------------------------------------------------------------------*/

REQUEST HB_QTGUI

REQUEST DbfCdx

#define TAB_1   1
#define TAB_2   2
#define TAB_3   3
#define TAB_4   4
#define TAB_5   5
#define TAB_6   6
#define TAB_7   7
#define TAB_8   8

#define CRLF    chr( 13 )+chr( 10 )

#ifdef __HARBOUR__
THREAD STATIC oMLE         /* Change Font elsewhere */
#else
STATIC oMLE
#endif

/*----------------------------------------------------------------------*/

PROCEDURE Main()

   //hb_threadStart( {|| _BuildADialog() } )
   _BuildADialog()

   RETURN

/*----------------------------------------------------------------------*/

FUNCTION _BuildADialog()

   hb_gtReload( 'GUI' )
   BuildADialog()

   RETURN NIL

/*----------------------------------------------------------------------*/

PROCEDURE DispMem( cMessage )
   HB_SYMBOL_UNUSED( cMessage )
   HB_TRACE( HB_TR_DEBUG, cMessage )
   RETURN

/*----------------------------------------------------------------------*/

PROCEDURE BuildADialog()
   LOCAL oDlg, mp1, mp2, oXbp, nEvent, aSize, oDa, aTabs, oHTM
   LOCAL nThread := hb_threadId()
   //LOCAL oStat, aMenu, aTool, aBrow, aChek, a3Sta

DispMem( "At Startup of Thread" )

   /* Create Application Window */
   oDlg := GuiStdDialog( "Harbour - Xbase++ - QT Dialog  [ "+ hb_ntos( nThread )+" ]" )

DispMem( "oDlg := GuiStdDialog" )

   oDlg:close := {|| MsgBox( "You can also close me by pressing [ESC]" ), .T. }
   SetAppWindow( oDlg )
   oDlg:Show()

   oDa := oDlg:drawingArea

   /* Obtain desktop dimensions */
   aSize := AppDesktop():currentSize()
   /* Place on the center of desktop */
   oDlg:setPos( { ( aSize[ 1 ] - oDlg:currentSize()[ 1 ] ) / 2, ;
                  ( aSize[ 2 ] - oDlg:currentSize()[ 2 ] ) / 2 } )

   /* Make background color of :drawingArea different */
   oDa:setColorBG( GraMakeRGBColor( { 134,128,200 } ) )
   oDa:setFontCompoundName( "10.Tohama italics" )
   //oDa:setColorFG( GraMakeRGBColor( { 255,255,255 } ) )

   #ifdef __HARBOUR__
   oDA:hbLayout := HBPLAYOUT_TYPE_VERTBOX
   #endif

   /* Install menu system */
   Build_MenuBar( oDlg )
DispMem( "Build_MenuBar" )
   /* Install Statusbar */
   Build_StatusBar( oDa )
DispMem( "Build_StatusBar" )
   /* Install Toolbar */
   Build_ToolBar( oDa )
DispMem( "Build_ToolBar" )
   /* Install Tab Pages */
   aTabs := Build_TabPages( oDa )
DispMem( "Build_TabPages" )
   /* Build XBPBrowse() */
   Build_Browse( aTabs[ TAB_1 ] )
DispMem( "Build_Browse" )
   /* Install Multi-Line Edit */
   oMLE := Build_MLE( aTabs[ TAB_2 ] )
DispMem( "Build_MLE" )
   /* Install checkboxes */
   Build_CheckBox( aTabs[ TAB_3 ] )
DispMem( "Build_CheckBox" )
   /* Install 3state checkboxes */
   Build_3State( aTabs[ TAB_3 ] )
DispMem( "Build_3State" )
   /* Install Radio Buttons */
   Build_RadioButton( aTabs[ TAB_3 ] )
DispMem( "Build_RadioButton" )
   /* Install Spin Buttons */
   Build_SpinButtons( aTabs[ TAB_3 ] )
DispMem( "Build_SpinButton" )
   /* Install TreeView */
   Build_TreeView( aTabs[ TAB_4 ] )
DispMem( "Build_TreeView" )
   /* Install ListBox */
   Build_ListBox( aTabs[ TAB_5 ] )
DispMem( "Build_ListBox" )
   /* Install Combo Box */
   Build_ComboBox( aTabs[ TAB_5 ] )
DispMem( "Build_ComboBox" )
   /* Install Push Buttons */
   Build_PushButton( aTabs[ TAB_5 ] )
DispMem( "Build_PushButton" )
   /* Install Single Line Edits */
   Build_SLEs( aTabs[ TAB_5 ] )
DispMem( "Build_SLEs" )
   /* Install ScrollBar */
   Build_ScrollBar( aTabs[ TAB_5 ] )
DispMem( "Build_ScrollBars" )
   /* Build RTF */
   Build_Rtf( aTabs[ TAB_6 ] )
DispMem( "Build_Rtf" )
   /* Build HTML Viewer */
   //oHTM := Build_HTMLViewer( aTabs[ TAB_7 ] )
DispMem( "Build_HTMLViewer" )
   /* Build Statics */
   Build_Statics( aTabs[ TAB_8 ] )
DispMem( "Build_Statics" )
   /* Present the dialog on the screen */
   oDlg:Show()

   /* Enter Xbase++ Event Loop - working */
   DO WHILE .t.
      nEvent := AppEvent( @mp1, @mp2, @oXbp )

      IF ( nEvent == xbeP_Close ) .OR. ( nEvent == xbeP_Keyboard .and. mp1 == xbeK_ESC )
         EXIT
      ELSEIF nEvent == xbeP_Keyboard .and. mp1 == xbeK_F1
         #if 0
         oHtm:setHTML( '<html><h1>Direct HTML Injection</h1><p><font color="#ab00ff" size="16">'+;
                       'This HTML content</font> is pushed dynamically with<br><br>:setHTML()</br></br>.</html>' )
         #endif
      ENDIF

      oXbp:handleEvent( nEvent, mp1, mp2 )
   ENDDO

   oDlg:destroy()
   oHTM := NIL

   RETURN

/*----------------------------------------------------------------------*/

PROCEDURE AppSys()
   RETURN

/*----------------------------------------------------------------------*/

#ifdef __XPP__
FUNCTION Hb_Symbol_Unused()  ; RETURN nil
FUNCTION Hb_NtoS( n )        ; RETURN ltrim( str( n ) )
FUNCTION Hb_ThreadStart()    ; RETURN nil
FUNCTION hb_DirBase()        ; RETURN CurDir()
#endif

/*----------------------------------------------------------------------*/

STATIC FUNCTION uiXtoS( xVar )
   LOCAL cType

   cType := valtype( xVar )
   DO CASE
   CASE cType == "N"
      RETURN str( xVar )
   CASE cType == "D"
      RETURN dtoc( xVar )
   CASE cType == "L"
      RETURN IF( xVar, "Yes", "No" )
   CASE cType == "M"
      RETURN xVar
   CASE cType == "C"
      RETURN xVar
   CASE cType == "A"
      RETURN "A:"+hb_ntos( len( xVar ) )
   CASE cType == "O"
      RETURN "[OBJECT]"
   OTHERWISE
      RETURN "["+cType+"]"
   ENDCASE

   RETURN xVar

/*----------------------------------------------------------------------*/

FUNCTION MyDebug( p1, p2, p3, p4, p5, p6, p7, p8, p9, p10 )
   LOCAL s

   s := uiXtoS( p1 ) + CRLF
   s += uiXtoS( p2 ) + CRLF
   s += uiXtoS( p3 ) + CRLF
   s += uiXtoS( p4 ) + CRLF
   s += uiXtoS( p5 ) + CRLF
   s += uiXtoS( p6 ) + CRLF
   s += uiXtoS( p7 ) + CRLF
   s += uiXtoS( p8 ) + CRLF
   s += uiXtoS( p9 ) + CRLF
   s += uiXtoS( p10 )

   MsgBox( s )

   RETURN nil

/*----------------------------------------------------------------------*/

STATIC FUNCTION PP_Debug( oXbp )
   LOCAL aPP := oXbp:setPresParam()
   LOCAL s := ''

   aeval( aPP, {|e_| s += ( hb_ntos( e_[ 1 ] ) +' '+ valtype( e_[ 2 ] ) +' '+ ;
        IF( valtype( e_[ 2 ] )=='N', hb_ntos( e_[ 2 ] ), ' ' ) + ';  '+ CRLF ) } )

   MsgBox( s )

   RETURN nil

/*----------------------------------------------------------------------*/

STATIC FUNCTION GuiStdDialog( cTitle )
   LOCAL oDlg

   DEFAULT cTitle TO "Standard Dialog Window"

   oDlg          := XbpDialog():new( , , {10,10}, {900,500}, , .f. )

   /* NOTE: method to install the windows icon is bit different than Windows */
   /* So curretly we can only place disk icon file only */
   oDlg:icon     := hb_DirBase() + "test.ico"

   /* TODO: still not implemented*/
   oDlg:taskList := .T.

   oDlg:title    := cTitle
   oDlg:create()

   RETURN oDlg

/*----------------------------------------------------------------------*/

STATIC FUNCTION Build_MenuBar( oDlg )
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

   #ifdef __HARBOUR__
   #if 1
      oSubMenu := XbpMenu():new( oMenuBar ):create()
      oSubMenu:title := "~Dialogs"
      #if 1             /*  T H R E D E D   D I A L O G */
         oSubMenu:addItem( { "~One More Instance"+ chr( K_TAB ) +"Ctrl+M", ;
                                    {|| hb_threadStart( {|| BuildADialog() } ) } } )
      #else
         oSubMenu:addItem( { "~One More Instance"+ chr( K_TAB )+ "Ctrl+M", {|| _BuildADialog() } } )
      #endif
   #endif
   #endif
   oMenuBar:addItem( { oSubMenu, NIL } )

   oSubMenu := XbpMenu():new( oMenuBar ):create()
   oSubMenu:title := "~Miscellaneous"
   oSubMenu:addItem( { "Convert Images - XbpBitmap()", {|| Build_Bitmap( oDlg ) } } )
   oMenuBar:addItem( { oSubMenu, NIL } )

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
   LOCAL oTBar, s, txt_:= {}

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
      oTBar:addItem( "Save"        , hb_DirBase() + "new.png"  , , , , , "1" )
      oTBar:addItem( "Open"        , hb_DirBase() + "open.png" , , , , , "2" )
      oTBar:addItem( "Font Dialog" , hb_DirBase() + "copy.png" , , , , , "3" )
      oTBar:addItem( "Print Dialog", hb_DirBase() + "print.png", , , , , "4" )

   #else
      oTBar:addItem( "Save"        )//, 100 )
      oTBar:addItem( "Open"        )//, 101 )
      oTBar:addItem( "Font Dialog" )
      oTBar:addItem( "Print Dialog" )
   #endif

   oTBar:transparentColor := GRA_CLR_INVALID
   oTBar:buttonClick := {|oButton| ExeToolbar( oButton, oDa ) }

   #ifdef __HARBOUR__
   aadd( txt_, ' ' )
   aadd( txt_, ' QToolBar {                                                    ' )
   aadd( txt_, '     background: cyan;                                         ' )
   aadd( txt_, '     spacing: 3px; /* spacing between items in the tool bar */ ' )
   aadd( txt_, ' }                                                             ' )
   aadd( txt_, '                                                               ' )
   aadd( txt_, ' QToolBar::handle {                                            ' )
   aadd( txt_, '     image: url(save.png);                                     ' )
   aadd( txt_, ' }                                                             ' )
   aadd( txt_, ' ' )

   s := ""
   aeval( txt_, {|e| s += e + chr( 13 )+chr( 10 ) } )

   oTBar:setStyleSheet( s )
   #endif

   RETURN nil

/*----------------------------------------------------------------------*/

STATIC FUNCTION ExeToolbar( oButton, oDa )

   DO CASE
   CASE oButton:caption == "Save"
      Build_FileDialog( oDA,"save" )
   CASE oButton:caption == "Open"
      Build_FileDialog( oDA,"open" )
   CASE oButton:caption == "Font Dialog"
      Build_FontDialog( oDa )
   CASE oButton:caption == "Print Dialog"
      Build_PrintDialog( oDa )
   ENDCASE

   RETURN nil

/*----------------------------------------------------------------------*/

FUNCTION Build_StatusBar( oWnd )
   LOCAL oSBar, oPanel

   oSBar := XbpStatusBar():new( oWnd )
   oSBar:create( oWnd, , { 0,0 }, { oWnd:currentSize()[1],30 } )

   oPanel := oSBar:getItem( 1 )
   oPanel:caption  := "Harbour-QT-Xbase++ is Ready"
   oPanel:autosize := XBPSTATUSBAR_AUTOSIZE_SPRING

   #ifdef __XPP__
   oSBar:setPointer( , XBPSTATIC_SYSICON_SIZEWE, XBPWINDOW_POINTERTYPE_SYSPOINTER )
   #else
   oSBar:setPointer( , hb_DirBase() + "vr.png", XBPWINDOW_POINTERTYPE_ICON )
   #endif

   RETURN nil

/*----------------------------------------------------------------------*/

FUNCTION Build_TabPages( oWnd )
   LOCAL nHeight := 390
   LOCAL aTabs   := { NIL,NIL,NIL,NIL,NIL,NIL,NIL,NIL }
   LOCAL aPos    := { 20,20 }
   //LOCAL aPos    := { 510, 20 }
   LOCAL aSize   := { 860, nHeight }
   //LOCAL aSize   := { 360, nHeight }

   aTabs[ TAB_1 ] := XbpTabPage():new( oWnd, , aPos, aSize, , .t. )
   aTabs[ TAB_1 ]:caption    := "Brw"
   aTabs[ TAB_1 ]:preOffset  := 10
   aTabs[ TAB_1 ]:postOffset := 300
   aTabs[ TAB_1 ]:minimized  := .F.
   aTabs[ TAB_1 ]:create()
   aTabs[ TAB_1 ]:TabActivate := SetMaximized( aTabs, 1 )
   #ifdef __HARBOUR__
   aTabs[ TAB_1 ]:hbLayout := HBPLAYOUT_TYPE_VERTBOX
   #endif

   aTabs[ TAB_2 ] := XbpTabPage():new( oWnd, , aPos, aSize, , .t. )
   aTabs[ TAB_2 ]:caption    := "MLE"
   aTabs[ TAB_2 ]:preOffset  := 20
   aTabs[ TAB_2 ]:postOffset := 120
   aTabs[ TAB_2 ]:create()
   aTabs[ TAB_2 ]:TabActivate := SetMaximized( aTabs, 2 )
   #ifdef __HARBOUR__
   aTabs[ TAB_2 ]:hbLayout := HBPLAYOUT_TYPE_VERTBOX
   #endif

   aTabs[ TAB_3 ] := XbpTabPage():new( oWnd, , aPos, aSize, , .t. )
   aTabs[ TAB_3 ]:caption    := "Btns"
   aTabs[ TAB_3 ]:preOffset  := 40
   aTabs[ TAB_3 ]:postOffset := 100
   aTabs[ TAB_3 ]:create()
   aTabs[ TAB_3 ]:TabActivate := SetMaximized( aTabs, 3 )
   #ifdef __HARBOUR__
   aTabs[ TAB_3 ]:hbLayout := HBPLAYOUT_TYPE_HORZBOX
   #endif

   aTabs[ TAB_4 ] := XbpTabPage():new( oWnd, , aPos, aSize, , .t. )
   aTabs[ TAB_4 ]:caption    := "Tree"
   aTabs[ TAB_4 ]:preOffset  := 60
   aTabs[ TAB_4 ]:postOffset := 80
   aTabs[ TAB_4 ]:create()
   aTabs[ TAB_4 ]:TabActivate := SetMaximized( aTabs, 4 )
   aTabs[ TAB_4 ]:setColorBG( GraMakeRGBColor( {198,198,198} ) )
   #ifdef __HARBOUR__
   aTabs[ TAB_4 ]:hbLayout := HBPLAYOUT_TYPE_HORZBOX
   #endif

   aTabs[ TAB_5 ] := XbpTabPage():new( oWnd, , aPos, aSize, , .t. )
   aTabs[ TAB_5 ]:minimized  := .F.
   aTabs[ TAB_5 ]:caption    := "Lists"
   aTabs[ TAB_5 ]:preOffset  := 80
   aTabs[ TAB_5 ]:postOffset := 60
   aTabs[ TAB_5 ]:create()
   aTabs[ TAB_5 ]:TabActivate := SetMaximized( aTabs, 5 )
   aTabs[ TAB_5 ]:setPointer( , XBPSTATIC_SYSICON_SIZENESW, XBPWINDOW_POINTERTYPE_SYSPOINTER )
   /* comment our following line to position tabs at the bottom */
   /* aTabs[ TAB_5 ]:type := XBPTABPAGE_TAB_BOTTOM */

   aTabs[ TAB_6 ] := XbpTabPage():new( oWnd, , aPos, aSize, , .t. )
   aTabs[ TAB_6 ]:caption    := "Rtf"
   aTabs[ TAB_6 ]:preOffset  := 100
   aTabs[ TAB_6 ]:postOffset := 40
   aTabs[ TAB_6 ]:create()
   aTabs[ TAB_6 ]:TabActivate := SetMaximized( aTabs, 6 )
   #ifdef __HARBOUR__
   // aTabs[ TAB_6 ]:hbLayout := HBPLAYOUT_TYPE_VERTBOX
   #endif

   aTabs[ TAB_7 ] := XbpTabPage():new( oWnd, , aPos, aSize, , .t. )
   aTabs[ TAB_7 ]:caption    := "Web"
   aTabs[ TAB_7 ]:preOffset  := 120
   aTabs[ TAB_7 ]:postOffset := 20
   aTabs[ TAB_7 ]:minimized  := .F.
   aTabs[ TAB_7 ]:create()
   aTabs[ TAB_7 ]:TabActivate := SetMaximized( aTabs, 7 )
   #ifdef __HARBOUR__
   aTabs[ TAB_7 ]:hbLayout := HBPLAYOUT_TYPE_VERTBOX
   #endif

   aTabs[ TAB_8 ] := XbpTabPage():new( oWnd, , aPos, aSize, , .t. )
   aTabs[ TAB_8 ]:caption    := "Statics"
   aTabs[ TAB_8 ]:preOffset  := 140
   aTabs[ TAB_8 ]:postOffset := 0
   aTabs[ TAB_8 ]:minimized  := .F.
   aTabs[ TAB_8 ]:create()
   aTabs[ TAB_8 ]:TabActivate := SetMaximized( aTabs, 8 )

   RETURN aTabs

/*----------------------------------------------------------------------*/

FUNCTION Build_CheckBox( oWnd )
   LOCAL oXbp

   oXbp := XbpCheckbox():new()
   oXbp:caption := "A"
   oXbp:create( oWnd, , {30,30}, {100,30} )

   // Determine state using mp1
   oXbp:selected := ;
      {| mp1, mp2, oChk| HB_SYMBOL_UNUSED( mp2 ), HB_SYMBOL_UNUSED( oChk ), ;
                   MsgBox( "Checkbox A", IIf( mp1, "selected", "not selected" ) ) }

   // Create second checkbox, specify position using :new()
   oXbp := XbpCheckbox():new( oWnd, , {30,70}, {100,30} )
   oXbp:caption := "B"
   oXbp:create()

   // Determine state using :getData()
   oXbp:selected := ;
      {| mp1, mp2, oChk| HB_SYMBOL_UNUSED( mp1 ), HB_SYMBOL_UNUSED( mp2 ), ;
             MsgBox( "Checkbox B", ;
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
   oXbp:selected := {| mp1, mp2, oBtn| HB_SYMBOL_UNUSED( mp2 ), HB_SYMBOL_UNUSED( oBtn ), MsgBox( "3State A", aState[ mp1+1 ] ) }

   // Create second 3State Button, passing the position to :new()
   //
   oXbp := Xbp3State():new( oWnd, , {130,70}, {100,30} )
   oXbp:caption := "3State B"
   oXbp:create()
   // Determine current state using :getData()
   oXbp:selected := {| mp1, mp2, oBtn|  HB_SYMBOL_UNUSED( mp1 ), HB_SYMBOL_UNUSED( mp2 ), MsgBox( "3State B", aState[ oBtn:getData()+1 ] ) }

   RETURN nil

/*----------------------------------------------------------------------*/

FUNCTION Build_RadioButton( oStatic )
   LOCAL bSelected, oRadio

   // Display which radiobutton is selected
   bSelected := {|mp1,mp2,obj| HB_SYMBOL_UNUSED( mp1 ), HB_SYMBOL_UNUSED( mp2 ), MsgBox( obj:caption ) }

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

STATIC FUNCTION SetMaximized( aTabs, nMax )
   RETURN {|| aeval( aTabs, {|o,i| IF( i != nMax, o:minimize(), ) } ), aTabs[ nMax ]:maximize() }

/*----------------------------------------------------------------------*/

FUNCTION Build_ListBox( oWnd )
   LOCAL oListBox, aItems

   aItems := { "India", "United States", "England", "Japan", "Hungary", "Argentina", "China" }

   // Create list box that allows multiple selections

   oListBox := XbpListBox():new()
   oListBox:markMode := XBPLISTBOX_MM_MULTIPLE
   oListBox:create( oWnd, , {10,10}, {150,320} )

   // Copy field names from the DbStruct() array to the list box
   aeval( aItems, {|e| oListBox:addItem( e ) } )

   // Code block for list box selection:
   oListBox:ItemSelected := {|mp1, mp2, obj| HB_SYMBOL_UNUSED( obj ), mp1 := oListBox:getData(), ;
                               iif( !empty( mp1 ), mp2 := oListBox:getItem( mp1[ 1 ] ), mp2 := "Nothing" ), ;
                                   MsgBox( "itemSelected: " + mp2 ) }
   oListBox:setColorFG( GraMakeRGBColor( {227,12,110} ) )
   oListBox:setColorBG( GraMakeRGBColor( {50,45,170} ) )

   oListBox:setPointer( , XBPSTATIC_SYSICON_MOVE, XBPWINDOW_POINTERTYPE_SYSPOINTER )

   RETURN nil

/*----------------------------------------------------------------------*/

STATIC FUNCTION Build_ComboBox( oWnd )
   LOCAL oCombo, i, bAction
   LOCAL cDay  := "< Monday >"
   LOCAL aDays := { "Monday" , "Tuesday"  , "Wednesday", "Thursday", ;
                    "Friday" , "Saturday" , "Sunday" }
   LOCAL aPNG  := { "copy", "cut", "new", "open", "paste", "save", "new" }

   // Create combo box with drop down list box
   oCombo      := XbpCombobox():new()
   oCombo:type := XBPCOMBO_DROPDOWN
   //oCombo:editable := .f.
   oCombo:create( oWnd,, {180, 10}, {200, 30} )

   // Link data from entry field to LOCAL variable
   oCombo:XbpSLE:dataLink := {|x| IIf( x==NIL, cDay, cDay := x ) }
   oCombo:XbpSLE:setData()

   // Code block for selection:
   //  - assign to LOCAL variable using :getData()
   //  - display LOCAL variable using DispoutAt()
   bAction := {|mp1, mp2, obj| HB_SYMBOL_UNUSED( mp1 ), HB_SYMBOL_UNUSED( mp2 ), obj:XbpSLE:getData() }

   // Assign code block for selection with Up and Down keys
   oCombo:ItemMarked := bAction

   // Assign code block for selection by left mouse click in list box
   oCombo:ItemSelected := {|mp1, mp2, obj| HB_SYMBOL_UNUSED( mp1 ), HB_SYMBOL_UNUSED( mp2 ), obj:XbpSLE:getData() }

   // Copy data from array to combo box, then discard array
   FOR i := 1 TO 7
      oCombo:addItem( aDays[ i ] )
      #ifdef __HARBOUR__
      /*  the command below is not Xbase++ compatible - will be documented while extended */
      oCombo:setIcon( i, hb_DirBase() + aPNG[ i ] + ".png" )
      #endif
   NEXT

   oCombo:XbpSLE:setData()
   RETURN nil

/*----------------------------------------------------------------------*/

FUNCTION Build_PushButton( oDA )
   LOCAL oXbp

    oXbp := XbpPushButton():new( oDA )
    oXbp:caption := "A"
    oXbp:create( , , {180,200}, {90,40} )
    oXbp:activate:= {|| MsgBox( "Pushbutton A" ) }
    /* Harbour supports presentation colors */
    //oXbp:setColorBG( GraMakeRGBColor( {133,240,90} ) )
    oXbp:setColorBG( GraMakeRGBColor( {0,0,255} ) )

    oXbp := XbpPushButton():new( oDA )
    oXbp:caption := hb_DirBase() + "new.png"
    oXbp:create( , , {290,200}, {90,40} )
    oXbp:activate:= {|| MsgBox( "Pushbutton B" ) }
    /* Harbour supports presentation colors */
    oXbp:setColorBG( GraMakeRGBColor( {255,255,0} ) )

    RETURN nil

/*----------------------------------------------------------------------*/

FUNCTION Build_SLEs( oWnd )
   LOCAL oXbp
   LOCAL cVarA := "Test A", cVarB := "Test B"

   // Create second SLE, specify position using :new()
   oXbp              := XbpSLE():new( oWnd, , {180,300}, {90,30} )
   oXbp:tabStop      := .T.
   oXbp:bufferLength := 15
   oXbp:dataLink     := {|x| IIf( x==NIL, cVarA, cVarA := x ) }
   oXbp:create()
   oXbp:setData()
   //oXbp:setInputFocus  := { |x,y,oSLE| oSLE:getData(), Qt_QDebug( "Var A =" + cVarA ) }
   //oXbp:setInputFocus  := { |x,y,oSLE| oSLE:getData() }

   oXbp:setColorBG( GraMakeRGBColor( { 170,170,170 } ) )

   oXbp              := XbpSLE():new()
   oXbp:autoTab      := .T.
   oXbp:bufferLength := 20
   // Data code block containing assignment to LOCAL variable
   oXbp:dataLink     := {|x| IIf( x==NIL, cVarB, cVarB := x ) }
   oXbp:create( oWnd, , {290,300}, {90,30} )
   oXbp:setData()
   // Assign the value of the edit buffer to a LOCAL variable
   // when the input focus is lost
   oXbp:killInputFocus := { |mp1,mp2,oSLE| HB_SYMBOL_UNUSED( mp1 ), HB_SYMBOL_UNUSED( mp2 ),  oSLE:getData() }

   oXbp:setColorBG( GraMakeRGBColor( { 190,190,190 } ) )

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
   oXbpH:range := { 1, 100 }           //50
   oXbpH:create( oWnd, , { 10,nHeight-50 }, { nWidth-40,nFat } )
   oXbpH:scroll := {|| oXbpV:setData( oXbpH:getData() ) }

   oXbpV := XbpScrollbar():new()
   oXbpV:type  := XBPSCROLL_VERTICAL
   oXbpV:range := { 1, 100 }
   oXbpV:create( oWnd, , { nWidth-30,10 }, { nFat,nHeight-60 } )
   oXbpV:scroll := {|| oXbpH:setData( oXbpV:getData() ) }

   RETURN nil

/*----------------------------------------------------------------------*/

FUNCTION Build_MLE( oWnd )
   LOCAL cText := "This is Xbase++ compatible implementation of XbpMLE()"

   // Create MLE, specify position using :create() and
   // assign data code block accessing LOCAL variable
   oMLE          := XbpMLE():new()
   oMLE:wordWrap := .t.
   oMLE:dataLink := {|x| IIf( x==NIL, cText, cText := x ) }
   oMLE:create( oWnd, , {10,10}, {oWnd:currentSize()[1]-25,oWnd:currentSize()[2]-45} )

   // Copy text from LOCAL variable into edit buffer
   // via :dataLink
   oMLE:setData()

   oMLE:setColorBG( GraMakeRGBColor( { 190,190,0 } ) )
   oMLE:setColorFG( GraMakeRGBColor( { 0,0,0 } ) )
   oMLE:setFontCompoundName( "14.Courier bold" )

   RETURN oMLE

/*----------------------------------------------------------------------*/

FUNCTION Build_SpinButtons( oWnd )
   LOCAL oSpinRed, oSpinGreen, oSpinBlue, bCallBack
   LOCAL nGreen := 5, nBlue := 12, nRed := 35
   LOCAL nX := 230, nY := 190

   // Callback code block
   bCallback := {|mp1, mp2, oXbp| HB_SYMBOL_UNUSED( mp1 ), HB_SYMBOL_UNUSED( mp2 ), nRed := oXbp:getData(), ;
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
   bCallback := {|mp1, mp2, oXbp| HB_SYMBOL_UNUSED( mp1 ), HB_SYMBOL_UNUSED( mp2 ), oXbp:getData(), ;
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
   RETURN GraMakeRGBColor( { b,g,r } )           /* a bug in Qt */

/*----------------------------------------------------------------------*/

FUNCTION Build_TreeView( oWnd )
   LOCAL i, oTree, oTree1
   LOCAL sz_:= oWnd:currentSize()
   LOCAL nMid := sz_[ 1 ] / 2

   /* Style Sheet Rendering */
   oTree := XbpTreeView():new( oWnd, , {10,10}, {nMid-15,sz_[2]-45} )
   oTree:hasLines   := .T.
   oTree:hasButtons := .T.
   oTree:create()
   oTree:itemCollapsed := {|oItem,aRect,oSelf| HB_SYMBOL_UNUSED( aRect ), HB_SYMBOL_UNUSED( oSelf ), MsgBox( oItem:caption ) }
   #ifdef __HARBOUR__
   oTree:setStyleSheet( GetTreeStyleSheet() )
   #endif
   FOR i := 1 TO 5
      WorkAreaInfo( oTree, i )
   NEXT

   /* Traditional Rendering */
   oTree1 := XbpTreeView():new( oWnd, , {nMid+10,10}, {nMid-25,sz_[2]-45} )
   oTree1:hasLines   := .T.
   oTree1:hasButtons := .T.
   oTree1:create()
   FOR i := 1 TO 5
      WorkAreaInfo( oTree1, i )
   NEXT

   RETURN nil

   ** Build the tree structure for a work area

PROCEDURE WorkAreaInfo( oTree, iIndex )
   LOCAL oArea, oStatus, oStruct

   // First level in the tree starts with oTree:rootItem
   oArea := oTree:rootItem:addItem( "Alias "+hb_ntos( iIndex ) )

   // Second level in the tree begins with a XbpTreeViewItem
   // Create XbpTreeViewItem explicitly (1st possibility)
   oStatus         := XbpTreeViewItem():new()
   oStatus:caption := "STATUS"
   oStatus:create()

   oArea:addItem( oStatus )
   #ifdef __HARBOUR__
   oArea:setImage( hb_DirBase() + "copy.png" )
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

   aadd( aStr, { "Name__"+hb_ntos( iIndex ), 'C', 20, 0 } )
   aadd( aStr, { "Birth" , 'D',  8, 0 } )
   aadd( aStr, { "Salary", 'N', 10, 2 } )

   AEval( aStr, ;
     {|a,i,oSub| HB_SYMBOL_UNUSED( i ), oSub := oItem:addItem( "FIELD_NAME = " + a[1] ), FieldStruct( oSub, a ) } )

   RETURN

   ** Create fourth level in the tree

PROCEDURE FieldStruct( oItem, aField )

   oItem:addItem( "FIELD_TYPE = " + aField[2] )
   oItem:addItem( "FIELD_LEN  = " + Str( aField[3], 3 ) )
   oItem:addItem( "FIELD_DEC  = " + Str( aField[4], 3 ) )

   RETURN

FUNCTION GetTreeStyleSheet()
   LOCAL s := "", txt_:={}

   aadd( txt_, 'QTreeView {                                                                                      ' )
   aadd( txt_, '     alternate-background-color: yellow;                                                         ' )
   aadd( txt_, ' }                                                                                               ' )
   aadd( txt_, '                                                                                                 ' )
   aadd( txt_, ' QTreeView {                                                                                     ' )
   aadd( txt_, '     show-decoration-selected: 1;                                                                ' )
   aadd( txt_, ' }                                                                                               ' )
   aadd( txt_, '                                                                                                 ' )
   aadd( txt_, ' QTreeView::item {                                                                               ' )
   aadd( txt_, '      border: 1px solid #d9d9d9;                                                                 ' )
   aadd( txt_, '     border-top-color: transparent;                                                              ' )
   aadd( txt_, '     border-bottom-color: transparent;                                                           ' )
   aadd( txt_, ' }                                                                                               ' )
   aadd( txt_, '                                                                                                 ' )
   aadd( txt_, ' QTreeView::item:hover {                                                                         ' )
   aadd( txt_, '     background: qlineargradient(x1: 0, y1: 0, x2: 0, y2: 1, stop: 0 #e7effd, stop: 1 #cbdaf1);  ' )
   aadd( txt_, '     border: 1px solid #bfcde4;                                                                  ' )
   aadd( txt_, ' }                                                                                               ' )
   aadd( txt_, '                                                                                                 ' )
   aadd( txt_, ' QTreeView::item:selected {                                                                      ' )
   aadd( txt_, '     border: 1px solid #567dbc;                                                                  ' )
   aadd( txt_, ' }                                                                                               ' )
   aadd( txt_, '                                                                                                 ' )
   aadd( txt_, ' QTreeView::item:selected:active{                                                                ' )
   aadd( txt_, '     background: qlineargradient(x1: 0, y1: 0, x2: 0, y2: 1, stop: 0 #6ea1f1, stop: 1 #567dbc);  ' )
   aadd( txt_, ' }                                                                                               ' )
   aadd( txt_, '                                                                                                 ' )
   aadd( txt_, ' QTreeView::item:selected:!active {                                                              ' )
   aadd( txt_, '     background: qlineargradient(x1: 0, y1: 0, x2: 0, y2: 1, stop: 0 #6b9be8, stop: 1 #577fbf);  ' )
   aadd( txt_, ' }                                                                                               ' )
   aadd( txt_, '                                                                                                 ' )
   aadd( txt_, ' QTreeView::branch {                                                                             ' )
   aadd( txt_, '         background: palette(base);                                                              ' )
   aadd( txt_, ' }                                                                                               ' )
   aadd( txt_, '                                                                                                 ' )
   aadd( txt_, ' QTreeView::branch:has-siblings:!adjoins-item {                                                  ' )
   aadd( txt_, '         background: cyan;                                                                       ' )
   aadd( txt_, ' }                                                                                               ' )
   aadd( txt_, '                                                                                                 ' )
   aadd( txt_, ' QTreeView::branch:has-siblings:adjoins-item {                                                   ' )
   aadd( txt_, '         background: red;                                                                        ' )
   aadd( txt_, ' }                                                                                               ' )
   aadd( txt_, '                                                                                                 ' )
   aadd( txt_, ' QTreeView::branch:!has-children:!has-siblings:adjoins-item {                                    ' )
   aadd( txt_, '         background: blue;                                                                       ' )
   aadd( txt_, ' }                                                                                               ' )
   aadd( txt_, '                                                                                                 ' )
   aadd( txt_, ' QTreeView::branch:closed:has-children:has-siblings {                                            ' )
   aadd( txt_, '         background: pink;                                                                       ' )
   aadd( txt_, ' }                                                                                               ' )
   aadd( txt_, '                                                                                                 ' )
   aadd( txt_, ' QTreeView::branch:has-children:!has-siblings:closed {                                           ' )
   aadd( txt_, '         background: gray;                                                                       ' )
   aadd( txt_, ' }                                                                                               ' )
   aadd( txt_, '                                                                                                 ' )
   aadd( txt_, ' QTreeView::branch:open:has-children:has-siblings {                                              ' )
   aadd( txt_, '         background: magenta;                                                                    ' )
   aadd( txt_, ' }                                                                                               ' )
   aadd( txt_, '                                                                                                 ' )
   aadd( txt_, ' QTreeView::branch:open:has-children:!has-siblings {                                             ' )
   aadd( txt_, '         background: green;                                                                      ' )
   aadd( txt_, ' }                                                                                               ' )
   aadd( txt_, '                                                                                                 ' )
   aadd( txt_, '                                                                                                 ' )
   aadd( txt_, 'vline.png   branch-more.png   branch-end.png   branch-closed.png   branch-open.png               ' )
   aadd( txt_, ' QTreeView::branch:has-siblings:!adjoins-item {                                                  ' )
   aadd( txt_, '     border-image: url(vline.png) 0;                                                             ' )
   aadd( txt_, ' }                                                                                               ' )
   aadd( txt_, '                                                                                                 ' )
   aadd( txt_, ' QTreeView::branch:has-siblings:adjoins-item {                                                   ' )
   aadd( txt_, '     border-image: url(branch-more.png) 0;                                                       ' )
   aadd( txt_, ' }                                                                                               ' )
   aadd( txt_, '                                                                                                 ' )
   aadd( txt_, ' QTreeView::branch:!has-children:!has-siblings:adjoins-item {                                    ' )
   aadd( txt_, '     border-image: url(branch-end.png) 0;                                                        ' )
   aadd( txt_, ' }                                                                                               ' )
   aadd( txt_, '                                                                                                 ' )
   aadd( txt_, ' QTreeView::branch:has-children:!has-siblings:closed,                                            ' )
   aadd( txt_, ' QTreeView::branch:closed:has-children:has-siblings {                                            ' )
   aadd( txt_, '         border-image: none;                                                                     ' )
   aadd( txt_, '         image: url(branch-closed.png);                                                          ' )
   aadd( txt_, ' }                                                                                               ' )
   aadd( txt_, '                                                                                                 ' )
   aadd( txt_, ' QTreeView::branch:open:has-children:!has-siblings,                                              ' )
   aadd( txt_, ' QTreeView::branch:open:has-children:has-siblings  {                                             ' )
   aadd( txt_, '         border-image: none;                                                                     ' )
   aadd( txt_, '         image: url(branch-open.png);                                                            ' )
   aadd( txt_, ' } ' )

   aeval( txt_, {|e| s += e + chr( 13 )+chr( 10 ) } )

   RETURN s

/*----------------------------------------------------------------------*/

FUNCTION Build_Statics( oWnd )
   LOCAL oGrp,oLbl, oLin, oBox, oBmp
   LOCAL nC1 := 10, nC2 := 45, nC3 := 110, nC4 := 175
   LOCAL nW := 50, nH := 50, nG := 10
   LOCAL nT := 20

   oGrp := XbpStatic():new( oWnd, , {250,10}, {240,oWnd:currentSize[ 2 ]-45} )
   oGrp:type := XBPSTATIC_TYPE_GROUPBOX
   oGrp:caption := " Harbour-QT-Statics "
   oGrp:create()
   oGrp:setColorFG( GraMakeRGBColor( {   0,255,255 } ) )
   oGrp:setColorBG( GraMakeRGBColor( { 134,128,220 } ) )
   #ifdef __HARBOUR__
   oGrp:setPointer( , hb_DirBase() + "abs3.png", XBPWINDOW_POINTERTYPE_ICON )
   #endif

   oLbl := XbpStatic():new( oWnd, , {10,10}, {240,30} )
   oLbl:type    := XBPSTATIC_TYPE_TEXT
   oLbl:options := XBPSTATIC_TEXT_CENTER + XBPSTATIC_TEXT_VCENTER
   oLbl:caption := "Harbour-QT"
   oLbl:create()
   oLbl:setFontCompoundName( "18.Courier normal" )
   oLbl:setColorFG( GraMakeRGBColor( { 255,0,0 } ) )

   oLbl := XbpStatic():new( oWnd, , {10,oWnd:currentSize[ 2 ]-45-25}, {240,40} )
   oLbl:type    := XBPSTATIC_TYPE_TEXT
   oLbl:options := XBPSTATIC_TEXT_CENTER + XBPSTATIC_TEXT_BOTTOM
   oLbl:caption := "Welcome"
   oLbl:create()
   oLbl:setFontCompoundName( "30.Courier normal" )
   oLbl:setColorFG( GraMakeRGBColor( { 255,255,0 } ) )


   // Horizontal Lines
   oLin := XbpStatic():new( oGrp, , {nC2,nT+(nH+nG)*5-5}, {180,10} )
   oLin:type := XBPSTATIC_TYPE_RAISEDLINE
   oLin:create()
   // OK
   oLin := XbpStatic():new( oGrp, , {nC2,nT+(nH+nG)*5+5}, {180,10} )
   oLin:type := XBPSTATIC_TYPE_RECESSEDLINE
   oLin:options := XBPSTATIC_FRAMETHICK
   oLin:create()

   // Vertical Lines
   oLin := XbpStatic():new( oGrp, , {nC1,nT}, {10,170+120} )
   oLin:type := XBPSTATIC_TYPE_RAISEDLINE
   oLin:options := XBPSTATIC_FRAMETHICK
   oLin:create()
   // OK
   oLin := XbpStatic():new( oGrp, , {nC1+15,nT}, {10,170+120} )
   oLin:type := XBPSTATIC_TYPE_RECESSEDLINE
   oLin:create()

   // OK
   oBox := XbpStatic():new( oGrp, , {nC2,nT}, {nW,nH} )
   oBox:type := XBPSTATIC_TYPE_RAISEDBOX
   oBox:create()
   // OK
   oBox := XbpStatic():new( oGrp, , {nC3,nT}, {nW,nH} )
   oBox:type := XBPSTATIC_TYPE_RECESSEDBOX
   oBox:create()

   // OK
   oBox := XbpStatic():new( oGrp, , {nC2,nT+nH+nG}, {nW,nH} )
   oBox:type := XBPSTATIC_TYPE_RAISEDRECT
   oBox:options := XBPSTATIC_FRAMETHICK
   oBox:create()
   // OK
   oBox := XbpStatic():new( oGrp, , {nC3,nT+nH+nG}, {nW,nH} )
   oBox:type := XBPSTATIC_TYPE_RECESSEDRECT
   oBox:options := XBPSTATIC_FRAMETHICK
   oBox:create()
   // OK
   oBox := XbpStatic():new( oGrp, , {nC2,nT+(nH+nG)*2}, {nW,nH} )
   oBox:type := XBPSTATIC_TYPE_FGNDFRAME
   oBox:options := XBPSTATIC_FRAMETHICK
   oBox:create()
   // OK
   oBox := XbpStatic():new( oGrp, , {nC3,nT+(nH+nG)*2}, {nW,nH} )
   oBox:type := XBPSTATIC_TYPE_BGNDFRAME
   oBox:options := XBPSTATIC_FRAMETHICK
   oBox:create()
   // OK
   oBox := XbpStatic():new( oGrp, , {nC2,nT+(nH+nG)*3}, {nW,nH} )
   oBox:type := XBPSTATIC_TYPE_FGNDRECT
   oBox:options := XBPSTATIC_FRAMETHICK
   oBox:create()
   // OK
   oBox := XbpStatic():new( oGrp, , {nC3,nT+(nH+nG)*3}, {nW,nH} )
   oBox:type := XBPSTATIC_TYPE_BGNDRECT
   oBox:options := XBPSTATIC_FRAMETHICK
   oBox:create()
   // OK
   oBox := XbpStatic():new( oGrp, , {nC2,nT+(nH+nG)*4}, {nW,nH} )
   oBox:type := XBPSTATIC_TYPE_HALFTONERECT
   oBox:options := XBPSTATIC_FRAMETHICK
   oBox:create()
   // OK
   oBox := XbpStatic():new( oGrp, , {nC3,nT+(nH+nG)*4}, {nW,nH} )
   oBox:type := XBPSTATIC_TYPE_HALFTONEFRAME
   oBox:options := XBPSTATIC_FRAMETHICK
   oBox:create()

   oBox := XbpStatic():new( oGrp, , {nC4,nT}, {nW,nH+nH+nG} )
   oBox:type := XBPSTATIC_TYPE_BITMAP
   oBox:options := XBPSTATIC_BITMAP_SCALED
   oBmp := XbpBitmap():new():create()
   oBmp:loadFile( hb_DirBase() + "paste.png" )
   oBox:caption := oBmp
   oBox:create()
   oBox:setColorBG( GraMakeRGBColor( { 0,100,100 } ) )

   oBox := XbpStatic():new( oGrp, , {nC4,nT+(nH+nG)*2}, {nW,nH} )
   oBox:type := XBPSTATIC_TYPE_BITMAP
   oBox:options := XBPSTATIC_BITMAP_TILED
   #ifdef __HARBOUR__
   oBox:caption := hb_DirBase() + "cut.png"
   #else
   oBmp1 := XbpBitmap():new():create()
   oBmp1:loadFile( hb_DirBase() + "paste.png" )
   oBox:caption := oBmp1
   #endif
   oBox:create()
   oBox:setColorBG( GraMakeRGBColor( { 100,0,100 } ) )


   #ifdef __HARBOUR__ /* Differes from Xbase++ by Disk File | Resource Name, ID */
   oBox := XbpStatic():new( oGrp, , {nC4,nT+(nH+nG)*3}, {nW,nH} )
   oBox:type := XBPSTATIC_TYPE_ICON
   oBox:caption := hb_DirBase() + "vr.png"
   oBox:create()
   oBox:setColorBG( GraMakeRGBColor( { 255,255,187 } ) )
   #endif

   oBox := XbpStatic():new( oGrp, , {nC4,nT+(nH+nG)*4}, {nW,nH} )
   oBox:type := XBPSTATIC_TYPE_SYSICON
   oBox:caption := XBPSTATIC_SYSICON_ICONINFORMATION //XBPSTATIC_SYSICON_ICONQUESTION //
   oBox:create()

   oLbl := XbpStatic():new( oWnd, , {30,60}, {200,240} )
   oLbl:type    := XBPSTATIC_TYPE_TEXT
   oLbl:options := XBPSTATIC_TEXT_CENTER + XBPSTATIC_TEXT_VCENTER + XBPSTATIC_TEXT_WORDBREAK
   oLbl:caption := "The GroupBox at the right demonstrates many static controls" + CRLF + ;
                   "  "                    + CRLF + ;
                   "XBPSTATIC_TYPE_TEXT"   + CRLF + ;
                   "XBPSTATIC_TYPE_*LINE"  + CRLF + ;
                   "XBPSTATIC_TYPE_*BOX"   + CRLF + ;
                   "XBPSTATIC_TYPE_*RECT"  + CRLF + ;
                   "XBPSTATIC_TYPE_*FRAME" + CRLF + ;
                   "XBPSTATIC_TYPE_BITMAP" + CRLF + ;
                   "  "                    + CRLF + ;
                   "BITMAP"                + CRLF + ;
                   "though, is not exactly Xbase++ compatible in the sense " +;
                   "that it is not pulled from a resource" + CRLF + ;
                   "( to be addressed later )"

   oLbl:create()
   oLbl:setFontCompoundName( "8.Times normal" )
   oLbl:setColorBG( GraMakeRGBColor( { 100,0,150 } ) )
   oLbl:setColorFG( GraMakeRGBColor( { 255,255,0 } ) )
   oLbl:setPointer( , XBPSTATIC_SYSICON_SIZE, XBPWINDOW_POINTERTYPE_SYSPOINTER )

   RETURN nil

/*----------------------------------------------------------------------*/

FUNCTION Build_HTMLViewer( oWnd )
   LOCAL oFrm, oHtm, sz_:= oWnd:currentSize()

   oFrm := XbpStatic():new( oWnd, , {5,5}, {sz_[1]-5-10,sz_[2]-30-7} )
   oFrm:type := XBPSTATIC_TYPE_RECESSEDBOX
   oFrm:options := XBPSTATIC_FRAMETHICK
   oFrm:create()
   #ifdef __HARBOUR__
   //oFrm:setStyleSheet( "border: 2px solid yellow;" )
   #endif

   sz_:= oFrm:currentSize()
   // oHtm := XbpHTMLViewer():new( oWnd, , {10,10}, {sz_[1]-25,sz_[2]-30-15} )
   oHtm := XbpHTMLViewer():new( oFrm, , {10,10}, {sz_[1]-10-10,sz_[2]-10-10} )
   oHtm:create()
   oHtm:navigate( "http://harbour-project.org" )
   oHtm:titleChange := {|e| HB_SYMBOL_UNUSED( e ) }

   RETURN oHtm

/*----------------------------------------------------------------------*/

FUNCTION Build_FileDialog( oWnd, cMode )
   LOCAL oDlg, aFiles

   oDlg := XbpFileDialog():new():create( oWnd, , { 10,10 } )
   IF cMode == "open"
      oDlg:title       := "Open Index or Database"
      oDlg:center      := .t.
      oDlg:fileFilters := { { "Index Files", "*.ntx" }, { "Database Files", "*.dbf" } }
      //oDlg:setColorBG( GraMakeRGBColor( { 170,170,170 } ) )
      aFiles := oDlg:open( "c:\temp", , .t. )
      IF !empty( aFiles )
         aeval( aFiles, {|e| HB_SYMBOL_UNUSED( e ) } )
      ENDIF
   ELSE
      oDlg:title       := "Save this Database"
      oDlg:fileFilters := { { "Database Files", "*.dbf" } }
      oDlg:quit        := {|| MsgBox( "Quitting the Dialog" ), 1 }
      oDlg:saveAs( "c:\temp\myfile.dbf" )
   ENDIF

   RETURN nil

/*----------------------------------------------------------------------*/

FUNCTION Build_Bitmap( oWnd )
   LOCAL oBmp, aFltr, cFile, cExt, nFrmt, oDlg
   LOCAL cExtns := { "png","gif","jpg","jpeg","bmp","tiff" }
   LOCAL nFrmts := { XBPBMP_FORMAT_PNG, XBPBMP_FORMAT_GIF, XBPBMP_FORMAT_JPG, ;
                     XBPBMP_FORMAT_JPG, XBPBMP_FORMAT_WIN3X }

   cFile := GetAnImageFile( oWnd, "Select an image to be converted" )

   IF !empty( cFile )
      oBmp := XbpBitmap():new():create()
      IF oBmp:loadFile( cFile )
         MsgBox( "x = "+hb_ntos( oBmp:xSize ) +" y = "+hb_ntos( oBmp:ySize )+" b = "+hb_ntos( oBmp:bits ) )

         aFltr := {}
         aadd( aFltr, { "Windows Bitmap             ", "*.bmp"  } )
         aadd( aFltr, { "Joint Photographic Experts ", "*.jpg; *.jpeg"  } )
         aadd( aFltr, { "Portable Network Graphics  ", "*.png"  } )
         aadd( aFltr, { "Portable Pixmap            ", "*.ppm"  } )
         aadd( aFltr, { "Tagged Image File Format   ", "*.tiff" } )
         aadd( aFltr, { "X11 Bitmap                 ", "*.xbm"  } )
         aadd( aFltr, { "X11 Pixmap                 ", "*.xpm"  } )

         oDlg := XbpFileDialog():new():create( oWnd, , { 10,10 } )
         oDlg:title := "Specify how to save it !"
         oDlg:fileFilters := aFltr
         cFile := oDlg:saveAs()
         oDlg:destroy()

         IF !empty( cFile )
            cExt := upper( substr( cFile, at( ".", cFile )+1 ) )
            IF !empty( cExt )
               nFrmt := nFrmts[ ascan( cExtns, cExt ) ]

               oBmp:saveFile( cFile, nFrmt )
            ENDIF
         ENDIF
      ENDIF
   ENDIF

   RETURN nil

/*----------------------------------------------------------------------*/

FUNCTION GetAnImageFile( oWnd, cTitle )
   LOCAL oDlg, aFltr := {}, xRet

   DEFAULT cTitle TO "Select an Image"

   aadd( aFltr, { "Portable Network Graphics  ", "*.png"  } )
   aadd( aFltr, { "Windows Bitmap             ", "*.bmp"  } )
   aadd( aFltr, { "Graphic Interchange Format ", "*.gif"  } )
   aadd( aFltr, { "Joint Photographic Experts ", "*.jpg; *.jpeg" } )
   aadd( aFltr, { "Portable Pixmap            ", "*.ppm"  } )
   aadd( aFltr, { "Tagged Image File Format   ", "*.tiff" } )
   aadd( aFltr, { "X11 Bitmap                 ", "*.xbm"  } )
   aadd( aFltr, { "X11 Pixmap                 ", "*.xpm"  } )
   aeval( aFltr, {|e_,i| aFltr[ i,1 ] := trim( e_[ 1 ] ) } )

   oDlg := XbpFileDialog():new( oWnd, , {10,10} )
   oDlg:title := cTitle
   oDlg:fileFilters := aFltr
   oDlg:create()

   xRet := oDlg:open( hb_DirBase(), , .f. )

   oDlg:destroy()

   RETURN xRet

/*----------------------------------------------------------------------*/

FUNCTION Build_FontDialog( oWnd )
   LOCAL oDlg

   oDlg := XbpFontDialog():new( oWnd, , , , { 20,20 } )
   oDlg:activateOk := {|oFont| DisplayFontInfo( oFont ) }
   oDlg:create()

   oDlg:display( 0 )

   RETURN nil

/*----------------------------------------------------------------------*/

FUNCTION DisplayFontInfo( oFont )

   oMLE:setFont( oFont )

   RETURN nil

/*----------------------------------------------------------------------*/

FUNCTION Build_PrintDialog( oWnd )
   LOCAL oDlg, oPrn

   oDlg := XbpPrintDialog():new( oWnd ):create()
   oDlg:enablePrintToFile := .t.
   #if 1
   ODlg:pageRange := { 1,3 }
   oDlg:printRange := XBPPDLG_PRINT_PAGERANGE
   #endif
   #if 0
   oDlg:enableMark := .t.
   oDlg:printRange := XBPPDLG_PRINT_MARK
   #endif

   IF valtype( oPrn := oDlg:display() ) == "O"

      MyDebug( oPrn:devName           , ;
               oPrn:setOrientation()  , ;
               oPrn:setFormSize()     , ;
               oPrn:setResolution()[1], ;
               oPrn:setNumCopies()    , ;
               oPrn:setPaperBin()     , ;
               oPrn:setColorMode()    , ;
               oPrn:setDuplexMode()   , ;
               oPrn:setCollationMode()  ;
             )
   ENDIF

   RETURN nil

/*----------------------------------------------------------------------*/

FUNCTION Build_Rtf( oWnd )
   LOCAL oRTF, oBtn
   LOCAL sz_:= oWnd:currentSize()
   LOCAL nW := 50, nG := 8, nH := 20, nT := sz_[2]-55

   //--------------------------------//

   oBtn := XbpPushButton():new( oWnd, , {10+(nW+nG)*0, nT}, {nW,nH} )
   oBtn:caption := 'Image'
   oBtn:create()
   oBtn:activate := {|| RtfInsertImage( oRtf ) }

   oBtn := XbpPushButton():new( oWnd, , {10+(nW+nG)*1, nT}, {nW,nH} )
   oBtn:caption := 'Undo'
   oBtn:create()
   oBtn:activate := {|| oRtf:undo() }

   oBtn := XbpPushButton():new( oWnd, , {10+(nW+nG)*2, nT}, {nW,nH} )
   oBtn:caption := 'Redo'
   oBtn:create()
   #ifdef __HARBOUR__
   oBtn:activate := {|| oRtf:redo() }
   #endif

   oBtn := XbpPushButton():new( oWnd, , {10+(nW+nG)*3, nT}, {nW,nH} )
   oBtn:caption := 'ULine'
   oBtn:create()
   oBtn:activate := {|| oRtf:selUnderline := .t. }

   oBtn := XbpPushButton():new( oWnd, , {10+(nW+nG)*4, nT}, {nW,nH} )
   oBtn:caption := 'Bold'
   oBtn:create()
   oBtn:activate := {|| oRtf:selBold := .t. }

   oBtn := XbpPushButton():new( oWnd, , {10+(nW+nG)*5, nT}, {nW,nH} )
   oBtn:caption := 'Italic'
   oBtn:create()
   oBtn:activate := {|| oRtf:selItalic := .t. }

   //-----------------------------------//

   oBtn := XbpPushButton():new( oWnd, , {10+(nW+nG)*0, nT-25}, {nW,nH} )
   oBtn:caption := 'Load'
   oBtn:create()
   oBtn:activate := {|| RtfLoadDocument( oRtf ) }

   oBtn := XbpPushButton():new( oWnd, , {10+(nW+nG)*1, nT-25}, {nW,nH} )
   oBtn:caption := 'Save'
   oBtn:create()
   oBtn:activate := {|| RtfSaveDocument( oRtf ) }

   oBtn := XbpPushButton():new( oWnd, , {10+(nW+nG)*2, nT-25}, {nW,nH} )
   oBtn:caption := 'Font++'
   oBtn:create()
   oBtn:activate := {|x| x := oRTF:selFontSize, IF( x == 0, x := 11, ), oRTF:selFontSize := x+1 }

   oBtn := XbpPushButton():new( oWnd, , {10+(nW+nG)*3, nT-25}, {nW,nH} )
   oBtn:caption := 'Font--'
   oBtn:create()
   oBtn:activate := {|x| x := oRTF:selFontSize, IF( x == 0, x := 11, ), oRTF:selFontSize := x-1 }

   oBtn := XbpPushButton():new( oWnd, , {10+(nW+nG)*4, nT-25}, {nW,nH} )
   oBtn:caption := 'Print'
   oBtn:create()
   oBtn:activate := {|| oRTF:print( , .t. ) }

   oBtn := XbpPushButton():new( oWnd, , {10+(nW+nG)*5, nT-25}, {nW,nH} )
   oBtn:caption := 'Replace'
   oBtn:create()
   oBtn:activate := {|x| x := oRTF:selText, oRTF:selText := "Harbour" }

   //-----------------------------------//

   oBtn := XbpPushButton():new( oWnd, , {10+(nW+nG)*0, nT-50}, {nW,nH} )
   oBtn:caption := 'Font'
   oBtn:create()
   oBtn:activate := {|| RtfApplyFont( oRtf ) }


   //-----------------------------------//
   oRTF := XbpRtf():new( oWnd )
   oRTF:create( , , { 10,10 }, { sz_[ 1 ]-23, sz_[ 2 ]-125 } )
   oRTF:setColorBG( GraMakeRGBColor( {255,255,200} ) )

   oRTF:setFontCompoundName( "12.Times" )

   // Assign text to the RTF object's text buffer
   oRTF:text := "Text with varying " + Chr(10) +;
                "text attributes. Made possible by the " + Chr(10) +;
                "XbpRtf edit control." + Chr(10)

   //
   // Use the selection manipulation methods to
   // assign different attributes to the text
   //
   // Set the selection using abolute character positions
   oRTF:SelStart    := 5
   oRTF:SelLength   := 30
   oRTF:SelColor    := GRA_CLR_BLUE

   // Set the selection by selecting a specific word
   // in the text
   oRTF:SelStart    := 0
   oRTF:Find( "Made possible" )
   oRTF:SelBold     := .T.

   // Find a specific word and expand the selection
   // to include another one
   oRTF:SelLength   := 0
   oRTF:Find( "XbpRtf" )
   // oRTF:Span( ".",, .T. )                /* TODO */
   oRTF:SelColor    := GRA_CLR_RED
   oRTF:SelFontName := "Courier New"
   oRTF:SelFontSize := 30
   oRTF:SelBold     := .T.
   oRTF:SelItalic   := .T.
   // oRTF:SelStrikeThru := .T.             /* OK */
   oRTF:SelUnderline := .T.
   oRTF:selAlignment := XBPRTF_ALIGN_CENTER

   // Reset the text cursor
   //
   oRTF:SelStart    := Len( oRTF:Text )

   RETURN nil

/*----------------------------------------------------------------------*/

STATIC FUNCTION RtfInsertImage( oRtf )
   #ifdef __HARBOUR__
   LOCAL cFile

   // Proivide a selection dialog
   cFile := GetAnImageFile( oRtf, 'Select Image to be Inserted' )
   IF empty( cFile )
      oRtf:insertImage( hb_DirBase() + "abs3.png" )
   ELSE
      oRtf:insertImage( cFile )
   ENDIF
   #endif
   RETURN nil

/*----------------------------------------------------------------------*/

STATIC FUNCTION RtfLoadDocument( oRTF )
   LOCAL oDlg, cFile, aFiltr := {}

   aadd( aFiltr, { "All Files", "*.*"    } )
   aadd( aFiltr, { "Text File", "*.txt"  } )
   aadd( aFiltr, { "RTF File" , "*.htm; *.html"  } )

   oDlg := XbpFileDialog():new():create( oRTF, , { 10,10 } )
   oDlg:title       := "Open an RTF Document"
   oDlg:center      := .t.
   oDlg:fileFilters := aFiltr
   oDlg:setColorBG( GraMakeRGBColor( { 255,255,200 } ) )

   cFile := oDlg:open()
   IF !empty( cFile )
      oRTF:loadFile( cFile )
   ENDIF

   RETURN nil

/*----------------------------------------------------------------------*/

STATIC FUNCTION RtfSaveDocument( oRTF )
   LOCAL oDlg, cFile, aFiltr := {}

   aadd( aFiltr, { "RTF File" , "*.htm; *.html" } )
   aadd( aFiltr, { "Text File", "*.txt"  } )

   oDlg := XbpFileDialog():new():create( oRTF, , { 10,10 } )
   oDlg:title       := "Open an RTF Document"
   oDlg:center      := .t.
   oDlg:fileFilters := aFiltr
   oDlg:setColorBG( GraMakeRGBColor( { 255,200,200 } ) )

   cFile := oDlg:saveAs()
   IF !empty( cFile )
      oRTF:saveFile( cFile )
   ENDIF

   RETURN nil

/*----------------------------------------------------------------------*/

STATIC FUNCTION RtfApplyFont( oRTF )
   LOCAL oDlg

   oDlg := XbpFontDialog():new( oRTF, , , , { 20,20 } )
   oDlg:create()
   oDlg:activateApply := {|oFont| oRTF:selFont := oFont }
   oDlg:display( 0 )

   RETURN nil

/*----------------------------------------------------------------------*/

FUNCTION Build_Browse( oWnd )
   LOCAL aPresParam, oXbpBrowse, oXbpColumn, s
   LOCAL cPath := hb_DirBase() + ".." + hb_ps() + ".." + hb_ps() + ".." + hb_ps() + "tests" + hb_ps()

   Set( _SET_DATEFORMAT, "yyyy.mm.dd" ) /* ANSI */

   USE ( cPath + "test.dbf" ) NEW SHARED READONLY VIA 'DBFCDX'
   #if 1
   //INDEX ON test->last TAG "LAST" TO ( cPath + "test.cdx" )
   #endif
   DbGotop()

   oXbpBrowse := XbpBrowse():new():create( oWnd, , { 10,10 }, { oWnd:currentSize()[1]-20,oWnd:currentSize()[2]-20 } )
   oXbpBrowse:setFontCompoundName( "10.Courier" )
   //oXbpBrowse:hScroll       := .f.          // OK
   //oXbpBrowse:vScroll       := .f.          // OK
   //oXbpBrowse:sizeCols      := .f.          // OK
   oXbpBrowse:cursorMode    := XBPBRW_CURSOR_ROW
   //oXbpBrowse:cursorMode    := XBPBRW_CURSOR_CELL

   /* Navigation Blocks */
   oXbpBrowse:skipBlock     := {|n| DbSkipBlock( n ) }
   oXbpBrowse:goTopBlock    := {| | DbGoTop()        }
   oXbpBrowse:goBottomBlock := {| | DbGoBottom()     }
   //
   oXbpBrowse:firstPosBlock := {| | 1                }
   oXbpBrowse:lastPosBlock  := {| | LastRec()        }
   IF indexOrd() == 0
      oXbpBrowse:posBlock      := {| | RecNo()          }
      oXbpBrowse:goPosBlock    := {|n| DbGoto( n )      }
      oXbpBrowse:phyPosBlock   := {| | RecNo()          }
   ELSE
      oXbpBrowse:posBlock      := {| | OrdKeyNo()       }
      oXbpBrowse:goPosBlock    := {|n| OrdKeyGoto( n )  }
      oXbpBrowse:phyPosBlock   := {| | OrdKeyNo()       }
   ENDIF

   oXbpBrowse:headerRbDown  := {|mp1, mp2, o| HB_SYMBOL_UNUSED( mp1 ), HB_SYMBOL_UNUSED( mp2 ), HB_SYMBOL_UNUSED( o ) }

   #ifdef __HARBOUR__
   s := "selection-background-color: qlineargradient(x1: 0, y1: 0, x2: 0.5, y2: 0.5, stop: 0 #FF92BB, stop: 1 gray); "
   oXbpBrowse:setStyleSheet( s )
   #endif

   aPresParam := {}
   aadd( aPresParam, { XBP_PP_COL_HA_CAPTION      , "Icons"                    } )
   aadd( aPresParam, { XBP_PP_COL_HA_FGCLR        , GRA_CLR_CYAN               } )
   aadd( aPresParam, { XBP_PP_COL_HA_BGCLR        , GRA_CLR_BLUE               } )
   aadd( aPresParam, { XBP_PP_COL_HA_HEIGHT       , 20                         } )
   aadd( aPresParam, { XBP_PP_COL_DA_CELLALIGNMENT, XBPALIGN_HCENTER           } )
   aadd( aPresParam, { XBP_PP_COL_DA_FGCLR        , GRA_CLR_BLACK              } )
   aadd( aPresParam, { XBP_PP_COL_DA_BGCLR        , RGB( 5,240,210 )           } )
   aadd( aPresParam, { XBP_PP_COL_DA_HILITE_FGCLR , GRA_CLR_WHITE              } )
   aadd( aPresParam, { XBP_PP_COL_DA_HILITE_BGCLR , GRA_CLR_DARKGRAY           } )
   aadd( aPresParam, { XBP_PP_COL_DA_ROWHEIGHT    , 20                         } )
   aadd( aPresParam, { XBP_PP_COL_DA_ROWWIDTH     , 40                         } )
   aadd( aPresParam, { XBP_PP_COL_FA_CAPTION      , " .. "                     } )
   aadd( aPresParam, { XBP_PP_COL_FA_FGCLR        , GRA_CLR_BLACK              } )
   aadd( aPresParam, { XBP_PP_COL_FA_BGCLR        , GRA_CLR_DARKGRAY           } )
   aadd( aPresParam, { XBP_PP_COL_FA_HEIGHT       , 25                         } )
   //
   oXbpColumn          := XbpColumn():new()
   oXbpColumn:type     := XBPCOL_TYPE_FILEICON
   cPath := hb_DirBase() + hb_ps()
   oXbpColumn:dataLink := {|n| n := recno(), IF( n%3 == 0, cPath + "abs3.png", IF( n%5 == 0, cPath + "copy.png", cPath + "vr.png" ) ) }
   oXbpColumn:create( , , , , aPresParam )
   //
   oXbpBrowse:addColumn( oXbpColumn )

   aeval( aPresParam, {|e_, i| e_[ 1 ] := NIL, e_[ 2 ] := NIL, aPresParam[ i ] := NIL } )
   aPresParam := {}
   aadd( aPresParam, { XBP_PP_COL_HA_CAPTION      , "Last"                     } )
   aadd( aPresParam, { XBP_PP_COL_HA_FGCLR        , XBPSYSCLR_WINDOWSTATICTEXT } )
   aadd( aPresParam, { XBP_PP_COL_HA_BGCLR        , XBPSYSCLR_DIALOGBACKGROUND } )
   aadd( aPresParam, { XBP_PP_COL_HA_HEIGHT       , 30                         } )
   aadd( aPresParam, { XBP_PP_COL_DA_FGCLR        , GRA_CLR_BLACK              } )
   aadd( aPresParam, { XBP_PP_COL_DA_BGCLR        , RGB( 248,210,194 )         } )
   aadd( aPresParam, { XBP_PP_COL_DA_HILITE_FGCLR , GRA_CLR_WHITE              } )
   aadd( aPresParam, { XBP_PP_COL_DA_HILITE_BGCLR , GRA_CLR_DARKGRAY           } )
   aadd( aPresParam, { XBP_PP_COL_DA_ROWSEPARATOR , XBPCOL_SEP_DOTTED          } )
   aadd( aPresParam, { XBP_PP_COL_DA_COLSEPARATOR , XBPCOL_SEP_DOTTED          } )
   aadd( aPresParam, { XBP_PP_COL_DA_ROWHEIGHT    , 20                         } )
   aadd( aPresParam, { XBP_PP_COL_FA_CAPTION      , "Last Name"                } )
   aadd( aPresParam, { XBP_PP_COL_FA_FGCLR        , GRA_CLR_BLACK              } )
   aadd( aPresParam, { XBP_PP_COL_FA_BGCLR        , GRA_CLR_DARKGRAY           } )
   aadd( aPresParam, { XBP_PP_COL_FA_HEIGHT       , 25                         } )

   oXbpColumn            := XbpColumn():new()
   oXbpColumn:dataLink   := {|| test->Last }
   oXbpColumn:colorBlock := {|x| IF( left( x,1 ) $ "L,H", { GRA_CLR_BLUE, GRA_CLR_YELLOW }, { NIL, NIL } ) }
   oXbpColumn:create( , , , , aPresParam )
   //
   oXbpBrowse:addColumn( oXbpColumn )

   aPresParam := {}
   aadd( aPresParam, { XBP_PP_COL_HA_CAPTION      , "First"                    } )
   aadd( aPresParam, { XBP_PP_COL_HA_ALIGNMENT    , XBPALIGN_LEFT              } )
   aadd( aPresParam, { XBP_PP_COL_HA_FGCLR        , GraMakeRGBColor( { 200, 100, 255 } ) } )
   aadd( aPresParam, { XBP_PP_COL_HA_BGCLR        , GRA_CLR_RED                } )
   aadd( aPresParam, { XBP_PP_COL_HA_HEIGHT       , 20                         } )
   aadd( aPresParam, { XBP_PP_COL_DA_FGCLR        , GRA_CLR_WHITE              } )
   aadd( aPresParam, { XBP_PP_COL_DA_BGCLR        , RGB( 120,130,230 )         } )
   aadd( aPresParam, { XBP_PP_COL_DA_HILITE_FGCLR , GRA_CLR_WHITE              } )
   aadd( aPresParam, { XBP_PP_COL_DA_HILITE_BGCLR , GRA_CLR_DARKGRAY           } )
   aadd( aPresParam, { XBP_PP_COL_DA_ROWSEPARATOR , XBPCOL_SEP_DOTTED          } )
   aadd( aPresParam, { XBP_PP_COL_DA_COLSEPARATOR , XBPCOL_SEP_DOTTED          } )
   aadd( aPresParam, { XBP_PP_COL_DA_ROWHEIGHT    , 20                         } )
   aadd( aPresParam, { XBP_PP_COL_FA_CAPTION      , "First Name"               } )
   aadd( aPresParam, { XBP_PP_COL_FA_FGCLR        , GRA_CLR_BLACK              } )
   aadd( aPresParam, { XBP_PP_COL_FA_BGCLR        , GRA_CLR_DARKRED            } )
   aadd( aPresParam, { XBP_PP_COL_FA_HEIGHT       , 25                         } )
   //
   oXbpColumn          := XbpColumn():new()
   oXbpColumn:dataLink := {|| test->First }
   oXbpColumn:create(  , , , , aPresParam )
   //
   oXbpBrowse:addColumn( oXbpColumn )

   aPresParam := {}
   aadd( aPresParam, { XBP_PP_COL_HA_CAPTION      , "Salary"                   } )
   aadd( aPresParam, { XBP_PP_COL_HA_ALIGNMENT    , XBPALIGN_RIGHT             } )
   aadd( aPresParam, { XBP_PP_COL_HA_FGCLR        , GRA_CLR_WHITE              } )
   aadd( aPresParam, { XBP_PP_COL_HA_BGCLR        , RGB( 140,170,240 )         } )
   aadd( aPresParam, { XBP_PP_COL_HA_HEIGHT       , 20                         } )
   aadd( aPresParam, { XBP_PP_COL_DA_FGCLR        , GRA_CLR_BLACK              } )
   aadd( aPresParam, { XBP_PP_COL_DA_BGCLR        , GRA_CLR_DARKGREEN          } )
   aadd( aPresParam, { XBP_PP_COL_DA_HILITE_FGCLR , GRA_CLR_WHITE              } )
   aadd( aPresParam, { XBP_PP_COL_DA_HILITE_BGCLR , GRA_CLR_DARKGRAY           } )
   aadd( aPresParam, { XBP_PP_COL_DA_ROWSEPARATOR , XBPCOL_SEP_DOTTED          } )
   aadd( aPresParam, { XBP_PP_COL_DA_COLSEPARATOR , XBPCOL_SEP_DOTTED          } )
   aadd( aPresParam, { XBP_PP_COL_DA_ROWHEIGHT    , 25                         } )
   aadd( aPresParam, { XBP_PP_COL_FA_CAPTION      , "$$"                       } )
   aadd( aPresParam, { XBP_PP_COL_FA_FGCLR        , GRA_CLR_BLACK              } )
   aadd( aPresParam, { XBP_PP_COL_FA_BGCLR        , GRA_CLR_DARKGRAY           } )
   aadd( aPresParam, { XBP_PP_COL_FA_HEIGHT       , 25                         } )
   //
   oXbpColumn          := XbpColumn():new()
   oXbpColumn:dataLink := {|| test->Salary }
   oXbpColumn:create( , , , , aPresParam )
   oXbpColumn:colorBlock := {|x| IF( x < 40000, { NIL, RGB( 255,0,0 ) }, {NIL,NIL} ) }
   //
   oXbpBrowse:addColumn( oXbpColumn )

   aPresParam := {}
   aadd( aPresParam, { XBP_PP_COL_HA_CAPTION      , "Hired On"                 } )
   aadd( aPresParam, { XBP_PP_COL_HA_FGCLR        , GraMakeRGBColor( { 255, 0, 255 } ) } )
   aadd( aPresParam, { XBP_PP_COL_HA_BGCLR        , GRA_CLR_YELLOW             } )
   aadd( aPresParam, { XBP_PP_COL_HA_HEIGHT       , 20                         } )
   aadd( aPresParam, { XBP_PP_COL_DA_FGCLR        , GRA_CLR_BLACK              } )
   aadd( aPresParam, { XBP_PP_COL_DA_BGCLR        , GRA_CLR_GREEN              } )
   aadd( aPresParam, { XBP_PP_COL_DA_HILITE_FGCLR , GRA_CLR_WHITE              } )
   aadd( aPresParam, { XBP_PP_COL_DA_HILITE_BGCLR , GRA_CLR_DARKGRAY           } )
   aadd( aPresParam, { XBP_PP_COL_DA_ROWSEPARATOR , XBPCOL_SEP_DOTTED          } )
   aadd( aPresParam, { XBP_PP_COL_DA_COLSEPARATOR , XBPCOL_SEP_DOTTED          } )
   aadd( aPresParam, { XBP_PP_COL_DA_ROWHEIGHT    , 20                         } )
   aadd( aPresParam, { XBP_PP_COL_FA_CAPTION      , "Year Mn Dy"               } )
   aadd( aPresParam, { XBP_PP_COL_FA_FGCLR        , GRA_CLR_BLACK              } )
   aadd( aPresParam, { XBP_PP_COL_FA_BGCLR        , GRA_CLR_DARKGRAY           } )
   aadd( aPresParam, { XBP_PP_COL_FA_HEIGHT       , 25                         } )
   //
   oXbpColumn          := XbpColumn():new()
   oXbpColumn:dataLink := {|| test->HireDate }
   oXbpColumn:create( , , , , aPresParam )
   //
   oXbpBrowse:addColumn( oXbpColumn )

   aPresParam := {}
   aadd( aPresParam, { XBP_PP_COL_HA_CAPTION      , "Age"                      } )
   aadd( aPresParam, { XBP_PP_COL_HA_FGCLR        , GraMakeRGBColor( { 200, 100, 255 } ) } )
   aadd( aPresParam, { XBP_PP_COL_HA_BGCLR        , GRA_CLR_BLUE               } )
   aadd( aPresParam, { XBP_PP_COL_HA_HEIGHT       , 20                         } )
   aadd( aPresParam, { XBP_PP_COL_DA_FGCLR        , GRA_CLR_BLACK              } )
   aadd( aPresParam, { XBP_PP_COL_DA_BGCLR        , GRA_CLR_YELLOW             } )
   aadd( aPresParam, { XBP_PP_COL_DA_HILITE_FGCLR , GRA_CLR_WHITE              } )
   aadd( aPresParam, { XBP_PP_COL_DA_HILITE_BGCLR , GRA_CLR_DARKGRAY           } )
   aadd( aPresParam, { XBP_PP_COL_DA_ROWSEPARATOR , XBPCOL_SEP_DOTTED          } )
   aadd( aPresParam, { XBP_PP_COL_DA_COLSEPARATOR , XBPCOL_SEP_DOTTED          } )
   aadd( aPresParam, { XBP_PP_COL_DA_ROWHEIGHT    , 20                         } )
   aadd( aPresParam, { XBP_PP_COL_FA_CAPTION      , "Yrs"                     } )
   aadd( aPresParam, { XBP_PP_COL_FA_FGCLR        , GRA_CLR_BLACK              } )
   aadd( aPresParam, { XBP_PP_COL_FA_BGCLR        , GRA_CLR_DARKGRAY           } )
   aadd( aPresParam, { XBP_PP_COL_FA_HEIGHT       , 25                         } )
   //
   oXbpColumn          := XbpColumn():new()
   oXbpColumn:dataLink := {|| test->Age }
   oXbpColumn:create( , , , , aPresParam )
   //
   oXbpBrowse:addColumn( oXbpColumn )

   aPresParam := {}
   aadd( aPresParam, { XBP_PP_COL_HA_CAPTION      , "City"                     } )
   aadd( aPresParam, { XBP_PP_COL_HA_FGCLR        , GRA_CLR_CYAN               } )
   aadd( aPresParam, { XBP_PP_COL_HA_BGCLR        , GRA_CLR_BLUE               } )
   aadd( aPresParam, { XBP_PP_COL_HA_HEIGHT       , 20                         } )
   aadd( aPresParam, { XBP_PP_COL_DA_FGCLR        , GRA_CLR_BLACK              } )
   aadd( aPresParam, { XBP_PP_COL_DA_BGCLR        , RGB( 205,240,210 )         } )
   aadd( aPresParam, { XBP_PP_COL_DA_HILITE_FGCLR , GRA_CLR_WHITE              } )
   aadd( aPresParam, { XBP_PP_COL_DA_HILITE_BGCLR , GRA_CLR_DARKGRAY           } )
   aadd( aPresParam, { XBP_PP_COL_DA_ROWSEPARATOR , XBPCOL_SEP_DOTTED          } )
   aadd( aPresParam, { XBP_PP_COL_DA_COLSEPARATOR , XBPCOL_SEP_DOTTED          } )
   aadd( aPresParam, { XBP_PP_COL_DA_ROWHEIGHT    , 20                         } )
   aadd( aPresParam, { XBP_PP_COL_FA_CAPTION      , "USA"                     } )
   aadd( aPresParam, { XBP_PP_COL_FA_FGCLR        , GRA_CLR_BLACK              } )
   aadd( aPresParam, { XBP_PP_COL_FA_BGCLR        , GRA_CLR_DARKGRAY           } )
   aadd( aPresParam, { XBP_PP_COL_FA_HEIGHT       , 25                         } )
   //
   oXbpColumn          := XbpColumn():new()
   oXbpColumn:dataLink := {|| test->City }
   oXbpColumn:create( , , , , aPresParam )
   //
   oXbpBrowse:addColumn( oXbpColumn )

   aPresParam := {}
   aadd( aPresParam, { XBP_PP_COL_HA_CAPTION      , "St"                       } )
   aadd( aPresParam, { XBP_PP_COL_HA_FGCLR        , GRA_CLR_CYAN               } )
   aadd( aPresParam, { XBP_PP_COL_HA_BGCLR        , GRA_CLR_BLUE               } )
   aadd( aPresParam, { XBP_PP_COL_HA_HEIGHT       , 20                         } )
   aadd( aPresParam, { XBP_PP_COL_DA_FGCLR        , GRA_CLR_BLACK              } )
   aadd( aPresParam, { XBP_PP_COL_DA_BGCLR        , RGB( 205,240,210 )         } )
   aadd( aPresParam, { XBP_PP_COL_DA_HILITE_FGCLR , GRA_CLR_WHITE              } )
   aadd( aPresParam, { XBP_PP_COL_DA_HILITE_BGCLR , GRA_CLR_DARKGRAY           } )
   aadd( aPresParam, { XBP_PP_COL_DA_ROWSEPARATOR , XBPCOL_SEP_DOTTED          } )
   aadd( aPresParam, { XBP_PP_COL_DA_COLSEPARATOR , XBPCOL_SEP_DOTTED          } )
   aadd( aPresParam, { XBP_PP_COL_DA_ROWHEIGHT    , 20                         } )
   aadd( aPresParam, { XBP_PP_COL_FA_CAPTION      , "USA"                      } )
   aadd( aPresParam, { XBP_PP_COL_FA_FGCLR        , GRA_CLR_BLACK              } )
   aadd( aPresParam, { XBP_PP_COL_FA_BGCLR        , GRA_CLR_DARKGRAY           } )
   aadd( aPresParam, { XBP_PP_COL_FA_HEIGHT       , 25                         } )
   //
   oXbpColumn          := XbpColumn():new()
   oXbpColumn:dataLink := {|| test->State }
   oXbpColumn:create( , , , , aPresParam )
   //
   oXbpBrowse:addColumn( oXbpColumn )

   aPresParam := {}
   aadd( aPresParam, { XBP_PP_COL_HA_CAPTION      , "Zipcode"                  } )
   aadd( aPresParam, { XBP_PP_COL_HA_FGCLR        , GRA_CLR_CYAN               } )
   aadd( aPresParam, { XBP_PP_COL_HA_BGCLR        , GRA_CLR_BLUE               } )
   aadd( aPresParam, { XBP_PP_COL_HA_HEIGHT       , 20                         } )
   aadd( aPresParam, { XBP_PP_COL_DA_FGCLR        , GRA_CLR_BLACK              } )
   aadd( aPresParam, { XBP_PP_COL_DA_BGCLR        , RGB( 205,240,210 )         } )
   aadd( aPresParam, { XBP_PP_COL_DA_HILITE_FGCLR , GRA_CLR_WHITE              } )
   aadd( aPresParam, { XBP_PP_COL_DA_HILITE_BGCLR , GRA_CLR_DARKGRAY           } )
   aadd( aPresParam, { XBP_PP_COL_DA_ROWSEPARATOR , XBPCOL_SEP_DOTTED          } )
   aadd( aPresParam, { XBP_PP_COL_DA_COLSEPARATOR , XBPCOL_SEP_DOTTED          } )
   aadd( aPresParam, { XBP_PP_COL_DA_ROWHEIGHT    , 20                         } )
   aadd( aPresParam, { XBP_PP_COL_FA_CAPTION      , "USA"                      } )
   aadd( aPresParam, { XBP_PP_COL_FA_FGCLR        , GRA_CLR_BLACK              } )
   aadd( aPresParam, { XBP_PP_COL_FA_BGCLR        , GRA_CLR_DARKGRAY           } )
   aadd( aPresParam, { XBP_PP_COL_FA_HEIGHT       , 25                         } )
   //
   oXbpColumn          := XbpColumn():new()
   oXbpColumn:dataLink := {|| test->Zip }
   oXbpColumn:create( , , , , aPresParam )
   //
   oXbpBrowse:addColumn( oXbpColumn )

   aPresParam := {}
   aadd( aPresParam, { XBP_PP_COL_HA_CAPTION      , "Notes"                    } )
   aadd( aPresParam, { XBP_PP_COL_HA_FGCLR        , GRA_CLR_CYAN               } )
   aadd( aPresParam, { XBP_PP_COL_HA_BGCLR        , GRA_CLR_BLUE               } )
   aadd( aPresParam, { XBP_PP_COL_HA_HEIGHT       , 20                         } )
   aadd( aPresParam, { XBP_PP_COL_DA_FGCLR        , GRA_CLR_BLACK              } )
   aadd( aPresParam, { XBP_PP_COL_DA_BGCLR        , RGB( 205,240,210 )         } )
   aadd( aPresParam, { XBP_PP_COL_DA_HILITE_FGCLR , GRA_CLR_WHITE              } )
   aadd( aPresParam, { XBP_PP_COL_DA_HILITE_BGCLR , GRA_CLR_DARKGRAY           } )
   aadd( aPresParam, { XBP_PP_COL_DA_ROWSEPARATOR , XBPCOL_SEP_DOTTED          } )
   aadd( aPresParam, { XBP_PP_COL_DA_COLSEPARATOR , XBPCOL_SEP_DOTTED          } )
   aadd( aPresParam, { XBP_PP_COL_DA_ROWHEIGHT    , 20                         } )
   aadd( aPresParam, { XBP_PP_COL_FA_CAPTION      , "Generic"                  } )
   aadd( aPresParam, { XBP_PP_COL_FA_FGCLR        , GRA_CLR_BLACK              } )
   aadd( aPresParam, { XBP_PP_COL_FA_BGCLR        , GRA_CLR_DARKGRAY           } )
   aadd( aPresParam, { XBP_PP_COL_FA_HEIGHT       , 25                         } )
   //
   oXbpColumn          := XbpColumn():new()
   oXbpColumn:dataLink := {|| test->Notes }
   oXbpColumn:create( , , , , aPresParam )
   //
   oXbpBrowse:addColumn( oXbpColumn )

   oXbpBrowse:setLeftFrozen( { 1 } )
   oXbpBrowse:setRightFrozen( { 6,5,4 } )

   RETURN nil

/*----------------------------------------------------------------------*/

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

/*----------------------------------------------------------------------*/

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

/*----------------------------------------------------------------------*/

STATIC FUNCTION TBPrev()
   LOCAL nSaveRecNum := Recno()
   LOCAL lMoved := .T.

   DBSkip( -1 )

   if Bof()
      DBGoTo( nSaveRecNum )
      lMoved := .F.
   endif

   RETURN lMoved

/*----------------------------------------------------------------------*/
