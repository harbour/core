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

#include "hbqtgui.ch"

#include "hbtrace.ch"

#define QT_EVE_TRIGGERED             "triggered()"
#define QT_EVE_TRIGGERED_B           "triggered(bool)"
#define QT_EVE_HOVERED               "hovered()"
#define QT_EVE_CLICKED               "clicked()"
#define QT_EVE_STATECHANGED_I        "stateChanged(int)"
#define QT_EVE_PRESSED               "pressed()"
#define QT_EVE_RELEASED              "released()"
#define QT_EVE_ACTIVATED_I           "activated(int)"
#define QT_EVE_CURRENTINDEXCHANGED_I "currentIndexChanged(int)"
#define QT_EVE_HIGHLIGHTED_I         "highlighted(int)"
#define QT_EVE_RETURNPRESSED         "returnPressed()"
#define QT_EVE_CLICKED_M             "clicked(QModelIndex)"
#define QT_EVE_VIEWPORTENTERED       "viewportEntered()"

/*----------------------------------------------------------------------*/

#include "common.ch"

REQUEST HB_QTGUI

THREAD STATIC s_events
THREAD STATIC s_slots

/*----------------------------------------------------------------------*/

INIT PROCEDURE Qt_Start()
   hbqt_errorsys()
   RETURN

/*----------------------------------------------------------------------*/

FUNCTION My_Events()
   HB_TRACE( HB_TR_ALWAYS, "Key Pressed" )
   RETURN nil

/*----------------------------------------------------------------------*/

PROCEDURE ExecOneMore()
   Local oLabel, oBtn, oDA, oWnd, oProg, oSBar
   LOCAL aMenu, aTool, aGrid, aTabs, aList, oEventLoop
   LOCAL lExit := .f.

   s_events := __HBQT_EVENTS_NEW()
   s_slots  := __HBQT_SLOTS_NEW()

   oWnd := QMainWindow()
   __HBQT_EVENTS_CONNECT( s_events,  oWnd, QEvent_Close, {|| lExit := .t. } )

   oWnd:setMouseTracking( .t. )
   oWnd:setWindowTitle( "Harbour-Qt Implementation Test Dialog" )
   oWnd:setWindowIcon( "test" )
   oWnd:resize( 900, 500 )

   oDA    := QWidget( oWnd )
   oWnd:setCentralWidget( oDA )

   oWnd:show()

   aMenu  := Build_MenuBar( oWnd )
   aTool  := Build_ToolBar( oWnd )
   oLabel := Build_Label( oDA, { 30,190 }, { 300, 30 } )
   oBtn   := Build_PushButton( oDA, { 30,240 }, { 100,50 }, ;
                                   "CLOSE", "This dialog will be closed now!", @lExit )
   aGrid  := Build_Grid( oDA, { 30, 30 }, { 450,150 } )
   aTabs  := Build_Tabs( oDA, { 510, 5 }, { 360, 400 } )
   oProg  := Build_ProgressBar( oDA, { 30,300 }, { 200,30 } )
   aList  := Build_ListBox( oDA, { 310,240 }, { 150, 100 } )

   oSBar  := QStatusBar( oWnd )
   oWnd:setStatusBar( oSBar )
   oSBar:showMessage( "Harbour-QT Statusbar Ready!" )

   oEventLoop := QEventLoop( oWnd )
   DO WHILE .t.
      oEventLoop:processEvents()
      IF lExit
         EXIT
      ENDIF
   ENDDO
   __HBQT_EVENTS_DISCONNECT( s_events,  oWnd, QEvent_Close )
   oEventLoop:exit( 0 )
   oEventLoop := 0

   RETURN

/*----------------------------------------------------------------------*/

STATIC FUNCTION Build_MenuBar( oWnd )
   LOCAL oMenuBar, oMenu1, oMenu2

   oMenuBar := QMenuBar()
   oMenuBar:resize( oWnd:width(), 25 )

   oMenu1 := QMenu()
   oMenu1:setTitle( "&File" )
   __HBQT_SLOTS_CONNECT( s_slots,  oMenu1:addAction_1( "new.png" , "&New"  ), QT_EVE_TRIGGERED_B, {|w,l| FileDialog( "New" , w, l ) } )
   __HBQT_SLOTS_CONNECT( s_slots,  oMenu1:addAction_1( "open.png", "&Open" ), QT_EVE_TRIGGERED_B, {|w,l| FileDialog( "Open", w, l ) } )
   oMenu1:addSeparator()
   __HBQT_SLOTS_CONNECT( s_slots,  oMenu1:addAction_1( "save.png", "&Save" ), QT_EVE_TRIGGERED_B, {|w,l| FileDialog( "Save", w, l ) } )
   oMenu1:addSeparator()
   __HBQT_SLOTS_CONNECT( s_slots,  oMenu1:addAction( "E&xit" ), QT_EVE_TRIGGERED_B, {|w,l| w := w, l := l, MsgInfo( "Exit ?" ) } )
   oMenuBar:addMenu( oMenu1 )

   oMenu2 := QMenu()
   oMenu2:setTitle( "&Dialogs" )
   __HBQT_SLOTS_CONNECT( s_slots,  oMenu2:addAction( "&Colors"    ), QT_EVE_TRIGGERED_B, {|w,l| Dialogs( "Colors"   , w, l ) } )
   __HBQT_SLOTS_CONNECT( s_slots,  oMenu2:addAction( "&Fonts"     ), QT_EVE_TRIGGERED_B, {|w,l| Dialogs( "Fonts"    , w, l ) } )
   oMenu2:addSeparator()
   __HBQT_SLOTS_CONNECT( s_slots,  oMenu2:addAction( "&PageSetup" ), QT_EVE_TRIGGERED_B, {|w,l| Dialogs( "PageSetup", w, l ) } )
   __HBQT_SLOTS_CONNECT( s_slots,  oMenu2:addAction( "P&review"   ), QT_EVE_TRIGGERED_B, {|w,l| Dialogs( "Preview"  , w, l ) } )
   oMenu2:addSeparator()
   __HBQT_SLOTS_CONNECT( s_slots,  oMenu2:addAction( "&Wizard"    ), QT_EVE_TRIGGERED_B, {|w,l| Dialogs( "Wizard"   , w, l ) } )
   __HBQT_SLOTS_CONNECT( s_slots,  oMenu2:addAction( "W&ebPage"   ), QT_EVE_TRIGGERED_B, {|w,l| Dialogs( "WebPage"  , w, l ) } )
   oMenu2:addSeparator()
   __HBQT_SLOTS_CONNECT( s_slots,  oMenu2:addAction( "&Another Dialog" ), QT_EVE_TRIGGERED_B, {|w,l| w := w, l := l, hb_threadStart( {|| ExecOneMore() } ) } )
   oMenuBar:addMenu( oMenu2 )

   oWnd:setMenuBar( oMenuBar )

   RETURN { oMenu1, oMenu2, oMenuBar }

/*----------------------------------------------------------------------*/

STATIC FUNCTION Build_ToolBar( oWnd )
   LOCAL oTB, oActNew, oActOpen, oActSave

   /* Create a Toolbar Object */
   oTB := QToolBar()

   /* Create an action */
   oActNew := QAction( oWnd )
   oActNew:setText( "&New" )
   oActNew:setIcon( "new.png" )
   oActNew:setToolTip( "A New File" )
   /* Attach codeblock to be triggered */
   __HBQT_SLOTS_CONNECT( s_slots, oActNew, QT_EVE_TRIGGERED_B, {|w,l| FileDialog( "New" , w, l ) } )
   /* Attach Action with Toolbar */
   oTB:addAction( oActNew )

   /* Create another action */
   oActOpen := QAction( oWnd )
   oActOpen:setText( "&Open" )
   oActOpen:setIcon( "open.png" )
   oActOpen:setToolTip( "Select a file to be opened!" )
   /* Attach codeblock to be triggered */
   __HBQT_SLOTS_CONNECT( s_slots, oActOpen, QT_EVE_TRIGGERED_B, {|w,l| FileDialog( "Open" , w, l ) } )
   /* Attach Action with Toolbar */
   oTB:addAction( oActOpen )

   oTB:addSeparator()

   /* Create another action */
   oActSave := QAction( oWnd )
   oActSave:setText( "&Save" )
   oActSave:setIcon( "save.png" )
   oActSave:setToolTip( "Save this file!" )
   /* Attach codeblock to be triggered */
   __HBQT_SLOTS_CONNECT( s_slots, oActSave, QT_EVE_TRIGGERED_B, {|w,l| FileDialog( "Save" , w, l ) } )
   /* Attach Action with Toolbar */
   oTB:addAction( oActSave )

   /* Add this toolbar with main window */
   oWnd:addToolBar_1( oTB )

   ///////////////////////////////////////////////////////////
#if 0
   /* Build another toolbar - we will have two toolbats now */
   oTB := QToolBar( oWnd )

   oAct := QAction( oWnd )
   oAct:setText( "&Colors" )
   oAct:setToolTip( "Colors Dialog" )
   __HBQT_SLOTS_CONNECT( s_slots, oAct, QT_EVE_TRIGGERED_B, {|w,l| Dialogs( "Colors", w, l ) } )
   oTB:addAction( oAct )

   oAct := QAction( oWnd )
   oAct:setText( "&Fonts" )
   oAct:setToolTip( "Fonts Dialog" )
   __HBQT_SLOTS_CONNECT( s_slots, oAct, QT_EVE_TRIGGERED_B, {|w,l| Dialogs( "Fonts", w, l ) } )
   oTB:addAction( oAct )

   oTB:addSeparator()

   oAct := QAction( oWnd )
   oAct:setText( "&PgSetup" )
   oAct:setToolTip( "Page Setup Dialog" )
   __HBQT_SLOTS_CONNECT( s_slots, oAct, QT_EVE_TRIGGERED_B, {|w,l| Dialogs( "PageSetup", w, l ) } )
   oTB:addAction( oAct )

   oAct := QAction( oWnd )
   oAct:setText( "&Preview" )
   oAct:setToolTip( "Page Preview Dialog" )
   __HBQT_SLOTS_CONNECT( s_slots, oAct, QT_EVE_TRIGGERED_B, {|w,l| Dialogs( "Preview", w, l ) } )
   oTB:addAction( oAct )

   oTB:addSeparator()

   oAct := QAction( oWnd )
   oAct:setText( "&Webpage" )
   oAct:setToolTip( "Web Browser Dialog" )
   __HBQT_SLOTS_CONNECT( s_slots, oAct, QT_EVE_TRIGGERED_B, {|w,l| Dialogs( "WebPage", w, l ) } )
   oTB:addAction( oAct )

   oAct := QAction( oWnd )
   oAct:setText( "&Wizard" )
   oAct:setToolTip( "Generic Wizard Dialog" )
   __HBQT_SLOTS_CONNECT( s_slots, oAct, QT_EVE_TRIGGERED_B, {|w,l| Dialogs( "Wizard", w, l ) } )
   oTB:addAction( oAct )

   oAct := QAction( oWnd )
   oAct:setText( "&SystemTray" )
   oAct:setToolTip( "Show in System Tray!" )
   __HBQT_SLOTS_CONNECT( s_slots, oAct, QT_EVE_TRIGGERED_B, {|w,l| ShowInSystemTray( oWnd, w, l ) } )
   oTB:addAction( oAct )

   /* Add this toolbar with main window */
   oWnd:addToolBar_1( oTB )
#endif
   RETURN { oActNew, oActOpen, oActSave, oTB }

/*----------------------------------------------------------------------*/

STATIC FUNCTION Build_PushButton( oWnd, aPos, aSize, cLabel, cMsg, lExit )
   LOCAL oBtn

   DEFAULT cLabel TO "Push Button"
   DEFAULT cMsg   TO "Push Button Pressed"

   oBtn := QPushButton( oWnd )
   oBtn:setText( cLabel )
   oBtn:move( aPos[ 1 ],aPos[ 2 ] )
   oBtn:resize( aSize[ 1 ],aSize[ 2 ] )
   oBtn:show()
   IF hb_isLogical( lExit )
      __HBQT_SLOTS_CONNECT( s_slots, oBtn, QT_EVE_CLICKED, {|| lExit := .t. } )
   ELSE
      __HBQT_SLOTS_CONNECT( s_slots, oBtn, QT_EVE_CLICKED, {|| MsgInfo( cMsg ), lExit := .t. } )
   ENDIF

   RETURN oBtn

/*----------------------------------------------------------------------*/

STATIC FUNCTION Build_Grid( oWnd, aPos, aSize )
   LOCAL oGrid, oBrushBackItem0x0, oBrushForeItem0x0, oGridItem0x0

   oGrid := QTableWidget( oWnd )
   oGrid:setRowCount( 2 )
   oGrid:setColumnCount( 4 )
   //
   oBrushBackItem0x0 := QBrush()
   oBrushBackItem0x0:setStyle( 1 )        // Solid Color
   oBrushBackItem0x0:setColor_1( 10 )     // http://doc.qtsoftware.com/4.5/qt.html#GlobalColor-enum
   //
   oBrushForeItem0x0 := QBrush()
   oBrushForeItem0x0:setColor_1( 7 )
   //
   oGridItem0x0 := QTableWidgetItem()
   oGridItem0x0:setBackground( oBrushBackItem0x0 )
   oGridItem0x0:setForeground( oBrushForeItem0x0 )
   oGridItem0x0:setText( "Item 0x0" )
   //
   oGrid:setItem( 0, 0, oGridItem0x0 )
   //
   oGrid:Move( aPos[ 1 ], aPos[ 2 ] )
   oGrid:ReSize( aSize[ 1 ], aSize[ 2 ] )
   //
   oGrid:Show()

   RETURN {  oBrushBackItem0x0, oBrushForeItem0x0, oGridItem0x0, oGrid }

/*----------------------------------------------------------------------*/

STATIC FUNCTION Build_Tabs( oWnd, aPos, aSize )
   LOCAL oTabWidget, oTab1, oTab2, oTab3, aTree, aCntl, aText

   oTabWidget := QTabWidget( oWnd )

   oTab1 := QWidget()
   oTab2 := QWidget()
   oTab3 := QWidget()

   oTabWidget:addTab( oTab1, "Folders"  )
   oTabWidget:addTab( oTab2, "Controls" )
   oTabWidget:addTab( oTab3, "TextBox"  )

   oTabWidget:Move( aPos[ 1 ], aPos[ 2 ] )
   oTabWidget:ReSize( aSize[ 1 ], aSize[ 2 ] )
   oTabWidget:show()

   aTree := Build_Treeview( oTab1 )
   aadd( aTree, oTab1 )
   aCntl := Build_Controls( oTab2 )
   aadd( aCntl, oTab2 )
   aText := Build_TextBox( oTab3 )
   aadd( aText, oTab3 )

   RETURN { aCntl, aTree, aText, oTabWidget }

/*----------------------------------------------------------------------*/

STATIC FUNCTION Build_TreeView( oWnd )
   LOCAL oTV, oDirModel

   oTV := QTreeView( oWnd )
   oTV:setMouseTracking( .t. )
*  __HBQT_SLOTS_CONNECT( s_slots, oTV, QT_EVE_HOVERED, {|i| HB_TRACE( HB_TR_ALWAYS, ( "oTV:hovered" ) } )
   oDirModel := QDirModel()
   oTV:setModel( oDirModel )
   oTV:move( 5, 7 )
   oTV:resize( 345, 365 )
   OTV:show()

   RETURN { oDirModel, oTV }

/*----------------------------------------------------------------------*/

STATIC FUNCTION Build_ListBox( oWnd, aPos, aSize )
   LOCAL oListBox, oStrList, oStrModel

   oListBox := QListView( oWnd )
   oListBox:setMouseTracking( .t. )
*  __HBQT_SLOTS_CONNECT( s_slots, oListBox, QT_EVE_HOVERED, {|i| HB_TRACE( HB_TR_ALWAYS, ( "oListBox:hovered" ) } )

   oStrList := QStringList()

   oStrList:append( "India"          )
   oStrList:append( "United States"  )
   oStrList:append( "England"        )
   oStrList:append( "Japan"          )
   oStrList:append( "Hungary"        )
   oStrList:append( "Argentina"      )
   oStrList:append( "China"          )
   oStrList:sort()

   oStrModel := QStringListModel()
   oStrModel:setStringList( oStrList )

   oListBox:setModel( oStrModel )
   oListBox:Move( aPos[ 1 ], aPos[ 2 ] )
   oListBox:ReSize( aSize[ 1 ], aSize[ 2 ] )
   oListBox:Show()

   RETURN { oStrList, oStrModel, oListBox }

/*----------------------------------------------------------------------*/

STATIC FUNCTION Build_TextBox( oWnd )
   LOCAL oTextBox

   oTextBox := QTextEdit( oWnd )
   oTextBox:Move( 5, 7 )
   oTextBox:Resize( 345,365 )
   oTextBox:setAcceptRichText( .t. )
   oTextBox:setPlainText( "This is Harbour QT implementation" )
   oTextBox:Show()

   RETURN oTextBox

/*----------------------------------------------------------------------*/

STATIC FUNCTION Build_Controls( oWnd )
   LOCAL oEdit, oCheckBox, oComboBox, oSpinBox, oRadioButton

   oEdit := QLineEdit( oWnd )
   __HBQT_SLOTS_CONNECT( s_slots, oEdit, QT_EVE_RETURNPRESSED, {|i| i := i, MsgInfo( oEdit:text() ) } )
   oEdit:move( 5, 10 )
   oEdit:resize( 345, 30 )
   oEdit:setMaxLength( 40 )
   oEdit:setText( "TextBox Testing Max Length = 40" )
   oEdit:setAlignment( 1 )   // 1: Left  2: Right  4: center 8: use all textbox length
   oEdit:show()

   oComboBox := QComboBox( oWnd )
   oComboBox:addItem( "First"  )
   oComboBox:addItem( "Second" )
   oComboBox:addItem( "Third"  )
   __HBQT_SLOTS_CONNECT( s_slots, oComboBox, QT_EVE_CURRENTINDEXCHANGED_I, {|i| i := i, MsgInfo( oComboBox:itemText( i ) ) } )
   oComboBox:move( 5, 60 )
   oComboBox:resize( 345, 30 )
   oComboBox:show()

   oCheckBox := QCheckBox( oWnd )
   __HBQT_SLOTS_CONNECT( s_slots, oCheckBox, QT_EVE_STATECHANGED_I, {|i| i := i, MsgInfo( IF( i == 0,"Uncheckd","Checked" ) ) } )
   oCheckBox:setText( "Testing CheckBox HbQt" )
   oCheckBox:move( 5, 110 )
   oCheckBox:resize( 345, 30 )
   oCheckBox:show()

   oSpinBox := QSpinBox( oWnd )
   oSpinBox:Move( 5, 160 )
   oSpinBox:ReSize( 345, 30 )
   oSpinBox:Show()

   oRadioButton := QRadioButton( oWnd )
   __HBQT_SLOTS_CONNECT( s_slots, oRadioButton, QT_EVE_CLICKED, {|i| i := i, MsgInfo( "Checked" ) } )
   oRadioButton:Move( 5, 210 )
   oRadioButton:ReSize( 345, 30 )
   oRadioButton:Show()

   RETURN { oEdit, oComboBox, oCheckBox, oSpinBox, oRadioButton }

/*----------------------------------------------------------------------*/

STATIC FUNCTION Build_ProgressBar( oWnd, aPos, aSize )
   LOCAL oProgressBar

   oProgressBar := QProgressBar( oWnd )
   oProgressBar:SetRange( 1, 1500 )
   oProgressBar:Setvalue( 500 )
   oProgressBar:Move( aPos[ 1 ], aPos[ 2 ] )
   oProgressBar:ReSize( aSize[ 1 ], aSize[ 2 ] )
   oProgressBar:Show()

   RETURN oProgressBar

/*----------------------------------------------------------------------*/

STATIC FUNCTION Build_Label( oWnd, aPos, aSize )
   LOCAL oLabel

   oLabel := QLabel( oWnd )
   oLabel:SetTextFormat( 1 )  // 0 text plain  1 RichText
   oLabel:SetText( [<font color="Blue" size=6 ><u>This is a</u> <i>Label</i> in <b>Harbour QT</b></font>] )
   oLabel:Move( aPos[ 1 ], aPos[ 2 ] )
   oLabel:ReSize( aSize[ 1 ], aSize[ 2 ] )
   oLabel:Show()

   RETURN oLabel

/*----------------------------------------------------------------------*/

STATIC FUNCTION MsgInfo( cMsg )
   LOCAL oMB

   oMB := QMessageBox()
   oMB:setInformativeText( cMsg )
   oMB:setWindowTitle( "Harbour-QT" )
   oMB:exec()

   oMB := NIL
   HB_GCALL( .T.)

   RETURN nil

/*----------------------------------------------------------------------*/

STATIC FUNCTION FileDialog()
   LOCAL oFD

   oFD := QFileDialog()
   oFD:setWindowTitle( "Select a File" )
   oFD:exec()

   oFD := NIL
   HB_GCALL( .T.)

   RETURN nil

/*----------------------------------------------------------------------*/

STATIC FUNCTION Dialogs( cType )
   LOCAL oDlg //, oUrl

   DO CASE
   CASE cType == "PageSetup"
      oDlg := QPageSetupDialog()
      oDlg:setWindowTitle( "Harbour-QT PageSetup Dialog" )
      oDlg:exec()
   CASE cType == "Preview"
      oDlg := QPrintPreviewDialog()
      oDlg:setWindowTitle( "Harbour-QT Preview Dialog" )
      oDlg:exec()
   CASE cType == "Wizard"
      oDlg := QWizard()
      oDlg:setWindowTitle( "Harbour-QT Wizard to Show Slides etc." )
      oDlg:exec()
   CASE cType == "Colors"
      oDlg := QColorDialog()
      oDlg:setWindowTitle( "Harbour-QT Color Selection Dialog" )
      oDlg:exec()
   CASE cType == "WebPage"
      #if 0    // Till we resolve for oDlg:show()
      oDlg := QWebView()
      oUrl := QUrl()
      oUrl:setUrl( "http://www.harbour.vouch.info" )
      oDlg:SetUrl( oUrl )
      oDlg:setWindowTitle( "Harbour-QT Web Page Navigator" )
      oDlg:exec()
      #endif
   CASE cType == "Fonts"
      oDlg := QFontDialog()
      oDlg:setWindowTitle( "Harbour-QT Font Selector" )
      oDlg:exec()
   ENDCASE

   oDlg := NIL
   HB_GCALL( .T.)

   RETURN nil

/*----------------------------------------------------------------------*/

#ifdef __PLATFORM__WINDOWS
#ifndef __WITH_WVT__
PROCEDURE hb_GtSys()
   HB_GT_GUI_DEFAULT()
   RETURN
#endif
#endif

/*----------------------------------------------------------------------*/

FUNCTION ShowInSystemTray( oWnd )
   LOCAL oSys
   LOCAL oMenu

   oMenu := QMenu( oWnd )
   oMenu:setTitle( "&File" )
   __HBQT_SLOTS_CONNECT( s_slots, oMenu:addAction_1( "new.png" , "&Show" ), QT_EVE_TRIGGERED_B, {|| oWnd:show() } )
   oMenu:addSeparator()
   __HBQT_SLOTS_CONNECT( s_slots, oMenu:addAction_1( "save.png", "&Hide" ), QT_EVE_TRIGGERED_B, {|| oWnd:hide() } )

   oSys := QSystemTrayIcon( oWnd )
   oSys:setIcon( 'new.png' )
   oSys:setContextMenu( oMenu )
   oSys:showMessage( "Harbour-QT", "This is Harbour-QT System Tray" )
   oSys:show()
   oWnd:hide()

   RETURN nil

/*----------------------------------------------------------------------*/
