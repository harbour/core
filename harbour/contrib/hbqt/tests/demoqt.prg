/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * QT wrapper main header
 *
 * Copyright 2009 Marcos Antonio Gambeta <marcosgambeta at gmail dot com>
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

/*----------------------------------------------------------------------*/
/*
 *                               A NOTE
 *
 *   This demo is built on auto generated classes by the engine. No attemp
 *   is exercised to refine the way the code must be written. At this moment
 *   my emphasis is on testing phase of QT wrapper functions and classes
 *   generated thereof. In near future the actual implementation will be
 *   based on the Xbase++ XBPParts  compatible framework. You just are
 *   encouraged to sense the power of QT through this expression.
 *
 *                             Pritpal Bedi
 */
/*----------------------------------------------------------------------*/

#include "common.ch"

STATIC oSys, oMenuSys, oActShow, oActHide

/*----------------------------------------------------------------------*/

FUNCTION My_Events( e )
   MsgInfo( "Pressed: " + hb_ntos( e:key() ) )
   RETURN nil

/*----------------------------------------------------------------------*/

PROCEDURE Main()
   Local oLabel, oBtn, oDA, oWnd, oProg, oSBar
   LOCAL aMenu, aTool, aGrid, aTabs, aList

   hbqt_errorsys()

   oWnd := QMainWindow()
   oWnd:show()

   oWnd:setMouseTracking( .t. )
   oWnd:setWindowTitle( "Harbour-Qt Implementation Test Dialog" )
   oWnd:setWindowIcon( "test" )
   oWnd:resize( 900, 500 )

   oDA    := QWidget( oWnd )
   oWnd:setCentralWidget( oDA )

   aMenu  := Build_MenuBar( oWnd )
   aTool  := Build_ToolBar( oWnd )

   oSBar  := QStatusBar( oWnd )
   oWnd:setStatusBar( oSBar )
   oSBar:showMessage( "Harbour-QT Statusbar Ready!" )

   oLabel := Build_Label( oDA, { 30,190 }, { 300, 30 } )
   oBtn   := Build_PushButton( oDA, { 30,240 }, { 100,50 } )
   aGrid  := Build_Grid( oDA, { 30, 30 }, { 450,150 } )
   aTabs  := Build_Tabs( oDA, { 510, 5 }, { 360, 400 } )

   oProg  := Build_ProgressBar( oDA, { 30,300 }, { 200,30 } )
   aList  := Build_ListBox( oDA, { 310,240 }, { 150, 100 } )

   oWnd:connect(  6, {|e| My_Events( e ) } )
   oWnd:connect( 19, {|| QApplication():quit() } )
   oWnd:Show()

   QApplication():exec()

   xReleaseMemory( { oBtn, oLabel, oProg, oSBar, aGrid, aList, aMenu, aTool, aTabs, oDA, oWnd } )

   RETURN

/*----------------------------------------------------------------------*/

FUNCTION xReleaseMemory( aObj )
   #if 1
   LOCAL i
   FOR i := 1 TO len( aObj )
      IF hb_isObject( aObj[ i ] )
         aObj[ i ] := NIL
      ELSEIF hb_isArray( aObj[ i ] )
         xReleaseMemory( aObj[ i ] )
      ENDIF
   NEXT
   #else
      HB_SYMBOL_UNUSED( aObj )
   #endif
   RETURN nil

/*----------------------------------------------------------------------*/

PROCEDURE ExecOneMore()
   Local oLabel, oBtn, oDA, oWnd, oProg, oSBar
   LOCAL aMenu, aTool, aGrid, aTabs, aList, oEventLoop
   LOCAL lExit := .f.

   oWnd := QMainWindow()

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
   oBtn   := Build_PushButton( oDA, { 30,240 }, { 100,50 }, "CLOSE", "This dialog will be closed now!", @lExit )
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
   oEventLoop:exit( 0 )
   oEventLoop := 0

   xReleaseMemory( { oBtn, oLabel, oProg, oSBar, aGrid, aList, aMenu, aTool, aTabs, oDA, oWnd, oEventLoop } )

   RETURN

/*----------------------------------------------------------------------*/

STATIC FUNCTION Build_MenuBar( oWnd )
   LOCAL oMenuBar, oMenu1, oMenu2
   LOCAL oActNew, oActOpen, oActSave, oActExit
   LOCAL oActColors, oActFonts, oActPgSetup, oActPreview, oActWiz, oActWeb, oActOther

   oMenuBar := QMenuBar()
   oMenuBar:resize( oWnd:width(), 25 )

   oMenu1 := QMenu()
   oMenu1:setTitle( "&File" )

   oActNew := QAction( oMenu1 )
   oActNew:setText( "&New" )
   oActNew:setIcon( hb_dirBase() + "new.png" )
   oActNew:connect( "triggered(bool)", {|w,l| FileDialog( "New" , w, l ) } )
   oMenu1:addAction( oActNew )

   oActOpen := oMenu1:addAction( hb_dirBase() + "open.png", "&Open" )
   oActOpen:connect( "triggered(bool)", {|w,l| FileDialog( "Open" , w, l ) } )

   oMenu1:addSeparator()

   oActSave := oMenu1:addAction( hb_dirBase() + "save.png", "&Save" )
   oActSave:connect( "triggered(bool)", {|w,l| FileDialog( "Save" , w, l ) } )

   oMenu1:addSeparator()

   oActExit := oMenu1:addAction( "E&xit" )
   oActExit:connect( "triggered(bool)", {|| QApplication():quit() } )

   oMenuBar:addMenu( oMenu1 )

   oMenu2 := QMenu()
   oMenu2:setTitle( "&Dialogs" )

   oActColors := oMenu2:addAction( "&Colors" )
   oActColors:connect( "triggered(bool)", {|w,l| Dialogs( "Colors", w, l ) } )

   oActFonts := oMenu2:addAction( "&Fonts" )
   oActFonts:connect( "triggered(bool)", {|w,l| Dialogs( "Fonts", w, l ) } )

   oMenu2:addSeparator()

   oActPgSetup := oMenu2:addAction( "&PageSetup" )
   oActPgSetup:connect( "triggered(bool)", {|w,l| Dialogs( "PageSetup", w, l ) } )

   oActPreview := oMenu2:addAction( "P&review" )
   oActPreview:connect( "triggered(bool)", {|w,l| Dialogs( "Preview", w, l ) } )

   oMenu2:addSeparator()

   oActWiz := oMenu2:addAction( "&Wizard" )
   oActWiz:connect( "triggered(bool)", {|w,l| Dialogs( "Wizard", w, l ) } )

   oActWeb := oMenu2:addAction( "W&ebPage" )
   oActWeb:connect( "triggered(bool)", {|w,l| Dialogs( "WebPage", w, l ) } )

   oMenu2:addSeparator()

   oActOther := oMenu2:addAction( "&Another Dialog" )
   oActOther:connect( "triggered(bool)", {|| ExecOneMore() } )

   oMenuBar:addMenu( oMenu2 )

   oWnd:setMenuBar( oMenuBar )

   RETURN { oMenu1, oMenu2, oMenuBar, oActNew, oActOpen, oActSave, oActExit, ;
            oActColors, oActFonts, oActPgSetup, oActPreview, oActWiz, oActWeb, oActOther }

/*----------------------------------------------------------------------*/

STATIC FUNCTION Build_ToolBar( oWnd )
   LOCAL oTB, oActNew, oActOpen, oActSave

   /* Create a Toolbar Object */
   oTB := QToolBar()

   /* Create an action */
   oActNew := QAction( oWnd )
   oActNew:setText( "&New" )
   oActNew:setIcon( hb_dirBase() + "new.png" )
   oActNew:setToolTip( "A New File" )
   oActNew:connect( "triggered(bool)", {|w,l| FileDialog( "New" , w, l ) } )

   /* Attach Action with Toolbar */
   oTB:addAction( oActNew )

   /* Create another action */
   oActOpen := QAction( oWnd )
   oActOpen:setText( "&Open" )
   oActOpen:setIcon( hb_dirBase() + "open.png" )
   oActOpen:setToolTip( "Select a file to be opened!" )
   oActOpen:connect( "triggered(bool)", {|w,l| FileDialog( "Open" , w, l ) } )
   /* Attach Action with Toolbar */
   oTB:addAction( oActOpen )

   oTB:addSeparator()

   /* Create another action */
   oActSave := QAction( oWnd )
   oActSave:setText( "&Save" )
   oActSave:setIcon( hb_dirBase() + "save.png" )
   oActSave:setToolTip( "Save this file!" )
   oActSave:connect( "triggered(bool)", {|w,l| FileDialog( "Save" , w, l ) } )
   /* Attach Action with Toolbar */
   oTB:addAction( oActSave )

   /* Add this toolbar with main window */
   oWnd:addToolBar( oTB )

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
      oBtn:connect( "clicked()", {|| lExit := .t. } )
   ELSE
      oBtn:connect( "clicked()", {|| MsgInfo( cMsg ), lExit := .t. } )
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
   oBrushBackItem0x0:setColor( 10 )     // http://doc.qtsoftware.com/4.5/qt.html#GlobalColor-enum
   //
   oBrushForeItem0x0 := QBrush()
   oBrushForeItem0x0:setColor( 7 )
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
*  oTV:connect( "hovered()", {|i| HB_TRACE( HB_TR_DEBUG, ( "oTV:hovered" ) } )
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
*  oListBox:connect( "hovered()", {|i| HB_TRACE( HB_TR_DEBUG, ( "oListBox:hovered" ) } )

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
   oEdit:connect( "returnPressed()", {|i| i := i, MsgInfo( oEdit:text() ) } )
   oEdit:move( 5, 10 )
   oEdit:resize( 345, 30 )
   oEdit:setMaxLength( 40 )
   oEdit:setText( "TextBox Testing Max Length = 40" )
   oEdit:setAlignment( 1 )   // 1: Left  2: Right  4: center 8: use all textbox length
   oEdit:show()

   oComboBox := QComboBox( oWnd )
   oComboBox:setInsertPolicy( QComboBox_InsertAlphabetically )
   oComboBox:addItem( "Third"  )
   oComboBox:addItem( "First"  )
   oComboBox:addItem( "Second" )
   oComboBox:connect( "currentIndexChanged(int)", {|i| i := i, MsgInfo( oComboBox:itemText( i ) ) } )
   oComboBox:move( 5, 60 )
   oComboBox:resize( 345, 30 )
   oComboBox:show()

   oCheckBox := QCheckBox( oWnd )
   oCheckBox:connect( "stateChanged(int)", {|i| i := i, MsgInfo( IF( i == 0,"Uncheckd","Checked" ) ) } )
   oCheckBox:setText( "Testing CheckBox HbQt" )
   oCheckBox:move( 5, 110 )
   oCheckBox:resize( 345, 30 )
   oCheckBox:show()

   oSpinBox := QSpinBox( oWnd )
   oSpinBox:Move( 5, 160 )
   oSpinBox:ReSize( 345, 30 )
   oSpinBox:Show()

   oRadioButton := QRadioButton( oWnd )
   oRadioButton:connect( "clicked()", {|i| i := i, MsgInfo( "Checked" ) } )
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

   RETURN nil

/*----------------------------------------------------------------------*/

STATIC FUNCTION FileDialog()
   LOCAL oFD

   oFD := QFileDialog()
   oFD:setWindowTitle( "Select a File" )
   oFD:exec()

   oFD := NIL

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
      oDlg:setWindowTitle( "Harbour-QT Web Page Navigator" )
      oDlg:exec()
      #endif
   CASE cType == "Fonts"
      oDlg := QFontDialog()
      oDlg:setWindowTitle( "Harbour-QT Font Selector" )
      oDlg:exec()
   ENDCASE

   oDlg := NIL

   RETURN nil

/*----------------------------------------------------------------------*/

#ifdef __PLATFORM__WINDOWS
PROCEDURE hb_GtSys()
   HB_GT_GUI_DEFAULT()
   RETURN
#endif

/*----------------------------------------------------------------------*/

FUNCTION ShowInSystemTray( oWnd )

   oMenuSys := QMenu( oWnd )
   oMenuSys:setTitle( "&File" )

   oActShow := oMenuSys:addAction( hb_dirBase() + "new.png" , "&Show" )
   oActShow:connect( "triggered(bool)", {|| oWnd:show() } )

   oMenuSys:addSeparator()

   oActHide := oMenuSys:addAction( hb_dirBase() + "new.png" , "&Show" )
   oActHide:connect( "triggered(bool)", {|| oWnd:hide() } )

   oSys := QSystemTrayIcon( oWnd )
   oSys:setIcon( hb_dirBase() + "new.png" )
   oSys:setContextMenu( oMenuSys )
   oSys:showMessage( "Harbour-QT", "This is Harbour-QT System Tray" )
   oSys:show()
   oWnd:hide()

   RETURN nil

/*----------------------------------------------------------------------*/
