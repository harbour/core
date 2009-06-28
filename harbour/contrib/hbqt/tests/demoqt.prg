/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * QT wrapper main header
 *
 * Copyright 2009 Marcos Antonio Gambeta <marcosgambeta at gmail dot com>
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

#define QT_PTROF( oObj )  ( oObj:pPtr )

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

INIT PROCEDURE Qt_Start()
   qt_qapplication()
   RETURN

EXIT PROCEDURE Qt_End()
   qt_qapplication_quit()
   RETURN

/*----------------------------------------------------------------------*/

PROCEDURE Main()
   Local oLabel, oBtn, oDA, oTabBar
   Local oWnd, oSize
   Local oMenuBar, pIcon
   Local oMenuA, pAction
   LOCAL oPS, oPPrv, oMB, oWZ, oCD, oWP, oSBar, oStyle

   oWnd := QMainWindow():new()
   oWnd:setMouseTracking( .t. )
   oWnd:setWindowTitle( "Testing - QMainWindow, QMenu, QMenuBar and QAction " )
   oWnd:setWindowIcon( "test" )
   pIcon := oWnd:windowIcon()

   /* The method 2 */
   oWnd:resize( 900, 500 )

   oDA := QWidget():new( oWnd )
   oWnd:setCentralWidget( QT_PTROF( oDA ) )

   Build_MenuBar( oWnd )
   Build_ToolBar( oWnd )

   oSBar := QStatusBar():new( QT_PTROF( oWnd ) )
   oWnd:setStatusBar( QT_PTROF( oSBar ) )
   oSBar:showMessage( "Harbour-QT Statusbar Ready!" )

   oStyle := QWindowsXPStyle()
   oStyle:standardIcon( 2 )
   oWnd:setStyle( QT_PTROF( oStyle ) )

   Build_Label( oDA, { 30,190 }, { 300, 30 } )
   Build_PushButton( oDA, { 30,240 }, { 100,50 } )
   Build_Grid( oDA, { 30, 30 }, { 450,150 } )
   Build_Tabs( oDA, { 510, 5 }, { 360, 400 } )
   Build_ProgressBar( oDA, { 30,300 }, { 200,30 } )
   Build_ListBox( oDA, { 310,240 }, { 150, 100 } )

   oWnd:Show()

   RETURN

/*----------------------------------------------------------------------*/

STATIC FUNCTION Build_MenuBar( oWnd )
   LOCAL oMenuBar, oMenu

   oMenuBar := QMenuBar():new( QT_PTROF( oWnd ) )
   oMenuBar:resize( oWnd:width(), 25 )

   oMenu := QMenu():new( QT_PTROF( oMenuBar ) )
   oMenu:setTitle( "&File" )
   Qt_Connect_Signal( oMenu:addAction_1( "new.png" , "&New"  ), QT_EVE_TRIGGERED_B, {|w,l| FileDialog( "New" , w, l ) } )
   Qt_Connect_Signal( oMenu:addAction_1( "open.png", "&Open" ), QT_EVE_TRIGGERED_B, {|w,l| FileDialog( "Open", w, l ) } )
   oMenu:addSeparator()
   Qt_Connect_Signal( oMenu:addAction_1( "save.png", "&Save" ), QT_EVE_TRIGGERED_B, {|w,l| FileDialog( "Save", w, l ) } )
   oMenu:addSeparator()
   Qt_Connect_Signal( oMenu:addAction( "E&xit" ), QT_EVE_TRIGGERED_B, {|w,l| MsgInfo( "Exit ?" ) } )
   oMenuBar:addMenu( QT_PTROF( oMenu ) )

   oMenu := QMenu():new( QT_PTROF( oMenuBar ) )
   oMenu:setTitle( "&Dialogs" )
   Qt_Connect_Signal( oMenu:addAction( "&Colors"    ), QT_EVE_TRIGGERED_B, {|w,l| Dialogs( "Colors"   , w, l ) } )
   Qt_Connect_Signal( oMenu:addAction( "&Fonts"     ), QT_EVE_TRIGGERED_B, {|w,l| Dialogs( "Fonts"    , w, l ) } )
   oMenu:addSeparator()
   Qt_Connect_Signal( oMenu:addAction( "&PageSetup" ), QT_EVE_TRIGGERED_B, {|w,l| Dialogs( "PageSetup", w, l ) } )
   Qt_Connect_Signal( oMenu:addAction( "P&review"   ), QT_EVE_TRIGGERED_B, {|w,l| Dialogs( "Preview"  , w, l ) } )
   oMenu:addSeparator()
   Qt_Connect_Signal( oMenu:addAction( "&Wizard"    ), QT_EVE_TRIGGERED_B, {|w,l| Dialogs( "Wizard"   , w, l ) } )
   Qt_Connect_Signal( oMenu:addAction( "W&ebPage"   ), QT_EVE_TRIGGERED_B, {|w,l| Dialogs( "WebPage"  , w, l ) } )
   oMenuBar:addMenu( QT_PTROF( oMenu ) )

   oWnd:setMenuBar( QT_PTROF( oMenuBar ) )

   RETURN nil

/*----------------------------------------------------------------------*/

STATIC FUNCTION Build_ToolBar( oWnd )
   LOCAL oTB, oAct

   /* Create a Toolbar Object */
   oTB := QToolBar():new( QT_PTROF( oWnd ) )

   /* Create an action */
   oAct := QAction():new( QT_PTROF( oWnd ) )
   oAct:setText( "&New" )
   oAct:setIcon( "new.png" )
   oAct:setToolTip( "A New File" )
   /* Attach codeblock to be triggered */
   Qt_Connect_Signal( QT_PTROF( oAct ), QT_EVE_TRIGGERED_B, {|w,l| FileDialog( "New" , w, l ) } )
   /* Attach Action with Toolbar */
   oTB:addAction( QT_PTROF( oAct ) )

   /* Create another action */
   oAct := QAction():new( QT_PTROF( oWnd ) )
   oAct:setText( "&Open" )
   oAct:setIcon( "open.png" )
   oAct:setToolTip( "Select a file to be opened!" )
   /* Attach codeblock to be triggered */
   Qt_Connect_Signal( QT_PTROF( oAct ), QT_EVE_TRIGGERED_B, {|w,l| FileDialog( "Open" , w, l ) } )
   /* Attach Action with Toolbar */
   oTB:addAction( QT_PTROF( oAct ) )

   oTB:addSeparator()

   /* Create another action */
   oAct := QAction():new( QT_PTROF( oWnd ) )
   oAct:setText( "&Save" )
   oAct:setIcon( "save.png" )
   oAct:setToolTip( "Save this file!" )
   /* Attach codeblock to be triggered */
   Qt_Connect_Signal( QT_PTROF( oAct ), QT_EVE_TRIGGERED_B, {|w,l| FileDialog( "Save" , w, l ) } )
   /* Attach Action with Toolbar */
   oTB:addAction( QT_PTROF( oAct ) )

   /* Add this toolbar with main window */
   oWnd:addToolBar_1( QT_PTROF( oTB ) )

   ///////////////////////////////////////////////////////////

   /* Build another toolbar - we will have two toolbats now */
   oTB := QToolBar():new( QT_PTROF( oWnd ) )

   oAct := QAction():new( QT_PTROF( oWnd ) )
   oAct:setText( "&Colors" )
   oAct:setToolTip( "Colors Dialog" )
   Qt_Connect_Signal( QT_PTROF( oAct ), QT_EVE_TRIGGERED_B, {|w,l| Dialogs( "Colors", w, l ) } )
   oTB:addAction( QT_PTROF( oAct ) )

   oAct := QAction():new( QT_PTROF( oWnd ) )
   oAct:setText( "&Fonts" )
   oAct:setToolTip( "Fonts Dialog" )
   Qt_Connect_Signal( QT_PTROF( oAct ), QT_EVE_TRIGGERED_B, {|w,l| Dialogs( "Fonts", w, l ) } )
   oTB:addAction( QT_PTROF( oAct ) )

   oTB:addSeparator()

   oAct := QAction():new( QT_PTROF( oWnd ) )
   oAct:setText( "&PgSetup" )
   oAct:setToolTip( "Page Setup Dialog" )
   Qt_Connect_Signal( QT_PTROF( oAct ), QT_EVE_TRIGGERED_B, {|w,l| Dialogs( "PageSetup", w, l ) } )
   oTB:addAction( QT_PTROF( oAct ) )

   oAct := QAction():new( QT_PTROF( oWnd ) )
   oAct:setText( "&Preview" )
   oAct:setToolTip( "Page Preview Dialog" )
   Qt_Connect_Signal( QT_PTROF( oAct ), QT_EVE_TRIGGERED_B, {|w,l| Dialogs( "Preview", w, l ) } )
   oTB:addAction( QT_PTROF( oAct ) )

   oTB:addSeparator()

   oAct := QAction():new( QT_PTROF( oWnd ) )
   oAct:setText( "&Webpage" )
   oAct:setToolTip( "Web Browser Dialog" )
   Qt_Connect_Signal( QT_PTROF( oAct ), QT_EVE_TRIGGERED_B, {|w,l| Dialogs( "WebPage", w, l ) } )
   oTB:addAction( QT_PTROF( oAct ) )

   oAct := QAction():new( QT_PTROF( oWnd ) )
   oAct:setText( "&Wizard" )
   oAct:setToolTip( "Generic Wizard Dialog" )
   Qt_Connect_Signal( QT_PTROF( oAct ), QT_EVE_TRIGGERED_B, {|w,l| Dialogs( "Wizard", w, l ) } )
   oTB:addAction( QT_PTROF( oAct ) )

   /* Add this toolbar with main window */
   oWnd:addToolBar_1( QT_PTROF( oTB ) )

   RETURN nil

/*----------------------------------------------------------------------*/

STATIC FUNCTION Build_PushButton( oWnd, aPos, aSize )
   LOCAL oBtn

   oBtn := QPushButton():new( QT_PTROF( oWnd ) )
   oBtn:setText( "Push Button" )
   oBtn:move( aPos[ 1 ],aPos[ 2 ] )
   oBtn:resize( aSize[ 1 ],aSize[ 2 ] )
   oBtn:show()
   Qt_Connect_Signal( QT_PTROF( oBtn ), QT_EVE_CLICKED, {|| MsgInfo( "Push Button Pressed" ) } )

   RETURN nil

/*----------------------------------------------------------------------*/

STATIC FUNCTION Build_Grid( oWnd, aPos, aSize )
   LOCAL oGrid, oBrushBackItem0x0, oBrushForeItem0x0, oGridItem0x0

   oGrid := QTableWidget():new( QT_PTROF( oWnd ) )
   oGrid:setRowCount( 2 )
   oGrid:setColumnCount( 4 )
   //
   oBrushBackItem0x0 := QBrush():new()
   oBrushBackItem0x0:setStyle( 1 )        // Solid Color
   oBrushBackItem0x0:setColor_1( 10 )     // http://doc.qtsoftware.com/4.5/qt.html#GlobalColor-enum
   //
   oBrushForeItem0x0 := QBrush():new()
   oBrushForeItem0x0:setColor_1( 7 )
   //
   oGridItem0x0 := QTableWidgetItem():new()
   oGridItem0x0:setBackground( QT_PTROF( oBrushBackItem0x0 ) )
   oGridItem0x0:setForeground( QT_PTROF( oBrushForeItem0x0 ) )
   oGridItem0x0:setText( "Item 0x0" )
   //
   oGrid:setItem( 0, 0, QT_PTROF( oGridItem0x0 ) )
   //
   oGrid:Move( aPos[ 1 ], aPos[ 2 ] )
   oGrid:ReSize( aSize[ 1 ], aSize[ 2 ] )
   //
   oGrid:Show()

   RETURN nil

/*----------------------------------------------------------------------*/

STATIC FUNCTION Build_Tabs( oWnd, aPos, aSize )
   LOCAL oTabWidget, oTab1, oTab2, oTab3

   oTabWidget := QTabWidget():new( QT_PTROF( oWnd ) )

   oTab1 := QWidget():new( QT_PTROF( oTabWidget ) )
   oTab2 := QWidget():new( QT_PTROF( oTabWidget ) )
   oTab3 := QWidget():new( QT_PTROF( oTabWidget ) )

   oTabWidget:addTab( QT_PTROF( oTab1 ), "Folders"  )
   oTabWidget:addTab( QT_PTROF( oTab2 ), "Controls" )
   oTabWidget:addTab( QT_PTROF( oTab3 ), "TextBox"  )

   oTabWidget:Move( aPos[ 1 ], aPos[ 2 ] )
   oTabWidget:ReSize( aSize[ 1 ], aSize[ 2 ] )
   oTabWidget:show()

   Build_Treeview( oTab1 )
   Build_Controls( oTab2 )
   Build_TextBox( oTab3 )

   RETURN { oTab1, oTab2, oTab3 }

/*----------------------------------------------------------------------*/

STATIC FUNCTION Build_TreeView( oWnd )
   LOCAL oTV, oDirModel

   oTV := QTreeView():new( QT_PTROF( oWnd ) )
   oTV:setMouseTracking( .t. )
   //Qt_Connect_Signal( QT_PTROF( oTV ), QT_EVE_HOVERED, {|o,i| hb_outDebug( "oTV:hovered" ) } )
   oDirModel := QDirModel():new( QT_PTROF( oTV ) )
   oTV:setModel( QT_PTROF( oDirModel ) )
   oTV:move( 5, 7 )
   oTV:resize( 345, 365 )
   OTV:show()

   RETURN nil

/*----------------------------------------------------------------------*/

STATIC FUNCTION Build_ListBox( oWnd, aPos, aSize )
   LOCAL oListBox, oStrList, oStrModel, oAct

   oListBox := QListView():New( QT_PTROF( oWnd ) )
   oListBox:setMouseTracking( .t. )
   //Qt_Connect_Signal( QT_PTROF( oListBox ), QT_EVE_HOVERED, {|o,i| hb_outDebug( "oListBox:hovered" ) } )

   oStrList := QStringList():new( QT_PTROF( oListBox ) )

   oStrList:append( "India"          )
   oStrList:append( "United States"  )
   oStrList:append( "England"        )
   oStrList:append( "Japan"          )
   oStrList:append( "Hungary"        )
   oStrList:append( "Argentina"      )
   oStrList:append( "China"          )
   oStrList:sort()

   oStrModel := QStringListModel():new( QT_PTROF( oListBox ) )
   oStrModel:setStringList( QT_PTROF( oStrList ) )

   oListBox:setModel( QT_PTROF( oStrModel ) )
   oListBox:Move( aPos[ 1 ], aPos[ 2 ] )
   oListBox:ReSize( aSize[ 1 ], aSize[ 2 ] )
   oListBox:Show()

   RETURN nil

/*----------------------------------------------------------------------*/

STATIC FUNCTION Build_TextBox( oWnd )
   LOCAL oTextBox

   oTextBox := QTextEdit():new( QT_PTROF( oWnd ) )
   oTextBox:Move( 5, 7 )
   oTextBox:Resize( 345,365 )
   oTextBox:setAcceptRichText( .t. )
   oTextBox:setPlainText( "This is Harbour QT implementation" )
   oTextBox:Show()

   RETURN nil

/*----------------------------------------------------------------------*/

STATIC FUNCTION Build_Controls( oWnd )
   LOCAL oEdit, oCheckBox, oComboBox, oSpinBox, oRadioButton, oVariant

   oEdit := QLineEdit():new( QT_PTROF( oWnd ) )
   Qt_Connect_Signal( QT_PTROF( oEdit ), QT_EVE_RETURNPRESSED, {|o,i| MsgInfo( oEdit:text() ) } )
   oEdit:move( 5, 10 )
   oEdit:resize( 345, 30 )
   oEdit:setMaxLength( 40 )
   oEdit:setText( "TextBox Testing Max Length = 40" )
   oEdit:setAlignment( 1 )   // 1: Left  2: Right  4: center 8: use all textbox length
   oEdit:show()

   oComboBox := QComboBox():New( QT_PTROF( oWnd ) )
   oComboBox:addItem( "First"  )
   oComboBox:addItem( "Second" )
   oComboBox:addItem( "Third"  )
   //Qt_Connect_Signal( QT_PTROF( oComboBox ), QT_EVE_HIGHLIGHTED_I        , {|o,i| hb_outDebug( oComboBox:itemText( i ) ) } )
   Qt_Connect_Signal( QT_PTROF( oComboBox ), QT_EVE_CURRENTINDEXCHANGED_I, {|o,i| MsgInfo( oComboBox:itemText( i ) ) } )
   oComboBox:move( 5, 60 )
   oComboBox:resize( 345, 30 )
   oComboBox:show()

   oCheckBox := QCheckBox():New( QT_PTROF( oWnd ) )
   Qt_Connect_Signal( QT_PTROF( oCheckBox ), QT_EVE_STATECHANGED_I, {|o,i| MsgInfo( IF( i == 0,"Uncheckd","Checked" ) ) } )
   oCheckBox:setText( "Testing CheckBox HbQt" )
   oCheckBox:move( 5, 110 )
   oCheckBox:resize( 345, 30 )
   oCheckBox:show()

   oSpinBox := QSpinBox():New( QT_PTROF( oWnd ) )
   oSpinBox:Move( 5, 160 )
   oSpinBox:ReSize( 345, 30 )
   oSpinBox:Show()

   oRadioButton := QRadioButton():New( QT_PTROF( oWnd ) )
   Qt_Connect_Signal( QT_PTROF( oRadioButton ), QT_EVE_CLICKED, {|o,i| MsgInfo( "Checked" ) } )
   oRadioButton:Move( 5, 210 )
   oRadioButton:ReSize( 345, 30 )
   oRadioButton:Show()

   RETURN nil

/*----------------------------------------------------------------------*/

STATIC FUNCTION Build_ProgressBar( oWnd, aPos, aSize )
   LOCAL oProgressBar

   oProgressBar := QProgressBar():New( QT_PTROF( oWnd ) )
   oProgressBar:SetRange( 1, 1500 )
   oProgressBar:Setvalue( 500 )
   oProgressBar:Move( aPos[ 1 ], aPos[ 2 ] )
   oProgressBar:ReSize( aSize[ 1 ], aSize[ 2 ] )
   oProgressBar:Show()

   RETURN nil

/*----------------------------------------------------------------------*/

STATIC FUNCTION Build_Label( oWnd, aPos, aSize )
   LOCAL oLabel

   oLabel := QLabel():New( QT_PTROF( oWnd ) )
   oLabel:SetTextFormat( 1 )  // 0 text plain  1 RichText
   oLabel:SetText( [<font color="Blue" size=6 ><u>This is a</u> <i>Label</i> in <b>Harbour QT</b></font>] )
   oLabel:Move( aPos[ 1 ], aPos[ 2 ] )
   oLabel:ReSize( aSize[ 1 ], aSize[ 2 ] )
   oLabel:Show()

   RETURN nil

/*----------------------------------------------------------------------*/

STATIC FUNCTION MsgInfo( cMsg )
   LOCAL oMB

   oMB := QMessageBox():new()
   oMB:setInformativeText( cMsg )
   oMB:setWindowTitle( "Harbour-QT" )
   oMB:show()

   RETURN nil

/*----------------------------------------------------------------------*/

STATIC FUNCTION FileDialog( cType, w, l )
   LOCAL oFD := QFileDialog():new()

   oFD:setWindowTitle( "Select a File" )
   oFD:show()

   RETURN nil

/*----------------------------------------------------------------------*/

STATIC FUNCTION Dialogs( cType, w, l )
   LOCAL oDlg, oUrl

   DO CASE
   CASE cType == "PageSetup"
      oDlg := QPageSetupDialog():new()
      oDlg:setWindowTitle( "Harbour-QT PageSetup Dialog" )
      oDlg:show()
   CASE cType == "Preview"
      oDlg := QPrintPreviewDialog():new()
      oDlg:setWindowTitle( "Harbour-QT Preview Dialog" )
      oDlg:show()
   CASE cType == "Wizard"
      oDlg := QWizard():new()
      oDlg:setWindowTitle( "Harbour-QT Wizard to Show Slides etc." )
      oDlg:show()
   CASE cType == "Colors"
      oDlg := QColorDialog():new()
      oDlg:setWindowTitle( "Harbour-QT Color Selection Dialog" )
      oDlg:show()
   CASE cType == "WebPage"
      oDlg := QWebView():new()
      oUrl := QUrl():new()
      oUrl:setUrl( "http://www.harbour.vouch.info" )
      QT_QWebView_SetUrl( QT_PTROF( oDlg ), QT_PTROF( oUrl ) )
      oDlg:setWindowTitle( "Harbour-QT Web Page Navigator" )
      oDlg:show()
   CASE cType == "Fonts"
      oDlg := QFontDialog():new()
      oDlg:setWindowTitle( "Harbour-QT Font Selector" )
      oDlg:show()
   ENDCASE

   RETURN nil

/*----------------------------------------------------------------------*/

#ifdef __PLATFORM__WINDOWS
PROCEDURE hb_GtSys()
   HB_GT_GUI_DEFAULT()
   RETURN
#endif

/*----------------------------------------------------------------------*/
/*
 * Just to Link Every New Widget
 */
STATIC FUNCTION Dummies()
   LOCAL oSome

   oSome := QAbstractButton():new()
   oSome := QAbstractItemView():new()
   oSome := QAbstractPrintDialog():new()
   oSome := QAbstractScrollArea():new()
   oSome := QAbstractSlider():new()
   oSome := QAbstractSpinBox():new()
   oSome := QAction():new()
   //oSome := QApplication():new()
   oSome := QBitmap():new()
   oSome := QBoxLayout():new()
   oSome := QBrush():new()
   oSome := QCalendarWidget():new()
   oSome := QCheckBox():new()
   oSome := QClipboard():new()
   oSome := QColor():new()
   oSome := QColorDialog():new()
   oSome := QComboBox():new()
   oSome := QCommandLinkButton():new()
   oSome := QCommonStyle():new()
   oSome := QConicalGradient():new()
   //oSome := QCoreApplication():new()
   oSome := QDateEdit():new()
   oSome := QDateTimeEdit():new()
   oSome := QDesktopWidget():new()
   oSome := QDial():new()
   oSome := QDialog():new()
   oSome := QDir():new()
   oSome := QDockWidget():new()
   oSome := QDoubleSpinBox():new()
   oSome := QErrorMessage():new()
   oSome := QEvent():new()
   oSome := QEventLoop():new()
   oSome := QFileDialog():new()
   oSome := QFocusFrame():new()
   oSome := QFont():new()
   oSome := QFontComboBox():new()
   oSome := QFontDialog():new()
   oSome := QFontInfo():new()
   oSome := QFormLayout():new()
   oSome := QFrame():new()
   oSome := QFtp():new()
   oSome := QGradient():new()
   oSome := QGridLayout():new()
   oSome := QGroupBox():new()
   oSome := QHBoxLayout():new()
   oSome := QHeaderView():new()
   oSome := QHttp():new()
   oSome := QIcon():new()
   oSome := QImage():new()
   oSome := QImageReader():new()
   oSome := QImageWriter():new()
   oSome := QInputDialog():new()
   oSome := QInputEvent():new()
   oSome := QLabel():new()
   oSome := QLayout():new()
   oSome := QLayoutItem():new()
   oSome := QLCDNumber():new()
   oSome := QLine():new()
   oSome := QLinearGradient():new()
   oSome := QLineEdit():new()
   oSome := QListView():new()
   oSome := QListWidget():new()
   oSome := QListWidgetItem():new()
   oSome := QMainWindow():new()
   oSome := QMenu():new()
   oSome := QMenuBar():new()
   oSome := QMessageBox():new()
   oSome := QObject():new()
   oSome := QPaintDevice():new()
   oSome := QPageSetupDialog():new()
   oSome := QPainter():new()
   oSome := QPaintEvent():new()
   oSome := QPalette():new()
   oSome := QPen():new()
   oSome := QPicture():new()
   oSome := QPixmap():new()
   oSome := QPoint():new()
   oSome := QPointF():new()
   oSome := QPrintDialog():new()
   oSome := QPrintPreviewDialog():new()
   oSome := QProgressBar():new()
   oSome := QProgressDialog():new()
   oSome := QPushButton():new()
   oSome := QRadialGradient():new()
   oSome := QRect():new()
   oSome := QRectF():new()
   oSome := QRadioButton():new()
   oSome := QRegion():new()
   oSome := QResource():new()
   oSome := QScrollArea():new()
   oSome := QScrollBar():new()
   oSome := QSignalMapper():new()
   oSome := QSize():new()
   oSome := QSizeF():new()
   oSome := QSizeGrip():new()
   oSome := QSizePolicy():new()
   oSome := QSlider():new()
   oSome := QSound():new()
   oSome := QSpinBox():new()
   oSome := QSplashScreen():new()
   oSome := QSplitter():new()
   oSome := QStandardItem():new()
   oSome := QStandardItemModel():new()
   oSome := QStatusBar():new()
   oSome := QStyle():new()
   oSome := QStyledItemDelegate():new()
   oSome := QStyleHintReturn():new()
   oSome := QStyleHintReturnMask():new()
   oSome := QStyleHintReturnVariant():new()
   oSome := QStyleOption():new()
   oSome := QStyleOptionButton():new()
   oSome := QStyleOptionComboBox():new()
   oSome := QStyleOptionComplex():new()
   oSome := QStyleOptionDockWidget():new()
   oSome := QStyleOptionFocusRect():new()
   oSome := QStyleOptionFrame():new()
   oSome := QStyleOptionGroupBox():new()
   oSome := QStyleOptionHeader():new()
   oSome := QStyleOptionMenuItem():new()
   oSome := QStyleOptionProgressBar():new()
   oSome := QStyleOptionSizeGrip():new()
   oSome := QStyleOptionSlider():new()
   oSome := QStyleOptionSpinBox():new()
   oSome := QStyleOptionTab():new()
   oSome := QStyleOptionTabBarBase():new()
   oSome := QStyleOptionTabWidgetFrame():new()
   oSome := QStyleOptionTitleBar():new()
   oSome := QStyleOptionToolBar():new()
   oSome := QStyleOptionToolBox():new()
   oSome := QStyleOptionToolButton():new()
   oSome := QStyleOptionViewItem():new()
   oSome := QStylePainter():new()
   oSome := QTabBar():new()
   oSome := QTableView():new()
   oSome := QTableWidget():new()
   oSome := QTableWidgetItem():new()
   oSome := QTabWidget():new()
   oSome := QTextBlock():new()
   oSome := QTextBlockFormat():new()
   oSome := QTextBlockGroup():new()
   oSome := QTextBrowser():new()
   oSome := QTextBoundaryFinder():new()
   oSome := QTextCharFormat():new()
   oSome := QTextCodec():new()
   oSome := QTextCursor():new()
   oSome := QTextDecoder():new()
   oSome := QTextDocument():new()
   oSome := QTextDocumentFragment():new()
   oSome := QTextDocumentWriter():new()
   oSome := QTextEdit():new()
   oSome := QTextEncoder():new()
   oSome := QTextFormat():new()
   oSome := QTextFragment():new()
   oSome := QTextFrame():new()
   oSome := QTextFrameFormat():new()
   oSome := QTextImageFormat():new()
   oSome := QTextInlineObject():new()
   oSome := QTextItem():new()
   oSome := QTextLayout():new()
   oSome := QTextLength():new()
   oSome := QTextLine():new()
   oSome := QTextObject():new()
   oSome := QTextStream():new()
   oSome := QTimeEdit():new()
   oSome := QTimer():new()
   oSome := QToolBar():new()
   oSome := QToolBox():new()
   oSome := QToolButton():new()
   oSome := QTreeView():new()
   oSome := QTreeWidget():new()
   oSome := QTreeWidgetItem():new()
   oSome := QUrl():new()
   oSome := QVariant():new()
   oSome := QVBoxLayout():new()
   oSome := QWebPage():new()
   oSome := QWidget():new()
   oSome := QWebView():new()
   oSome := QWindowsStyle():new()
   oSome := QWindowsXPStyle():new()
   oSome := QWizard():new()

   RETURN nil

/*----------------------------------------------------------------------*/

