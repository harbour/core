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

#define QT_EVE_TRIGGERED   "triggered(bool)"
#define QT_EVE_TRIGGERED_B "triggered(bool)"
#define QT_EVE_HOVERED     "hovered()"
#define QT_EVE_CLICKED     "clicked()"

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
   /*qt_qapplication_setstyle( QT_PTROF( QWindowsXPStyle():new() ) )*/
   RETURN

EXIT PROCEDURE Qt_End()
   qt_qapplication_exec()
   RETURN

/*----------------------------------------------------------------------*/

PROCEDURE Main()
   Local oLabel
   Local oWnd, oSize
   Local oMenuBar, pIcon
   Local oMenuA, pAction
   LOCAL oPS, oPPrv, oMB, oWZ, oCD, oWP, oSBar, oStyle

   oWnd := QMainWindow():new()
   oWnd:setWindowTitle( "Testing - QMainWindow, QMenu, QMenuBar and QAction " )
   oWnd:setWindowIcon( "test" )
   pIcon := oWnd:windowIcon()

   /* The method 2 */
   oWnd:resize( 640, 400 )
   #if 0
   /* The method 2 */
   oSize := QSize():new()
   oSize:setWidth( 640 )
   oSize:setHeight( 400 )
   oWnd:resize_1( QT_PTROF( oSize ) )
   #endif

   Build_MenuBar( oWnd )

   oSBar := QStatusBar():new( QT_PTROF( oWnd ) )
   oWnd:setStatusBar( QT_PTROF( oSBar ) )
   oSBar:showMessage( "Harbour-QT Statusbar Ready!" )

   oStyle := QWindowsXPStyle()
   oStyle:standardIcon( 2 )
   oWnd:setStyle( QT_PTROF( oStyle ) )

   oLabel := QLabel():New( QT_PTROF( oWnd ) )
   oLabel:setText( "Testing Harbour + Qt" )
   oLabel:move( 200,100 )
   oLabel:show()

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

   oMenuBar:show()

   RETURN nil

/*----------------------------------------------------------------------*/

FUNCTION MsgInfo( cMsg )
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

   oSome := QAction():new()
   oSome := QBoxLayout():new()
   oSome := QCalendarWidget():new()
   oSome := QCheckBox():new()
   oSome := QColorDialog():new()
   oSome := QComboBox():new()
   oSome := QCommandLinkButton():new()
   oSome := QDateEdit():new()
   oSome := QDateTimeEdit():new()
   oSome := QDial():new()
   oSome := QDialog():new()
   oSome := QDoubleSpinBox():new()
   oSome := QErrorMessage():new()
   oSome := QFileDialog():new()
   oSome := QFocusFrame():new()
   oSome := QFontComboBox():new()
   oSome := QFontDialog():new()
   oSome := QFormLayout():new()
   oSome := QFrame():new()
   oSome := QGroupBox():new()
   oSome := QHBoxLayout():new()
   oSome := QInputDialog():new()
   oSome := QLabel():new()
   oSome := QLayout():new()
   oSome := QLayoutItem():new()
   oSome := QLCDNumber():new()
   oSome := QLineEdit():new()
   oSome := QListView():new()
   oSome := QMainWindow():new()
   oSome := QMenu():new()
   oSome := QMenuBar():new()
   oSome := QMessageBox():new()
   oSome := QObject():new()
   oSome := QPageSetupDialog():new()
   oSome := QPaintDevice():new()
   oSome := QPainter():new()
   oSome := QPrintDialog():new()
   oSome := QPrintPreviewDialog():new()
   oSome := QProgressBar():new()
   oSome := QProgressDialog():New()
   oSome := QPushButton():new()
   oSome := QRadioButton():new()
   oSome := QScrollArea():new()
   oSome := QScrollBar():new()
   oSome := QSizeGrip():new()
   oSome := QSlider():new()
   oSome := QSpinBox():new()
   oSome := QSplitter():new()
   oSome := QTabBar():new()
   oSome := QTableView():new()
   oSome := QTableWidget():new()
   oSome := QTableWidgetItem():new()
   oSome := QTabWidget():new()
   oSome := QTextEdit():new()
   oSome := QTimeEdit():new()
   oSome := QToolBar():new()
   oSome := QToolBox():new()
   oSome := QToolButton():new()
   oSome := QTreeView():new()
   oSome := QTreeWidget():new()
   oSome := QTreeWidgetItem():new()
   oSome := QVBoxLayout():new()
   oSome := QWebPage():new()
   oSome := QWebView():new()
   oSome := QWidget():new()
   oSome := QWizard():new()

   RETURN nil

/*----------------------------------------------------------------------*/

