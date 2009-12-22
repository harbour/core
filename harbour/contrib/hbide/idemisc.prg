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
/*----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*/
/*
 *                                EkOnkar
 *                          ( The LORD is ONE )
 *
 *                            Harbour-Qt IDE
 *
 *                  Pritpal Bedi <pritpal@vouchcac.com>
 *                               23Nov2009
 */
/*----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*/

#include "common.ch"
#include "fileio.ch"

#include "xbp.ch"

#include "hbide.ch"

/*----------------------------------------------------------------------*/

PROCEDURE AppSys()
   RETURN

/*----------------------------------------------------------------------*/

PROCEDURE JustACall()
   RETURN

/*----------------------------------------------------------------------*/

FUNCTION ExecPopup( aPops, aPos, qParent )
   LOCAL i, qPop, qPoint, qAct, nAct, cAct, xRet, pAct

   qPop := QMenu():new( IIF( hb_isObject( qParent ), qParent, NIL ) )

   FOR i := 1 TO len( aPops )
      IF empty( aPops[ i,1 ] )
         qPop:addSeparator()
      ELSE
         qPop:addAction( aPops[ i, 1 ] )
      ENDIF
   NEXT

   qPoint := QPoint():new( aPos[ 1 ], aPos[ 2 ] )
   pAct   := qPop:exec_1( qPoint )
   qAct   := QAction():configure( pAct )

   IF !empty( qAct:pPtr ) .and. !empty( cAct := qAct:text() )
      IF ( nAct := ascan( aPops, {|e_| e_[ 1 ] == cAct } ) ) > 0
         xRet := eval( aPops[ nAct,2 ] )
      ENDIF
   ENDIF

   qPop:pPtr := 0

   RETURN xRet

/*----------------------------------------------------------------------*/

FUNCTION MenuAddSep( oMenu )

   oMenu:addItem( { NIL, NIL, XBPMENUBAR_MIS_SEPARATOR, NIL } )

   RETURN nil

/*----------------------------------------------------------------------*/

FUNCTION CreateTarget( cFile, txt_ )
   LOCAL hHandle := fcreate( cFile )
   LOCAL cNewLine := hb_OsNewLine()

   IF hHandle != F_ERROR
      aeval( txt_, { |e| fWrite( hHandle, e + cNewLine ) } )
      fClose( hHandle )
   ENDIF

   RETURN file( cFile )

/*----------------------------------------------------------------------*/

FUNCTION PosAndSize( qWidget )

   RETURN hb_ntos( qWidget:x() )     + "," + hb_ntos( qWidget:y() )      + "," + ;
          hb_ntos( qWidget:width() ) + "," + hb_ntos( qWidget:height() ) + ","

/*----------------------------------------------------------------------*/

FUNCTION ShowWarning( cMsg, cInfo, cTitle )
   LOCAL oMB

   DEFAULT cTitle TO "Information"

   oMB := QMessageBox():new()
   oMB:setText( cMsg )
   IF !empty( cInfo )
      oMB:setInformativeText( cInfo )
   ENDIF
   oMB:setIcon( QMessageBox_Critical )
   oMB:setParent( SetAppWindow():pWidget )
   oMB:setWindowFlags( Qt_Dialog )
   oMB:setWindowTitle( cTitle )

   RETURN oMB:exec()

/*----------------------------------------------------------------------*/

FUNCTION GetYesNo( cMsg, cInfo, cTitle )
   LOCAL oMB

   DEFAULT cTitle TO "Option Please!"

   oMB := QMessageBox():new()
   oMB:setText( "<b>"+ cMsg +"</b>" )
   IF !empty( cInfo )
      oMB:setInformativeText( cInfo )
   ENDIF
   oMB:setIcon( QMessageBox_Information )
   oMB:setParent( SetAppWindow():pWidget )
   oMB:setWindowFlags( Qt_Dialog )
   oMB:setWindowTitle( cTitle )
   oMB:setStandardButtons( QMessageBox_Yes + QMessageBox_No )

   RETURN ( oMB:exec() == QMessageBox_Yes )

/*----------------------------------------------------------------------*/

FUNCTION FetchAFile( oWnd, cTitle, aFlt, cDftDir )
   LOCAL oDlg, cFile

   DEFAULT cTitle  TO "Please Select a File"
   DEFAULT aFlt    TO { { "All Files", "*.*" } }
   DEFAULT cDftDir TO hb_dirBase()

   oDlg := XbpFileDialog():new():create( oWnd, , { 10,10 } )

   oDlg:title       := cTitle
   oDlg:center      := .t.
   oDlg:fileFilters := aFlt

   cFile := oDlg:open( cDftDir, , .f. )

   RETURN cFile

/*----------------------------------------------------------------------*/

FUNCTION ReadSource( cTxtFile )
   LOCAL cFileBody := MemoRead( cTxtFile )

   cFileBody := StrTran( cFileBody, Chr( 13 ) )

   RETURN hb_ATokens( cFileBody, Chr( 10 ) )

/*----------------------------------------------------------------------*/

FUNCTION EvalAsString( cExp )
   LOCAL cValue

   BEGIN SEQUENCE WITH { || break() }
      cValue := eval( &( "{|| " + cExp + "}" ) )
   RECOVER
      cValue := cExp
   END SEQUENCE

   IF !hb_isChar( cValue )
      cValue := ""
   ENDIF

   RETURN cValue

/*----------------------------------------------------------------------*/

FUNCTION FetchHbiStructFromBuffer( cBuffer )
   RETURN PullHbiStruct( hb_atokens( cBuffer, _EOL ) )

/*----------------------------------------------------------------------*/

FUNCTION FetchHbiStructFromFile( cProject )
   RETURN PullHbiStruct( ReadSource( cProject ) )

/*----------------------------------------------------------------------*/

STATIC FUNCTION PullHbiStruct( a_ )
   LOCAL n, s, nPart, cKey, cVal, ss
   LOCAL aPrp := { "Type", "Title", "Location", "WorkingFolder", "DestinationFolder", ;
                                            "Output", "LaunchParams", "LaunchProgram" }

   LOCAL a1_0 := afill( array( PRJ_PRP_PRP_VRBLS ), "" )
   LOCAL a1_1 := {}
   local a2_0 := {}
   local a2_1 := {}
   local a3_0 := {}
   local a3_1 := {}
   local a4_0 := {}
   local a4_1 := {}

   IF .t.
      FOR EACH ss IN a_
         s := alltrim( ss )

         IF .t.
            DO CASE
            CASE s == "[ PROPERTIES ]"
               nPart := PRJ_PRP_PROPERTIES
            CASE s == "[ FLAGS ]"
               nPart := PRJ_PRP_FLAGS
            CASE s == "[ SOURCES ]"
               nPart := PRJ_PRP_SOURCES
            CASE s == "[ METADATA ]"
               nPart := PRJ_PRP_METADATA
            OTHERWISE
               DO CASE
               CASE nPart == PRJ_PRP_PROPERTIES
                  IF ( n := at( "=", s ) ) > 0
                     cKey := alltrim( substr( s, 1, n-1 ) )
                     cVal := alltrim( substr( s, n+1 ) )
                     IF ( n := ascan( aPrp, cKey ) ) > 0
                        a1_0[ n ] := cVal
                     ENDIF
                  ENDIF
               CASE nPart == PRJ_PRP_FLAGS
                  aadd( a2_0, s )

               CASE nPart == PRJ_PRP_SOURCES
                  aadd( a3_0, s )

               CASE nPart == PRJ_PRP_METADATA
                  aadd( a4_0, s )
                  IF !( "#" == left( s,1 ) )
                     IF ( n := at( "=", s ) ) > 0
                        cKey := alltrim( substr( s, 1, n-1 ) )
                        cVal := EvalAsString( alltrim( substr( s, n+1 ) ) )
                        aadd( a4_1, { "<"+ cKey +">", cVal } )
                     ENDIF
                  ENDIF
               ENDCASE
            ENDCASE
         ENDIF
      NEXT

      /* General Properties */
      FOR EACH s IN a1_0
         aadd( a1_1, ParseWithMetaData( s, a4_1 ) )
      NEXT

      /* Parse Flags */
      IF !empty( a2_0 )
         FOR EACH s IN a2_0
            aadd( a2_1, ParseWithMetaData( s, a4_1 ) )
         NEXT
      ENDIF

      /* Parse Files */
      IF !empty( a3_0 )
         FOR EACH s IN a3_0
            IF !( "#" == left( s,1 ) ) .and. !empty( s )
               aadd( a3_1, ParseWithMetaData( s, a4_1 ) )
            ENDIF
         NEXT
      ENDIF

   ENDIF

   RETURN { { a1_0, a1_1 }, { a2_0, a2_1 }, { a3_0, a3_1 }, { a4_0, a4_1 } }

/*----------------------------------------------------------------------*/

FUNCTION SetupMetaKeys( a_ )
   LOCAL s, n, cKey, cVal
   LOCAL a4_1 := {}

   FOR EACH s IN a_
      IF !( "#" == left( s,1 ) )
         IF ( n := at( "=", s ) ) > 0
            cKey := alltrim( substr( s, 1, n-1 ) )
            cVal := EvalAsString( alltrim( substr( s, n+1 ) ) )
            aadd( a4_1, { "<"+ cKey +">", cVal } )
         ENDIF
      ENDIF
   NEXT

   RETURN a4_1

/*----------------------------------------------------------------------*/

FUNCTION ApplyMetaData( s, a_ )
   LOCAL k

   IF ! Empty( a_ )
      FOR EACH k IN a_
         s := StrTran( s, PathNormalized( k[ 2 ], .f. ), k[ 1 ] )
      NEXT
   ENDIF

   RETURN s

/*----------------------------------------------------------------------*/

FUNCTION ParseWithMetaData( s, a_ )
   LOCAL k

   IF ! Empty( a_ )
      FOR EACH k IN a_ DESCEND
         s := StrTran( s, k[ 1 ], k[ 2 ] )
      NEXT
   ENDIF

   RETURN s

/*----------------------------------------------------------------------*/

FUNCTION ArrayToMemo( a_ )
   LOCAL s := ""

   aeval( a_, {|e| s += e + CRLF } )

   s += CRLF

   RETURN s

/*----------------------------------------------------------------------*/

FUNCTION MemoToArray( s )
   LOCAL aLine := hb_ATokens( StrTran( RTrim( s ), CRLF, _EOL ), _EOL )
   LOCAL nNewSize := 0
   LOCAL line

   FOR EACH line IN aLine DESCEND
      IF ! Empty( line )
         nNewSize := line:__enumIndex()
         EXIT
      ENDIF
   NEXT

   ASize( aLine, nNewSize )

   RETURN aLine

/*----------------------------------------------------------------------*/

FUNCTION IsValidText( cSourceFile )
   LOCAL cExt

   hb_fNameSplit( cSourceFile, , , @cExt )
   cExt := lower( cExt )

   RETURN ( cExt $ ".c,.cpp,.prg,.h,.ch,.txt,.log,.ini,.env" )

/*----------------------------------------------------------------------*/

FUNCTION IsValidSource( cSourceFile )
   LOCAL cExt

   hb_fNameSplit( cSourceFile, , , @cExt )
   cExt := lower( cExt )

   RETURN ( cExt $ ".c,.cpp,.prg,.res,.rc" )

/*----------------------------------------------------------------------*/

FUNCTION PathNormalized( cPath, lLower )
   LOCAL S

   DEFAULT lLower TO .T.

   s := strtran( cPath, "\", "/" )

   RETURN IF( lLower, lower( s ), s )

/*----------------------------------------------------------------------*/

FUNCTION FilesToSources( aFiles )
   LOCAL aSrc := {}
   LOCAL s

   FOR EACH s IN aFiles
      IF IsValidSource( s )
         aadd( aSrc, s )
      ENDIF
   NEXT

   RETURN aSrc

/*----------------------------------------------------------------------*/

FUNCTION RequestModules()

   hbxbp_just( QAbstractButton():new()             )
   hbxbp_just( QAbstractItemModel():new()          )
   hbxbp_just( QAbstractItemView():new()           )
   hbxbp_just( QAbstractListModel():new()          )
   hbxbp_just( QAbstractPrintDialog():new()        )
   hbxbp_just( QAbstractScrollArea():new()         )
   hbxbp_just( QAbstractSlider():new()             )
   hbxbp_just( QAbstractSpinBox():new()            )
   hbxbp_just( QAbstractTableModel():new()         )
   hbxbp_just( QAction():new()                     )
   hbxbp_just( QApplication():new()                )
   hbxbp_just( QBitmap():new()                     )
   hbxbp_just( QBoxLayout():new()                  )
   hbxbp_just( QBrush():new()                      )
   hbxbp_just( QButtonGroup():new()                )
   hbxbp_just( QCalendarWidget():new()             )
   hbxbp_just( QCheckBox():new()                   )
   hbxbp_just( QClipboard():new()                  )
   hbxbp_just( QColor():new()                      )
   hbxbp_just( QColorDialog():new()                )
   hbxbp_just( QComboBox():new()                   )
   hbxbp_just( QCommandLinkButton():new()          )
   hbxbp_just( QCommonStyle():new()                )
   hbxbp_just( QConicalGradient():new()            )
   hbxbp_just( QCoreApplication():new()            )
   hbxbp_just( QCursor():new()                     )
   hbxbp_just( QDateEdit():new()                   )
   hbxbp_just( QDateTime():new()                   )
   hbxbp_just( QDateTimeEdit():new()               )
   hbxbp_just( QDesktopWidget():new()              )
   hbxbp_just( QDial():new()                       )
   hbxbp_just( QDialog():new()                     )
   hbxbp_just( QDir():new()                        )
   hbxbp_just( QDirModel():new()                   )
   hbxbp_just( QDockWidget():new()                 )
   hbxbp_just( QDoubleSpinBox():new()              )
   hbxbp_just( QDropEvent():new()                  )
   hbxbp_just( QDragMoveEvent():new()              )
   hbxbp_just( QDragEnterEvent():new()             )
   hbxbp_just( QDragLeaveEvent():new()             )
   hbxbp_just( QErrorMessage():new()               )
   hbxbp_just( QEvent():new()                      )
   hbxbp_just( QEventLoop():new()                  )
   hbxbp_just( QFileDialog():new()                 )
   hbxbp_just( QFileSystemModel():new()            )
   hbxbp_just( QFocusEvent():new()                 )
   hbxbp_just( QFocusFrame():new()                 )
   hbxbp_just( QFont():new()                       )
   hbxbp_just( QFontComboBox():new()               )
   hbxbp_just( QFontDatabase():new()               )
   hbxbp_just( QFontDialog():new()                 )
   hbxbp_just( QFontInfo():new()                   )
   hbxbp_just( QFontMetrics():new()                )
   hbxbp_just( QFontMetricsF():new()               )
   hbxbp_just( QFormLayout():new()                 )
   hbxbp_just( QFrame():new()                      )
   hbxbp_just( QFtp():new()                        )
   hbxbp_just( QGradient():new()                   )
   hbxbp_just( QGridLayout():new()                 )
   hbxbp_just( QGroupBox():new()                   )
   hbxbp_just( QHBoxLayout():new()                 )
   hbxbp_just( QHeaderView():new()                 )
   hbxbp_just( QHttp():new()                       )
   hbxbp_just( QIcon():new()                       )
   hbxbp_just( QImage():new()                      )
   hbxbp_just( QImageReader():new()                )
   hbxbp_just( QImageWriter():new()                )
   hbxbp_just( QInputDialog():new()                )
   hbxbp_just( QInputEvent():new()                 )
   hbxbp_just( QIODevice():new()                   )
   hbxbp_just( QKeyEvent():new()                   )
   hbxbp_just( QKeySequence():new()                )
   hbxbp_just( QLabel():new()                      )
   hbxbp_just( QLatin1Char():new()                 )
   hbxbp_just( QLatin1String():new()               )
   hbxbp_just( QLayout():new()                     )
   hbxbp_just( QLayoutItem():new()                 )
   hbxbp_just( QLCDNumber():new()                  )
   hbxbp_just( QLine():new()                       )
   hbxbp_just( QLinearGradient():new()             )
   hbxbp_just( QLineEdit():new()                   )
   hbxbp_just( QList():new()                       )
   hbxbp_just( QListView():new()                   )
   hbxbp_just( QListWidget():new()                 )
   hbxbp_just( QListWidgetItem():new()             )
   hbxbp_just( QMainWindow():new()                 )
   hbxbp_just( QMenu():new()                       )
   hbxbp_just( QMenuBar():new()                    )
   hbxbp_just( QMessageBox():new()                 )
   hbxbp_just( QModelIndex():new()                 )
   hbxbp_just( QMouseEvent():new()                 )
   hbxbp_just( QMoveEvent():new()                  )
   hbxbp_just( QObject():new()                     )
   hbxbp_just( QPaintDevice():new()                )
   hbxbp_just( QPageSetupDialog():new()            )
   hbxbp_just( QPainter():new()                    )
   hbxbp_just( QPaintEvent():new()                 )
   hbxbp_just( QPalette():new()                    )
   hbxbp_just( QPen():new()                        )
   hbxbp_just( QPicture():new()                    )
   hbxbp_just( QPixmap():new()                     )
   hbxbp_just( QPoint():new()                      )
   hbxbp_just( QPointF():new()                     )
   hbxbp_just( QPrintDialog():new()                )
   hbxbp_just( QPrintEngine():new()                )
   hbxbp_just( QPrinter():new()                    )
   hbxbp_just( QPrintPreviewDialog():new()         )
   hbxbp_just( QProcess():new()                    )
   hbxbp_just( QProgressBar():new()                )
   hbxbp_just( QProgressDialog():new()             )
   hbxbp_just( QPushButton():new()                 )
   hbxbp_just( QRadialGradient():new()             )
   hbxbp_just( QRadioButton():new()                )
   hbxbp_just( QRect():new()                       )
   hbxbp_just( QRectF():new()                      )
   hbxbp_just( QRegion():new()                     )
   hbxbp_just( QResizeEvent():new()                )
   hbxbp_just( QResource():new()                   )
   hbxbp_just( QScrollArea():new()                 )
   hbxbp_just( QScrollBar():new()                  )
   hbxbp_just( QSignalMapper():new()               )
   hbxbp_just( QSize():new()                       )
   hbxbp_just( QSizeF():new()                      )
   hbxbp_just( QSizeGrip():new()                   )
   hbxbp_just( QSizePolicy():new()                 )
   hbxbp_just( QSlider():new()                     )
   hbxbp_just( QSound():new()                      )
   hbxbp_just( QSpinBox():new()                    )
   hbxbp_just( QSplashScreen():new()               )
   hbxbp_just( QSplitter():new()                   )
   hbxbp_just( QStandardItem():new()               )
   hbxbp_just( QStandardItemModel():new()          )
   hbxbp_just( QStatusBar():new()                  )
   hbxbp_just( QStringList():new()                 )
   hbxbp_just( QStringListModel():new()            )
   hbxbp_just( QSystemTrayIcon():new()             )
   hbxbp_just( QTabBar():new()                     )
   hbxbp_just( QTableView():new()                  )
   hbxbp_just( QTableWidget():new()                )
   hbxbp_just( QTableWidgetItem():new()            )
   hbxbp_just( QTabWidget():new()                  )
   hbxbp_just( QTextBlock():new()                  )
   hbxbp_just( QTextBlockFormat():new()            )
   hbxbp_just( QTextBlockGroup():new()             )
   hbxbp_just( QTextBrowser():new()                )
   hbxbp_just( QTextBoundaryFinder():new()         )
   hbxbp_just( QTextCharFormat():new()             )
   hbxbp_just( QTextCodec():new()                  )
   hbxbp_just( QTextCursor():new()                 )
   hbxbp_just( QTextDecoder():new()                )
   hbxbp_just( QTextDocument():new()               )
   hbxbp_just( QTextDocumentFragment():new()       )
   hbxbp_just( QTextDocumentWriter():new()         )
   hbxbp_just( QTextEdit():new()                   )
   hbxbp_just( QTextFrame():new()                  )
   hbxbp_just( QTextItem():new()                   )
   hbxbp_just( QTextLayout():new()                 )
   hbxbp_just( QTextLength():new()                 )
   hbxbp_just( QTextLine():new()                   )
   hbxbp_just( QTextObject():new()                 )
   hbxbp_just( QTextStream():new()                 )
   hbxbp_just( QTimeEdit():new()                   )
   hbxbp_just( QTimer():new()                      )
   hbxbp_just( QToolBar():new()                    )
   hbxbp_just( QToolBox():new()                    )
   hbxbp_just( QToolButton():new()                 )
   hbxbp_just( QTreeView():new()                   )
   hbxbp_just( QTreeWidget():new()                 )
   hbxbp_just( QTreeWidgetItem():new()             )
   hbxbp_just( QUrl():new()                        )
   hbxbp_just( QVariant():new()                    )
   hbxbp_just( QVBoxLayout():new()                 )
   hbxbp_just( QWheelEvent():new()                 )
   hbxbp_just( QWidget():new()                     )
   hbxbp_just( QWidgetItem():new()                 )

   RETURN NIL

/*----------------------------------------------------------------------*/
