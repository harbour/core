/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * QT wrapper main header
 *
 * Copyright 2009 Pritpal Bedi <pritpal@vouchcac.com>
 *
 * Copyright 2009 Marcos Antonio Gambeta <marcosgambeta at gmail dot com>
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


#ifndef __HBQT_H
#define __HBQT_H

#include <qglobal.h>

#if !( QT_VERSION >= 0x040500 )
   #error QT library version 4.5.0 or upper is required for hbqt.
#endif

#define QT_G_FUNC( hbfunc )   void hbfunc( void * Cargo ) /* callback function for cleaning garbage memory pointer */
typedef QT_G_FUNC( QT_G_FUNC_ );
typedef QT_G_FUNC_ * QT_G_FUNC_PTR;

typedef struct
{
  void * ph;
  bool bNew;
  QT_G_FUNC_PTR func;
} QGC_POINTER;

HB_GARBAGE_FUNC( Q_release );

extern void * hbqt_gcpointer( int iParam );
extern const HB_GC_FUNCS * hbqt_gcFuncs( void );

extern int hbqt_getmemused( void );

extern void * hbqt_pPtrFromObj( int iParam );
extern void * hbqt_pPtrFromItem( PHB_ITEM pObj );

#include "hbqt_garbage.h"

/* TOFIX: Here basically we're using GC pointers without pointer type identification,
          which means that it's very easy to cause a GPF by passing wrong type. */
#define hbqt_par_ExtensionOption( n )               ( ( ExtensionOption             * ) hbqt_gcpointer( n ) )
#define hbqt_par_IUnknown( n )                      ( ( IUnknown                    * ) hbqt_gcpointer( n ) )
#define hbqt_par_QAbstractButton( n )               ( ( QAbstractButton             * ) hbqt_gcpointer( n ) )
#define hbqt_par_QAbstractEventDispatcher( n )      ( ( QAbstractEventDispatcher    * ) hbqt_gcpointer( n ) )
#define hbqt_par_QAbstractItemDelegate( n )         ( ( QAbstractItemDelegate       * ) hbqt_gcpointer( n ) )
#define hbqt_par_QAbstractItemModel( n )            ( ( QAbstractItemModel          * ) hbqt_gcpointer( n ) )
#define hbqt_par_QAbstractItemView( n )             ( ( QAbstractItemView           * ) hbqt_gcpointer( n ) )
#define hbqt_par_QAbstractListModel( n )            ( ( QAbstractListModel          * ) hbqt_gcpointer( n ) )
#define hbqt_par_QAbstractPrintDialog( n )          ( ( QAbstractPrintDialog        * ) hbqt_gcpointer( n ) )
#define hbqt_par_QAbstractProxyModel( n )           ( ( QAbstractProxyModel         * ) hbqt_gcpointer( n ) )
#define hbqt_par_QAbstractScrollArea( n )           ( ( QAbstractScrollArea         * ) hbqt_gcpointer( n ) )
#define hbqt_par_QAbstractSlider( n )               ( ( QAbstractSlider             * ) hbqt_gcpointer( n ) )
#define hbqt_par_QAbstractSpinBox( n )              ( ( QAbstractSpinBox            * ) hbqt_gcpointer( n ) )
#define hbqt_par_QAbstractTableModel( n )           ( ( QAbstractTableModel         * ) hbqt_gcpointer( n ) )
#define hbqt_par_QAbstractTextDocumentLayout( n )   ( ( QAbstractTextDocumentLayout * ) hbqt_gcpointer( n ) )
#define hbqt_par_QAction( n )                       ( ( QAction                     * ) hbqt_gcpointer( n ) )
#define hbqt_par_QActionGroup( n )                  ( ( QActionGroup                * ) hbqt_gcpointer( n ) )
#define hbqt_par_QApplication( n )                  ( ( QApplication                * ) hbqt_gcpointer( n ) )
#define hbqt_par_QAxBase( n )                       ( ( QAxBase                     * ) hbqt_gcpointer( n ) )
#define hbqt_par_QBitArray( n )                     ( ( QBitArray                   * ) hbqt_gcpointer( n ) )
#define hbqt_par_QBitmap( n )                       ( ( QBitmap                     * ) hbqt_gcpointer( n ) )
#define hbqt_par_QBoxLayout( n )                    ( ( QBoxLayout                  * ) hbqt_gcpointer( n ) )
#define hbqt_par_QBrush( n )                        ( ( QBrush                      * ) hbqt_gcpointer( n ) )
#define hbqt_par_QButtonGroup( n )                  ( ( QButtonGroup                * ) hbqt_gcpointer( n ) )
#define hbqt_par_QByteArray( n )                    ( ( QByteArray                  * ) hbqt_gcpointer( n ) )
#define hbqt_par_QCalendarWidget( n )               ( ( QCalendarWidget             * ) hbqt_gcpointer( n ) )
#define hbqt_par_QChar( n )                         ( ( QChar                       * ) hbqt_gcpointer( n ) )
#define hbqt_par_QCheckBox( n )                     ( ( QCheckBox                   * ) hbqt_gcpointer( n ) )
#define hbqt_par_QClipboard( n )                    ( ( QClipboard                  * ) hbqt_gcpointer( n ) )
#define hbqt_par_QColor( n )                        ( ( QColor                      * ) hbqt_gcpointer( n ) )
#define hbqt_par_QColorDialog( n )                  ( ( QColorDialog                * ) hbqt_gcpointer( n ) )
#define hbqt_par_QComboBox( n )                     ( ( QComboBox                   * ) hbqt_gcpointer( n ) )
#define hbqt_par_QCommandLinkButton( n )            ( ( QCommandLinkButton          * ) hbqt_gcpointer( n ) )
#define hbqt_par_QCommonStyle( n )                  ( ( QCommonStyle                * ) hbqt_gcpointer( n ) )
#define hbqt_par_QCompleter( n )                    ( ( QCompleter                  * ) hbqt_gcpointer( n ) )
#define hbqt_par_QConicalGradient( n )              ( ( QConicalGradient            * ) hbqt_gcpointer( n ) )
#define hbqt_par_QContextMenuEvent( n )             ( ( QContextMenuEvent           * ) hbqt_gcpointer( n ) )
#define hbqt_par_QCoreApplication( n )              ( ( QCoreApplication            * ) hbqt_gcpointer( n ) )
#define hbqt_par_QCursor( n )                       ( ( QCursor                     * ) hbqt_gcpointer( n ) )
#define hbqt_par_QDateEdit( n )                     ( ( QDateEdit                   * ) hbqt_gcpointer( n ) )
#define hbqt_par_QDataStream( n )                   ( ( QDataStream                 * ) hbqt_gcpointer( n ) )
#define hbqt_par_QDate( n )                         ( ( QDate                       * ) hbqt_gcpointer( n ) )
#define hbqt_par_QDateTime( n )                     ( ( QDateTime                   * ) hbqt_gcpointer( n ) )
#define hbqt_par_QDateTimeEdit( n )                 ( ( QDateTimeEdit               * ) hbqt_gcpointer( n ) )
#define hbqt_par_QDesktopWidget( n )                ( ( QDesktopWidget              * ) hbqt_gcpointer( n ) )
#define hbqt_par_QDial( n )                         ( ( QDial                       * ) hbqt_gcpointer( n ) )
#define hbqt_par_QDialog( n )                       ( ( QDialog                     * ) hbqt_gcpointer( n ) )
#define hbqt_par_QDir( n )                          ( ( QDir                        * ) hbqt_gcpointer( n ) )
#define hbqt_par_QDirModel( n )                     ( ( QDirModel                   * ) hbqt_gcpointer( n ) )
#define hbqt_par_QDockWidget( n )                   ( ( QDockWidget                 * ) hbqt_gcpointer( n ) )
#define hbqt_par_QDockWidget( n )                   ( ( QDockWidget                 * ) hbqt_gcpointer( n ) )
#define hbqt_par_QDoubleSpinBox( n )                ( ( QDoubleSpinBox              * ) hbqt_gcpointer( n ) )
#define hbqt_par_QDragEnterEvent( n )               ( ( QDragEnterEvent             * ) hbqt_gcpointer( n ) )
#define hbqt_par_QDragLeaveEvent( n )               ( ( QDragLeaveEvent             * ) hbqt_gcpointer( n ) )
#define hbqt_par_QDragMoveEvent( n )                ( ( QDragMoveEvent              * ) hbqt_gcpointer( n ) )
#define hbqt_par_QDropEvent( n )                    ( ( QDropEvent                  * ) hbqt_gcpointer( n ) )
#define hbqt_par_QErrorMessage( n )                 ( ( QErrorMessage               * ) hbqt_gcpointer( n ) )
#define hbqt_par_QEvent( n )                        ( ( QEvent                      * ) hbqt_gcpointer( n ) )
#define hbqt_par_QEventLoop( n )                    ( ( QEventLoop                  * ) hbqt_gcpointer( n ) )
#define hbqt_par_QFile( n )                         ( ( QFile                       * ) hbqt_gcpointer( n ) )
#define hbqt_par_QFileDialog( n )                   ( ( QFileDialog                 * ) hbqt_gcpointer( n ) )
#define hbqt_par_QFileIconProvider( n )             ( ( QFileIconProvider           * ) hbqt_gcpointer( n ) )
#define hbqt_par_QFileInfo( n )                     ( ( QFileInfo                   * ) hbqt_gcpointer( n ) )
#define hbqt_par_QFileSystemModel( n )              ( ( QFileSystemModel            * ) hbqt_gcpointer( n ) )
#define hbqt_par_QFocusEvent( n )                   ( ( QFocusEvent                 * ) hbqt_gcpointer( n ) )
#define hbqt_par_QFocusFrame( n )                   ( ( QFocusFrame                 * ) hbqt_gcpointer( n ) )
#define hbqt_par_QFont( n )                         ( ( QFont                       * ) hbqt_gcpointer( n ) )
#define hbqt_par_QFontComboBox( n )                 ( ( QFontComboBox               * ) hbqt_gcpointer( n ) )
#define hbqt_par_QFontDatabase( n )                 ( ( QFontDatabase               * ) hbqt_gcpointer( n ) )
#define hbqt_par_QFontDialog( n )                   ( ( QFontDialog                 * ) hbqt_gcpointer( n ) )
#define hbqt_par_QFontInfo( n )                     ( ( QFontInfo                   * ) hbqt_gcpointer( n ) )
#define hbqt_par_QFontMetrics( n )                  ( ( QFontMetrics                * ) hbqt_gcpointer( n ) )
#define hbqt_par_QFontMetricsF( n )                 ( ( QFontMetricsF               * ) hbqt_gcpointer( n ) )
#define hbqt_par_QFormLayout( n )                   ( ( QFormLayout                 * ) hbqt_gcpointer( n ) )
#define hbqt_par_QFrame( n )                        ( ( QFrame                      * ) hbqt_gcpointer( n ) )
#define hbqt_par_QFtp( n )                          ( ( QFtp                        * ) hbqt_gcpointer( n ) )
#define hbqt_par_QGradient( n )                     ( ( QGradient                   * ) hbqt_gcpointer( n ) )
#define hbqt_par_QGradientStops( n )                ( ( QGradientStops              * ) hbqt_gcpointer( n ) )
#define hbqt_par_QGridLayout( n )                   ( ( QGridLayout                 * ) hbqt_gcpointer( n ) )
#define hbqt_par_QGroupBox( n )                     ( ( QGroupBox                   * ) hbqt_gcpointer( n ) )
#define hbqt_par_QHBoxLayout( n )                   ( ( QHBoxLayout                 * ) hbqt_gcpointer( n ) )
#define hbqt_par_QHeaderView( n )                   ( ( QHeaderView                 * ) hbqt_gcpointer( n ) )
#define hbqt_par_QHelpEvent( n )                    ( ( QHelpEvent                  * ) hbqt_gcpointer( n ) )
#define hbqt_par_QHttp( n )                         ( ( QHttp                       * ) hbqt_gcpointer( n ) )
#define hbqt_par_QHttpHeader( n )                   ( ( QHttpHeader                 * ) hbqt_gcpointer( n ) )
#define hbqt_par_QHttpResponseHeader( n )           ( ( QHttpResponseHeader         * ) hbqt_gcpointer( n ) )
#define hbqt_par_QHttpRequestHeader( n )            ( ( QHttpRequestHeader          * ) hbqt_gcpointer( n ) )
#define hbqt_par_QIcon( n )                         ( ( QIcon                       * ) hbqt_gcpointer( n ) )
#define hbqt_par_QImage( n )                        ( ( QImage                      * ) hbqt_gcpointer( n ) )
#define hbqt_par_QImageReader( n )                  ( ( QImageReader                * ) hbqt_gcpointer( n ) )
#define hbqt_par_QImageWriter( n )                  ( ( QImageWriter                * ) hbqt_gcpointer( n ) )
#define hbqt_par_QInputContext( n )                 ( ( QInputContext               * ) hbqt_gcpointer( n ) )
#define hbqt_par_QInputDialog( n )                  ( ( QInputDialog                * ) hbqt_gcpointer( n ) )
#define hbqt_par_QInputEvent( n )                   ( ( QInputEvent                 * ) hbqt_gcpointer( n ) )
#define hbqt_par_QInputMethodEvent( n )             ( ( QInputMethodEvent           * ) hbqt_gcpointer( n ) )
#define hbqt_par_QIODevice( n )                     ( ( QIODevice                   * ) hbqt_gcpointer( n ) )
#define hbqt_par_QItemEditorFactory( n )            ( ( QItemEditorFactory          * ) hbqt_gcpointer( n ) )
#define hbqt_par_QItemSelection( n )                ( ( QItemSelection              * ) hbqt_gcpointer( n ) )
#define hbqt_par_QItemSelectionModel( n )           ( ( QItemSelectionModel         * ) hbqt_gcpointer( n ) )
#define hbqt_par_QKeyEvent( n )                     ( ( QKeyEvent                   * ) hbqt_gcpointer( n ) )
#define hbqt_par_QKeySequence( n )                  ( ( QKeySequence                * ) hbqt_gcpointer( n ) )
#define hbqt_par_QLabel( n )                        ( ( QLabel                      * ) hbqt_gcpointer( n ) )
#define hbqt_par_QLatin1Char( n )                   ( ( QLatin1Char                 * ) hbqt_gcpointer( n ) )
#define hbqt_par_QLatin1String( n )                 ( ( QLatin1String               * ) hbqt_gcpointer( n ) )
#define hbqt_par_QLayout( n )                       ( ( QLayout                     * ) hbqt_gcpointer( n ) )
#define hbqt_par_QLayoutItem( n )                   ( ( QLayoutItem                 * ) hbqt_gcpointer( n ) )
#define hbqt_par_QLCDNumber( n )                    ( ( QLCDNumber                  * ) hbqt_gcpointer( n ) )
#define hbqt_par_QLibraryInfo( n )                  ( ( QLibraryInfo                * ) hbqt_gcpointer( n ) )
#define hbqt_par_QLine( n )                         ( ( QLine                       * ) hbqt_gcpointer( n ) )
#define hbqt_par_QLinearGradient( n )               ( ( QLinearGradient             * ) hbqt_gcpointer( n ) )
#define hbqt_par_QLineEdit( n )                     ( ( QLineEdit                   * ) hbqt_gcpointer( n ) )
#define hbqt_par_QLineF( n )                        ( ( QLineF                      * ) hbqt_gcpointer( n ) )
#define hbqt_par_QList( n )                         ( ( QList< void * >             * ) hbqt_gcpointer( n ) )
#define hbqt_par_QListView( n )                     ( ( QListView                   * ) hbqt_gcpointer( n ) )
#define hbqt_par_QListWidget( n )                   ( ( QListWidget                 * ) hbqt_gcpointer( n ) )
#define hbqt_par_QListWidgetItem( n )               ( ( QListWidgetItem             * ) hbqt_gcpointer( n ) )
#define hbqt_par_QLocale( n )                       ( ( QLocale                     * ) hbqt_gcpointer( n ) )
#define hbqt_par_QMainWindow( n )                   ( ( QMainWindow                 * ) hbqt_gcpointer( n ) )
#define hbqt_par_QMatrix( n )                       ( ( QMatrix                     * ) hbqt_gcpointer( n ) )
#define hbqt_par_QMdiArea( n )                      ( ( QMdiArea                    * ) hbqt_gcpointer( n ) )
#define hbqt_par_QMdiSubWindow( n )                 ( ( QMdiSubWindow               * ) hbqt_gcpointer( n ) )
#define hbqt_par_QMenu( n )                         ( ( QMenu                       * ) hbqt_gcpointer( n ) )
#define hbqt_par_QMenuBar( n )                      ( ( QMenuBar                    * ) hbqt_gcpointer( n ) )
#define hbqt_par_QMessageBox( n )                   ( ( QMessageBox                 * ) hbqt_gcpointer( n ) )
#define hbqt_par_QMimeData( n )                     ( ( QMimeData                   * ) hbqt_gcpointer( n ) )
#define hbqt_par_QModelIndex( n )                   ( ( QModelIndex                 * ) hbqt_gcpointer( n ) )
#define hbqt_par_QModelIndexList( n )               ( ( QModelIndexList             * ) hbqt_gcpointer( n ) )
#define hbqt_par_QMouseEvent( n )                   ( ( QMouseEvent                 * ) hbqt_gcpointer( n ) )
#define hbqt_par_QMoveEvent( n )                    ( ( QMoveEvent                  * ) hbqt_gcpointer( n ) )
#define hbqt_par_QMovie( n )                        ( ( QMovie                      * ) hbqt_gcpointer( n ) )
#define hbqt_par_QNetworkAccessManager( n )         ( ( QNetworkAccessManager       * ) hbqt_gcpointer( n ) )
#define hbqt_par_QNetworkProxy( n )                 ( ( QNetworkProxy               * ) hbqt_gcpointer( n ) )
#define hbqt_par_QNetworkRequest( n )               ( ( QNetworkRequest             * ) hbqt_gcpointer( n ) )
#define hbqt_par_QObject( n )                       ( ( QObject                     * ) hbqt_gcpointer( n ) )
#define hbqt_par_QPageSetupDialog( n )              ( ( QPageSetupDialog            * ) hbqt_gcpointer( n ) )
#define hbqt_par_QPaintDevice( n )                  ( ( QPaintDevice                * ) hbqt_gcpointer( n ) )
#define hbqt_par_QPaintEngine( n )                  ( ( QPaintEngine                * ) hbqt_gcpointer( n ) )
#define hbqt_par_QPaintEngineState( n )             ( ( QPaintEngineState           * ) hbqt_gcpointer( n ) )
#define hbqt_par_QPainter( n )                      ( ( QPainter                    * ) hbqt_gcpointer( n ) )
#define hbqt_par_QPainterPath( n )                  ( ( QPainterPath                * ) hbqt_gcpointer( n ) )
#define hbqt_par_QPaintEvent( n )                   ( ( QPaintEvent                 * ) hbqt_gcpointer( n ) )
#define hbqt_par_QPalette( n )                      ( ( QPalette                    * ) hbqt_gcpointer( n ) )
#define hbqt_par_QPen( n )                          ( ( QPen                        * ) hbqt_gcpointer( n ) )
#define hbqt_par_QPicture( n )                      ( ( QPicture                    * ) hbqt_gcpointer( n ) )
#define hbqt_par_QPixmap( n )                       ( ( QPixmap                     * ) hbqt_gcpointer( n ) )
#define hbqt_par_QPlainTextEdit( n )                ( ( QPlainTextEdit              * ) hbqt_gcpointer( n ) )
#define hbqt_par_QPoint( n )                        ( ( QPoint                      * ) hbqt_gcpointer( n ) )
#define hbqt_par_QPointF( n )                       ( ( QPointF                     * ) hbqt_gcpointer( n ) )
#define hbqt_par_QPolygon( n )                      ( ( QPolygon                    * ) hbqt_gcpointer( n ) )
#define hbqt_par_QPolygonF( n )                     ( ( QPolygonF                   * ) hbqt_gcpointer( n ) )
#define hbqt_par_QPrintDialog( n )                  ( ( QPrintDialog                * ) hbqt_gcpointer( n ) )
#define hbqt_par_QPrintEngine( n )                  ( ( QPrintEngine                * ) hbqt_gcpointer( n ) )
#define hbqt_par_QPrinter( n )                      ( ( QPrinter                    * ) hbqt_gcpointer( n ) )
#define hbqt_par_QPrintPreviewDialog( n )           ( ( QPrintPreviewDialog         * ) hbqt_gcpointer( n ) )
#define hbqt_par_QProcess( n )                      ( ( QProcess                    * ) hbqt_gcpointer( n ) )
#define hbqt_par_QProgressBar( n )                  ( ( QProgressBar                * ) hbqt_gcpointer( n ) )
#define hbqt_par_QProgressDialog( n )               ( ( QProgressDialog             * ) hbqt_gcpointer( n ) )
#define hbqt_par_QPushButton( n )                   ( ( QPushButton                 * ) hbqt_gcpointer( n ) )
#define hbqt_par_QRadialGradient( n )               ( ( QRadialGradient             * ) hbqt_gcpointer( n ) )
#define hbqt_par_QRadioButton( n )                  ( ( QRadioButton                * ) hbqt_gcpointer( n ) )
#define hbqt_par_QRect( n )                         ( ( QRect                       * ) hbqt_gcpointer( n ) )
#define hbqt_par_QRectF( n )                        ( ( QRectF                      * ) hbqt_gcpointer( n ) )
#define hbqt_par_QRegExp( n )                       ( ( QRegExp                     * ) hbqt_gcpointer( n ) )
#define hbqt_par_QRegion( n )                       ( ( QRegion                     * ) hbqt_gcpointer( n ) )
#define hbqt_par_QResizeEvent( n )                  ( ( QResizeEvent                * ) hbqt_gcpointer( n ) )
#define hbqt_par_QResource( n )                     ( ( QResource                   * ) hbqt_gcpointer( n ) )
#define hbqt_par_QScrollArea( n )                   ( ( QScrollArea                 * ) hbqt_gcpointer( n ) )
#define hbqt_par_QScrollBar( n )                    ( ( QScrollBar                  * ) hbqt_gcpointer( n ) )
#define hbqt_par_QSessionManager( n )               ( ( QSessionManager             * ) hbqt_gcpointer( n ) )
#define hbqt_par_QSettings( n )                     ( ( QSettings                   * ) hbqt_gcpointer( n ) )
#define hbqt_par_QSignalMapper( n )                 ( ( QSignalMapper               * ) hbqt_gcpointer( n ) )
#define hbqt_par_QSize( n )                         ( ( QSize                       * ) hbqt_gcpointer( n ) )
#define hbqt_par_QSizeF( n )                        ( ( QSizeF                      * ) hbqt_gcpointer( n ) )
#define hbqt_par_QSizeGrip( n )                     ( ( QSizeGrip                   * ) hbqt_gcpointer( n ) )
#define hbqt_par_QSizePolicy( n )                   ( ( QSizePolicy                 * ) hbqt_gcpointer( n ) )
#define hbqt_par_QSlider( n )                       ( ( QSlider                     * ) hbqt_gcpointer( n ) )
#define hbqt_par_QSound( n )                        ( ( QSound                      * ) hbqt_gcpointer( n ) )
#define hbqt_par_QSpacerItem( n )                   ( ( QSpacerItem                 * ) hbqt_gcpointer( n ) )
#define hbqt_par_QSpinBox( n )                      ( ( QSpinBox                    * ) hbqt_gcpointer( n ) )
#define hbqt_par_QSplashScreen( n )                 ( ( QSplashScreen               * ) hbqt_gcpointer( n ) )
#define hbqt_par_QSplitter( n )                     ( ( QSplitter                   * ) hbqt_gcpointer( n ) )
#define hbqt_par_QStandardItem( n )                 ( ( QStandardItem               * ) hbqt_gcpointer( n ) )
#define hbqt_par_QStandardItemModel( n )            ( ( QStandardItemModel          * ) hbqt_gcpointer( n ) )
#define hbqt_par_QStatusBar( n )                    ( ( QStatusBar                  * ) hbqt_gcpointer( n ) )
#define hbqt_par_QStringList( n )                   ( ( QStringList                 * ) hbqt_gcpointer( n ) )
#define hbqt_par_QStringListModel( n )              ( ( QStringListModel            * ) hbqt_gcpointer( n ) )
#define hbqt_par_QStyle( n )                        ( ( QStyle                      * ) hbqt_gcpointer( n ) )
#define hbqt_par_QStyledItemDelegate( n )           ( ( QStyledItemDelegate         * ) hbqt_gcpointer( n ) )
#define hbqt_par_QStyleFactory( n )                 ( ( QStyleFactory               * ) hbqt_gcpointer( n ) )
#define hbqt_par_QStyleHintReturn( n )              ( ( QStyleHintReturn            * ) hbqt_gcpointer( n ) )
#define hbqt_par_QStyleHintReturnMask( n )          ( ( QStyleHintReturnMask        * ) hbqt_gcpointer( n ) )
#define hbqt_par_QStyleHintReturnVariant( n )       ( ( QStyleHintReturnVariant     * ) hbqt_gcpointer( n ) )
#define hbqt_par_QStyleOption( n )                  ( ( QStyleOption                * ) hbqt_gcpointer( n ) )
#define hbqt_par_QStyleOptionButton( n )            ( ( QStyleOptionButton          * ) hbqt_gcpointer( n ) )
#define hbqt_par_QStyleOptionComboBox( n )          ( ( QStyleOptionComboBox        * ) hbqt_gcpointer( n ) )
#define hbqt_par_QStyleOptionComplex( n )           ( ( QStyleOptionComplex         * ) hbqt_gcpointer( n ) )
#define hbqt_par_QStyleOptionDockWidget( n )        ( ( QStyleOptionDockWidget      * ) hbqt_gcpointer( n ) )
#define hbqt_par_QStyleOptionFocusRect( n )         ( ( QStyleOptionFocusRect       * ) hbqt_gcpointer( n ) )
#define hbqt_par_QStyleOptionFrame( n )             ( ( QStyleOptionFrame           * ) hbqt_gcpointer( n ) )
#define hbqt_par_QStyleOptionGroupBox( n )          ( ( QStyleOptionGroupBox        * ) hbqt_gcpointer( n ) )
#define hbqt_par_QStyleOptionHeader( n )            ( ( QStyleOptionHeader          * ) hbqt_gcpointer( n ) )
#define hbqt_par_QStyleOptionMenuItem( n )          ( ( QStyleOptionMenuItem        * ) hbqt_gcpointer( n ) )
#define hbqt_par_QStyleOptionProgressBar( n )       ( ( QStyleOptionProgressBar     * ) hbqt_gcpointer( n ) )
#define hbqt_par_QStyleOptionSizeGrip( n )          ( ( QStyleOptionSizeGrip        * ) hbqt_gcpointer( n ) )
#define hbqt_par_QStyleOptionSlider( n )            ( ( QStyleOptionSlider          * ) hbqt_gcpointer( n ) )
#define hbqt_par_QStyleOptionSpinBox( n )           ( ( QStyleOptionSpinBox         * ) hbqt_gcpointer( n ) )
#define hbqt_par_QStyleOptionTab( n )               ( ( QStyleOptionTab             * ) hbqt_gcpointer( n ) )
#define hbqt_par_QStyleOptionTabBarBase( n )        ( ( QStyleOptionTabBarBase      * ) hbqt_gcpointer( n ) )
#define hbqt_par_QStyleOptionTabWidgetFrame( n )    ( ( QStyleOptionTabWidgetFrame  * ) hbqt_gcpointer( n ) )
#define hbqt_par_QStyleOptionTitleBar( n )          ( ( QStyleOptionTitleBar        * ) hbqt_gcpointer( n ) )
#define hbqt_par_QStyleOptionToolBar( n )           ( ( QStyleOptionToolBar         * ) hbqt_gcpointer( n ) )
#define hbqt_par_QStyleOptionToolBox( n )           ( ( QStyleOptionToolBox         * ) hbqt_gcpointer( n ) )
#define hbqt_par_QStyleOptionToolButton( n )        ( ( QStyleOptionToolButton      * ) hbqt_gcpointer( n ) )
#define hbqt_par_QStyleOptionViewItem( n )          ( ( QStyleOptionViewItem        * ) hbqt_gcpointer( n ) )
#define hbqt_par_QStyleOptionViewItem( n )          ( ( QStyleOptionViewItem        * ) hbqt_gcpointer( n ) )
#define hbqt_par_QStylePainter( n )                 ( ( QStylePainter               * ) hbqt_gcpointer( n ) )
#define hbqt_par_QSyntaxHighlighter( n )            ( ( QSyntaxHighlighter          * ) hbqt_gcpointer( n ) )
#define hbqt_par_QSystemTrayIcon( n )               ( ( QSystemTrayIcon             * ) hbqt_gcpointer( n ) )
#define hbqt_par_QTabBar( n )                       ( ( QTabBar                     * ) hbqt_gcpointer( n ) )
#define hbqt_par_QTableView( n )                    ( ( QTableView                  * ) hbqt_gcpointer( n ) )
#define hbqt_par_QTableWidget( n )                  ( ( QTableWidget                * ) hbqt_gcpointer( n ) )
#define hbqt_par_QTableWidgetItem( n )              ( ( QTableWidgetItem            * ) hbqt_gcpointer( n ) )
#define hbqt_par_QTableWidgetSelectionRange( n )    ( ( QTableWidgetSelectionRange  * ) hbqt_gcpointer( n ) )
#define hbqt_par_QTabWidget( n )                    ( ( QTabWidget                  * ) hbqt_gcpointer( n ) )
#define hbqt_par_QTcpSocket( n )                    ( ( QTcpSocket                  * ) hbqt_gcpointer( n ) )
#define hbqt_par_QTextBlock( n )                    ( ( QTextBlock                  * ) hbqt_gcpointer( n ) )
#define hbqt_par_QTextBlockFormat( n )              ( ( QTextBlockFormat            * ) hbqt_gcpointer( n ) )
#define hbqt_par_QTextBlockGroup( n )               ( ( QTextBlockGroup             * ) hbqt_gcpointer( n ) )
#define hbqt_par_QTextBlockUserData( n )            ( ( QTextBlockUserData          * ) hbqt_gcpointer( n ) )
#define hbqt_par_QTextBoundaryFinder( n )           ( ( QTextBoundaryFinder         * ) hbqt_gcpointer( n ) )
#define hbqt_par_QTextBrowser( n )                  ( ( QTextBrowser                * ) hbqt_gcpointer( n ) )
#define hbqt_par_QTextCharFormat( n )               ( ( QTextCharFormat             * ) hbqt_gcpointer( n ) )
#define hbqt_par_QTextCodec( n )                    ( ( QTextCodec                  * ) hbqt_gcpointer( n ) )
#define hbqt_par_QTextCodec( n )                    ( ( QTextCodec                  * ) hbqt_gcpointer( n ) )
#define hbqt_par_QTextCursor( n )                   ( ( QTextCursor                 * ) hbqt_gcpointer( n ) )
#define hbqt_par_QTextCursor( n )                   ( ( QTextCursor                 * ) hbqt_gcpointer( n ) )
#define hbqt_par_QTextDecoder( n )                  ( ( QTextDecoder                * ) hbqt_gcpointer( n ) )
#define hbqt_par_QTextDocument( n )                 ( ( QTextDocument               * ) hbqt_gcpointer( n ) )
#define hbqt_par_QTextDocumentFragment( n )         ( ( QTextDocumentFragment       * ) hbqt_gcpointer( n ) )
#define hbqt_par_QTextDocumentWriter( n )           ( ( QTextDocumentWriter         * ) hbqt_gcpointer( n ) )
#define hbqt_par_QTextEdit( n )                     ( ( QTextEdit                   * ) hbqt_gcpointer( n ) )
#define hbqt_par_QTextEncoder( n )                  ( ( QTextEncoder                * ) hbqt_gcpointer( n ) )
#define hbqt_par_QTextEngine( n )                   ( ( QTextEngine                 * ) hbqt_gcpointer( n ) )
#define hbqt_par_QTextFormat( n )                   ( ( QTextFormat                 * ) hbqt_gcpointer( n ) )
#define hbqt_par_QTextFragment( n )                 ( ( QTextFragment               * ) hbqt_gcpointer( n ) )
#define hbqt_par_QTextFrame( n )                    ( ( QTextFrame                  * ) hbqt_gcpointer( n ) )
#define hbqt_par_QTextFrameFormat( n )              ( ( QTextFrameFormat            * ) hbqt_gcpointer( n ) )
#define hbqt_par_QTextImageFormat( n )              ( ( QTextImageFormat            * ) hbqt_gcpointer( n ) )
#define hbqt_par_QTextInlineObject( n )             ( ( QTextInlineObject           * ) hbqt_gcpointer( n ) )
#define hbqt_par_QTextItem( n )                     ( ( QTextItem                   * ) hbqt_gcpointer( n ) )
#define hbqt_par_QTextList( n )                     ( ( QTextList                   * ) hbqt_gcpointer( n ) )
#define hbqt_par_QTextLayout( n )                   ( ( QTextLayout                 * ) hbqt_gcpointer( n ) )
#define hbqt_par_QTextLength( n )                   ( ( QTextLength                 * ) hbqt_gcpointer( n ) )
#define hbqt_par_QTextLine( n )                     ( ( QTextLine                   * ) hbqt_gcpointer( n ) )
#define hbqt_par_QTextListFormat( n )               ( ( QTextListFormat             * ) hbqt_gcpointer( n ) )
#define hbqt_par_QTextObject( n )                   ( ( QTextObject                 * ) hbqt_gcpointer( n ) )
#define hbqt_par_QTextOption( n )                   ( ( QTextOption                 * ) hbqt_gcpointer( n ) )
#define hbqt_par_QTextStream( n )                   ( ( QTextStream                 * ) hbqt_gcpointer( n ) )
#define hbqt_par_QTextTableFormat( n )              ( ( QTextTableFormat            * ) hbqt_gcpointer( n ) )
#define hbqt_par_QThread( n )                       ( ( QThread                     * ) hbqt_gcpointer( n ) )
#define hbqt_par_QTime( n )                         ( ( QTime                       * ) hbqt_gcpointer( n ) )
#define hbqt_par_QTimeEdit( n )                     ( ( QTimeEdit                   * ) hbqt_gcpointer( n ) )
#define hbqt_par_QTimer( n )                        ( ( QTimer                      * ) hbqt_gcpointer( n ) )
#define hbqt_par_QToolBar( n )                      ( ( QToolBar                    * ) hbqt_gcpointer( n ) )
#define hbqt_par_QToolBox( n )                      ( ( QToolBox                    * ) hbqt_gcpointer( n ) )
#define hbqt_par_QToolButton( n )                   ( ( QToolButton                 * ) hbqt_gcpointer( n ) )
#define hbqt_par_QTransform( n )                    ( ( QTransform                  * ) hbqt_gcpointer( n ) )
#define hbqt_par_QTranslator( n )                   ( ( QTranslator                 * ) hbqt_gcpointer( n ) )
#define hbqt_par_QTreeView( n )                     ( ( QTreeView                   * ) hbqt_gcpointer( n ) )
#define hbqt_par_QTreeWidget( n )                   ( ( QTreeWidget                 * ) hbqt_gcpointer( n ) )
#define hbqt_par_QTreeWidgetItem( n )               ( ( QTreeWidgetItem             * ) hbqt_gcpointer( n ) )
#define hbqt_par_QUiLoader( n )                     ( ( QUiLoader                   * ) hbqt_gcpointer( n ) )
#define hbqt_par_QUrl( n )                          ( ( QUrl                        * ) hbqt_gcpointer( n ) )
#define hbqt_par_QValidator( n )                    ( ( QValidator                  * ) hbqt_gcpointer( n ) )
#define hbqt_par_QVariant( n )                      ( ( QVariant                    * ) hbqt_gcpointer( n ) )
#define hbqt_par_QVBoxLayout( n )                   ( ( QVBoxLayout                 * ) hbqt_gcpointer( n ) )
#define hbqt_par_QVector( n )                       ( ( QVector                     * ) hbqt_gcpointer( n ) )
#define hbqt_par_QWebFrame( n )                     ( ( QWebFrame                   * ) hbqt_gcpointer( n ) )
#define hbqt_par_QWebHistory( n )                   ( ( QWebHistory                 * ) hbqt_gcpointer( n ) )
#define hbqt_par_QWebHistoryInterface( n )          ( ( QWebHistoryInterface        * ) hbqt_gcpointer( n ) )
#define hbqt_par_QWebHistoryItem( n )               ( ( QWebHistoryItem             * ) hbqt_gcpointer( n ) )
#define hbqt_par_QWebHitTestResult( n )             ( ( QWebHitTestResult           * ) hbqt_gcpointer( n ) )
#define hbqt_par_QWebNetworkRequest( n )            ( ( QWebNetworkRequest          * ) hbqt_gcpointer( n ) )
#define hbqt_par_QWebPage( n )                      ( ( QWebPage                    * ) hbqt_gcpointer( n ) )
#define hbqt_par_QWebPage( n )                      ( ( QWebPage                    * ) hbqt_gcpointer( n ) )
#define hbqt_par_QWebPluginFactory( n )             ( ( QWebPluginFactory           * ) hbqt_gcpointer( n ) )
#define hbqt_par_QWebSecurityOrigin( n )            ( ( QWebSecurityOrigin          * ) hbqt_gcpointer( n ) )
#define hbqt_par_QWebSettings( n )                  ( ( QWebSettings                * ) hbqt_gcpointer( n ) )
#define hbqt_par_QWebView( n )                      ( ( QWebView                    * ) hbqt_gcpointer( n ) )
#define hbqt_par_QWheelEvent( n )                   ( ( QWheelEvent                 * ) hbqt_gcpointer( n ) )
#define hbqt_par_QWidget( n )                       ( ( QWidget                     * ) hbqt_gcpointer( n ) )
#define hbqt_par_QWidgetAction( n )                 ( ( QWidgetAction               * ) hbqt_gcpointer( n ) )
#define hbqt_par_QWidgetItem( n )                   ( ( QWidgetItem                 * ) hbqt_gcpointer( n ) )
#define hbqt_par_QWindowsStyle( n )                 ( ( QWindowsStyle               * ) hbqt_gcpointer( n ) )
#define hbqt_par_QWindowSurface( n )                ( ( QWindowSurface              * ) hbqt_gcpointer( n ) )
#define hbqt_par_QWindowsXPStyle( n )               ( ( QWindowsXPStyle             * ) hbqt_gcpointer( n ) )
#define hbqt_par_QWizard( n )                       ( ( QWizard                     * ) hbqt_gcpointer( n ) )
#define hbqt_par_QWizardPage( n )                   ( ( QWizardPage                 * ) hbqt_gcpointer( n ) )
#define hbqt_par_QWSEvent( n )                      ( ( QWSEvent                    * ) hbqt_gcpointer( n ) )

#define hbqt_par_HBDbfModel( n )                    ( ( HBDbfModel                  * ) hbqt_gcpointer( n ) )
#define hbqt_par_HBEvents( n )                      ( ( HBEvents                    * ) hbqt_gcpointer( n ) )
#define hbqt_par_HBQMainWindow( n )                 ( ( HBQMainWindow               * ) hbqt_gcpointer( n ) )
#define hbqt_par_HBQPlainTextEdit( n )              ( ( HBQPlainTextEdit            * ) hbqt_gcpointer( n ) )
#define hbqt_par_HBQTableView( n )                  ( ( HBQTableView                * ) hbqt_gcpointer( n ) )
#define hbqt_par_HBQSyntaxHighlighter( n )          ( ( HBQSyntaxHighlighter        * ) hbqt_gcpointer( n ) )
#define hbqt_par_HBQTextBlockUserData( n )          ( ( HBQTextBlockUserData        * ) hbqt_gcpointer( n ) )
#define hbqt_par_HBSlots( n )                       ( ( HBSlots                     * ) hbqt_gcpointer( n ) )

#define hbqt_par_QString( n )                       ( ( QString ) hb_parcx( n ) )
#define hbqt_par_QRgb( n )                          ( hb_parnint( n ) )
#define hbqt_par_Bool( n )                          ( hb_parl( n ) )
#define hbqt_par_char( n )                          ( hb_parcx( n ) )

#endif /* __HBQT_H */
