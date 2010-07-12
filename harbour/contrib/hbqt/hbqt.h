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


#ifndef __HBQT_H
#define __HBQT_H

#include "hbapi.h"

#if defined( HB_OS_OS2 )
#  define OS2EMX_PLAIN_CHAR
#  define INCL_BASE
#  define INCL_PM
#  include <os2.h>
#endif

#include <QtCore/qglobal.h>

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
  int type;
} QGC_POINTER;

HB_GARBAGE_FUNC( Q_release );

extern void * hbqt_gcpointer( int iParam );
extern const HB_GC_FUNCS * hbqt_gcFuncs( void );

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
#define hbqt_par_QBuffer( n )                       ( ( QBuffer                     * ) hbqt_gcpointer( n ) )
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
#define hbqt_par_QHideEvent( n )                    ( ( QHideEvent                  * ) hbqt_gcpointer( n ) )
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
#define hbqt_par_QItemDelegate( n )                 ( ( QItemDelegate               * ) hbqt_gcpointer( n ) )
#define hbqt_par_QItemEditorCreator( n )            ( ( QItemEditorCreator          * ) hbqt_gcpointer( n ) )
#define hbqt_par_QItemEditorCreatorBase( n )        ( ( QItemEditorCreatorBase      * ) hbqt_gcpointer( n ) )
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
#define hbqt_par_QPlainTextDocumentLayout( n )      ( ( QPlainTextDocumentLayout    * ) hbqt_gcpointer( n ) )
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
#define hbqt_par_QShowEvent( n )                    ( ( QShowEvent                  * ) hbqt_gcpointer( n ) )
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
#define hbqt_par_QStackedWidget( n )                ( ( QStackedWidget              * ) hbqt_gcpointer( n ) )
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
#define hbqt_par_QWheelEvent( n )                   ( ( QWheelEvent                 * ) hbqt_gcpointer( n ) )
#define hbqt_par_QWidget( n )                       ( ( QWidget                     * ) hbqt_gcpointer( n ) )
#define hbqt_par_QWidgetAction( n )                 ( ( QWidgetAction               * ) hbqt_gcpointer( n ) )
#define hbqt_par_QWidgetItem( n )                   ( ( QWidgetItem                 * ) hbqt_gcpointer( n ) )
#define hbqt_par_QWindowsStyle( n )                 ( ( QWindowsStyle               * ) hbqt_gcpointer( n ) )
#define hbqt_par_QWindowSurface( n )                ( ( QWindowSurface              * ) hbqt_gcpointer( n ) )
#define hbqt_par_QWindowsXPStyle( n )               ( ( QWindowsXPStyle             * ) hbqt_gcpointer( n ) )
#define hbqt_par_QWindowStateChangeEvent( n )       ( ( QWindowStateChangeEvent     * ) hbqt_gcpointer( n ) )
#define hbqt_par_QWizard( n )                       ( ( QWizard                     * ) hbqt_gcpointer( n ) )
#define hbqt_par_QWizardPage( n )                   ( ( QWizardPage                 * ) hbqt_gcpointer( n ) )
#define hbqt_par_QWSEvent( n )                      ( ( QWSEvent                    * ) hbqt_gcpointer( n ) )

#define hbqt_par_HBDbfModel( n )                    ( ( HBDbfModel                  * ) hbqt_gcpointer( n ) )
#define hbqt_par_HBQAbstractItemModel( n )          ( ( HBQAbstractItemModel        * ) hbqt_gcpointer( n ) )
#define hbqt_par_HBEvents( n )                      ( ( HBEvents                    * ) hbqt_gcpointer( n ) )
#define hbqt_par_HBQMainWindow( n )                 ( ( HBQMainWindow               * ) hbqt_gcpointer( n ) )
#define hbqt_par_HBQPlainTextEdit( n )              ( ( HBQPlainTextEdit            * ) hbqt_gcpointer( n ) )
#define hbqt_par_HBQTableView( n )                  ( ( HBQTableView                * ) hbqt_gcpointer( n ) )
#define hbqt_par_HBQSyntaxHighlighter( n )          ( ( HBQSyntaxHighlighter        * ) hbqt_gcpointer( n ) )
#define hbqt_par_HBQTextBlockUserData( n )          ( ( HBQTextBlockUserData        * ) hbqt_gcpointer( n ) )
#define hbqt_par_HBSlots( n )                       ( ( HBSlots                     * ) hbqt_gcpointer( n ) )

#define hbqt_par_QString( n )                       ( ( QString ) hb_parcx( n ) )
#define hbqt_par_uchar( n )                         ( ( uchar * ) hb_parcx( n ) )
#define hbqt_par_QRgb( n )                          ( hb_parnint( n ) )
#define hbqt_par_Bool( n )                          ( hb_parl( n ) )
#define hbqt_par_char( n )                          ( hb_parcx( n ) )

/*
 *   DEFINES FOR HBQt PSEUDO-CASTING
 *
 *   Format:
 *   HBQT_TYPE_(Qt class)
 */

#define HBQT_TYPE_ExtensionOption                   1001
#define HBQT_TYPE_IUnknown                          1002
#define HBQT_TYPE_QAbstractButton                   1003
#define HBQT_TYPE_QAbstractEventDispatcher          1004
#define HBQT_TYPE_QAbstractItemDelegate             1005
#define HBQT_TYPE_QAbstractItemModel                1006
#define HBQT_TYPE_QAbstractItemView                 1007
#define HBQT_TYPE_QAbstractListModel                1008
#define HBQT_TYPE_QAbstractPrintDialog              1009
#define HBQT_TYPE_QAbstractProxyModel               1010
#define HBQT_TYPE_QAbstractScrollArea               1011
#define HBQT_TYPE_QAbstractSlider                   1012
#define HBQT_TYPE_QAbstractSpinBox                  1013
#define HBQT_TYPE_QAbstractTableModel               1014
#define HBQT_TYPE_QAbstractTextDocumentLayout       1015
#define HBQT_TYPE_QAction                           1016
#define HBQT_TYPE_QActionGroup                      1017
#define HBQT_TYPE_QApplication                      1018
#define HBQT_TYPE_QAxBase                           1019
#define HBQT_TYPE_QBitArray                         1020
#define HBQT_TYPE_QBitmap                           1021
#define HBQT_TYPE_QBoxLayout                        1022
#define HBQT_TYPE_QBrush                            1023
#define HBQT_TYPE_QBuffer                           1024
#define HBQT_TYPE_QButtonGroup                      1025
#define HBQT_TYPE_QByteArray                        1026
#define HBQT_TYPE_QCalendarWidget                   1027
#define HBQT_TYPE_QChar                             1028
#define HBQT_TYPE_QCheckBox                         1029
#define HBQT_TYPE_QClipboard                        1030
#define HBQT_TYPE_QColor                            1031
#define HBQT_TYPE_QColorDialog                      1032
#define HBQT_TYPE_QComboBox                         1033
#define HBQT_TYPE_QCommandLinkButton                1034
#define HBQT_TYPE_QCommonStyle                      1035
#define HBQT_TYPE_QCompleter                        1036
#define HBQT_TYPE_QConicalGradient                  1037
#define HBQT_TYPE_QContextMenuEvent                 1038
#define HBQT_TYPE_QCoreApplication                  1039
#define HBQT_TYPE_QCursor                           1040
#define HBQT_TYPE_QDateEdit                         1041
#define HBQT_TYPE_QDataStream                       1042
#define HBQT_TYPE_QDate                             1043
#define HBQT_TYPE_QDateTime                         1044
#define HBQT_TYPE_QDateTimeEdit                     1045
#define HBQT_TYPE_QDesktopWidget                    1046
#define HBQT_TYPE_QDial                             1047
#define HBQT_TYPE_QDialog                           1048
#define HBQT_TYPE_QDir                              1049
#define HBQT_TYPE_QDirModel                         1050
#define HBQT_TYPE_QDockWidget                       1051
#define HBQT_TYPE_QDoubleSpinBox                    1053
#define HBQT_TYPE_QDragEnterEvent                   1054
#define HBQT_TYPE_QDragLeaveEvent                   1055
#define HBQT_TYPE_QDragMoveEvent                    1056
#define HBQT_TYPE_QDropEvent                        1057
#define HBQT_TYPE_QErrorMessage                     1058
#define HBQT_TYPE_QEvent                            1059
#define HBQT_TYPE_QEventLoop                        1060
#define HBQT_TYPE_QFile                             1061
#define HBQT_TYPE_QFileDialog                       1062
#define HBQT_TYPE_QFileIconProvider                 1063
#define HBQT_TYPE_QFileInfo                         1064
#define HBQT_TYPE_QFileSystemModel                  1065
#define HBQT_TYPE_QFocusEvent                       1066
#define HBQT_TYPE_QFocusFrame                       1067
#define HBQT_TYPE_QFont                             1068
#define HBQT_TYPE_QFontComboBox                     1069
#define HBQT_TYPE_QFontDatabase                     1070
#define HBQT_TYPE_QFontDialog                       1071
#define HBQT_TYPE_QFontInfo                         1072
#define HBQT_TYPE_QFontMetrics                      1073
#define HBQT_TYPE_QFontMetricsF                     1074
#define HBQT_TYPE_QFormLayout                       1075
#define HBQT_TYPE_QFrame                            1076
#define HBQT_TYPE_QFtp                              1077
#define HBQT_TYPE_QGradient                         1078
#define HBQT_TYPE_QGradientStops                    1079
#define HBQT_TYPE_QGridLayout                       1080
#define HBQT_TYPE_QGroupBox                         1081
#define HBQT_TYPE_QHBoxLayout                       1082
#define HBQT_TYPE_QHeaderView                       1083
#define HBQT_TYPE_QHelpEvent                        1084
#define HBQT_TYPE_QHideEvent                        1085
#define HBQT_TYPE_QHttp                             1086
#define HBQT_TYPE_QHttpHeader                       1087
#define HBQT_TYPE_QHttpResponseHeader               1088
#define HBQT_TYPE_QHttpRequestHeader                1089
#define HBQT_TYPE_QIcon                             1090
#define HBQT_TYPE_QImage                            1091
#define HBQT_TYPE_QImageReader                      1092
#define HBQT_TYPE_QImageWriter                      1093
#define HBQT_TYPE_QInputContext                     1094
#define HBQT_TYPE_QInputDialog                      1095
#define HBQT_TYPE_QInputEvent                       1096
#define HBQT_TYPE_QInputMethodEvent                 1097
#define HBQT_TYPE_QIODevice                         1098
#define HBQT_TYPE_QItemDelegate                     1099
#define HBQT_TYPE_QItemEditorCreator                1100
#define HBQT_TYPE_QItemEditorCreatorBase            1101
#define HBQT_TYPE_QItemEditorFactory                1102
#define HBQT_TYPE_QItemSelection                    1103
#define HBQT_TYPE_QItemSelectionModel               1104
#define HBQT_TYPE_QKeyEvent                         1105
#define HBQT_TYPE_QKeySequence                      1106
#define HBQT_TYPE_QLabel                            1107
#define HBQT_TYPE_QLatin1Char                       1108
#define HBQT_TYPE_QLatin1String                     1109
#define HBQT_TYPE_QLayout                           1110
#define HBQT_TYPE_QLayoutItem                       1111
#define HBQT_TYPE_QLCDNumber                        1112
#define HBQT_TYPE_QLibraryInfo                      1113
#define HBQT_TYPE_QLine                             1114
#define HBQT_TYPE_QLinearGradient                   1115
#define HBQT_TYPE_QLineEdit                         1116
#define HBQT_TYPE_QLineF                            1117
#define HBQT_TYPE_QList                             1118
#define HBQT_TYPE_QListView                         1119
#define HBQT_TYPE_QListWidget                       1120
#define HBQT_TYPE_QListWidgetItem                   1121
#define HBQT_TYPE_QLocale                           1122
#define HBQT_TYPE_QMainWindow                       1123
#define HBQT_TYPE_QMatrix                           1124
#define HBQT_TYPE_QMdiArea                          1125
#define HBQT_TYPE_QMdiSubWindow                     1126
#define HBQT_TYPE_QMenu                             1127
#define HBQT_TYPE_QMenuBar                          1128
#define HBQT_TYPE_QMessageBox                       1129
#define HBQT_TYPE_QMimeData                         1130
#define HBQT_TYPE_QModelIndex                       1131
#define HBQT_TYPE_QModelIndexList                   1132
#define HBQT_TYPE_QMouseEvent                       1133
#define HBQT_TYPE_QMoveEvent                        1134
#define HBQT_TYPE_QMovie                            1135
#define HBQT_TYPE_QNetworkAccessManager             1136
#define HBQT_TYPE_QNetworkProxy                     1137
#define HBQT_TYPE_QNetworkRequest                   1138
#define HBQT_TYPE_QObject                           1139
#define HBQT_TYPE_QPageSetupDialog                  1140
#define HBQT_TYPE_QPaintDevice                      1141
#define HBQT_TYPE_QPaintEngine                      1142
#define HBQT_TYPE_QPaintEngineState                 1143
#define HBQT_TYPE_QPainter                          1144
#define HBQT_TYPE_QPainterPath                      1145
#define HBQT_TYPE_QPaintEvent                       1146
#define HBQT_TYPE_QPalette                          1147
#define HBQT_TYPE_QPen                              1148
#define HBQT_TYPE_QPicture                          1149
#define HBQT_TYPE_QPixmap                           1150
#define HBQT_TYPE_QPlainTextDocumentLayout          1151
#define HBQT_TYPE_QPlainTextEdit                    1152
#define HBQT_TYPE_QPoint                            1153
#define HBQT_TYPE_QPointF                           1154
#define HBQT_TYPE_QPolygon                          1155
#define HBQT_TYPE_QPolygonF                         1156
#define HBQT_TYPE_QPrintDialog                      1157
#define HBQT_TYPE_QPrintEngine                      1158
#define HBQT_TYPE_QPrinter                          1159
#define HBQT_TYPE_QPrintPreviewDialog               1160
#define HBQT_TYPE_QProcess                          1161
#define HBQT_TYPE_QProgressBar                      1162
#define HBQT_TYPE_QProgressDialog                   1163
#define HBQT_TYPE_QPushButton                       1164
#define HBQT_TYPE_QRadialGradient                   1165
#define HBQT_TYPE_QRadioButton                      1166
#define HBQT_TYPE_QRect                             1167
#define HBQT_TYPE_QRectF                            1168
#define HBQT_TYPE_QRegExp                           1169
#define HBQT_TYPE_QRegion                           1170
#define HBQT_TYPE_QResizeEvent                      1171
#define HBQT_TYPE_QResource                         1172
#define HBQT_TYPE_QScrollArea                       1173
#define HBQT_TYPE_QScrollBar                        1174
#define HBQT_TYPE_QSessionManager                   1175
#define HBQT_TYPE_QSettings                         1176
#define HBQT_TYPE_QShowEvent                        1177
#define HBQT_TYPE_QSignalMapper                     1178
#define HBQT_TYPE_QSize                             1179
#define HBQT_TYPE_QSizeF                            1180
#define HBQT_TYPE_QSizeGrip                         1181
#define HBQT_TYPE_QSizePolicy                       1182
#define HBQT_TYPE_QSlider                           1183
#define HBQT_TYPE_QSound                            1184
#define HBQT_TYPE_QSpacerItem                       1185
#define HBQT_TYPE_QSpinBox                          1186
#define HBQT_TYPE_QSplashScreen                     1187
#define HBQT_TYPE_QSplitter                         1188
#define HBQT_TYPE_QStackedWidget                    1189
#define HBQT_TYPE_QStandardItem                     1190
#define HBQT_TYPE_QStandardItemModel                1191
#define HBQT_TYPE_QStatusBar                        1192
#define HBQT_TYPE_QStringList                       1193
#define HBQT_TYPE_QStringListModel                  1194
#define HBQT_TYPE_QStyle                            1195
#define HBQT_TYPE_QStyledItemDelegate               1196
#define HBQT_TYPE_QStyleFactory                     1197
#define HBQT_TYPE_QStyleHintReturn                  1198
#define HBQT_TYPE_QStyleHintReturnMask              1199
#define HBQT_TYPE_QStyleHintReturnVariant           1200
#define HBQT_TYPE_QStyleOption                      1201
#define HBQT_TYPE_QStyleOptionButton                1202
#define HBQT_TYPE_QStyleOptionComboBox              1203
#define HBQT_TYPE_QStyleOptionComplex               1204
#define HBQT_TYPE_QStyleOptionDockWidget            1205
#define HBQT_TYPE_QStyleOptionFocusRect             1206
#define HBQT_TYPE_QStyleOptionFrame                 1207
#define HBQT_TYPE_QStyleOptionGroupBox              1208
#define HBQT_TYPE_QStyleOptionHeader                1209
#define HBQT_TYPE_QStyleOptionMenuItem              1210
#define HBQT_TYPE_QStyleOptionProgressBar           1211
#define HBQT_TYPE_QStyleOptionSizeGrip              1212
#define HBQT_TYPE_QStyleOptionSlider                1213
#define HBQT_TYPE_QStyleOptionSpinBox               1214
#define HBQT_TYPE_QStyleOptionTab                   1215
#define HBQT_TYPE_QStyleOptionTabBarBase            1216
#define HBQT_TYPE_QStyleOptionTabWidgetFrame        1217
#define HBQT_TYPE_QStyleOptionTitleBar              1218
#define HBQT_TYPE_QStyleOptionToolBar               1219
#define HBQT_TYPE_QStyleOptionToolBox               1220
#define HBQT_TYPE_QStyleOptionToolButton            1221
#define HBQT_TYPE_QStyleOptionViewItem              1222
#define HBQT_TYPE_QStylePainter                     1224
#define HBQT_TYPE_QSyntaxHighlighter                1225
#define HBQT_TYPE_QSystemTrayIcon                   1226
#define HBQT_TYPE_QTabBar                           1227
#define HBQT_TYPE_QTableView                        1228
#define HBQT_TYPE_QTableWidget                      1229
#define HBQT_TYPE_QTableWidgetItem                  1230
#define HBQT_TYPE_QTableWidgetSelectionRange        1231
#define HBQT_TYPE_QTabWidget                        1232
#define HBQT_TYPE_QTcpSocket                        1233
#define HBQT_TYPE_QTextBlock                        1234
#define HBQT_TYPE_QTextBlockFormat                  1235
#define HBQT_TYPE_QTextBlockGroup                   1236
#define HBQT_TYPE_QTextBlockUserData                1237
#define HBQT_TYPE_QTextBoundaryFinder               1238
#define HBQT_TYPE_QTextBrowser                      1239
#define HBQT_TYPE_QTextCharFormat                   1240
#define HBQT_TYPE_QTextCodec                        1241
#define HBQT_TYPE_QTextCursor                       1244
#define HBQT_TYPE_QTextDecoder                      1245
#define HBQT_TYPE_QTextDocument                     1246
#define HBQT_TYPE_QTextDocumentFragment             1247
#define HBQT_TYPE_QTextDocumentWriter               1248
#define HBQT_TYPE_QTextEdit                         1249
#define HBQT_TYPE_QTextEncoder                      1250
#define HBQT_TYPE_QTextEngine                       1251
#define HBQT_TYPE_QTextFormat                       1252
#define HBQT_TYPE_QTextFragment                     1253
#define HBQT_TYPE_QTextFrame                        1254
#define HBQT_TYPE_QTextFrameFormat                  1255
#define HBQT_TYPE_QTextImageFormat                  1256
#define HBQT_TYPE_QTextInlineObject                 1257
#define HBQT_TYPE_QTextItem                         1258
#define HBQT_TYPE_QTextList                         1259
#define HBQT_TYPE_QTextLayout                       1260
#define HBQT_TYPE_QTextLength                       1261
#define HBQT_TYPE_QTextLine                         1262
#define HBQT_TYPE_QTextListFormat                   1263
#define HBQT_TYPE_QTextObject                       1264
#define HBQT_TYPE_QTextOption                       1265
#define HBQT_TYPE_QTextStream                       1266
#define HBQT_TYPE_QTextTableFormat                  1267
#define HBQT_TYPE_QThread                           1268
#define HBQT_TYPE_QTime                             1269
#define HBQT_TYPE_QTimeEdit                         1270
#define HBQT_TYPE_QTimer                            1271
#define HBQT_TYPE_QToolBar                          1272
#define HBQT_TYPE_QToolBox                          1273
#define HBQT_TYPE_QToolButton                       1274
#define HBQT_TYPE_QTransform                        1275
#define HBQT_TYPE_QTranslator                       1276
#define HBQT_TYPE_QTreeView                         1277
#define HBQT_TYPE_QTreeWidget                       1278
#define HBQT_TYPE_QTreeWidgetItem                   1279
#define HBQT_TYPE_QUiLoader                         1280
#define HBQT_TYPE_QUrl                              1281
#define HBQT_TYPE_QValidator                        1282
#define HBQT_TYPE_QVariant                          1283
#define HBQT_TYPE_QVBoxLayout                       1284
#define HBQT_TYPE_QVector                           1285
#define HBQT_TYPE_QWheelEvent                       1286
#define HBQT_TYPE_QWidget                           1287
#define HBQT_TYPE_QWidgetAction                     1288
#define HBQT_TYPE_QWidgetItem                       1289
#define HBQT_TYPE_QWindowsStyle                     1290
#define HBQT_TYPE_QWindowSurface                    1291
#define HBQT_TYPE_QWindowsXPStyle                   1292
#define HBQT_TYPE_QWindowStateChangeEvent           1293
#define HBQT_TYPE_QWizard                           1294
#define HBQT_TYPE_QWizardPage                       1295
#define HBQT_TYPE_QWSEvent                          1296

#define HBQT_TYPE_HBDbfModel                        1401
#define HBQT_TYPE_HBQAbstractItemModel              1402
#define HBQT_TYPE_HBEvents                          1403
#define HBQT_TYPE_HBQMainWindow                     1404
#define HBQT_TYPE_HBQPlainTextEdit                  1405
#define HBQT_TYPE_HBQTableView                      1406
#define HBQT_TYPE_HBQSyntaxHighlighter              1407
#define HBQT_TYPE_HBQTextBlockUserData              1408
#define HBQT_TYPE_HBSlots                           1409

/*
 *   DEFINES HBQt CODEBLOCKs
 *
 *   Format:
 *   HBQT_(Qt class initials)_(Qt overloaded member)
 */

#define HBQT_QAIM_data                              1001
#define HBQT_QAIM_flags                             1003
#define HBQT_QAIM_headerData                        2001
#define HBQT_QAIM_rowCount                          3001
#define HBQT_QAIM_columnCount                       3002

#endif /* __HBQT_H */
