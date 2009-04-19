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


#ifndef __HBQT_H
#define __HBQT_H

#include <qt/qglobal.h>
#include <qtgui/QTextDocumentFragment>
#include <qtgui/QTextDocument>
#include <qtgui/QTextDocumentWriter>
#include <qtgui/QTextBlock>
#include <qtgui/QTextCursor>

#if QT_VERSION >= 0x040500


#define hbqt_par_QAbstractButton( n )        ( ( QAbstractButton* ) hb_parptr( n ) )
#define hbqt_par_QAbstractItemView( n )      ( ( QAbstractItemView* ) hb_parptr( n ) )
#define hbqt_par_QAbstractItemModel( n )     ( ( QAbstractItemModel* ) hb_parptr( n ) )
#define hbqt_par_QAbstractItemDelegate( n )  ( ( QAbstractItemDelegate* ) hb_parptr( n ) )
#define hbqt_par_QAbstractPrintDialog( n )   ( ( QAbstractPrintDialog* ) hb_parptr( n ) )
#define hbqt_par_QAbstractProxyModel( n )    ( ( QAbstractProxyModel* ) hb_parptr( n ) )
#define hbqt_par_QAbstractScrollArea( n )    ( ( QAbstractScrollArea* ) hb_parptr( n ) )
#define hbqt_par_QAbstractSlider( n )        ( ( QAbstractSlider* ) hb_parptr( n ) )
#define hbqt_par_QAbstractSpinBox( n )       ( ( QAbstractSpinBox* ) hb_parptr( n ) )
#define hbqt_par_QAction( n )                ( ( QAction* ) hb_parptr( n ) )
#define hbqt_par_QActionGroup( n )           ( ( QActionGroup* ) hb_parptr( n ) )
#define hbqt_par_QApplication( n )           ( ( QApplication* ) hb_parptr( n ) )
#define hbqt_par_QBoxLayout( n )             ( ( QBoxLayout* ) hb_parptr( n ) )
#define hbqt_par_QBrush( n )                 ( ( QBrush* ) hb_parptr( n ) )
#define hbqt_par_QCalendarWidget( n )        ( ( QCalendarWidget* ) hb_parptr( n ) )
#define hbqt_par_QCheckBox( n )              ( ( QCheckBox* ) hb_parptr( n ) )
#define hbqt_par_QColor( n )                 ( ( QColor* ) hb_parptr( n ) )
#define hbqt_par_QColorDialog( n )           ( ( QColorDialog* ) hb_parptr( n ) )
#define hbqt_par_QComboBox( n )              ( ( QComboBox* ) hb_parptr( n ) )
#define hbqt_par_QCommandLinkButton( n )     ( ( QCommandLinkButton* ) hb_parptr( n ) )
#define hbqt_par_QCompleter( n )             ( ( QCompleter* ) hb_parptr( n ) )
#define hbqt_par_QCoreApplication( n )       ( ( QCoreApplication* ) hb_parptr( n ) )
#define hbqt_par_QDateTimeEdit( n )          ( ( QDateTimeEdit* ) hb_parptr( n ) )
#define hbqt_par_QDial( n )                  ( ( QDial* ) hb_parptr( n ) )
#define hbqt_par_QDialog( n )                ( ( QDialog* ) hb_parptr( n ) )
#define hbqt_par_QDockWidget( n )            ( ( QDockWidget* ) hb_parptr( n ) )
#define hbqt_par_QDoubleSpinBox( n )         ( ( QDoubleSpinBox* ) hb_parptr( n ) )
#define hbqt_par_QEvent( n )                 ( ( QEvent* ) hb_parptr( n ) )
#define hbqt_par_QFileDialog( n )            ( ( QFileDialog* ) hb_parptr( n ) )
#define hbqt_par_QFileIconProvider( n )      ( ( QFileIconProvider* ) hb_parptr( n ) )
#define hbqt_par_QFocusFrame( n )            ( ( QFocusFrame* ) hb_parptr( n ) )
#define hbqt_par_QFont( n )                  ( ( QFont* ) hb_parptr( n ) )
#define hbqt_par_QFontComboBox( n )          ( ( QFontComboBox* ) hb_parptr( n ) )
#define hbqt_par_QFontDialog( n )            ( ( QFontDialog* ) hb_parptr( n ) )
#define hbqt_par_QFormLayout( n )            ( ( QFormLayout* ) hb_parptr( n ) )
#define hbqt_par_QFrame( n )                 ( ( QFrame* ) hb_parptr( n ) )
#define hbqt_par_QGroupBox( n )              ( ( QGroupBox* ) hb_parptr( n ) )
#define hbqt_par_QHeaderView( n )            ( ( QHeaderView* ) hb_parptr( n ) )
#define hbqt_par_QInputContext( n )          ( ( QInputContext* ) hb_parptr( n ) )
#define hbqt_par_QInputDialog( n )           ( ( QInputDialog* ) hb_parptr( n ) )
#define hbqt_par_QItemSelectionModel( n )    ( ( QItemSelectionModel* ) hb_parptr( n ) )
#define hbqt_par_QLabel( n )                 ( ( QLabel* ) hb_parptr( n ) )
#define hbqt_par_QLayout( n )                ( ( QLayout* ) hb_parptr( n ) )
#define hbqt_par_QLayoutItem( n )            ( ( QLayoutItem* ) hb_parptr( n ) )
#define hbqt_par_QLCDNumber( n )             ( ( QLCDNumber* ) hb_parptr( n ) )
#define hbqt_par_QLine( n )                  ( ( QLine* ) hb_parptr( n ) )
#define hbqt_par_QLineEdit( n )              ( ( QLineEdit* ) hb_parptr( n ) )
#define hbqt_par_QLineF( n )                 ( ( QLineF* ) hb_parptr( n ) )
#define hbqt_par_QListView( n )              ( ( QListView* ) hb_parptr( n ) )
#define hbqt_par_QLocale( n )                ( ( QLocale* ) hb_parptr( n ) )
#define hbqt_par_QMainWindow( n )            ( ( QMainWindow* ) hb_parptr( n ) )
#define hbqt_par_QMenu( n )                  ( ( QMenu* ) hb_parptr( n ) )
#define hbqt_par_QMenuBar( n )               ( ( QMenuBar* ) hb_parptr( n ) )
#define hbqt_par_QMessageBox( n )            ( ( QMessageBox* ) hb_parptr( n ) )
#define hbqt_par_QMovie( n )                 ( ( QMovie* ) hb_parptr( n ) )
#define hbqt_par_QObject( n )                ( ( QObject* ) hb_parptr( n ) )
#define hbqt_par_QPageSetupDialog( n )       ( ( QPageSetupDialog* ) hb_parptr( n ) )
#define hbqt_par_QPainter( n )               ( ( QPainter* ) hb_parptr( n ) )
#define hbqt_par_QPaintDevice( n )           ( ( QPaintDevice* ) hb_parptr( n ) )
#define hbqt_par_QPen( n )                   ( ( QPen* ) hb_parptr( n ) )
#define hbqt_par_QPoint( n )                 ( ( QPoint* ) hb_parptr( n ) )
#define hbqt_par_QPointF( n )                ( ( QPointF* ) hb_parptr( n ) )
#define hbqt_par_QPrintDialog( n )           ( ( QPrintDialog* ) hb_parptr( n ) )
#define hbqt_par_QPrintPreviewDialog( n )    ( ( QPrintPreviewDialog* ) hb_parptr( n ) )
#define hbqt_par_QPrinter( n )               ( ( QPrinter* ) hb_parptr( n ) )
#define hbqt_par_QProgressBar( n )           ( ( QProgressBar* ) hb_parptr( n ) )
#define hbqt_par_QProgressDialog( n )        ( ( QProgressDialog* ) hb_parptr( n ) )
#define hbqt_par_QPushButton( n )            ( ( QPushButton* ) hb_parptr( n ) )
#define hbqt_par_QRect( n )                  ( ( QRect* ) hb_parptr( n ) )
#define hbqt_par_QRectF( n )                 ( ( QRectF* ) hb_parptr( n ) )
#define hbqt_par_QScrollArea( n )            ( ( QScrollArea* ) hb_parptr( n ) )
#define hbqt_par_QScrollBar( n )             ( ( QScrollBar* ) hb_parptr( n ) )
#define hbqt_par_QSpacerItem( n )            ( ( QSpacerItem* ) hb_parptr( n ) )
#define hbqt_par_QSpinBox( n )               ( ( QSpinBox* ) hb_parptr( n ) )
#define hbqt_par_QSplitter( n )              ( ( QSplitter* ) hb_parptr( n ) )
#define hbqt_par_QStatusBar( n )             ( ( QStatusBar* ) hb_parptr( n ) )
#define hbqt_par_QStyle( n )                 ( ( QStyle* ) hb_parptr( n ) )
#define hbqt_par_QSlider( n )                ( ( QSlider* ) hb_parptr( n ) )
#define hbqt_par_QTabBar( n )                ( ( QTabBar* ) hb_parptr( n ) )
#define hbqt_par_QTableView( n )             ( ( QTableView* ) hb_parptr( n ) )
#define hbqt_par_QTableWidget( n )           ( ( QTableWidget* ) hb_parptr( n ) )
#define hbqt_par_QTableWidgetItem( n )       ( ( QTableWidgetItem* ) hb_parptr( n ) )
#define hbqt_par_QTabWidget( n )             ( ( QTabWidget* ) hb_parptr( n ) )
#define hbqt_par_QTextEdit( n )              ( ( QTextEdit* ) hb_parptr( n ) )
#define hbqt_par_QTextDocument( n )          ( ( QTextDocument* ) hb_parptr( n ) )
#define hbqt_par_QThread( n )                ( ( QThread* ) hb_parptr( n ) )
#define hbqt_par_QToolBar( n )               ( ( QToolBar* ) hb_parptr( n ) )
#define hbqt_par_QToolBox( n )               ( ( QToolBox* ) hb_parptr( n ) )
#define hbqt_par_QToolButton( n )            ( ( QToolButton* ) hb_parptr( n ) )
#define hbqt_par_QTreeView( n )              ( ( QTreeView* ) hb_parptr( n ) )
#define hbqt_par_QTreeWidget( n )            ( ( QTreeWidget* ) hb_parptr( n ) )
#define hbqt_par_QTreeWidgetItem( n )        ( ( QTreeWidgetItem* ) hb_parptr( n ) )
#define hbqt_par_QValidator( n )             ( ( QValidator* ) hb_parptr( n ) )
#define hbqt_par_QWebView( n )               ( ( QWebView* ) hb_parptr( n ) )
#define hbqt_par_QWidget( n )                ( ( QWidget* ) hb_parptr( n ) )
#define hbqt_par_QWindowSurface( n )         ( ( QWindowSurface* ) hb_parptr( n ) )
#define hbqt_par_QWizard( n )                ( ( QWizard* ) hb_parptr( n ) )
#define hbqt_par_QWizardPage( n )            ( ( QWizardPage* ) hb_parptr( n ) )
#define hbqt_par_QWSEvent( n )               ( ( QWSEvent* ) hb_parptr( n ) )
#define hbqt_par_QWebPage( n )               ( ( QWebPage* ) hb_parptr( n ) )
#define hbqt_par_QNetworkAccessManager( n )  ( ( QNetworkAccessManager* ) hb_parptr( n ) )
#define hbqt_par_QWebPluginFactory( n )      ( ( QWebPluginFactory* ) hb_parptr( n ) )
#define hbqt_par_QContextMenuEvent( n )      ( ( QContextMenuEvent* ) hb_parptr( n ) )
#define hbqt_par_QDesktopWidget( n )         ( ( QDesktopWidget* ) hb_parptr( n ) )
#define hbqt_par_QFontInfo( n )              ( ( QFontInfo* ) hb_parptr( n ) )
#define hbqt_par_QDir( n )                   ( ( QDir* ) hb_parptr( n ) )
#define hbqt_par_QDockWidget( n )            ( ( QDockWidget* ) hb_parptr( n ) )
#define hbqt_par_QGridLayout( n )            ( ( QGridLayout* ) hb_parptr( n ) )
#define hbqt_par_QHeaderView( n )            ( ( QHeaderView* ) hb_parptr( n ) )
#define hbqt_par_QListWidget( n )            ( ( QListWidget* ) hb_parptr( n ) )
#define hbqt_par_QListWidgetItem( n )        ( ( QListWidgetItem* ) hb_parptr( n ) )
#define hbqt_par_QTimer( n )                 ( ( QTimer* ) hb_parptr( n ) )
#define hbqt_par_QUrl( n )                   ( ( QUrl* ) hb_parptr( n ) )
#define hbqt_par_QWebPage( n )               ( ( QWebPage* ) hb_parptr( n ) )
#define hbqt_par_QNetworkAccessManager( n )  ( ( QNetworkAccessManager* ) hb_parptr( n ) )
#define hbqt_par_QWebPluginFactory( n )      ( ( QWebPluginFactory* ) hb_parptr( n ) )
#define hbqt_par_QContextMenuEvent( n )      ( ( QContextMenuEvent* ) hb_parptr( n ) )
#define hbqt_par_QAxBase( n )                ( ( QAxBase* ) hb_parptr( n ) )
#define hbqt_par_IUnknown( n )               ( ( IUnknown* ) hb_parptr( n ) )
#define hbqt_par_QSignalMapper( n )          ( ( QSignalMapper* ) hb_parptr( n ) )
#define hbqt_par_QSplashScreen( n )          ( ( QSplashScreen* ) hb_parptr( n ) )
#define hbqt_par_QHttp( n )                  ( ( QHttp* ) hb_parptr( n ) )
#define hbqt_par_QFtp( n )                   ( ( QFtp* ) hb_parptr( n ) )
#define hbqt_par_QIODevice( n )              ( ( QIODevice* ) hb_parptr( n ) )
#define hbqt_par_QTcpSocket( n )             ( ( QTcpSocket* ) hb_parptr( n ) )
#define hbqt_par_QPainterPath( n )           ( ( QPainterPath* ) hb_parptr( n ) )
#define hbqt_par_QTransform( n )             ( ( QTransform* ) hb_parptr( n ) )
#define hbqt_par_QMatrix( n )                ( ( QMatrix* ) hb_parptr( n ) )
#define hbqt_par_QTextOption( n )            ( ( QTextOption* ) hb_parptr( n ) )
#define hbqt_par_QPicture( n )               ( ( QPicture* ) hb_parptr( n ) )
#define hbqt_par_QPixmap( n )                ( ( QPixmap* ) hb_parptr( n ) )
#define hbqt_par_QRegion( n )                ( ( QRegion* ) hb_parptr( n ) )
#define hbqt_par_QPolygon( n )               ( ( QPolygon* ) hb_parptr( n ) )
#define hbqt_par_QPolygonF( n )              ( ( QPolygonF* ) hb_parptr( n ) )
#define hbqt_par_QVector( n )                ( ( QVector* ) hb_parptr( n ) )
#define hbqt_par_QImage( n )                 ( ( QImage* ) hb_parptr( n ) )
#define hbqt_par_QKeySequence( n )           ( ( QKeySequence* ) hb_parptr( n ) )
#define hbqt_par_QSize( n )                  ( ( QSize* ) hb_parptr( n ) )
#define hbqt_par_QSizeF( n )                 ( ( QSizeF* ) hb_parptr( n ) )
#define hbqt_par_QModelIndex( n )            ( ( QModelIndex* ) hb_parptr( n ) )
#define hbqt_par_QVariant( n )               ( ( QVariant* ) hb_parptr( n ) )
#define hbqt_par_QSessionManager( n )        ( ( QSessionManager* ) hb_parptr( n ) )
#define hbqt_par_QDate( n )                  ( ( QDate* ) hb_parptr( n ) )
#define hbqt_par_QTime( n )                  ( ( QTime* ) hb_parptr( n ) )
#define hbqt_par_QDateTime( n )              ( ( QDateTime* ) hb_parptr( n ) )
#define hbqt_par_QTextCharFormat( n )        ( ( QTextCharFormat* ) hb_parptr( n ) )
#define hbqt_par_QStringList( n )            ( ( QStringList* ) hb_parptr( n ) )
#define hbqt_par_QErrorMessage( n )          ( ( QErrorMessage* ) hb_parptr( n ) )
#define hbqt_par_QByteArray( n )             ( ( QByteArray* ) hb_parptr( n ) )
#define hbqt_par_QDataStream( n )            ( ( QDataStream* ) hb_parptr( n ) )
#define hbqt_par_QTextCursor( n )            ( ( QTextCursor* ) hb_parptr( n ) )
#define hbqt_par_QPalette( n )               ( ( QPalette* ) hb_parptr( n ) )
#define hbqt_par_QCursor( n )                ( ( QCursor* ) hb_parptr( n ) )
#define hbqt_par_QNetworkRequest( n )        ( ( QNetworkRequest* ) hb_parptr( n ) )
#define hbqt_par_QTableWidgetSelectionRange( n ) ( ( QTableWidgetSelectionRange* ) hb_parptr( n ) )
#define hbqt_par_QWSEvent( n )               ( ( QWSEvent* ) hb_parptr( n ) )
#define hbqt_par_QHttpRequestHeader( n )     ( ( QHttpRequestHeader* ) hb_parptr( n ) )
#define hbqt_par_QNetworkProxy( n )          ( ( QNetworkProxy* ) hb_parptr( n ) )
#define hbqt_par_QBitmap( n )                ( ( QBitmap* ) hb_parptr( n ) )
#define hbqt_par_QTextStream( n )            ( ( QTextStream* ) hb_parptr( n ) )
#define hbqt_par_QTextCodec( n )             ( ( QTextCodec* ) hb_parptr( n ) )
#define hbqt_par_QEventLoop( n )             ( ( QEventLoop* ) hb_parptr( n ) )
#define hbqt_par_QPaintEvent( n )            ( ( QPaintEvent* ) hb_parptr( n ) )
#define hbqt_par_QInputEvent( n )            ( ( QInputEvent* ) hb_parptr( n ) )
#define hbqt_par_QIcon( n )                  ( ( QIcon* ) hb_parc( n ) )
#define hbqt_par_QStyleOption( n )           ( ( QStyleOption* ) hb_parptr( n ) )
#define hbqt_par_QStyleOptionComplex( n )    ( ( QStyleOptionComplex* ) hb_parptr( n ) )
#define hbqt_par_QFontMetrics( n )           ( ( QFontMetrics* ) hb_parptr( n ) )
#define hbqt_par_QStyleHintReturn( n )       ( ( QStyleHintReturn* ) hb_parptr( n ) )
#define hbqt_par_QClipboard( n )             ( ( QClipboard* ) hb_parptr( n ) )
#define hbqt_par_QMimeData( n )              ( ( QMimeData* ) hb_parptr( n ) )
#define hbqt_par_QTextBlock( n )             ( ( QTextBlock* ) hb_parptr( n ) )
#define hbqt_par_QTextBlockUserData( n )     ( ( QTextBlockUserData* ) hb_parptr( n ) )
#define hbqt_par_QTextBrowser( n )           ( ( QTextBrowser* ) hb_parptr( n ) )
#define hbqt_par_QTextFormat( n )            ( ( QTextFormat* ) hb_parptr( n ) )
#define hbqt_par_QTextBlockFormat( n )       ( ( QTextBlockFormat* ) hb_parptr( n ) )
#define hbqt_par_QTextBlockGroup( n )        ( ( QTextBlockGroup* ) hb_parptr( n ) )
#define hbqt_par_QTextBoundaryFinder( n )    ( ( QTextBoundaryFinder* ) hb_parptr( n ) )
#define hbqt_par_QTextListFormat( n )        ( ( QTextListFormat* ) hb_parptr( n ) )
#define hbqt_par_QTextFrameFormat( n )       ( ( QTextFrameFormat* ) hb_parptr( n ) )
#define hbqt_par_QTextImageFormat( n )       ( ( QTextImageFormat* ) hb_parptr( n ) )
#define hbqt_par_QTextTableFormat( n )       ( ( QTextTableFormat* ) hb_parptr( n ) )
#define hbqt_par_QTextDocumentFragment( n )  ( ( QTextDocumentFragment* ) hb_parptr( n ) )
#define hbqt_par_QTextDecoder( n )           ( ( QTextDecoder* ) hb_parptr( n ) )
#define hbqt_par_QTextDecument( n )          ( ( QTextDocument* ) hb_parptr( n ) )
#define hbqt_par_QTextDocumentWriter( n )    ( ( QTextDocumentWriter* ) hb_parptr( n ) )
#define hbqt_par_QTextCursor( n )            ( ( QTextCursor* ) hb_parptr( n ) )
#define hbqt_par_QRegExp( n )                ( ( QRegExp* ) hb_parptr( n ) )
#define hbqt_par_QAbstractTextDocumentLayout( n ) ( ( QAbstractTextDocumentLayout* ) hb_parptr( n ) )

#define hbqt_par_QString( n )                ( ( QString ) hb_parc( n ) )
#define hbqt_par_QRgb( n )                   ( hb_parnint( n ) )
#define hbqt_par_Bool( n )                   ( hb_parl( n ) )
#define hbqt_par_char( n )                   ( hb_parc( n ) )

#define hbqt_ret_QWidget( p )                ( hb_retptr( ( QWidget* ) p ) )
#define hbqt_ret_QAbstractItemDelegate( p )  ( hb_retptr( ( QAbstractItemDelegate* ) p ) )
#define hbqt_ret_QAbstractItemModel( p )     ( hb_retptr( ( QAbstractItemModel* ) p ) )
#define hbqt_ret_QPrinter( p )               ( hb_retptr( ( QPrinter* ) p ) )

#include <QtGui/QWidget>

void    hbqt_ret_QRect( QRect );
void    hbqt_ret_QSize( QSize );
void    hbqt_ret_QPoint( QPoint );

QRect   hbqt_const_QRect( int );
QSize   hbqt_const_QSize( int );
QPoint  hbqt_const_QPoint( int );

void    hbqt_ret_QRectF( QRectF );
void    hbqt_ret_QSizeF( QSizeF );
void    hbqt_ret_QPointF( QPointF );

QRectF  hbqt_const_QRectF( int );
QSizeF  hbqt_const_QSizeF( int );
QPointF hbqt_const_QPointF( int );

#endif

#endif /* __HBQT_H */

