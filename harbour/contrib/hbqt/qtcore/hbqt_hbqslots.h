/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * QT wrapper main header
 *
 * Copyright 2009 Marcos Antonio Gambeta <marcosgambeta at gmail dot com>
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

#ifndef HBQT_SLOTS_H
#define HBQT_SLOTS_H

#include "hbapiitm.h"

#include "hbqtcore.h"

/* TOFIX: QtGui components should not be accessed from this component */
#include <QtCore/QProcess>
#include <QtCore/QUrl>
#include <QtCore/QDate>
#include <QtCore/QDateTime>
#include <QtCore/QTime>
#include <QtGui/QTextCursor>
#include <QtGui/QTextCharFormat>
#include <QtGui/QTreeWidgetItem>
#include <QtGui/QListWidgetItem>
#include <QtGui/QTableWidgetItem>
#include <QtGui/QDockWidget>
#include <QtGui/QHeaderView>
#include <QtGui/QSystemTrayIcon>
#include <QtGui/QMdiSubWindow>
#include <QtGui/QSessionManager>

/*----------------------------------------------------------------------*/

#define __PRITPAL__

#ifdef __PRITPAL__

class HBQSlots: public QObject
{
   Q_OBJECT

public:
   HBQSlots( QObject *parent = 0 );
   ~HBQSlots();
   QList<PHB_ITEM>  listBlock;
   QList<QObject *> listObj;

   bool hbConnect( PHB_ITEM pObj, const char * slot, PHB_ITEM bBlock );
   bool hbDisconnect( PHB_ITEM pObj, const char * slot );
   bool hbIsConnected( PHB_ITEM pObj, const char * slot );
   bool isConnected( QObject * object, const char * slot );
   bool hbClear();

public slots:
   /* QWidget */
   void customContextMenuRequested( const QPoint & pos );
   void clicked();
   void triggered();
   void triggered( bool checked );
   void hovered();
   void stateChanged( int state );
   void pressed();
   void released();
   void activated( int index );
   void currentIndexChanged( int index );
   void highlighted( int index );
   void clicked( const QModelIndex & index );
   void doubleClicked( const QModelIndex & index );
   void entered( const QModelIndex & index );
   void viewportEntered();
   //bool event( QEvent * event, QWidget * w );
   void hovered( QAction * action );
   void currentChanged( int index );
   /* QAbstractSlider() */
   void actionTriggered( int action );
   void rangeChanged( int min, int max );
   void sliderMoved( int value );
   void sliderPressed();
   void sliderReleased();
   void valueChanged( int value );
   /* QLineEdit() */
   void cursorPositionChanged( int iOld, int iNew );
   void editingFinished();
   void returnPressed();
   void selectionChanged();
   void textChanged( const QString & text );
   void textEdited( const QString & text );
   /* QTreeWidget */
   void currentItemChanged( QTreeWidgetItem * current, QTreeWidgetItem * previous );
   void itemActivated( QTreeWidgetItem * item, int column );
   void itemChanged( QTreeWidgetItem * item, int column );
   void itemClicked( QTreeWidgetItem * item, int column );
   void itemCollapsed( QTreeWidgetItem * item );
   void itemDoubleClicked( QTreeWidgetItem * item, int column );
   void itemEntered( QTreeWidgetItem * item, int column );
   void itemExpanded( QTreeWidgetItem * item );
   void itemPressed( QTreeWidgetItem * item, int column );
   void itemSelectionChanged();
   /* QWebPage */
#if 0
   void contentsChanged();
   void databaseQuotaExceeded( QWebFrame * frame, QString databaseName );
   void downloadRequested( const QNetworkRequest & request );
   void frameCreated( QWebFrame * frame );
   void geometryChangeRequested( const QRect & geom );
   void linkClicked( const QUrl & url );
   void linkHovered( const QString & link, const QString & title, const QString & textContent );
   void loadFinished( bool ok );
   void loadProgress( int progress );
   void loadStarted();
   void menuBarVisibilityChangeRequested( bool visible );
   void microFocusChanged();
   void printRequested( QWebFrame * frame );
   void repaintRequested( const QRect & dirtyRect );
   void restoreFrameStateRequested( QWebFrame * frame );
   void saveFrameStateRequested( QWebFrame * frame, QWebHistoryItem * item );
   void scrollRequested( int dx, int dy, const QRect & rectToScroll );
   void statusBarMessage( const QString & text );
   void statusBarVisibilityChangeRequested( bool visible );
   void toolBarVisibilityChangeRequested( bool visible );
   void unsupportedContent( QNetworkReply * reply );
   void windowCloseRequested();
   /* QWebView */
   void iconChanged();
   void titleChanged( const QString & title );
   void urlChanged( const QUrl & url );
#endif
   /* QDialog - QFontDialog QFileDialog */
   void currentFontChanged( const QFont & font );
   void fontSelected( const QFont & font );
   void accepted();
   void finished( int result );
   void rejected();
   void currentChanged( const QString & path );
   void directoryEntered( const QString & directory );
   void fileSelected( const QString & file );
   void filesSelected( const QStringList & selected );
   void filterSelected( const QString & filter );
   void accepted( QPrinter * printer );
   void copyAvailable( bool yes );
   void currentCharFormatChanged( const QTextCharFormat & f );
   void cursorPositionChanged();
   void redoAvailable( bool available );
   void textChanged();
   void undoAvailable( bool available );
   void timeout();
   /* Generic Keyboard and Mouse Events */
   void keyPressEvent( QKeyEvent * event );
   void keyReleaseEvent( QKeyEvent * event );
   void mouseMoveEvent( QMouseEvent * event );
   void mousePressEvent( QMouseEvent * event );
   void mouseReleaseEvent( QMouseEvent * event );
   void mouseDoubleClickEvent( QMouseEvent * event );
   void wheelEvent( QWheelEvent * event );
   void resizeEvent( QResizeEvent * event );
   void scrollContentsBy( int x, int y );
   void geometriesChanged();
   void sectionAutoResize( int logicalIndex, QHeaderView::ResizeMode mode );
   void sectionClicked( int logicalIndex );
   void sectionCountChanged( int oldCount, int newCount );
   void sectionDoubleClicked( int logicalIndex );
   void sectionEntered( int logicalIndex );
   void sectionHandleDoubleClicked( int logicalIndex );
   void sectionMoved( int logicalIndex, int oldVisualIndex, int newVisualIndex );
   void sectionPressed( int logicalIndex );
   void sectionResized( int logicalIndex, int oldSize, int newSize );
   void sortIndicatorChanged( int logicalIndex, Qt::SortOrder order );
   void buttonClicked( int id );
   void buttonPressed( int id );
   void buttonReleased( int id );
   void linkActivated( const QString & link );
   void linkHovered( const QString & link );
   void tabCloseRequested( int index );
   void paintRequested( QPrinter * printer );
   /* QIODevice */
   void aboutToClose();
   void bytesWritten( qint64 bytes );
   void readChannelFinished();
   void readyRead();
   /* QProcess */
   void error( QProcess::ProcessError error );
   void finished( int exitCode, QProcess::ExitStatus exitStatus );
   void readyReadStandardError();
   void readyReadStandardOutput();
   void started();
   void stateChanged( QProcess::ProcessState newState );
   /* QComboBox */
   void activated( const QString & text );
   void currentIndexChanged( const QString & text );
   void editTextChanged( const QString & text );
   void highlighted( const QString & text );
   /* QTextDocument */
   void blockCountChanged( int newBlockCount );
   void contentsChange( int position, int charsRemoved, int charsAdded );
   void contentsChanged();
   void cursorPositionChanged( const QTextCursor & cursor );
   void documentLayoutChanged();
   void modificationChanged( bool changed );
   void undoCommandAdded();
   /* QPlainTextEdit */
   void updateRequest( const QRect & rect, int dy );
   /* QItemSelectionModel */
   void currentChanged( const QModelIndex & currentIndex, const QModelIndex & previousIndex );
   void currentColumnChanged( const QModelIndex & currentIndex, const QModelIndex & previousIndex );
   void currentRowChanged( const QModelIndex & currentIndex, const QModelIndex & previousIndex );
   void selectionChanged( const QItemSelection & selected, const QItemSelection & deselected );
   /* QListWidget */
   void currentRowChanged( int currentRow );
   void currentTextChanged( const QString & currentText );
   void currentItemChanged( QListWidgetItem * current, QListWidgetItem * previous );
   void itemActivated( QListWidgetItem * item );
   void itemChanged( QListWidgetItem * item );
   void itemClicked( QListWidgetItem * item );
   void itemDoubleClicked( QListWidgetItem * item );
   void itemEntered( QListWidgetItem * item );
   void itemPressed( QListWidgetItem * item );
   /* QTableWidget */
   void cellActivated( int row, int column );
   void cellChanged( int row, int column );
   void cellClicked( int row, int column );
   void cellDoubleClicked( int row, int column );
   void cellEntered( int row, int column );
   void cellPressed( int row, int column );
   void currentCellChanged( int currentRow, int currentColumn, int previousRow, int previousColumn );
   void currentItemChanged( QTableWidgetItem * current, QTableWidgetItem * previous );
   void itemActivated( QTableWidgetItem * item );
   void itemChanged( QTableWidgetItem * item );
   void itemClicked( QTableWidgetItem * item );
   void itemDoubleClicked( QTableWidgetItem * item );
   void itemEntered( QTableWidgetItem * item );
   void itemPressed( QTableWidgetItem * item );
   /* QTextBrowser */
   void anchorClicked( const QUrl & link );
   void backwardAvailable( bool available );
   void forwardAvailable( bool available );
   void highlighted( const QUrl & link );
   void historyChanged();
   void sourceChanged( const QUrl & src );
   /* QDockWidget */
   void allowedAreasChanged( Qt::DockWidgetAreas allowedAreas );
   void dockLocationChanged( Qt::DockWidgetArea area );
   void featuresChanged( QDockWidget::DockWidgetFeatures features );
   void topLevelChanged( bool topLevel );
   void visibilityChanged( bool visible );
   /* QCompleter */
   void activated( const QModelIndex & index );
   void highlighted( const QModelIndex & index );
   /* QAbstractButton */
   void toggled( bool checked );
   /* QSystemTrayIcon */
   void activated( QSystemTrayIcon::ActivationReason reason );
   /* QMdiArea */
   void subWindowActivated( QMdiSubWindow * window );
   /* QMdiSubWindow */
   void aboutToActivate();
   void windowStateChanged( Qt::WindowStates oldState, Qt::WindowStates newState );
   /* QAbstractItemDelegate */
   void closeEditor( QWidget * editor, QAbstractItemDelegate::EndEditHint hint );
   void commitData( QWidget * editor );
   void sizeHintChanged( const QModelIndex & index );
   /* QGraphicsScene */
   void sceneRectChanged( const QRectF & rect );
   /* QDateTimeEdit */
   void dateChanged( const QDate & date );
   void dateTimeChanged( const QDateTime & datetime );
   void timeChanged( const QTime & time );
   /* QApplication */
//   void commitDataRequest( QSessionManager & manager );
   void focusChanged( QWidget * old, QWidget * now );
   void fontDatabaseChanged();
   void lastWindowClosed();
//   void saveStateRequest( QSessionManager & manager );
   /* Latest */
};

#else

class HBQSlots: public QObject
{

public:
   HBQSlots( QObject *parent = 0 );
   ~HBQSlots();
   QList<PHB_ITEM>  listBlock;
   QList<QObject *> listObj;

   int qt_metacall( QMetaObject::Call call, int id, void **arguments );

   bool hbConnect( PHB_ITEM pObj, const char * slot, PHB_ITEM bBlock );
   bool hbDisconnect( PHB_ITEM pObj, const char * slot );
   bool hbIsConnected( PHB_ITEM pObj, const char * slot );
   bool isConnected( QObject * object, const char * slot );
   bool hbClear();
};

#endif
/*----------------------------------------------------------------------*/

#endif
