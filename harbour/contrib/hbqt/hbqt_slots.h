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


#ifndef SLOTS_H

#define SLOTS_H

#include <QObject>
#include <QMainWindow>
#include <QList>
#include <QTextCharFormat>
#include <QModelIndex>
#include <QEvent>
#include <QKeyEvent>
#include <QMouseEvent>
#include <QLineEdit>
#include <QTreeWidget>
#include <QTreeWidgetItem>
#include <QWebFrame>
#include <QNetworkRequest>

#include "hbapi.h"
#include "hbapiitm.h"

/*----------------------------------------------------------------------*/

class MyMainWindow : public QMainWindow
{
   Q_OBJECT

public:
   MyMainWindow();
   virtual ~MyMainWindow();
};

/*----------------------------------------------------------------------*/

class MyDrawingArea : public QWidget
{
   Q_OBJECT

public:
   MyDrawingArea( QWidget *parent = 0 );
   virtual ~MyDrawingArea( void );

   void keyPressEvent( QKeyEvent * event );
   void mouseMoveEvent( QMouseEvent * event );

signals:
   void sg_mouseMoveEvent( QMouseEvent * event );
   void sg_keyPressEvent( QKeyEvent * event );
};

/*----------------------------------------------------------------------*/

class Slots: public QObject
{
   Q_OBJECT

public:
   Slots( QObject *parent = 0 );
   ~Slots();
   QList<PHB_ITEM>  listBlock;
   QList<bool>      listActv;

public slots:
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
   void keyPressEvent( QKeyEvent * event );
   void mouseMoveEvent( QMouseEvent * event );
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
};

class Events: public QObject
{
   Q_OBJECT

public:
   Events( QObject *parent = 0 );
   ~Events();
   QList<PHB_ITEM>     listBlock;
   QList<bool>         listActv;

protected:
   bool eventFilter( QObject * obj, QEvent * event );

};

/*----------------------------------------------------------------------*/
#endif
