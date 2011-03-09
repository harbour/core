/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * QT wrapper main header
 *
 * Copyright 2009 Marcos Antonio Gambeta (marcosgambeta at gmail dot com)
 * Copyright 2009 Pritpal Bedi (pritpal@vouchcac.com)
 * Copyright 2010 Viktor Szakats (harbour.01 syenar.hu)
 * Copyright 2010 Francesco Perillo ()
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

#include "hbqt.h"
#include "hbqtinit.h"

#include "hbapierr.h"
#include "hbvm.h"
#include "hbinit.h"

#if QT_VERSION >= 0x040500

#include <QtGui/QApplication>

#include <QtGui/QItemSelection>
#include <QtGui/QTextCursor>
#include <QtGui/QTextCharFormat>
#include <QtGui/QTextBlock>
#include <QtGui/QSessionManager>
#include <QtGui/QColor>
#include <QtGui/QAbstractButton>
#include <QtGui/QAction>
#include <QtGui/QMdiSubWindow>
#include <QtGui/QPrinter>
#include <QtGui/QStandardItem>
#include <QtGui/QListWidgetItem>
#include <QtGui/QTreeWidgetItem>
#include <QtGui/QWidget>
#include <QtGui/QKeyEvent>

#include <QtCore/QStringList>

HB_EXTERN_BEGIN
extern void * hbqt_gcAllocate_QColor( void * pObj, bool bNew );
extern void * hbqt_gcAllocate_QItemSelection( void * pObj, bool bNew );
extern void * hbqt_gcAllocate_QTextCharFormat( void * pObj, bool bNew );
extern void * hbqt_gcAllocate_QFont( void * pObj, bool bNew );
extern void * hbqt_gcAllocate_QTextCursor( void * pObj, bool bNew );
extern void * hbqt_gcAllocate_QTextBlock( void * pObj, bool bNew );
extern void * hbqt_gcAllocate_QAbstractButton( void * pObj, bool bNew );
extern void * hbqt_gcAllocate_QAction( void * pObj, bool bNew );
extern void * hbqt_gcAllocate_QMdiSubWindow( void * pObj, bool bNew );
extern void * hbqt_gcAllocate_QPrinter( void * pObj, bool bNew );
extern void * hbqt_gcAllocate_QStandardItem( void * pObj, bool bNew );
extern void * hbqt_gcAllocate_QListWidgetItem( void * pObj, bool bNew );
extern void * hbqt_gcAllocate_QTreeWidgetItem( void * pObj, bool bNew );
extern void * hbqt_gcAllocate_QTableWidgetItem( void * pObj, bool bNew );
extern void * hbqt_gcAllocate_QWidget( void * pObj, bool bNew );
HB_EXTERN_END

/*----------------------------------------------------------------------*/

static void hbqt_SlotsExecQColor( PHB_ITEM * codeBlock, void ** arguments, QStringList pList )
{
   Q_UNUSED( pList );

   hb_vmPushEvalSym();
   hb_vmPush( codeBlock );
   hb_vmPush( hbqt_create_objectGC( hbqt_gcAllocate_QColor( new QColor( ( *reinterpret_cast< QColor( * ) >( arguments[ 1 ] ) ) ), true ), "hb_QColor" ) );
   hb_vmSend( 1 );
}

static void hbqt_SlotsExecItemSelItemSel( PHB_ITEM * codeBlock, void ** arguments, QStringList pList )
{
   Q_UNUSED( pList );

   hb_vmPushEvalSym();
   hb_vmPush( codeBlock );
   hb_vmPush( hbqt_create_objectGC( hbqt_gcAllocate_QItemSelection( new QItemSelection( ( *reinterpret_cast< QItemSelection( * ) >( arguments[ 1 ] ) ) ), true ), "hb_QItemSelection" ) );
   hb_vmPush( hbqt_create_objectGC( hbqt_gcAllocate_QItemSelection( new QItemSelection( ( *reinterpret_cast< QItemSelection( * ) >( arguments[ 2 ] ) ) ), true ), "hb_QItemSelection" ) );
   hb_vmSend( 2 );
}

static void hbqt_SlotsExecQTextCharFormat( PHB_ITEM * codeBlock, void ** arguments, QStringList pList )
{
   Q_UNUSED( pList );

   hb_vmPushEvalSym();
   hb_vmPush( codeBlock );
   hb_vmPush( hbqt_create_objectGC( hbqt_gcAllocate_QTextCharFormat( new QTextCharFormat( ( *reinterpret_cast< QTextCharFormat( * ) >( arguments[ 1 ] ) ) ), true ), "hb_QTextCharFormat" ) );
   hb_vmSend( 1 );
}

static void hbqt_SlotsExecQFont( PHB_ITEM * codeBlock, void ** arguments, QStringList pList )
{
   Q_UNUSED( pList );

   hb_vmPushEvalSym();
   hb_vmPush( codeBlock );
   hb_vmPush( hbqt_create_objectGC( hbqt_gcAllocate_QFont( new QFont( ( *reinterpret_cast< QFont( * ) >( arguments[ 1 ] ) ) ), true ), "hb_QFont" ) );
   hb_vmSend( 1 );
}

static void hbqt_SlotsExecQTextCursor( PHB_ITEM * codeBlock, void ** arguments, QStringList pList )
{
   Q_UNUSED( pList );

   hb_vmPushEvalSym();
   hb_vmPush( codeBlock );
   hb_vmPush( hbqt_create_objectGC( hbqt_gcAllocate_QTextCursor( new QTextCursor( ( *reinterpret_cast< QTextCursor( * ) >( arguments[ 1 ] ) ) ), true ), "hb_QTextCursor" ) );
   hb_vmSend( 1 );
}

static void hbqt_SlotsExecQTextBlock( PHB_ITEM * codeBlock, void ** arguments, QStringList pList )
{
   Q_UNUSED( pList );

   hb_vmPushEvalSym();
   hb_vmPush( codeBlock );
   hb_vmPush( hbqt_create_objectGC( hbqt_gcAllocate_QTextBlock( new QTextBlock( ( *reinterpret_cast< QTextBlock( * ) >( arguments[ 1 ] ) ) ), true ), "hb_QTextBlock" ) );
   hb_vmSend( 1 );
}

static void hbqt_SlotsExecQAbstractButton( PHB_ITEM * codeBlock, void ** arguments, QStringList pList )
{
   Q_UNUSED( pList );

   hb_vmPushEvalSym();
   hb_vmPush( codeBlock );
   hb_vmPush( hbqt_create_objectGC( hbqt_gcAllocate_QAbstractButton( new QWidget( (  QAbstractButton * ) ( arguments[ 1 ] ) ) , true ), "hb_QAbstractButton" ) );
   hb_vmSend( 1 );
}

static void hbqt_SlotsExecQAction( PHB_ITEM * codeBlock, void ** arguments, QStringList pList )
{
   Q_UNUSED( pList );

   hb_vmPushEvalSym();
   hb_vmPush( codeBlock );
   hb_vmPush( hbqt_create_objectGC( hbqt_gcAllocate_QAction( new QAction( ( QAction * ) ( arguments[ 1 ] ) ), true ), "hb_QAction" ) );
   hb_vmSend( 1 );
}

static void hbqt_SlotsExecQMdiSubWindow( PHB_ITEM * codeBlock, void ** arguments, QStringList pList )
{
   Q_UNUSED( pList );

   hb_vmPushEvalSym();
   hb_vmPush( codeBlock );
   hb_vmPush( hbqt_create_objectGC( hbqt_gcAllocate_QMdiSubWindow( ( *reinterpret_cast< void*( * ) >( arguments[ 1 ] ) ), false ), "hb_QMdiSubWindow" ) );
   hb_vmSend( 1 );
}

static void hbqt_SlotsExecQTreeWidgetItem( PHB_ITEM * codeBlock, void ** arguments, QStringList pList )
{
   Q_UNUSED( pList );

   hb_vmPushEvalSym();
   hb_vmPush( codeBlock );
   hb_vmPush( hbqt_create_objectGC( hbqt_gcAllocate_QTreeWidgetItem( ( *reinterpret_cast< void*( * ) >( arguments[ 1 ] ) ), false ), "hb_QTreeWidgetItem" ) );
   hb_vmSend( 1 );
}

static void hbqt_SlotsExecQTreeWidgetItemInt( PHB_ITEM * codeBlock, void ** arguments, QStringList pList )
{
   Q_UNUSED( pList );

   hb_vmPushEvalSym();
   hb_vmPush( codeBlock );
   hb_vmPush( hbqt_create_objectGC( hbqt_gcAllocate_QTreeWidgetItem( ( *reinterpret_cast< void*( * ) >( arguments[ 1 ] ) ), false ), "hb_QTreeWidgetItem" ) );
   hb_vmPushInteger( *reinterpret_cast< int( * ) >( arguments[ 2 ] ) );
   hb_vmSend( 2 );
}

static void hbqt_SlotsExecQPrinter( PHB_ITEM * codeBlock, void ** arguments, QStringList pList )
{
   Q_UNUSED( pList );

   hb_vmPushEvalSym();
   hb_vmPush( codeBlock );
   hb_vmPush( hbqt_create_objectGC( hbqt_gcAllocate_QPrinter( ( *reinterpret_cast< void*(*)> ( arguments[ 1 ] ) ), false ), "hb_QPrinter" ) );
   hb_vmSend( 1 );
}

static void hbqt_SlotsExecQStandardItem( PHB_ITEM * codeBlock, void ** arguments, QStringList pList )
{
   Q_UNUSED( pList );

   hb_vmPushEvalSym();
   hb_vmPush( codeBlock );
   hb_vmPush( hbqt_create_objectGC( hbqt_gcAllocate_QStandardItem( ( *reinterpret_cast< void*( * )>( arguments[ 1 ] ) ), false ), "hb_QStandardItem" ) );
   hb_vmSend( 1 );
}

static void hbqt_SlotsExecQListWidgetItem( PHB_ITEM * codeBlock, void ** arguments, QStringList pList )
{
   Q_UNUSED( pList );

   hb_vmPushEvalSym();
   hb_vmPush( codeBlock );
   hb_vmPush( hbqt_create_objectGC( hbqt_gcAllocate_QListWidgetItem( ( *reinterpret_cast<void*( * )>( arguments[ 1 ] ) ), false ), "hb_QListWidgetItem" ) );
   hb_vmSend( 1 );
}

static void hbqt_SlotsExecQListWidgetItemQListWidgetItem( PHB_ITEM * codeBlock, void ** arguments, QStringList pList )
{
   Q_UNUSED( pList );

   hb_vmPushEvalSym();
   hb_vmPush( codeBlock );
   hb_vmPush( hbqt_create_objectGC( hbqt_gcAllocate_QListWidgetItem( ( *reinterpret_cast<void*( * )>( arguments[ 1 ] ) ), false ), "hb_QListWidgetItem" ) );
   hb_vmPush( hbqt_create_objectGC( hbqt_gcAllocate_QListWidgetItem( ( *reinterpret_cast<void*( * )>( arguments[ 2 ] ) ), false ), "hb_QListWidgetItem" ) );
   hb_vmSend( 2 );
}

static void hbqt_SlotsExecQTableWidgetItem( PHB_ITEM * codeBlock, void ** arguments, QStringList pList )
{
   Q_UNUSED( pList );

   hb_vmPushEvalSym();
   hb_vmPush( codeBlock );
   hb_vmPush( hbqt_create_objectGC( hbqt_gcAllocate_QTableWidgetItem( ( *reinterpret_cast<void*( * )>( arguments[ 1 ] ) ), false ), "hb_QTableWidgetItem" ) );
   hb_vmSend( 1 );
}

static void hbqt_SlotsExecQTableWidgetItemQTableWidgetItem( PHB_ITEM * codeBlock, void ** arguments, QStringList pList )
{
   Q_UNUSED( pList );

   hb_vmPushEvalSym();
   hb_vmPush( codeBlock );
   hb_vmPush( hbqt_create_objectGC( hbqt_gcAllocate_QTableWidgetItem( ( *reinterpret_cast<void*( * )>( arguments[ 1 ] ) ), false ), "hb_QTableWidgetItem" ) );
   hb_vmPush( hbqt_create_objectGC( hbqt_gcAllocate_QTableWidgetItem( ( *reinterpret_cast<void*( * )>( arguments[ 2 ] ) ), false ), "hb_QTableWidgetItem" ) );
   hb_vmSend( 2 );
}

static void hbqt_SlotsExecQTreeWidgetItemQTreeWidgetItem( PHB_ITEM * codeBlock, void ** arguments, QStringList pList )
{
   Q_UNUSED( pList );

   hb_vmPushEvalSym();
   hb_vmPush( codeBlock );
   hb_vmPush( hbqt_create_objectGC( hbqt_gcAllocate_QTreeWidgetItem( ( *reinterpret_cast<void*( * )>( arguments[ 1 ] ) ), false ), "hb_QTreeWidgetItem" ) );
   hb_vmPush( hbqt_create_objectGC( hbqt_gcAllocate_QTreeWidgetItem( ( *reinterpret_cast<void*( * )>( arguments[ 2 ] ) ), false ), "hb_QTreeWidgetItem" ) );
   hb_vmSend( 2 );
}

static void hbqt_SlotsExecQWidget( PHB_ITEM * codeBlock, void ** arguments, QStringList pList )
{
   Q_UNUSED( pList );

   hb_vmPushEvalSym();
   hb_vmPush( codeBlock );
   hb_vmPush( hbqt_create_objectGC( hbqt_gcAllocate_QWidget( ( *reinterpret_cast< void*( * ) >( arguments[ 1 ] ) ), false ), "hb_QWidget" ) );
   hb_vmSend( 1 );
}

static void hbqt_SlotsExecQWidgetQWidget( PHB_ITEM * codeBlock, void ** arguments, QStringList pList )
{
   Q_UNUSED( pList );

   hb_vmPushEvalSym();
   hb_vmPush( codeBlock );
   hb_vmPush( hbqt_create_objectGC( hbqt_gcAllocate_QWidget( ( *reinterpret_cast<void*( * )>( arguments[ 1 ] ) ), false ), "hb_QWidget" ) );
   hb_vmPush( hbqt_create_objectGC( hbqt_gcAllocate_QWidget( ( *reinterpret_cast<void*( * )>( arguments[ 2 ] ) ), false ), "hb_QWidget" ) );
   hb_vmSend( 2 );
}

static void hbqt_SlotsExecQWidgetInt( PHB_ITEM * codeBlock, void ** arguments, QStringList pList )
{
   Q_UNUSED( pList );

   hb_vmPushEvalSym();
   hb_vmPush( codeBlock );
   hb_vmPush( hbqt_create_objectGC( hbqt_gcAllocate_QWidget( ( *reinterpret_cast< void*( * ) >( arguments[ 1 ] ) ), false ), "hb_QWidget" ) );
   hb_vmPushInteger( *reinterpret_cast< int( * ) >( arguments[ 2 ] ) );
   hb_vmSend( 2 );
}


HB_FUNC_EXTERN( HB_QABSTRACTBUTTON );
HB_FUNC_EXTERN( HB_QACTION );
HB_FUNC_EXTERN( HB_QMDISUBWINDOW );
HB_FUNC_EXTERN( HB_QPRINTER );
HB_FUNC_EXTERN( HB_QSTANDARDITEM );
HB_FUNC_EXTERN( HB_QLISTWIDGETITEM );
HB_FUNC_EXTERN( HB_QTABLEWIDGETITEM );
HB_FUNC_EXTERN( HB_QTREEWIDGETITEM );

/*----------------------------------------------------------------------*/

HB_FUNC_EXTERN( HB_QCONTEXTMENUEVENT );
HB_FUNC_EXTERN( HB_QDRAGENTEREVENT );
HB_FUNC_EXTERN( HB_QDRAGLEAVEEVENT );
HB_FUNC_EXTERN( HB_QDRAGMOVEEVENT );
HB_FUNC_EXTERN( HB_QDROPEVENT );
HB_FUNC_EXTERN( HB_QEVENT );
HB_FUNC_EXTERN( HB_QFOCUSEVENT );
HB_FUNC_EXTERN( HB_QFOCUSEVENT );
HB_FUNC_EXTERN( HB_QGRAPHICSSCENECONTEXTMENUEVENT );
HB_FUNC_EXTERN( HB_QGRAPHICSSCENEDRAGDROPEVENT );
HB_FUNC_EXTERN( HB_QGRAPHICSSCENEHOVEREVENT );
HB_FUNC_EXTERN( HB_QGRAPHICSSCENEMOUSEEVENT );
HB_FUNC_EXTERN( HB_QGRAPHICSSCENEMOVEEVENT );
HB_FUNC_EXTERN( HB_QGRAPHICSSCENERESIZEEVENT );
HB_FUNC_EXTERN( HB_QGRAPHICSSCENEWHEELEVENT );
HB_FUNC_EXTERN( HB_QHELPEVENT );
HB_FUNC_EXTERN( HB_QHIDEEVENT );
HB_FUNC_EXTERN( HB_QINPUTMETHODEVENT );
HB_FUNC_EXTERN( HB_QKEYEVENT );
HB_FUNC_EXTERN( HB_QMOUSEEVENT );
HB_FUNC_EXTERN( HB_QMOVEEVENT );
HB_FUNC_EXTERN( HB_QPAINTEVENT );
HB_FUNC_EXTERN( HB_QRESIZEEVENT );
HB_FUNC_EXTERN( HB_QSHOWEVENT );
HB_FUNC_EXTERN( HB_QWHEELEVENT );
HB_FUNC_EXTERN( HB_QWINDOWSTATECHANGEEVENT );

void _hbqtgui_force_link_for_event( void )
{
   HB_FUNC_EXEC( HB_QCONTEXTMENUEVENT );
   HB_FUNC_EXEC( HB_QDRAGENTEREVENT );
   HB_FUNC_EXEC( HB_QDRAGLEAVEEVENT );
   HB_FUNC_EXEC( HB_QDRAGMOVEEVENT );
   HB_FUNC_EXEC( HB_QDROPEVENT );
   HB_FUNC_EXEC( HB_QEVENT );
   HB_FUNC_EXEC( HB_QFOCUSEVENT );
   HB_FUNC_EXEC( HB_QFOCUSEVENT );
   HB_FUNC_EXEC( HB_QGRAPHICSSCENECONTEXTMENUEVENT );
   HB_FUNC_EXEC( HB_QGRAPHICSSCENEDRAGDROPEVENT );
   HB_FUNC_EXEC( HB_QGRAPHICSSCENEHOVEREVENT );
   HB_FUNC_EXEC( HB_QGRAPHICSSCENEMOUSEEVENT );
   HB_FUNC_EXEC( HB_QGRAPHICSSCENEMOVEEVENT );
   HB_FUNC_EXEC( HB_QGRAPHICSSCENERESIZEEVENT );
   HB_FUNC_EXEC( HB_QGRAPHICSSCENEWHEELEVENT );
   HB_FUNC_EXEC( HB_QHELPEVENT );
   HB_FUNC_EXEC( HB_QHIDEEVENT );
   HB_FUNC_EXEC( HB_QINPUTMETHODEVENT );
   HB_FUNC_EXEC( HB_QKEYEVENT );
   HB_FUNC_EXEC( HB_QMOUSEEVENT );
   HB_FUNC_EXEC( HB_QMOVEEVENT );
   HB_FUNC_EXEC( HB_QPAINTEVENT );
   HB_FUNC_EXEC( HB_QRESIZEEVENT );
   HB_FUNC_EXEC( HB_QSHOWEVENT );
   HB_FUNC_EXEC( HB_QWHEELEVENT );
   HB_FUNC_EXEC( HB_QWINDOWSTATECHANGEEVENT );

   HB_FUNC_EXEC( HB_QABSTRACTBUTTON );
   HB_FUNC_EXEC( HB_QACTION );
   HB_FUNC_EXEC( HB_QMDISUBWINDOW );
   HB_FUNC_EXEC( HB_QPRINTER );
   HB_FUNC_EXEC( HB_QSTANDARDITEM );
   HB_FUNC_EXEC( HB_QLISTWIDGETITEM );
   HB_FUNC_EXEC( HB_QTABLEWIDGETITEM );
   HB_FUNC_EXEC( HB_QTREEWIDGETITEM );
}

static void hbqt_registerCallbacks( void )
{
   hbqt_slots_register_callback( "QColor"                              , hbqt_SlotsExecQColor           );
   hbqt_slots_register_callback( "QFont"                               , hbqt_SlotsExecQFont            );
   hbqt_slots_register_callback( "QItemSelection$QItemSelection"       , hbqt_SlotsExecItemSelItemSel   );
   hbqt_slots_register_callback( "QTextBlock"                          , hbqt_SlotsExecQTextBlock       );
   hbqt_slots_register_callback( "QTextCharFormat"                     , hbqt_SlotsExecQTextCharFormat  );
   hbqt_slots_register_callback( "QTextCursor"                         , hbqt_SlotsExecQTextCursor      );

   hbqt_slots_register_callback( "QAbstractButton*"                    , hbqt_SlotsExecQAbstractButton  );
   hbqt_slots_register_callback( "QAction*"                            , hbqt_SlotsExecQAction          );
   hbqt_slots_register_callback( "QListWidgetItem*$QListWidgetItem*"   , hbqt_SlotsExecQListWidgetItemQListWidgetItem );
   hbqt_slots_register_callback( "QMdiSubWindow*"                      , hbqt_SlotsExecQMdiSubWindow    );
   hbqt_slots_register_callback( "QPrinter*"                           , hbqt_SlotsExecQPrinter         );
   hbqt_slots_register_callback( "QStandardItem*"                      , hbqt_SlotsExecQStandardItem    );
   hbqt_slots_register_callback( "QTableWidgetItem"                    , hbqt_SlotsExecQTableWidgetItem );
   hbqt_slots_register_callback( "QTableWidgetItem*$QTableWidgetItem*" , hbqt_SlotsExecQTableWidgetItemQTableWidgetItem );
   hbqt_slots_register_callback( "QTreeWidgetItem*$int"                , hbqt_SlotsExecQTreeWidgetItemInt );
   hbqt_slots_register_callback( "QTreeWidgetItem*$QTreeWidgetItem*"   , hbqt_SlotsExecQTreeWidgetItemQTreeWidgetItem );
   hbqt_slots_register_callback( "QWidget*$int"                        , hbqt_SlotsExecQWidgetInt       );
   hbqt_slots_register_callback( "QWidget*$QWidget*"                   , hbqt_SlotsExecQWidgetQWidget   );
   hbqt_slots_register_callback( "QTreeWidgetItem*"                    , hbqt_SlotsExecQTreeWidgetItem  );
   hbqt_slots_register_callback( "QListWidgetItem*"                    , hbqt_SlotsExecQListWidgetItem  );
   hbqt_slots_register_callback( "QWidget*"                            , hbqt_SlotsExecQWidget          );

   hbqt_events_register_createobj( QEvent::MouseButtonPress                  , "hb_QMouseEvent"                    );
   hbqt_events_register_createobj( QEvent::MouseButtonRelease                , "hb_QMouseEvent"                    );
   hbqt_events_register_createobj( QEvent::MouseButtonDblClick               , "hb_QMouseEvent"                    );
   hbqt_events_register_createobj( QEvent::MouseMove                         , "hb_QMouseEvent"                    );
   hbqt_events_register_createobj( QEvent::KeyPress                          , "hb_QKeyEvent"                      );
   hbqt_events_register_createobj( QEvent::KeyRelease                        , "hb_QKeyEvent"                      );
   hbqt_events_register_createobj( QEvent::FocusIn                           , "hb_QFocusEvent"                    );
   hbqt_events_register_createobj( QEvent::FocusOut                          , "hb_QFocusEvent"                    );
   hbqt_events_register_createobj( QEvent::Enter                             , "hb_QMouseEvent"                    );
   hbqt_events_register_createobj( QEvent::Leave                             , "hb_QMouseEvent"                    );
   hbqt_events_register_createobj( QEvent::Paint                             , "hb_QPaintEvent"                    );
   hbqt_events_register_createobj( QEvent::Move                              , "hb_QMoveEvent"                     );
   hbqt_events_register_createobj( QEvent::Resize                            , "hb_QResizeEvent"                   );
   hbqt_events_register_createobj( QEvent::Show                              , "hb_QShowEvent"                     );
   hbqt_events_register_createobj( QEvent::Hide                              , "hb_QHideEvent"                     );
   hbqt_events_register_createobj( QEvent::Close                             , "hb_QEvent"                         );
   hbqt_events_register_createobj( QEvent::ParentChange                      , "hb_QEvent"                         );
   hbqt_events_register_createobj( QEvent::WindowActivate                    , "hb_QEvent"                         );
   hbqt_events_register_createobj( QEvent::WindowDeactivate                  , "hb_QEvent"                         );
   hbqt_events_register_createobj( QEvent::ShowToParent                      , "hb_QEvent"                         );
   hbqt_events_register_createobj( QEvent::HideToParent                      , "hb_QEvent"                         );
   hbqt_events_register_createobj( QEvent::Wheel                             , "hb_QWheelEvent"                    );
   hbqt_events_register_createobj( QEvent::WindowTitleChange                 , "hb_QEvent"                         );
   hbqt_events_register_createobj( QEvent::WindowIconChange                  , "hb_QEvent"                         );
   hbqt_events_register_createobj( QEvent::ApplicationWindowIconChange       , "hb_QEvent"                         );
   hbqt_events_register_createobj( QEvent::ApplicationFontChange             , "hb_QEvent"                         );
   hbqt_events_register_createobj( QEvent::ApplicationLayoutDirectionChange  , "hb_QEvent"                         );
   hbqt_events_register_createobj( QEvent::ApplicationPaletteChange          , "hb_QEvent"                         );
   hbqt_events_register_createobj( QEvent::PaletteChange                     , "hb_QEvent"                         );
   hbqt_events_register_createobj( QEvent::Clipboard                         , "hb_QEvent"                         );
   hbqt_events_register_createobj( QEvent::MetaCall                          , "hb_QEvent"                         );
   hbqt_events_register_createobj( QEvent::SockAct                           , "hb_QEvent"                         );
   hbqt_events_register_createobj( QEvent::ShortcutOverride                  , "hb_QKeyEvent"                      );
   hbqt_events_register_createobj( QEvent::DeferredDelete                    , "hb_QEvent"                         );
   hbqt_events_register_createobj( QEvent::DragEnter                         , "hb_QDragEnterEvent"                );
   hbqt_events_register_createobj( QEvent::DragLeave                         , "hb_QDragLeaveEvent"                );
   hbqt_events_register_createobj( QEvent::DragMove                          , "hb_QDragMoveEvent"                 );
   hbqt_events_register_createobj( QEvent::Drop                              , "hb_QDropEvent"                     );
   hbqt_events_register_createobj( QEvent::ChildAdded                        , "hb_QEvent"                         );
   hbqt_events_register_createobj( QEvent::ChildPolished                     , "hb_QEvent"                         );
   hbqt_events_register_createobj( QEvent::ChildRemoved                      , "hb_QEvent"                         );
   hbqt_events_register_createobj( QEvent::PolishRequest                     , "hb_QEvent"                         );
   hbqt_events_register_createobj( QEvent::Polish                            , "hb_QEvent"                         );
   hbqt_events_register_createobj( QEvent::LayoutRequest                     , "hb_QEvent"                         );
   hbqt_events_register_createobj( QEvent::UpdateRequest                     , "hb_QEvent"                         );
   hbqt_events_register_createobj( QEvent::UpdateLater                       , "hb_QEvent"                         );
   hbqt_events_register_createobj( QEvent::ContextMenu                       , "hb_QContextMenuEvent"              );
   hbqt_events_register_createobj( QEvent::InputMethod                       , "hb_QInputMethodEvent"              );
   hbqt_events_register_createobj( QEvent::AccessibilityPrepare              , "hb_QEvent"                         );
   hbqt_events_register_createobj( QEvent::TabletMove                        , "hb_QEvent"                         );
   hbqt_events_register_createobj( QEvent::LocaleChange                      , "hb_QEvent"                         );
   hbqt_events_register_createobj( QEvent::LanguageChange                    , "hb_QEvent"                         );
   hbqt_events_register_createobj( QEvent::LayoutDirectionChange             , "hb_QEvent"                         );
   hbqt_events_register_createobj( QEvent::TabletPress                       , "hb_QEvent"                         );
   hbqt_events_register_createobj( QEvent::TabletRelease                     , "hb_QEvent"                         );
   hbqt_events_register_createobj( QEvent::OkRequest                         , "hb_QEvent"                         );
   hbqt_events_register_createobj( QEvent::IconDrag                          , "hb_QEvent"                         );
   hbqt_events_register_createobj( QEvent::FontChange                        , "hb_QEvent"                         );
   hbqt_events_register_createobj( QEvent::EnabledChange                     , "hb_QEvent"                         );
   hbqt_events_register_createobj( QEvent::ActivationChange                  , "hb_QEvent"                         );
   hbqt_events_register_createobj( QEvent::StyleChange                       , "hb_QEvent"                         );
   hbqt_events_register_createobj( QEvent::IconTextChange                    , "hb_QEvent"                         );
   hbqt_events_register_createobj( QEvent::ModifiedChange                    , "hb_QEvent"                         );
   hbqt_events_register_createobj( QEvent::WindowBlocked                     , "hb_QEvent"                         );
   hbqt_events_register_createobj( QEvent::WindowUnblocked                   , "hb_QEvent"                         );
   hbqt_events_register_createobj( QEvent::WindowStateChange                 , "hb_QWindowStateChangeEvent"        );
   hbqt_events_register_createobj( QEvent::MouseTrackingChange               , "hb_QEvent"                         );
   hbqt_events_register_createobj( QEvent::ToolTip                           , "hb_QHelpEvent"                     );
   hbqt_events_register_createobj( QEvent::WhatsThis                         , "hb_QHelpEvent"                     );
   hbqt_events_register_createobj( QEvent::StatusTip                         , "hb_QEvent"                         );
   hbqt_events_register_createobj( QEvent::ActionChanged                     , "hb_QEvent"                         );
   hbqt_events_register_createobj( QEvent::ActionAdded                       , "hb_QEvent"                         );
   hbqt_events_register_createobj( QEvent::ActionRemoved                     , "hb_QEvent"                         );
   hbqt_events_register_createobj( QEvent::FileOpen                          , "hb_QEvent"                         );
   hbqt_events_register_createobj( QEvent::Shortcut                          , "hb_QEvent"                         );
   hbqt_events_register_createobj( QEvent::WhatsThisClicked                  , "hb_QEvent"                         );
   hbqt_events_register_createobj( QEvent::AccessibilityHelp                 , "hb_QEvent"                         );
   hbqt_events_register_createobj( QEvent::ToolBarChange                     , "hb_QEvent"                         );
   hbqt_events_register_createobj( QEvent::ApplicationActivate               , "hb_QEvent"                         );
   hbqt_events_register_createobj( QEvent::ApplicationActivated              , "hb_QEvent"                         );
   hbqt_events_register_createobj( QEvent::ApplicationDeactivate             , "hb_QEvent"                         );
   hbqt_events_register_createobj( QEvent::QueryWhatsThis                    , "hb_QEvent"                         );
   hbqt_events_register_createobj( QEvent::EnterWhatsThisMode                , "hb_QEvent"                         );
   hbqt_events_register_createobj( QEvent::LeaveWhatsThisMode                , "hb_QEvent"                         );
   hbqt_events_register_createobj( QEvent::ZOrderChange                      , "hb_QEvent"                         );
   hbqt_events_register_createobj( QEvent::HoverEnter                        , "hb_QEvent"                         );
   hbqt_events_register_createobj( QEvent::HoverLeave                        , "hb_QEvent"                         );
   hbqt_events_register_createobj( QEvent::HoverMove                         , "hb_QEvent"                         );
   hbqt_events_register_createobj( QEvent::AccessibilityDescription          , "hb_QEvent"                         );
   hbqt_events_register_createobj( QEvent::ParentAboutToChange               , "hb_QEvent"                         );
   hbqt_events_register_createobj( QEvent::WinEventAct                       , "hb_QEvent"                         );
#if defined( QT_KEYPAD_NAVIGATION )
   hbqt_events_register_createobj( QEvent::EnterEditFocus                    , "hb_QEvent"                         );
   hbqt_events_register_createobj( QEvent::LeaveEditFocus                    , "hb_QEvent"                         );
#endif
   hbqt_events_register_createobj( QEvent::MenubarUpdated                    , "hb_QEvent"                         );
   hbqt_events_register_createobj( QEvent::GraphicsSceneMouseMove            , "hb_QGraphicsSceneMouseEvent"       );
   hbqt_events_register_createobj( QEvent::GraphicsSceneMousePress           , "hb_QGraphicsSceneMouseEvent"       );
   hbqt_events_register_createobj( QEvent::GraphicsSceneMouseRelease         , "hb_QGraphicsSceneMouseEvent"       );
   hbqt_events_register_createobj( QEvent::GraphicsSceneMouseDoubleClick     , "hb_QGraphicsSceneMouseEvent"       );
   hbqt_events_register_createobj( QEvent::GraphicsSceneContextMenu          , "hb_QGraphicsSceneContextMenuEvent" );
   hbqt_events_register_createobj( QEvent::GraphicsSceneHoverEnter           , "hb_QGraphicsSceneHoverEvent"       );
   hbqt_events_register_createobj( QEvent::GraphicsSceneHoverMove            , "hb_QGraphicsSceneHoverEvent"       );
   hbqt_events_register_createobj( QEvent::GraphicsSceneHoverLeave           , "hb_QGraphicsSceneHoverEvent"       );
   hbqt_events_register_createobj( QEvent::GraphicsSceneHelp                 , "hb_QHelpEvent"                     );
   hbqt_events_register_createobj( QEvent::GraphicsSceneDragEnter            , "hb_QGraphicsSceneDragDropEvent"    );
   hbqt_events_register_createobj( QEvent::GraphicsSceneDragMove             , "hb_QGraphicsSceneDragDropEvent"    );
   hbqt_events_register_createobj( QEvent::GraphicsSceneDragLeave            , "hb_QGraphicsSceneDragDropEvent"    );
   hbqt_events_register_createobj( QEvent::GraphicsSceneDrop                 , "hb_QGraphicsSceneDragDropEvent"    );
   hbqt_events_register_createobj( QEvent::GraphicsSceneWheel                , "hb_QGraphicsSceneWheelEvent"       );
   hbqt_events_register_createobj( QEvent::KeyboardLayoutChange              , "hb_QEvent"                         );
   hbqt_events_register_createobj( QEvent::DynamicPropertyChange             , "hb_QEvent"                         );
   hbqt_events_register_createobj( QEvent::TabletEnterProximity              , "hb_QEvent"                         );
   hbqt_events_register_createobj( QEvent::TabletLeaveProximity              , "hb_QEvent"                         );
   hbqt_events_register_createobj( QEvent::NonClientAreaMouseMove            , "hb_QEvent"                         );
   hbqt_events_register_createobj( QEvent::NonClientAreaMouseButtonPress     , "hb_QEvent"                         );
   hbqt_events_register_createobj( QEvent::NonClientAreaMouseButtonRelease   , "hb_QEvent"                         );
   hbqt_events_register_createobj( QEvent::NonClientAreaMouseButtonDblClick  , "hb_QEvent"                         );
   hbqt_events_register_createobj( QEvent::MacSizeChange                     , "hb_QEvent"                         );
   hbqt_events_register_createobj( QEvent::ContentsRectChange                , "hb_QEvent"                         );
   hbqt_events_register_createobj( QEvent::GraphicsSceneResize               , "hb_QGraphicsSceneResizeEvent"      );
   hbqt_events_register_createobj( QEvent::GraphicsSceneMove                 , "hb_QGraphicsSceneMoveEvent"        );
   hbqt_events_register_createobj( QEvent::CursorChange                      , "hb_QEvent"                         );
   hbqt_events_register_createobj( QEvent::ToolTipChange                     , "hb_QEvent"                         );
   hbqt_events_register_createobj( QEvent::GrabMouse                         , "hb_QEvent"                         );
   hbqt_events_register_createobj( QEvent::UngrabMouse                       , "hb_QEvent"                         );
   hbqt_events_register_createobj( QEvent::GrabKeyboard                      , "hb_QEvent"                         );
   hbqt_events_register_createobj( QEvent::UngrabKeyboard                    , "hb_QEvent"                         );
}

/*----------------------------------------------------------------------*/

static QApplication * s_app = NULL;

static int s_argc;
static char ** s_argv;

HB_FUNC_EXTERN( __HBQTCORE );

HB_FUNC( __HBQTGUI )
{
   HB_FUNC_EXEC( __HBQTCORE );
}

HB_EXTERN_BEGIN
extern HB_EXPORT QApplication * __hbqtgui_app( void );
HB_EXTERN_END

QApplication * __hbqtgui_app( void )
{
   return s_app;
}

static void hbqt_lib_init( void * cargo )
{
   HB_SYMBOL_UNUSED( cargo );

   s_argc = hb_cmdargARGC();
   s_argv = hb_cmdargARGV();

   s_app = new QApplication( s_argc, s_argv );

   if( ! s_app )
      hb_errInternal( 11001, "hbqt_lib_init(): HBQTGUI Initilization Error.", NULL, NULL );

   hb_cmdargInit( s_argc, s_argv );

   hbqt_registerCallbacks();
}

static void hbqt_lib_exit( void * cargo )
{
   HB_SYMBOL_UNUSED( cargo );
}


HB_CALL_ON_STARTUP_BEGIN( _hbqtgui_init_ )
   hb_vmAtInit( hbqt_lib_init, NULL );
   hb_vmAtExit( hbqt_lib_exit, NULL );
HB_CALL_ON_STARTUP_END( _hbqtgui_init_ )

#if defined( HB_PRAGMA_STARTUP )
   #pragma startup _hbqtgui_init_
#elif defined( HB_DATASEG_STARTUP )
   #define HB_DATASEG_BODY    HB_DATASEG_FUNC( _hbqtgui_init_ )
   #include "hbiniseg.h"
#endif

#endif
