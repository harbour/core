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

#include "hbapi.h"
#include "hbvm.h"
#include "hbapiitm.h"
#include "hbstack.h"
#include "hbthread.h"

#include "hbqt.h"

#if QT_VERSION >= 0x040500

#include "hbqt_slots.h"

#include <QWidget>
#include <QString>
#include <QList>
#include <QKeyEvent>
#include <QAction>
#include <QObject>
#include <QEvent>
#include <QMessageBox>
#include <QFileDialog>

static PHB_ITEM s_mutex = NULL;

/*----------------------------------------------------------------------*/

typedef struct
{
   Events * events;
} HB_EVENTS, * PHB_EVENTS;

static HB_TSD_NEW( s_events, sizeof( HB_EVENTS ), NULL, NULL );

#define HB_GETQTEVENTFILTER()       ( ( PHB_EVENTS ) hb_stackGetTSD( &s_events ) )

typedef struct
{
   Slots * slot;
} HB_SLOTS, * PHB_SLOTS;

static HB_TSD_NEW( s_slots, sizeof( HB_SLOTS ), NULL, NULL );

#define HB_GETQTEVENTSLOTS()       ( ( PHB_SLOTS ) hb_stackGetTSD( &s_slots ) )

/*----------------------------------------------------------------------*/

static void qt_setEventFilter()
{
   Events * s_e = HB_GETQTEVENTFILTER()->events;
   if( ! s_e )
      HB_GETQTEVENTFILTER()->events = new Events();
}

static Events * qt_getEventFilter( void )
{
   return HB_GETQTEVENTFILTER()->events;
}

static void qt_setEventSlots()
{
   Slots * s_s = HB_GETQTEVENTSLOTS()->slot;
   if( ! s_s )
      HB_GETQTEVENTSLOTS()->slot = new Slots();
}

static Slots * qt_getEventSlots( void )
{
   return HB_GETQTEVENTSLOTS()->slot;
}

/*----------------------------------------------------------------------*/

Slots::Slots( QObject* parent ) : QObject( parent )
{
}

Slots::~Slots()
{
   listBlock.clear();
}

static void hbqt_SlotsExec( QObject * object, const char * event )
{
   if( object )
   {
      Slots * s_s = qt_getEventSlots();
      int i = object->property( event ).toInt();
      if( i > 0 && i <= s_s->listBlock.size() && hb_vmRequestReenter() )
      {
         PHB_ITEM pObject = hb_itemPutPtr( NULL, object );
         hb_vmEvalBlockV( s_s->listBlock.at( i - 1 ), 1, pObject );
         hb_itemRelease( pObject );
         hb_vmRequestRestore();
      }
   }
}

static void hbqt_SlotsExecBool( QObject * object, const char* event, bool bBool )
{
   if( object )
   {
      Slots * s_s = qt_getEventSlots();
      int i = object->property( event ).toInt();
      if( i > 0 && i <= s_s->listBlock.size() && hb_vmRequestReenter() )
      {
         PHB_ITEM pObject = hb_itemPutPtr( NULL, object );
         PHB_ITEM pBool = hb_itemPutL( NULL, bBool );
         hb_vmEvalBlockV( s_s->listBlock.at( i - 1 ), 2, pObject, pBool );
         hb_itemRelease( pObject );
         hb_itemRelease( pBool );
         hb_vmRequestRestore();
      }
   }
}

static void hbqt_SlotsExecInt( QObject * object, const char * event, int iValue )
{
   if( object )
   {
      Slots * s_s = qt_getEventSlots();
      int i = object->property( event ).toInt();
      if( i > 0 && i <= s_s->listBlock.size() && hb_vmRequestReenter() )
      {
         PHB_ITEM pObject = hb_itemPutPtr( NULL, object );
         PHB_ITEM pState = hb_itemPutNI( NULL, iValue );
         hb_vmEvalBlockV( s_s->listBlock.at( i - 1 ), 2, pObject, pState );
         hb_itemRelease( pObject );
         hb_itemRelease( pState );
         hb_vmRequestRestore();
      }
   }
}

static void hbqt_SlotsExecIntInt( QObject * object, const char * event, int iValue1, int iValue2 )
{
   if( object )
   {
      Slots * s_s = qt_getEventSlots();
      int i = object->property( event ).toInt();
      if( i > 0 && i <= s_s->listBlock.size() && hb_vmRequestReenter() )
      {
         PHB_ITEM pObject = hb_itemPutPtr( NULL, object );
         PHB_ITEM pValue1 = hb_itemPutNI( NULL, iValue1 );
         PHB_ITEM pValue2 = hb_itemPutNI( NULL, iValue2 );
         hb_vmEvalBlockV( s_s->listBlock.at( i - 1 ), 3, pObject, pValue1, pValue2 );
         hb_itemRelease( pObject );
         hb_itemRelease( pValue1 );
         hb_itemRelease( pValue2 );
         hb_vmRequestRestore();
      }
   }
}

static void hbqt_SlotsExecIntIntInt( QObject * object, const char * event, int iValue1, int iValue2, int iValue3 )
{
   if( object )
   {
      Slots * s_s = qt_getEventSlots();
      int i = object->property( event ).toInt();
      if( i > 0 && i <= s_s->listBlock.size() && hb_vmRequestReenter() )
      {
         PHB_ITEM pObject = hb_itemPutPtr( NULL, object );
         PHB_ITEM pValue1 = hb_itemPutNI( NULL, iValue1 );
         PHB_ITEM pValue2 = hb_itemPutNI( NULL, iValue2 );
         PHB_ITEM pValue3 = hb_itemPutNI( NULL, iValue3 );
         hb_vmEvalBlockV( s_s->listBlock.at( i - 1 ), 4, pObject, pValue1, pValue2, pValue3 );
         hb_itemRelease( pObject );
         hb_itemRelease( pValue1 );
         hb_itemRelease( pValue2 );
         hb_itemRelease( pValue3 );
         hb_vmRequestRestore();
      }
   }
}

static void hbqt_SlotsExecIntIntIntInt( QObject * object, const char * event, int iValue1, int iValue2, int iValue3, int iValue4 )
{
   if( object )
   {
      Slots * s_s = qt_getEventSlots();
      int i = object->property( event ).toInt();
      if( i > 0 && i <= s_s->listBlock.size() && hb_vmRequestReenter() )
      {
         PHB_ITEM pObject = hb_itemPutPtr( NULL, object );
         PHB_ITEM pValue1 = hb_itemPutNI( NULL, iValue1 );
         PHB_ITEM pValue2 = hb_itemPutNI( NULL, iValue2 );
         PHB_ITEM pValue3 = hb_itemPutNI( NULL, iValue3 );
         PHB_ITEM pValue4 = hb_itemPutNI( NULL, iValue4 );
         hb_vmEvalBlockV( s_s->listBlock.at( i - 1 ), 5, pObject, pValue1, pValue2, pValue3, pValue4 );
         hb_itemRelease( pObject );
         hb_itemRelease( pValue1 );
         hb_itemRelease( pValue2 );
         hb_itemRelease( pValue3 );
         hb_itemRelease( pValue4 );
         hb_vmRequestRestore();
      }
   }
}

static void hbqt_SlotsExecString( QObject * object, const char * event, const QString & string )
{
   if( object )
   {
      Slots * s_s = qt_getEventSlots();
      int i = object->property( event ).toInt();
      if( i > 0 && i <= s_s->listBlock.size() && hb_vmRequestReenter() )
      {
         PHB_ITEM pObject = hb_itemPutPtr( NULL, object );
         PHB_ITEM pString = hb_itemPutC( NULL, string.toAscii().data() );
         hb_vmEvalBlockV( s_s->listBlock.at( i - 1 ), 2, pObject, pString );
         hb_itemRelease( pObject );
         hb_itemRelease( pString );
         hb_vmRequestRestore();
      }
   }
}

static void hbqt_SlotsExecModel( QObject * object, const char * event, const QModelIndex & index )
{
   if( object )
   {
      Slots * s_s = qt_getEventSlots();
      int i = object->property( event ).toInt();
      if( i > 0 && i <= s_s->listBlock.size() && hb_vmRequestReenter() )
      {
         PHB_ITEM pObject = hb_itemPutPtr( NULL, object );
         PHB_ITEM pState = hb_itemPutPtr( NULL, ( QModelIndex * ) new QModelIndex( index ) );
         hb_vmEvalBlockV( s_s->listBlock.at( i - 1 ), 2, pObject, pState );
         hb_itemRelease( pObject );
         delete ( ( QModelIndex * ) hb_itemGetPtr( pState ) );
         hb_itemRelease( pState );
         hb_vmRequestRestore();
      }
   }
}

static void hbqt_SlotsExecTextCharFormat( QObject * object, const char * event, const QTextCharFormat & f )
{
   if( object )
   {
      Slots * s_s = qt_getEventSlots();
      int i = object->property( event ).toInt();
      if( i > 0 && i <= s_s->listBlock.size() && hb_vmRequestReenter() )
      {
         PHB_ITEM pObject = hb_itemPutPtr( NULL, object );
         PHB_ITEM p1 = hb_itemPutPtr( NULL, new QTextCharFormat( f ) );
         hb_vmEvalBlockV( s_s->listBlock.at( i - 1 ), 2, pObject, p1 );
         hb_itemRelease( pObject );
         delete ( ( QTextCharFormat * ) hb_itemGetPtr( p1 ) );
         hb_itemRelease( p1 );
         hb_vmRequestRestore();
      }
   }
}

static void hbqt_SlotsExecFont( QObject * object, const char * event, const QFont & font )
{
   if( object )
   {
      Slots * s_s = qt_getEventSlots();
      int i = object->property( event ).toInt();
      if( i > 0 && i <= s_s->listBlock.size() && hb_vmRequestReenter() )
      {
         PHB_ITEM pObject = hb_itemPutPtr( NULL, object );
         PHB_ITEM p1 = hb_itemPutPtr( NULL, new QFont( font ) );
         hb_vmEvalBlockV( s_s->listBlock.at( i - 1 ), 2, pObject, p1 );
         hb_itemRelease( pObject );
         delete ( ( QFont * ) hb_itemGetPtr( p1 ) );
         hb_itemRelease( p1 );
         hb_vmRequestRestore();
      }
   }
}

static void hbqt_SlotsExecStringList( QObject * object, const char * event, const QStringList & stringList )
{
   if( object )
   {
      Slots * s_s = qt_getEventSlots();
      int i = object->property( event ).toInt();
      if( i > 0 && i <= s_s->listBlock.size() && hb_vmRequestReenter() )
      {
         PHB_ITEM pObject = hb_itemPutPtr( NULL, object );
         PHB_ITEM p1 = hb_itemPutPtr( NULL, new QStringList( stringList ) );
         hb_vmEvalBlockV( s_s->listBlock.at( i - 1 ), 2, pObject, p1 );
         hb_itemRelease( pObject );
         delete ( ( QStringList * ) hb_itemGetPtr( p1 ) );
         hb_itemRelease( p1 );
         hb_vmRequestRestore();
      }
   }
}

static void hbqt_SlotsExecPointer( QObject * object, const char * event, void * p1 )
{
   if( object )
   {
      Slots * s_s = qt_getEventSlots();
      int i = object->property( event ).toInt();
      if( i > 0 && i <= s_s->listBlock.size() && hb_vmRequestReenter() )
      {
         PHB_ITEM pObject = hb_itemPutPtr( NULL, object );
         PHB_ITEM pP1 = hb_itemPutPtr( NULL, p1 );
         hb_vmEvalBlockV( s_s->listBlock.at( i - 1 ), 2, pObject, pP1 );
         hb_itemRelease( pObject );
         hb_itemRelease( pP1 );
         hb_vmRequestRestore();
      }
   }
}

static void hbqt_SlotsExecPointerInt( QObject * object, const char * event, void * p1, int iInt )
{
   if( object )
   {
      Slots * s_s = qt_getEventSlots();
      int i = object->property( event ).toInt();
      if( i > 0 && i <= s_s->listBlock.size() && hb_vmRequestReenter() )
      {
         PHB_ITEM pObject = hb_itemPutPtr( NULL, object );
         PHB_ITEM pP1 = hb_itemPutPtr( NULL, p1 );
         PHB_ITEM pI1 = hb_itemPutNI( NULL, iInt );
         hb_vmEvalBlockV( s_s->listBlock.at( i - 1 ), 3, pObject, pP1, pI1 );
         hb_itemRelease( pObject );
         hb_itemRelease( pP1 );
         hb_itemRelease( pI1 );
         hb_vmRequestRestore();
      }
   }
}

static void hbqt_SlotsExecPointerPointer( QObject * object, const char * event, void * p1, void * p2 )
{
   if( object )
   {
      Slots * s_s = qt_getEventSlots();
      int i = object->property( event ).toInt();
      if( i > 0 && i <= s_s->listBlock.size() && hb_vmRequestReenter() )
      {
         PHB_ITEM pObject = hb_itemPutPtr( NULL, object );
         PHB_ITEM pP1 = hb_itemPutPtr( NULL, p1 );
         PHB_ITEM pP2 = hb_itemPutPtr( NULL, p2 );
         hb_vmEvalBlockV( s_s->listBlock.at( i - 1 ), 3, pObject, pP1, pP2 );
         hb_itemRelease( pObject );
         hb_itemRelease( pP1 );
         hb_itemRelease( pP2 );
         hb_vmRequestRestore();
      }
   }
}

/* Generic Key and Mouse Events emitted by subclass objects */
void Slots::keyPressEvent( QKeyEvent * event )
{
   QObject *object = qobject_cast<QObject *>( sender() );
   hbqt_SlotsExecPointer( object, "keyPressEvent()", event );
}

void Slots::keyReleaseEvent( QKeyEvent * event )
{
   QObject *object = qobject_cast<QObject *>( sender() );
   hbqt_SlotsExecPointer( object, "keyReleaseEvent()", event );
}

void Slots::mouseMoveEvent( QMouseEvent * event )
{
   QObject *object = qobject_cast<QObject *>( sender() );
   hbqt_SlotsExecPointer( object, "mouseMoveEvent()", event );
}

void Slots::mouseDoubleClickEvent( QMouseEvent * event )
{
   QObject *object = qobject_cast<QObject *>( sender() );
   hbqt_SlotsExecPointer( object, "mouseDoubleClickEvent()", event );
}

void Slots::mousePressEvent( QMouseEvent * event )
{
   QObject *object = qobject_cast<QObject *>( sender() );
   hbqt_SlotsExecPointer( object, "mousePressEvent()", event );
}

void Slots::mouseReleaseEvent( QMouseEvent * event )
{
   QObject *object = qobject_cast<QObject *>( sender() );
   hbqt_SlotsExecPointer( object, "mouseReleaseEvent()", event );
}

void Slots::wheelEvent( QWheelEvent * event )
{
   QObject *object = qobject_cast<QObject *>( sender() );
   hbqt_SlotsExecPointer( object, "wheelEvent()", event );
}

void Slots::resizeEvent( QResizeEvent * event )
{
   QObject *object = qobject_cast<QObject *>( sender() );
   hbqt_SlotsExecPointer( object, "resizeEvent()", event );
}

/* ........................................................ */

void Slots::triggered( bool checked )
{
   QObject *object = qobject_cast<QObject *>( sender() );
   hbqt_SlotsExecBool( object, "triggered(bool)", checked );
}

void Slots::hovered( QAction * action )
{
   QObject *object = qobject_cast<QObject *>( sender() );
   hbqt_SlotsExecPointer( object, "hovered(action)", action );
}

void Slots::clicked()
{
   QObject *object = qobject_cast<QObject *>( sender() );
   hbqt_SlotsExec( object, "clicked()" );
}

void Slots::returnPressed()
{
   QObject *object = qobject_cast<QObject *>( sender() );
   hbqt_SlotsExec( object, "returnPressed()" );
}

void Slots::viewportEntered()
{
   QObject *object = qobject_cast<QObject *>( sender() );
   hbqt_SlotsExec( object, "viewportEntered()" );
}

void Slots::pressed()
{
   QObject *object = qobject_cast<QObject *>( sender() );
   hbqt_SlotsExec( object, "pressed()" );
}

void Slots::released()
{
   QObject *object = qobject_cast<QObject *>( sender() );
   hbqt_SlotsExec( object, "released()" );
}

void Slots::triggered()
{
   QObject *object = qobject_cast<QObject *>( sender() );
   hbqt_SlotsExec( object, "triggered()" );
}

void Slots::hovered()
{
   QAction *object = qobject_cast<QAction *>( sender() );
   hbqt_SlotsExec( object, "hovered()" );
}

void Slots::stateChanged( int state )
{
   QObject *object = qobject_cast<QObject *>( sender() );
   hbqt_SlotsExecInt( object, "stateChanged(int)", state );
}

void Slots::activated( int index )
{
   QObject *object = qobject_cast<QObject *>( sender() );
   hbqt_SlotsExecInt( object, "activated(int)", index );
}

void Slots::currentIndexChanged( int index )
{
   QObject *object = qobject_cast<QObject *>( sender() );
   hbqt_SlotsExecInt( object, "currentIndexChanged(int)", index );
}

void Slots::currentChanged( int index )
{
   QObject *object = qobject_cast<QObject *>( sender() );
   hbqt_SlotsExecInt( object, "currentChanged(int)", index );
}

void Slots::highlighted( int index )
{
   QObject *object = qobject_cast<QObject *>( sender() );
   hbqt_SlotsExecInt( object, "highlighted(int)", index );
}

void Slots::clicked( const QModelIndex & index )
{
   QObject *object = qobject_cast<QObject *>( sender() );
   hbqt_SlotsExecModel( object, "clicked(QModelIndex)", index );
}

void Slots::doubleClicked( const QModelIndex & index )
{
   QObject *object = qobject_cast<QObject *>( sender() );
   hbqt_SlotsExecModel( object, "doubleClicked(QModelIndex)", index );
}

void Slots::entered( const QModelIndex & index )
{
   QObject *object = qobject_cast<QObject *>( sender() );
   hbqt_SlotsExecModel( object, "entered(QModelIndex)", index );
}

void Slots::actionTriggered( int action )
{
   QObject *object = qobject_cast<QObject *>( sender() );
   hbqt_SlotsExecInt( object, "actionTriggered(int)", action );
}

void Slots::rangeChanged( int min, int max )
{
   QObject *object = qobject_cast<QObject *>( sender() );
   hbqt_SlotsExecIntInt( object, "rangeChanged(int)", min, max );
}

void Slots::sliderMoved( int value )
{
   QObject *object = qobject_cast<QObject *>( sender() );
   hbqt_SlotsExecInt( object, "sliderMoved(int)", value );
}

void Slots::sliderPressed()
{
   QObject *object = qobject_cast<QObject *>( sender() );
   hbqt_SlotsExec( object, "sliderPressed()" );
}

void Slots::sliderReleased()
{
   QObject *object = qobject_cast<QObject *>( sender() );
   hbqt_SlotsExec( object, "sliderReleased()" );
}

void Slots::valueChanged( int value )
{
   QObject *object = qobject_cast<QObject *>( sender() );
   hbqt_SlotsExecInt( object, "valueChanged(int)", value );
}

void Slots::cursorPositionChanged( int iOld, int iNew )
{
   QObject *object = qobject_cast<QObject *>( sender() );
   hbqt_SlotsExecIntInt( object, "cursorPositionChanged(int,int)", iOld, iNew );
}

void Slots::editingFinished()
{
   QObject *object = qobject_cast<QObject *>( sender() );
   hbqt_SlotsExec( object, "editingFinished()" );
}

void Slots::selectionChanged()
{
   QObject *object = qobject_cast<QObject *>( sender() );
   hbqt_SlotsExec( object, "selectionChanged()" );
}

void Slots::textChanged( const QString & text )
{
   QObject *object = qobject_cast<QObject *>( sender() );
   hbqt_SlotsExecString( object, "textChanged(QString)", text );
}

void Slots::textEdited( const QString & text )
{
   QObject *object = qobject_cast<QObject *>( sender() );
   hbqt_SlotsExecString( object, "textEdited(QString)", text );
}

/*  TreeViewobject */
void Slots::currentItemChanged( QTreeWidgetItem * current, QTreeWidgetItem * previous )
{
   QObject *object = qobject_cast<QObject *>( sender() );
   hbqt_SlotsExecPointerPointer( object, "currentItemChanged(QTWItem)", current, previous );
}

void Slots::itemActivated( QTreeWidgetItem * item, int column )
{
   QObject *object = qobject_cast<QObject *>( sender() );
   hbqt_SlotsExecPointerInt( object, "itemActivated(QTWItem)", item, column );
}

void Slots::itemChanged( QTreeWidgetItem * item, int column )
{
   QObject *object = qobject_cast<QObject *>( sender() );
   hbqt_SlotsExecPointerInt( object, "itemChanged(QTWItem)", item, column );
}

void Slots::itemClicked( QTreeWidgetItem * item, int column )
{
   QObject *object = qobject_cast<QObject *>( sender() );
   hbqt_SlotsExecPointerInt( object, "itemClicked(QTWItem)", item, column );
}

void Slots::itemDoubleClicked( QTreeWidgetItem * item, int column )
{
   QObject *object = qobject_cast<QObject *>( sender() );
   hbqt_SlotsExecPointerInt( object, "itemDoubleClicked(QTWItem)", item, column );
}

void Slots::itemEntered( QTreeWidgetItem * item, int column )
{
   QObject *object = qobject_cast<QObject *>( sender() );
   hbqt_SlotsExecPointerInt( object, "itemEntered(QTWItem)", item, column );
}

void Slots::itemPressed( QTreeWidgetItem * item, int column )
{
   QObject *object = qobject_cast<QObject *>( sender() );
   hbqt_SlotsExecPointerInt( object, "itemPressed(QTWItem)", item, column );
}

void Slots::itemExpanded( QTreeWidgetItem * item )
{
   QObject *object = qobject_cast<QObject *>( sender() );
   hbqt_SlotsExecPointer( object, "itemExpanded(QTWItem)", item );
}

void Slots::itemCollapsed( QTreeWidgetItem * item )
{
   QObject *object = qobject_cast<QObject *>( sender() );
   hbqt_SlotsExecPointer( object, "itemCollapsed(QTWItem)", item );
}

void Slots::itemSelectionChanged()
{
   QObject *object = qobject_cast<QObject *>( sender() );
   hbqt_SlotsExec( object, "itemSelectionChanged()" );
}

/* QDialog (s)*/
void Slots::currentFontChanged( const QFont & font )
{
   QObject *object = qobject_cast<QObject *>( sender() );
   hbqt_SlotsExecFont( object, "currentFontChanged(QFont)", font );
}

void Slots::fontSelected( const QFont & font )
{
   QObject *object = qobject_cast<QObject *>( sender() );
   hbqt_SlotsExecFont( object, "fontSelected(QFont)", font );
}

void Slots::accepted()
{
   QObject *object = qobject_cast<QObject *>( sender() );
   hbqt_SlotsExec( object, "accepted()" );
}

void Slots::finished( int result )
{
   QObject *object = qobject_cast<QObject *>( sender() );
   hbqt_SlotsExecInt( object, "finished(int)", result );
}

void Slots::rejected()
{
   QObject *object = qobject_cast<QObject *>( sender() );
   hbqt_SlotsExec( object, "rejected()" );
}

void Slots::currentChanged( const QString & path )
{
   QObject *object = qobject_cast<QObject *>( sender() );
   hbqt_SlotsExecString( object, "currentChanged(QString)", path );
}

void Slots::directoryEntered( const QString & directory )
{
   QObject *object = qobject_cast<QObject *>( sender() );
   hbqt_SlotsExecString( object, "directoryEntered(QString)", directory );
}

void Slots::fileSelected( const QString & file )
{
   QObject *object = qobject_cast<QObject *>( sender() );
   hbqt_SlotsExecString( object, "fileSelected(QString)", file );
}

void Slots::filesSelected( const QStringList & selected )
{
   QObject *object = qobject_cast<QObject *>( sender() );
   hbqt_SlotsExecStringList( object, "filesSelected(QStringList)", selected );
}

void Slots::filterSelected( const QString & filter )
{
   QObject *object = qobject_cast<QObject *>( sender() );
   hbqt_SlotsExecString( object, "filterSelected(QString)", filter );
}

/* QPrintDialog */
void Slots::accepted( QPrinter * printer )
{
   QObject *object = qobject_cast<QObject *>( sender() );
   hbqt_SlotsExecPointer( object, "accepted(QPrinter)", printer );
}

/* QTextEdit */
void Slots::copyAvailable( bool yes )
{
   QObject *object = qobject_cast<QObject *>( sender() );
   hbqt_SlotsExecBool( object, "copyAvailable(bool)", yes );
}

void Slots::currentCharFormatChanged( const QTextCharFormat & f )
{
   QObject *object = qobject_cast<QObject *>( sender() );
   hbqt_SlotsExecTextCharFormat( object, "currentCharFormatChanged(QTextCharFormat)", f );
}

void Slots::cursorPositionChanged()
{
   QObject *object = qobject_cast<QObject *>( sender() );
   hbqt_SlotsExec( object, "cursorPositionChanged()" );
}

void Slots::redoAvailable( bool available )
{
   QObject *object = qobject_cast<QObject *>( sender() );
   hbqt_SlotsExecBool( object, "redoAvailable(bool)", available );
}

void Slots::textChanged()
{
   QObject *object = qobject_cast<QObject *>( sender() );
   hbqt_SlotsExec( object, "textChanged()" );
}

void Slots::undoAvailable( bool available )
{
   QObject *object = qobject_cast<QObject *>( sender() );
   hbqt_SlotsExecBool( object, "undoAvailable(available)", available );
}

void Slots::timeout()
{
   QObject *object = qobject_cast<QObject *>( sender() );
   hbqt_SlotsExec( object, "timeout()" );
}

void Slots::scrollContentsBy( int x, int y )
{
   QObject *object = qobject_cast<QObject *>( sender() );
   hbqt_SlotsExecIntInt( object, "scrollContentsBy(int,int)", x, y );
}

void Slots::geometriesChanged()
{
   QObject *object = qobject_cast<QObject *>( sender() );
   hbqt_SlotsExec( object, "geometriesChanged()" );
}

void Slots::sectionAutoResize( int logicalIndex, QHeaderView::ResizeMode mode )
{
   QObject *object = qobject_cast<QObject *>( sender() );
   hbqt_SlotsExecIntInt( object, "sectionAutoResize(int,int)", logicalIndex, mode );
}

void Slots::sectionClicked( int logicalIndex )
{
   QObject *object = qobject_cast<QObject *>( sender() );
   hbqt_SlotsExecInt( object, "sectionClicked(int)", logicalIndex );
}

void Slots::sectionCountChanged( int oldCount, int newCount )
{
   QObject *object = qobject_cast<QObject *>( sender() );
   hbqt_SlotsExecIntInt( object, "sectionCountChanged(int,int)", oldCount, newCount );
}

void Slots::sectionDoubleClicked( int logicalIndex )
{
   QObject *object = qobject_cast<QObject *>( sender() );
   hbqt_SlotsExecInt( object, "sectionDoubleClicked(int)", logicalIndex );
}

void Slots::sectionEntered( int logicalIndex )
{
   QObject *object = qobject_cast<QObject *>( sender() );
   hbqt_SlotsExecInt( object, "sectionEntered(int)", logicalIndex );
}

void Slots::sectionHandleDoubleClicked( int logicalIndex )
{
   QObject *object = qobject_cast<QObject *>( sender() );
   hbqt_SlotsExecInt( object, "sectionHandleDoubleClicked(int)", logicalIndex );
}

void Slots::sectionMoved( int logicalIndex, int oldVisualIndex, int newVisualIndex )
{
   QObject *object = qobject_cast<QObject *>( sender() );
   hbqt_SlotsExecIntIntInt( object, "sectionMoved(int,int,int)", logicalIndex, oldVisualIndex, newVisualIndex );
}

void Slots::sectionPressed( int logicalIndex )
{
   QObject *object = qobject_cast<QObject *>( sender() );
   hbqt_SlotsExecInt( object, "sectionPressed(int)", logicalIndex );
}

void Slots::sectionResized( int logicalIndex, int oldSize, int newSize )
{
   QObject *object = qobject_cast<QObject *>( sender() );
   hbqt_SlotsExecIntIntInt( object, "sectionResized(int,int,int)", logicalIndex, oldSize, newSize );
}

void Slots::sortIndicatorChanged( int logicalIndex, Qt::SortOrder order )
{
   QObject *object = qobject_cast<QObject *>( sender() );
   hbqt_SlotsExecIntInt( object, "sortIndicatorChanged(int,int)", logicalIndex, order );
}

void Slots::buttonClicked( int id )
{
   QObject *object = qobject_cast<QObject *>( sender() );
   hbqt_SlotsExecInt( object, "buttonClicked(int)", id );
}

void Slots::buttonPressed( int id )
{
   QObject *object = qobject_cast<QObject *>( sender() );
   hbqt_SlotsExecInt( object, "buttonPressed(int)", id );
}

void Slots::buttonReleased( int id )
{
   QObject *object = qobject_cast<QObject *>( sender() );
   hbqt_SlotsExecInt( object, "buttonReleased(int)", id );
}

void Slots::linkActivated( const QString & link )
{
   QObject *object = qobject_cast<QObject *>( sender() );
   hbqt_SlotsExecString( object, "linkActivated(QString)", link );
}

void Slots::linkHovered( const QString & link )
{
   QObject *object = qobject_cast<QObject *>( sender() );
   hbqt_SlotsExecString( object, "linkHovered(QString)", link );
}

void Slots::cellActivated( int row, int column )
{
   QObject *object = qobject_cast<QObject *>( sender() );
   hbqt_SlotsExecIntInt( object, "cellActivated(int,int)", row, column );
}

void Slots::cellChanged( int row, int column )
{
   QObject *object = qobject_cast<QObject *>( sender() );
   hbqt_SlotsExecIntInt( object, "cellChanged(int,int)", row, column );
}

void Slots::cellClicked( int row, int column )
{
   QObject *object = qobject_cast<QObject *>( sender() );
   hbqt_SlotsExecIntInt( object, "cellClicked(int,int)", row, column );
}

void Slots::cellDoubleClicked( int row, int column )
{
   QObject *object = qobject_cast<QObject *>( sender() );
   hbqt_SlotsExecIntInt( object, "cellDoubleClicked(int,int)", row, column );
}

void Slots::cellEntered( int row, int column )
{
   QObject *object = qobject_cast<QObject *>( sender() );
   hbqt_SlotsExecIntInt( object, "cellEntered(int,int)", row, column );
}

void Slots::cellPressed( int row, int column )
{
   QObject *object = qobject_cast<QObject *>( sender() );
   hbqt_SlotsExecIntInt( object, "cellEntered(int,int)", row, column );
}

void Slots::currentCellChanged( int currentRow, int currentColumn, int previousRow, int previousColumn )
{
   QObject *object = qobject_cast<QObject *>( sender() );
   hbqt_SlotsExecIntIntIntInt( object, "currentCellChanged(int,int,int,int)", currentRow, currentColumn, previousRow, previousColumn );
}

void Slots::tabCloseRequested( int index )
{
   QObject *object = qobject_cast<QObject *>( sender() );
   hbqt_SlotsExecInt( object, "tabCloseRequested(int)", index );
}

void Slots::paintRequested( QPrinter * printer )
{
   QObject *object = qobject_cast<QObject *>( sender() );
   hbqt_SlotsExecPointer( object, "paintRequested(QPrinter)", printer );
}

/*----------------------------------------------------------------------*/
/*
 * harbour function to connect signals with slots
 */
HB_FUNC( QT_CONNECT_SIGNAL )
{
   QObject * object    = ( QObject * ) hbqt_gcpointer( 1 );         /* get sender    */

   if( object == NULL )
   {
       hb_retl( HB_FALSE );
       return;
   }

   QString   signal    = hb_parcx( 2 );                             /* get signal    */
   PHB_ITEM  codeblock = hb_itemNew( hb_param( 3, HB_IT_BLOCK ) );  /* get codeblock */
   bool      ret       = false;                                     /* return value  */

   qt_setEventSlots();
   Slots   * s_s       = qt_getEventSlots();

   if(      signal == ( QString ) "clicked()" )                                 ret = object->connect( object, SIGNAL( clicked() )                                                 , s_s, SLOT( clicked() )                                                 , Qt::AutoConnection );
   else if( signal == ( QString ) "returnPressed()" )                           ret = object->connect( object, SIGNAL( returnPressed() )                                           , s_s, SLOT( returnPressed() )                                           , Qt::AutoConnection );
   else if( signal == ( QString ) "triggered()" )                               ret = object->connect( object, SIGNAL( triggered() )                                               , s_s, SLOT( triggered() )                                               , Qt::AutoConnection );
   else if( signal == ( QString ) "hovered()" )                                 ret = object->connect( object, SIGNAL( hovered() )                                                 , s_s, SLOT( hovered() )                                                 , Qt::AutoConnection );
   else if( signal == ( QString ) "viewportEntered()" )                         ret = object->connect( object, SIGNAL( viewportEntered() )                                         , s_s, SLOT( viewportEntered() )                                         , Qt::AutoConnection );
   else if( signal == ( QString ) "pressed()" )                                 ret = object->connect( object, SIGNAL( pressed() )                                                 , s_s, SLOT( pressed() )                                                 , Qt::AutoConnection );
   else if( signal == ( QString ) "released()" )                                ret = object->connect( object, SIGNAL( released() )                                                , s_s, SLOT( released() )                                                , Qt::AutoConnection );
   else if( signal == ( QString ) "stateChanged(int)" )                         ret = object->connect( object, SIGNAL( stateChanged( int ) )                                       , s_s, SLOT( stateChanged( int ) )                                       , Qt::AutoConnection );
   else if( signal == ( QString ) "activated(int)" )                            ret = object->connect( object, SIGNAL( activated( int ) )                                          , s_s, SLOT( activated( int ) )                                          , Qt::AutoConnection );
   else if( signal == ( QString ) "currentIndexChanged(int)" )                  ret = object->connect( object, SIGNAL( currentIndexChanged( int ) )                                , s_s, SLOT( currentIndexChanged( int ) )                                , Qt::AutoConnection );
   else if( signal == ( QString ) "highlighted(int)" )                          ret = object->connect( object, SIGNAL( highlighted( int ) )                                        , s_s, SLOT( highlighted( int ) )                                        , Qt::AutoConnection );
   else if( signal == ( QString ) "triggered(bool)" )                           ret = object->connect( object, SIGNAL( triggered( bool ) )                                         , s_s, SLOT( triggered( bool ) )                                         , Qt::AutoConnection );
   else if( signal == ( QString ) "clicked(QModelIndex)" )                      ret = object->connect( object, SIGNAL( clicked( const QModelIndex & ) )                            , s_s, SLOT( clicked( const QModelIndex & ) )                            , Qt::AutoConnection );
   else if( signal == ( QString ) "doubleClicked(QModelIndex)" )                ret = object->connect( object, SIGNAL( doubleClicked( const QModelIndex & ) )                      , s_s, SLOT( doubleClicked( const QModelIndex & ) )                      , Qt::AutoConnection );
   else if( signal == ( QString ) "entered(QModelIndex)" )                      ret = object->connect( object, SIGNAL( entered( const QModelIndex & ) )                            , s_s, SLOT( entered( const QModelIndex & ) )                            , Qt::AutoConnection );
   else if( signal == ( QString ) "hovered(action)" )                           ret = object->connect( object, SIGNAL( hovered( QAction * ) )                                      , s_s, SLOT( hovered( QAction * ) )                                      , Qt::AutoConnection );
   else if( signal == ( QString ) "currentChanged(int)" )                       ret = object->connect( object, SIGNAL( currentChanged( int ) )                                     , s_s, SLOT( currentChanged( int ) )                                     , Qt::AutoConnection );
   else if( signal == ( QString ) "actionTriggered(int)" )                      ret = object->connect( object, SIGNAL( actionTriggered(int) )                                      , s_s, SLOT( actionTriggered(int) )                                      , Qt::AutoConnection );
   else if( signal == ( QString ) "rangeChanged(int,int)" )                     ret = object->connect( object, SIGNAL( rangeChanged(int,int) )                                     , s_s, SLOT( rangeChanged(int,int) )                                     , Qt::AutoConnection );
   else if( signal == ( QString ) "sliderMoved(int)" )                          ret = object->connect( object, SIGNAL( sliderMoved(int) )                                          , s_s, SLOT( sliderMoved(int) )                                          , Qt::AutoConnection );
   else if( signal == ( QString ) "sliderPressed()" )                           ret = object->connect( object, SIGNAL( sliderPressed() )                                           , s_s, SLOT( sliderPressed() )                                           , Qt::AutoConnection );
   else if( signal == ( QString ) "sliderReleased()" )                          ret = object->connect( object, SIGNAL( sliderReleased() )                                          , s_s, SLOT( sliderReleased() )                                          , Qt::AutoConnection );
   else if( signal == ( QString ) "valueChanged(int)" )                         ret = object->connect( object, SIGNAL( valueChanged(int) )                                         , s_s, SLOT( valueChanged(int) )                                         , Qt::AutoConnection );
   else if( signal == ( QString ) "cursorPositionChanged(int,int)" )            ret = object->connect( object, SIGNAL( cursorPositionChanged(int,int) )                            , s_s, SLOT( cursorPositionChanged(int,int) )                            , Qt::AutoConnection );
   else if( signal == ( QString ) "editingFinished()" )                         ret = object->connect( object, SIGNAL( editingFinished() )                                         , s_s, SLOT( editingFinished() )                                         , Qt::AutoConnection );
   else if( signal == ( QString ) "returnPressed()" )                           ret = object->connect( object, SIGNAL( returnPressed() )                                           , s_s, SLOT( returnPressed() )                                           , Qt::AutoConnection );
   else if( signal == ( QString ) "selectionChanged()" )                        ret = object->connect( object, SIGNAL( selectionChanged() )                                        , s_s, SLOT( selectionChanged() )                                        , Qt::AutoConnection );
   else if( signal == ( QString ) "textChanged(QString)" )                      ret = object->connect( object, SIGNAL( textChanged( const QString &) )                             , s_s, SLOT( textChanged( const QString & ) )                            , Qt::AutoConnection );
   else if( signal == ( QString ) "textEdited(QString)" )                       ret = object->connect( object, SIGNAL( textEdited( const QString &) )                              , s_s, SLOT( textEdited( const QString & ) )                             , Qt::AutoConnection );
   /* QTreeViewWidget */
   else if( signal == ( QString ) "currentItemChanged(QTWItem)" )               ret = object->connect( object, SIGNAL( currentItemChanged( QTreeWidgetItem *, QTreeWidgetItem * ) ), s_s, SLOT( currentItemChanged( QTreeWidgetItem *, QTreeWidgetItem * ) ), Qt::AutoConnection );
   else if( signal == ( QString ) "itemActivated(QTWItem)" )                    ret = object->connect( object, SIGNAL( itemActivated( QTreeWidgetItem *, int ) )                   , s_s, SLOT( itemActivated( QTreeWidgetItem *, int ) )                   , Qt::AutoConnection );
   else if( signal == ( QString ) "itemChanged(QTWItem)" )                      ret = object->connect( object, SIGNAL( itemChanged( QTreeWidgetItem *, int ) )                     , s_s, SLOT( itemChanged( QTreeWidgetItem *, int ) )                     , Qt::AutoConnection );
   else if( signal == ( QString ) "itemClicked(QTWItem)" )                      ret = object->connect( object, SIGNAL( itemClicked( QTreeWidgetItem *, int ) )                     , s_s, SLOT( itemClicked( QTreeWidgetItem *, int ) )                     , Qt::AutoConnection );
   else if( signal == ( QString ) "itemCollapsed(QTWItem)" )                    ret = object->connect( object, SIGNAL( itemCollapsed( QTreeWidgetItem * ) )                        , s_s, SLOT( itemCollapsed( QTreeWidgetItem * ) )                        , Qt::AutoConnection );
   else if( signal == ( QString ) "itemDoubleClicked(QTWItem)" )                ret = object->connect( object, SIGNAL( itemDoubleClicked( QTreeWidgetItem *, int ) )               , s_s, SLOT( itemDoubleClicked( QTreeWidgetItem *, int ) )               , Qt::AutoConnection );
   else if( signal == ( QString ) "itemEntered(QTWItem)" )                      ret = object->connect( object, SIGNAL( itemEntered( QTreeWidgetItem *, int ) )                     , s_s, SLOT( itemEntered( QTreeWidgetItem *, int ) )                     , Qt::AutoConnection );
   else if( signal == ( QString ) "itemExpanded(QTWItem)" )                     ret = object->connect( object, SIGNAL( itemExpanded( QTreeWidgetItem * ) )                         , s_s, SLOT( itemExpanded( QTreeWidgetItem * ) )                         , Qt::AutoConnection );
   else if( signal == ( QString ) "itemPressed(QTWItem)" )                      ret = object->connect( object, SIGNAL( itemPressed( QTreeWidgetItem *, int ) )                     , s_s, SLOT( itemPressed( QTreeWidgetItem *, int ) )                     , Qt::AutoConnection );
   else if( signal == ( QString ) "itemSelectionChanged()" )                    ret = object->connect( object, SIGNAL( itemSelectionChanged() )                                    , s_s, SLOT( itemSelectionChanged() )                                    , Qt::AutoConnection );
   /* QDialog (s) QFontDialog, QFileDialog */
   else if( signal == ( QString ) "currentFontChanged(QFont)" )                 ret = object->connect( object, SIGNAL( currentFontChanged( const QFont & ) )                       , s_s, SLOT( currentFontChanged( const QFont & ) )                       , Qt::AutoConnection );
   else if( signal == ( QString ) "fontSelected(QFont)" )                       ret = object->connect( object, SIGNAL( fontSelected( const QFont & ) )                             , s_s, SLOT( fontSelected( const QFont & ) )                             , Qt::AutoConnection );
   else if( signal == ( QString ) "accepted()" )                                ret = object->connect( object, SIGNAL( accepted() )                                                , s_s, SLOT( accepted() )                                                , Qt::AutoConnection );
   else if( signal == ( QString ) "finished(int)" )                             ret = object->connect( object, SIGNAL( finished( int ) )                                           , s_s, SLOT( finished( int ) )                                           , Qt::AutoConnection );
   else if( signal == ( QString ) "rejected()" )                                ret = object->connect( object, SIGNAL( rejected() )                                                , s_s, SLOT( rejected() )                                                , Qt::AutoConnection );
   else if( signal == ( QString ) "currentChanged(QString)" )                   ret = object->connect( object, SIGNAL( currentChanged( const QString & ) )                         , s_s, SLOT( currentChanged( const QString & ) )                         , Qt::AutoConnection );
   else if( signal == ( QString ) "directoryEntered(QString)" )                 ret = object->connect( object, SIGNAL( directoryEntered( const QString & ) )                       , s_s, SLOT( directoryEntered( const QString & ) )                       , Qt::AutoConnection );
   else if( signal == ( QString ) "fileSelected(QString)" )                     ret = object->connect( object, SIGNAL( fileSelected( const QString & ) )                           , s_s, SLOT( fileSelected( const QString & ) )                           , Qt::AutoConnection );
   else if( signal == ( QString ) "filesSelected(QStringList)" )                ret = object->connect( object, SIGNAL( filesSelected( const QStringList & ) )                      , s_s, SLOT( filesSelected( const QStringList & ) )                      , Qt::AutoConnection );
   else if( signal == ( QString ) "filterSelected(QString)" )                   ret = object->connect( object, SIGNAL( filterSelected( const QString & ) )                         , s_s, SLOT( filterSelected( const QString & ) )                         , Qt::AutoConnection );
   else if( signal == ( QString ) "accepted(QPrinter)" )                        ret = object->connect( object, SIGNAL( accepted( QPrinter * ) )                                    , s_s, SLOT( accepted( QPrinter * ) )                                    , Qt::AutoConnection );
   else if( signal == ( QString ) "copyAvailable(bool)" )                       ret = object->connect( object, SIGNAL( copyAvailable( bool ) )                                     , s_s, SLOT( copyAvailable( bool ) )                                     , Qt::AutoConnection );
   else if( signal == ( QString ) "currentCharFormatChanged(QTextCharFormat)" ) ret = object->connect( object, SIGNAL( currentCharFormatChanged( const QTextCharFormat & ) )       , s_s, SLOT( currentCharFormatChanged( const QTextCharFormat & ) )       , Qt::AutoConnection );
   else if( signal == ( QString ) "cursorPositionChanged()" )                   ret = object->connect( object, SIGNAL( cursorPositionChanged() )                                   , s_s, SLOT( cursorPositionChanged() )                                   , Qt::AutoConnection );
   else if( signal == ( QString ) "redoAvailable(bool)" )                       ret = object->connect( object, SIGNAL( redoAvailable( bool ) )                                     , s_s, SLOT( redoAvailable( bool ) )                                     , Qt::AutoConnection );
   else if( signal == ( QString ) "textChanged()" )                             ret = object->connect( object, SIGNAL( textChanged() )                                             , s_s, SLOT( textChanged() )                                             , Qt::AutoConnection );
   else if( signal == ( QString ) "undoAvailable(available)" )                  ret = object->connect( object, SIGNAL( undoAvailable( bool ) )                                     , s_s, SLOT( undoAvailable( bool ) )                                     , Qt::AutoConnection );
   else if( signal == ( QString ) "timeout()" )                                 ret = object->connect( object, SIGNAL( timeout() )                                                 , s_s, SLOT( timeout() )                                                 , Qt::AutoConnection );
   /* Generic purpose mechanism to receive key and mouse events off subclasses */
   else if( signal == ( QString ) "keyPressEvent()" )                           ret = object->connect( object, SIGNAL( sg_keyPressEvent( QKeyEvent * ) )                           , s_s, SLOT( keyPressEvent( QKeyEvent * ) )                              , Qt::AutoConnection );
   else if( signal == ( QString ) "keyReleaseEvent()" )                         ret = object->connect( object, SIGNAL( sg_keyReleaseEvent( QKeyEvent * ) )                         , s_s, SLOT( keyReleaseEvent( QKeyEvent * ) )                            , Qt::AutoConnection );
   else if( signal == ( QString ) "mouseMoveEvent()" )                          ret = object->connect( object, SIGNAL( sg_mouseMoveEvent( QMouseEvent * ) )                        , s_s, SLOT( mouseMoveEvent( QMouseEvent * ) )                           , Qt::AutoConnection );
   else if( signal == ( QString ) "mouseDoubleClickEvent()" )                   ret = object->connect( object, SIGNAL( sg_mouseDoubleClickEvent( QMouseEvent * ) )                 , s_s, SLOT( mouseDoubleClickEvent( QMouseEvent * ) )                    , Qt::AutoConnection );
   else if( signal == ( QString ) "mousePressEvent()" )                         ret = object->connect( object, SIGNAL( sg_mousePressEvent( QMouseEvent * ) )                       , s_s, SLOT( mousePressEvent( QMouseEvent * ) )                          , Qt::AutoConnection );
   else if( signal == ( QString ) "mouseReleaseEvent()" )                       ret = object->connect( object, SIGNAL( sg_mouseReleaseEvent( QMouseEvent * ) )                     , s_s, SLOT( mouseReleaseEvent( QMouseEvent * ) )                        , Qt::AutoConnection );
   else if( signal == ( QString ) "wheelEvent()" )                              ret = object->connect( object, SIGNAL( sg_wheelEvent( QWheelEvent * ) )                            , s_s, SLOT( wheelEvent( QWheelEvent * ) )                               , Qt::AutoConnection );
   else if( signal == ( QString ) "resizeEvent()" )                             ret = object->connect( object, SIGNAL( sg_resizeEvent( QResizeEvent * ) )                          , s_s, SLOT( resizeEvent( QResizeEvent * ) )                             , Qt::AutoConnection );
   else if( signal == ( QString ) "scrollContentsBy(int,int)" )                 ret = object->connect( object, SIGNAL( sg_scrollContentsBy( int, int ) )                           , s_s, SLOT( scrollContentsBy( int, int ) )                              , Qt::AutoConnection );
   else if( signal == ( QString ) "geometriesChanged()" )                       ret = object->connect( object, SIGNAL( geometriesChanged() )                                       , s_s, SLOT( geometriesChanged() )                                       , Qt::AutoConnection );
   else if( signal == ( QString ) "sectionAutoResize(int,int)" )                ret = object->connect( object, SIGNAL( sectionAutoResize( int, QHeaderView::ResizeMode ) )         , s_s, SLOT( sectionAutoResize( int, QHeaderView::ResizeMode ) )         , Qt::AutoConnection );
   else if( signal == ( QString ) "sectionClicked(int)" )                       ret = object->connect( object, SIGNAL( sectionClicked( int ) )                                     , s_s, SLOT( sectionClicked( int ) )                                     , Qt::AutoConnection );
   else if( signal == ( QString ) "sectionCountChanged(int,int)" )              ret = object->connect( object, SIGNAL( sectionCountChanged( int, int ) )                           , s_s, SLOT( sectionCountChanged( int, int ) )                           , Qt::AutoConnection );
   else if( signal == ( QString ) "sectionDoubleClicked(int)" )                 ret = object->connect( object, SIGNAL( sectionDoubleClicked( int ) )                               , s_s, SLOT( sectionDoubleClicked( int ) )                               , Qt::AutoConnection );
   else if( signal == ( QString ) "sectionEntered(int)" )                       ret = object->connect( object, SIGNAL( sectionEntered( int ) )                                     , s_s, SLOT( sectionEntered( int ) )                                     , Qt::AutoConnection );
   else if( signal == ( QString ) "sectionHandleDoubleClicked(int)" )           ret = object->connect( object, SIGNAL( sectionHandleDoubleClicked( int ) )                         , s_s, SLOT( sectionHandleDoubleClicked( int ) )                         , Qt::AutoConnection );
   else if( signal == ( QString ) "sectionMoved(int,int,int)" )                 ret = object->connect( object, SIGNAL( sectionMoved( int, int, int ) )                             , s_s, SLOT( sectionMoved( int, int, int ) )                             , Qt::AutoConnection );
   else if( signal == ( QString ) "sectionPressed(int)" )                       ret = object->connect( object, SIGNAL( sectionPressed( int ) )                                     , s_s, SLOT( sectionPressed( int ) )                                     , Qt::AutoConnection );
   else if( signal == ( QString ) "sectionResized(int,int,int)" )               ret = object->connect( object, SIGNAL( sectionResized( int, int, int ) )                           , s_s, SLOT( sectionResized( int, int, int ) )                           , Qt::AutoConnection );
   else if( signal == ( QString ) "sortIndicatorChanged(int,int)" )             ret = object->connect( object, SIGNAL( sortIndicatorChanged( int, Qt::SortOrder ) )                , s_s, SLOT( sortIndicatorChanged( int, Qt::SortOrder ) )                , Qt::AutoConnection );
   else if( signal == ( QString ) "buttonClicked(int)" )                        ret = object->connect( object, SIGNAL( buttonClicked( int ) )                                      , s_s, SLOT( buttonClicked( int ) )                                      , Qt::AutoConnection );
   else if( signal == ( QString ) "buttonPressed(int)" )                        ret = object->connect( object, SIGNAL( buttonPressed( int ) )                                      , s_s, SLOT( buttonPressed( int ) )                                      , Qt::AutoConnection );
   else if( signal == ( QString ) "buttonReleased(int)" )                       ret = object->connect( object, SIGNAL( buttonReleased( int ) )                                     , s_s, SLOT( buttonReleased( int ) )                                     , Qt::AutoConnection );
   else if( signal == ( QString ) "linkActivated(QString)" )                    ret = object->connect( object, SIGNAL( linkActivated( const QString & ) )                          , s_s, SLOT( linkActivated( const QString & ) )                          , Qt::AutoConnection );
   else if( signal == ( QString ) "linkHovered(QString)" )                      ret = object->connect( object, SIGNAL( linkHovered( const QString & ) )                            , s_s, SLOT( linkHovered( const QString & ) )                            , Qt::AutoConnection );
   else if( signal == ( QString ) "cellActivated(int,int)" )                    ret = object->connect( object, SIGNAL( cellActivated( int, int ) )                                 , s_s, SLOT( cellActivated( int, int ) )                                 , Qt::AutoConnection );
   else if( signal == ( QString ) "cellChanged(int,int)" )                      ret = object->connect( object, SIGNAL( cellChanged( int, int ) )                                   , s_s, SLOT( cellChanged( int, int ) )                                   , Qt::AutoConnection );
   else if( signal == ( QString ) "cellClicked(int,int)" )                      ret = object->connect( object, SIGNAL( cellClicked( int, int ) )                                   , s_s, SLOT( cellClicked( int, int ) )                                   , Qt::AutoConnection );
   else if( signal == ( QString ) "cellDoubleClicked(int,int)" )                ret = object->connect( object, SIGNAL( cellDoubleClicked( int, int ) )                             , s_s, SLOT( cellDoubleClicked( int, int ) )                             , Qt::AutoConnection );
   else if( signal == ( QString ) "cellEntered(int,int)" )                      ret = object->connect( object, SIGNAL( cellEntered( int, int ) )                                   , s_s, SLOT( cellEntered( int, int ) )                                   , Qt::AutoConnection );
   else if( signal == ( QString ) "cellPressed(int,int)" )                      ret = object->connect( object, SIGNAL( cellPressed( int, int ) )                                   , s_s, SLOT( cellPressed( int, int ) )                                   , Qt::AutoConnection );
   else if( signal == ( QString ) "currentCellChanged(int,int,int,int)" )       ret = object->connect( object, SIGNAL( currentCellChanged( int, int, int, int ) )                  , s_s, SLOT( currentCellChanged( int, int, int, int ) )                  , Qt::AutoConnection );
   else if( signal == ( QString ) "tabCloseRequested(int)" )                    ret = object->connect( object, SIGNAL( tabCloseRequested( int ) )                                  , s_s, SLOT( tabCloseRequested( int ) )                                  , Qt::AutoConnection );
   else if( signal == ( QString ) "paintRequested(QPrinter)" )                  ret = object->connect( object, SIGNAL( paintRequested( QPrinter * ) )                              , s_s, SLOT( paintRequested( QPrinter * ) )                              , Qt::AutoConnection );

   hb_retl( ret == true );

   if( ret == true )
   {
      s_s->listBlock  << codeblock;
      object->setProperty( hb_parcx( 2 ), ( int ) s_s->listBlock.size() );
   }
}

static bool disconnect_signal( QObject * object, const char * signal )
{
   if(      signal == ( QString ) "clicked()" )                                 return object->disconnect( SIGNAL( clicked() ) );
   else if( signal == ( QString ) "returnPressed()" )                           return object->disconnect( SIGNAL( returnPressed() ) );
   else if( signal == ( QString ) "triggered()" )                               return object->disconnect( SIGNAL( triggered() ) );
   else if( signal == ( QString ) "hovered()" )                                 return object->disconnect( SIGNAL( hovered() ) );
   else if( signal == ( QString ) "viewportEntered()" )                         return object->disconnect( SIGNAL( viewportEntered() ) );
   else if( signal == ( QString ) "pressed()" )                                 return object->disconnect( SIGNAL( pressed() ) );
   else if( signal == ( QString ) "released()" )                                return object->disconnect( SIGNAL( released() ) );
   else if( signal == ( QString ) "stateChanged(int)" )                         return object->disconnect( SIGNAL( stateChanged( int ) ) );
   else if( signal == ( QString ) "activated(int)" )                            return object->disconnect( SIGNAL( activated( int ) ) );
   else if( signal == ( QString ) "currentIndexChanged(int)" )                  return object->disconnect( SIGNAL( currentIndexChanged( int ) ) );
   else if( signal == ( QString ) "highlighted(int)" )                          return object->disconnect( SIGNAL( highlighted( int ) ) );
   else if( signal == ( QString ) "triggered(bool)" )                           return object->disconnect( SIGNAL( triggered( bool ) ) );
   else if( signal == ( QString ) "clicked(QModelIndex)" )                      return object->disconnect( SIGNAL( clicked( const QModelIndex & ) ) );
   else if( signal == ( QString ) "doubleClicked(QModelIndex)" )                return object->disconnect( SIGNAL( doubleClicked( const QModelIndex & ) ) );
   else if( signal == ( QString ) "entered(QModelIndex)" )                      return object->disconnect( SIGNAL( entered( const QModelIndex & ) ) );
   else if( signal == ( QString ) "hovered(action)" )                           return object->disconnect( SIGNAL( hovered( QAction * ) ) );
   else if( signal == ( QString ) "currentChanged(int)" )                       return object->disconnect( SIGNAL( currentChanged( int ) ) );
   else if( signal == ( QString ) "actionTriggered(int)" )                      return object->disconnect( SIGNAL( actionTriggered(int) ) );
   else if( signal == ( QString ) "rangeChanged(int,int)" )                     return object->disconnect( SIGNAL( rangeChanged(int,int) ) );
   else if( signal == ( QString ) "sliderMoved(int)" )                          return object->disconnect( SIGNAL( sliderMoved(int) ) );
   else if( signal == ( QString ) "sliderPressed()" )                           return object->disconnect( SIGNAL( sliderPressed() ) );
   else if( signal == ( QString ) "sliderReleased()" )                          return object->disconnect( SIGNAL( sliderReleased() ) );
   else if( signal == ( QString ) "valueChanged(int)" )                         return object->disconnect( SIGNAL( valueChanged(int) ) );
   else if( signal == ( QString ) "cursorPositionChanged(int,int)" )            return object->disconnect( SIGNAL( cursorPositionChanged(int,int) ) );
   else if( signal == ( QString ) "editingFinished()" )                         return object->disconnect( SIGNAL( editingFinished() ) );
   else if( signal == ( QString ) "returnPressed()" )                           return object->disconnect( SIGNAL( returnPressed() ) );
   else if( signal == ( QString ) "selectionChanged()" )                        return object->disconnect( SIGNAL( selectionChanged() ) );
   else if( signal == ( QString ) "textChanged(QString)" )                      return object->disconnect( SIGNAL( textChanged( const QString &) ) );
   else if( signal == ( QString ) "textEdited(QString)" )                       return object->disconnect( SIGNAL( textEdited( const QString &) ) );
   else if( signal == ( QString ) "currentItemChanged(QTWItem)" )               return object->disconnect( SIGNAL( currentItemChanged( QTreeWidgetItem *, QTreeWidgetItem * ) ) );
   else if( signal == ( QString ) "itemActivated(QTWItem)" )                    return object->disconnect( SIGNAL( itemActivated( QTreeWidgetItem *, int ) ) );
   else if( signal == ( QString ) "itemChanged(QTWItem)" )                      return object->disconnect( SIGNAL( itemChanged( QTreeWidgetItem *, int ) ) );
   else if( signal == ( QString ) "itemClicked(QTWItem)" )                      return object->disconnect( SIGNAL( itemClicked( QTreeWidgetItem *, int ) ) );
   else if( signal == ( QString ) "itemCollapsed(QTWItem)" )                    return object->disconnect( SIGNAL( itemCollapsed( QTreeWidgetItem * ) ) );
   else if( signal == ( QString ) "itemDoubleClicked(QTWItem)" )                return object->disconnect( SIGNAL( itemDoubleClicked( QTreeWidgetItem *, int ) ) );
   else if( signal == ( QString ) "itemEntered(QTWItem)" )                      return object->disconnect( SIGNAL( itemEntered( QTreeWidgetItem *, int ) ) );
   else if( signal == ( QString ) "itemExpanded(QTWItem)" )                     return object->disconnect( SIGNAL( itemExpanded( QTreeWidgetItem * ) ) );
   else if( signal == ( QString ) "itemPressed(QTWItem)" )                      return object->disconnect( SIGNAL( itemPressed( QTreeWidgetItem *, int ) ) );
   else if( signal == ( QString ) "itemSelectionChanged()" )                    return object->disconnect( SIGNAL( itemSelectionChanged() ) );
   else if( signal == ( QString ) "iconChanged()" )                             return object->disconnect( SIGNAL( iconChanged() ) );
   else if( signal == ( QString ) "titleChanged(QString)" )                     return object->disconnect( SIGNAL( titleChanged( const QString & ) ) );
   else if( signal == ( QString ) "urlChanged(QUrl)" )                          return object->disconnect( SIGNAL( urlChanged( const QUrl & ) ) );
   else if( signal == ( QString ) "currentFontChanged(QFont)" )                 return object->disconnect( SIGNAL( currentFontChanged( const QFont & ) ) );
   else if( signal == ( QString ) "fontSelected(QFont)" )                       return object->disconnect( SIGNAL( fontSelected( const QFont & ) ) );
   else if( signal == ( QString ) "accepted()" )                                return object->disconnect( SIGNAL( accepted() ) );
   else if( signal == ( QString ) "finished(int)" )                             return object->disconnect( SIGNAL( finished( int ) ) );
   else if( signal == ( QString ) "rejected()" )                                return object->disconnect( SIGNAL( rejected() ) );
   else if( signal == ( QString ) "currentChanged(QString)" )                   return object->disconnect( SIGNAL( currentChanged( const QString & ) ) );
   else if( signal == ( QString ) "directoryEntered(QString)" )                 return object->disconnect( SIGNAL( directoryEntered( const QString & ) ) );
   else if( signal == ( QString ) "fileSelected(QString)" )                     return object->disconnect( SIGNAL( fileSelected( const QString & ) ) );
   else if( signal == ( QString ) "filesSelected(QStringList)" )                return object->disconnect( SIGNAL( filesSelected( const QStringList & ) ) );
   else if( signal == ( QString ) "filterSelected(QString)" )                   return object->disconnect( SIGNAL( filterSelected( const QString & ) ) );
   else if( signal == ( QString ) "accepted(QPrinter)" )                        return object->disconnect( SIGNAL( accepted( QPrinter * ) ) );
   else if( signal == ( QString ) "copyAvailable(bool)" )                       return object->disconnect( SIGNAL( copyAvailable( bool ) ) );
   else if( signal == ( QString ) "currentCharFormatChanged(QTextCharFormat)" ) return object->disconnect( SIGNAL( currentCharFormatChanged( const QTextCharFormat & ) ) );
   else if( signal == ( QString ) "cursorPositionChanged()" )                   return object->disconnect( SIGNAL( cursorPositionChanged() ) );
   else if( signal == ( QString ) "redoAvailable(bool)" )                       return object->disconnect( SIGNAL( redoAvailable( bool ) ) );
   else if( signal == ( QString ) "textChanged()" )                             return object->disconnect( SIGNAL( textChanged() ) );
   else if( signal == ( QString ) "undoAvailable(available)" )                  return object->disconnect( SIGNAL( undoAvailable( bool ) ) );
   else if( signal == ( QString ) "timeout()" )                                 return object->disconnect( SIGNAL( timeout() ) );
   else if( signal == ( QString ) "keyPressEvent()" )                           return object->disconnect( SIGNAL( sg_keyPressEvent( QKeyEvent * ) ) );
   else if( signal == ( QString ) "keyReleaseEvent()" )                         return object->disconnect( SIGNAL( sg_keyReleaseEvent( QKeyEvent * ) ) );
   else if( signal == ( QString ) "mouseMoveEvent()" )                          return object->disconnect( SIGNAL( sg_mouseMoveEvent( QMouseEvent * ) ) );
   else if( signal == ( QString ) "mouseDoubleClickEvent()" )                   return object->disconnect( SIGNAL( sg_mouseDoubleClickEvent( QMouseEvent * ) ) );
   else if( signal == ( QString ) "mousePressEvent()" )                         return object->disconnect( SIGNAL( sg_mousePressEvent( QMouseEvent * ) ) );
   else if( signal == ( QString ) "mouseReleaseEvent()" )                       return object->disconnect( SIGNAL( sg_mouseReleaseEvent( QMouseEvent * ) ) );
   else if( signal == ( QString ) "wheelEvent()" )                              return object->disconnect( SIGNAL( sg_wheelEvent( QWheelEvent * ) ) );
   else if( signal == ( QString ) "resizeEvent()" )                             return object->disconnect( SIGNAL( sg_resizeEvent( QResizeEvent * ) ) );
   else if( signal == ( QString ) "scrollContentsBy(int,int)" )                 return object->disconnect( SIGNAL( sg_scrollContentsBy( int, int ) ) );
   else if( signal == ( QString ) "geometriesChanged()" )                       return object->disconnect( SIGNAL( geometriesChanged() ) );
   else if( signal == ( QString ) "sectionAutoResize(int,int)" )                return object->disconnect( SIGNAL( sectionAutoResize( int, QHeaderView::ResizeMode ) ) );
   else if( signal == ( QString ) "sectionClicked(int)" )                       return object->disconnect( SIGNAL( sectionClicked( int ) ) );
   else if( signal == ( QString ) "sectionCountChanged(int,int)" )              return object->disconnect( SIGNAL( sectionCountChanged( int, int ) ) );
   else if( signal == ( QString ) "sectionDoubleClicked(int)" )                 return object->disconnect( SIGNAL( sectionDoubleClicked( int ) ) );
   else if( signal == ( QString ) "sectionEntered(int)" )                       return object->disconnect( SIGNAL( sectionEntered( int ) ) );
   else if( signal == ( QString ) "sectionHandleDoubleClicked(int)" )           return object->disconnect( SIGNAL( sectionHandleDoubleClicked( int ) ) );
   else if( signal == ( QString ) "sectionMoved(int,int,int)" )                 return object->disconnect( SIGNAL( sectionMoved( int, int, int ) ) );
   else if( signal == ( QString ) "sectionPressed(int)" )                       return object->disconnect( SIGNAL( sectionPressed( int ) ) );
   else if( signal == ( QString ) "sectionResized(int,int,int)" )               return object->disconnect( SIGNAL( sectionResized( int, int, int ) ) );
   else if( signal == ( QString ) "sortIndicatorChanged(int,int)" )             return object->disconnect( SIGNAL( sortIndicatorChanged( int, Qt::SortOrder ) ) );
   else if( signal == ( QString ) "buttonClicked(int)" )                        return object->disconnect( SIGNAL( buttonClicked( int ) ) );
   else if( signal == ( QString ) "buttonPressed(int)" )                        return object->disconnect( SIGNAL( buttonPressed( int ) ) );
   else if( signal == ( QString ) "buttonReleased(int)" )                       return object->disconnect( SIGNAL( buttonReleased( int ) ) );
   else if( signal == ( QString ) "linkActivated(QString)" )                    return object->disconnect( SIGNAL( linkActivated( const QString & ) ) );
   else if( signal == ( QString ) "linkHovered(QString)" )                      return object->disconnect( SIGNAL( linkHovered( const QString & ) ) );
   else if( signal == ( QString ) "cellActivated(int,int)" )                    return object->disconnect( SIGNAL( cellActivated( int, int ) ) );
   else if( signal == ( QString ) "cellChanged(int,int)" )                      return object->disconnect( SIGNAL( cellChanged( int, int ) ) );
   else if( signal == ( QString ) "cellClicked(int,int)" )                      return object->disconnect( SIGNAL( cellClicked( int, int ) ) );
   else if( signal == ( QString ) "cellDoubleClicked(int,int)" )                return object->disconnect( SIGNAL( cellDoubleClicked( int, int ) ) );
   else if( signal == ( QString ) "cellEntered(int,int)" )                      return object->disconnect( SIGNAL( cellEntered( int, int ) ) );
   else if( signal == ( QString ) "cellPressed(int,int)" )                      return object->disconnect( SIGNAL( cellPressed( int, int ) ) );
   else if( signal == ( QString ) "currentCellChanged(int,int,int,int)" )       return object->disconnect( SIGNAL( currentCellChanged( int, int, int, int ) ) );
   else if( signal == ( QString ) "tabCloseRequested(int)" )                    return object->disconnect( SIGNAL( tabCloseRequested( int ) ) );
   else if( signal == ( QString ) "paintRequested(QPrinter)" )                  return object->disconnect( SIGNAL( paintRequested( QPrinter * ) ) );

   return false;
}

/*
 * harbour function to disconnect signals
 */
HB_FUNC( QT_DISCONNECT_SIGNAL )
{
   QObject * object = ( QObject* ) hbqt_gcpointer( 1 );
   bool bFreed = false;

   if( object )
   {
      Slots * s_s = qt_getEventSlots();
      const char * signal = hb_parcx( 2 );
      int i = object->property( signal ).toInt();

      if( i > 0 && i <= s_s->listBlock.size() )
      {
         hb_itemRelease( s_s->listBlock.at( i - 1 ) );
         s_s->listBlock[ i - 1 ] = NULL;
         bFreed = disconnect_signal( object, signal );
#if defined( __HB_DEBUG__ )
hbqt_debug( "      QT_DISCONNECT_SIGNAL: %s    %s", ret ? "YES" : "NO", signal );
#endif
      }
   }
   hb_retl( bFreed );
}

HB_FUNC( QT_SLOTS_DESTROY )
{
   qt_getEventSlots()->~Slots();
}

/*----------------------------------------------------------------------*/

Events::Events( QObject * parent ) : QObject( parent )
{
}
Events::~Events()
{
   listBlock.clear();
}

bool Events::eventFilter( QObject * object, QEvent * event )
{
   QEvent::Type eventtype = event->type();
#if defined( __HB_DEBUG__ )
hbqt_debug( "0 Events::eventFilter = %i", ( int ) eventtype );
#endif
   if( ( int ) eventtype == 0 )
   {
#if defined( __HB_DEBUG__ )
hbqt_debug( "x Events::eventFilter =            0" );
#endif
      return false;
   }

   char prop[ 10 ];
   hb_snprintf( prop, sizeof( prop ), "%s%i%s", "P", eventtype, "P" );
   int found = object->property( prop ).toInt();
   if( found == 0 )
   {
#if defined( __HB_DEBUG__ )
hbqt_debug( "f Events::eventFilter = %s  %i", "       found=0", ( int ) eventtype );
#endif
      return false;
   }

   //bool ret = false;
   bool ret = true;
   if( found <= listBlock.size() && listObj.at( found - 1 ) == object && hb_vmRequestReenter() )
   {
      PHB_ITEM pObject = hb_itemPutPtr( NULL, object );
      PHB_ITEM pEvent  = hb_itemPutPtr( NULL, event  );
#if defined( __HB_DEBUG__ )
hbqt_debug( "0 Events::eventFilter = %i", ( int ) eventtype );
#endif
      ret = hb_itemGetL( hb_vmEvalBlockV( ( PHB_ITEM ) listBlock.at( found - 1 ), 2, pObject, pEvent ) );
#if defined( __HB_DEBUG__ )
hbqt_debug( "1 Events::eventFilter = %s", ret ? "   yes" : "   no" );
#endif
      hb_itemRelease( pObject );
      hb_itemRelease( pEvent  );

      hb_vmRequestRestore();

      if( eventtype == QEvent::Close )
      {
         event->ignore();
      }
   }
#if defined( __HB_DEBUG__ )
hbqt_debug( "1 Events::eventFilter = %i", ( int ) eventtype );
#endif
   return ret;
}

HB_FUNC( QT_SETEVENTFILTER )
{
   qt_setEventFilter();
}
HB_FUNC( QT_SETEVENTSLOTS )
{
   qt_setEventSlots();
}
HB_FUNC( QT_EVENTS_DESTROY )
{
   qt_getEventFilter()->~Events();
}

HB_FUNC( QT_QEVENTFILTER )
{
   hb_retptr( qt_getEventFilter() );
}

HB_FUNC( QT_CONNECT_EVENT )
{
   int       type      = hb_parni( 2 );
   PHB_ITEM  codeblock = hb_itemNew( hb_param( 3, HB_IT_BLOCK | HB_IT_BYREF ) );
   Events  * s_e       = qt_getEventFilter();
   QObject * object    = ( QObject* ) hbqt_gcpointer( 1 );          /* get sender    */

   char prop[ 20 ];
   hb_snprintf( prop, sizeof( prop ), "%s%i%s", "P", type, "P" );    /* Make it a unique identifier */

   s_e->listBlock << codeblock;
   s_e->listObj   << object;

   object->setProperty( prop, ( int ) s_e->listBlock.size() );

   hb_retl( HB_TRUE );
}

HB_FUNC( QT_DISCONNECT_EVENT )
{
   int       type   = hb_parni( 2 );
   bool      bRet   = false;
   Events  * s_e    = qt_getEventFilter();
   QObject * object = ( QObject* ) hbqt_gcpointer( 1 );

   char prop[ 10 ];
   hb_snprintf( prop, sizeof( prop ), "%s%i%s", "P", type, "P" );    /* Make it a unique identifier */

   int i = object->property( prop ).toInt();
   if( i > 0 && i <= s_e->listBlock.size() )
   {
      hb_itemRelease( s_e->listBlock.at( i - 1 ) );
      s_e->listBlock[ i - 1 ] = NULL;
      s_e->listObj[ i - 1 ]   = NULL;
      object->setProperty( prop, QVariant() );
      bRet = true;
#if defined( __HB_DEBUG__ )
hbqt_debug( "      QT_DISCONNECT_EVENT: %i", type );
#endif
   }
   hb_retl( bRet );
}

/*----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*/

MyMainWindow::MyMainWindow( PHB_ITEM pBlock, int iThreadID )
{
   Qt::WindowFlags flags = Qt::WindowCloseButtonHint    | Qt::WindowMaximizeButtonHint |
                           Qt::WindowMinimizeButtonHint | Qt::WindowSystemMenuHint     |
                           Qt::CustomizeWindowHint      | Qt::WindowTitleHint          |
                           Qt::Window;
   setWindowFlags( flags );
   setFocusPolicy( Qt::StrongFocus );
   setAttribute( Qt::WA_DeleteOnClose );
   //setAttribute( Qt::WA_NoSystemBackground );
   //setAttribute( Qt::WA_PaintOnScreen );
   //setMouseTracking( true );

   block     = pBlock;
   threadID  = iThreadID;
}
MyMainWindow::~MyMainWindow( void )
{
#if defined( __HB_DEBUG__ )
hbqt_debug( "               MyMainWindow::~MyMainWindow 0" );
#endif
   hb_itemRelease( block );
#if defined( __HB_DEBUG__ )
hbqt_debug( "               MyMainWindow::~MyMainWindow 1" );
#endif
}
void MyMainWindow::paintEvent( QPaintEvent * event )
{
   hb_threadMutexLock( s_mutex );
   if( hb_vmRequestReenter() )
   {
      PHB_ITEM p0 = hb_itemPutNI( NULL, QEvent::Paint );
      PHB_ITEM p1 = hb_itemPutPtr( NULL, event );
      hb_vmEvalBlockV( block, 2, p0, p1 );
      hb_itemRelease( p0 );
      hb_itemRelease( p1 );
      hb_vmRequestRestore();
   }
   hb_threadMutexUnlock( s_mutex );
}
bool MyMainWindow::event( QEvent * event )
{
   hb_threadMutexLock( s_mutex );
   bool bRet = QWidget::event( event );
   hb_threadMutexUnlock( s_mutex );
   return bRet;
}
void MyMainWindow::focusInEvent( QFocusEvent *event )
{
   hb_threadMutexLock( s_mutex );
   if( hb_vmRequestReenter() )
   {
      PHB_ITEM p0  = hb_itemPutNI( NULL, QEvent::FocusIn );
      PHB_ITEM p1  = hb_itemPutPtr( NULL, event );
      hb_vmEvalBlockV( block, 2, p0, p1 );
      hb_itemRelease( p0 );
      hb_itemRelease( p1 );
      hb_vmRequestRestore();
   }
   QWidget::focusInEvent( event );
   hb_threadMutexUnlock( s_mutex );
}
void MyMainWindow::focusOutEvent( QFocusEvent *event )
{
   hb_threadMutexLock( s_mutex );
   if( hb_vmRequestReenter() )
   {
      PHB_ITEM p0  = hb_itemPutNI( NULL, QEvent::FocusOut );
      PHB_ITEM p1  = hb_itemPutPtr( NULL, event );
      hb_vmEvalBlockV( block, 2, p0, p1 );
      hb_itemRelease( p0 );
      hb_itemRelease( p1 );
      hb_vmRequestRestore();
   }
   QWidget::focusOutEvent( event );
   hb_threadMutexUnlock( s_mutex );
}
void MyMainWindow::keyPressEvent( QKeyEvent * event )
{
   hb_threadMutexLock( s_mutex );
   if( hb_vmRequestReenter() )
   {
      PHB_ITEM p0  = hb_itemPutNI( NULL, QEvent::KeyPress );
      PHB_ITEM p1  = hb_itemPutPtr( NULL, event );
      hb_vmEvalBlockV( block, 2, p0, p1 );
      hb_itemRelease( p0 );
      hb_itemRelease( p1 );
      hb_vmRequestRestore();
   }
   QWidget::keyPressEvent( event );
   hb_threadMutexUnlock( s_mutex );
}
void MyMainWindow::mouseDoubleClickEvent( QMouseEvent * event )
{
   hb_threadMutexLock( s_mutex );
   if( hb_vmRequestReenter() )
   {
      PHB_ITEM p0  = hb_itemPutNI( NULL, QEvent::MouseButtonDblClick );
      PHB_ITEM p1  = hb_itemPutPtr( NULL, event );
      hb_vmEvalBlockV( block, 2, p0, p1 );
      hb_itemRelease( p0 );
      hb_itemRelease( p1 );
      hb_vmRequestRestore();
   }
   QWidget::mouseDoubleClickEvent( event );
   hb_threadMutexUnlock( s_mutex );
}
void MyMainWindow::mouseMoveEvent( QMouseEvent * event )
{
   hb_threadMutexLock( s_mutex );
   if( hb_vmRequestReenter() )
   {
      PHB_ITEM p0  = hb_itemPutNI( NULL, QEvent::MouseMove );
      PHB_ITEM p1  = hb_itemPutPtr( NULL, event );
      hb_vmEvalBlockV( block, 2, p0, p1 );
      hb_itemRelease( p0 );
      hb_itemRelease( p1 );
      hb_vmRequestRestore();
   }
   QWidget::mouseMoveEvent( event );
   hb_threadMutexUnlock( s_mutex );
}
void MyMainWindow::mousePressEvent( QMouseEvent * event )
{
   hb_threadMutexLock( s_mutex );
   if( hb_vmRequestReenter() )
   {
      PHB_ITEM p0  = hb_itemPutNI( NULL, QEvent::MouseButtonPress );
      PHB_ITEM p1  = hb_itemPutPtr( NULL, event );
      hb_vmEvalBlockV( block, 2, p0, p1 );
      hb_itemRelease( p0 );
      hb_itemRelease( p1 );
      hb_vmRequestRestore();
   }
   QWidget::mousePressEvent( event );
   hb_threadMutexUnlock( s_mutex );
}
void MyMainWindow::mouseReleaseEvent( QMouseEvent * event )
{
   hb_threadMutexLock( s_mutex );
   if( hb_vmRequestReenter() )
   {
      PHB_ITEM p0  = hb_itemPutNI( NULL, QEvent::MouseButtonRelease );
      PHB_ITEM p1  = hb_itemPutPtr( NULL, event );
      hb_vmEvalBlockV( block, 2, p0, p1 );
      hb_itemRelease( p0 );
      hb_itemRelease( p1 );
      hb_vmRequestRestore();
   }
   QWidget::mouseReleaseEvent( event );
   hb_threadMutexUnlock( s_mutex );
}
void MyMainWindow::wheelEvent( QWheelEvent * event )
{
   hb_threadMutexLock( s_mutex );
   if( hb_vmRequestReenter() )
   {
      PHB_ITEM p0  = hb_itemPutNI( NULL, QEvent::Wheel );
      PHB_ITEM p1  = hb_itemPutPtr( NULL, event );
      hb_vmEvalBlockV( block, 2, p0, p1 );
      hb_itemRelease( p0 );
      hb_itemRelease( p1 );
      hb_vmRequestRestore();
   }
   QWidget::wheelEvent( event );
   hb_threadMutexUnlock( s_mutex );
}
void MyMainWindow::resizeEvent( QResizeEvent * event )
{
   hb_threadMutexLock( s_mutex );
   if( hb_vmRequestReenter() )
   {
      PHB_ITEM p0  = hb_itemPutNI( NULL, QEvent::Resize );
      PHB_ITEM p1  = hb_itemPutPtr( NULL, event );
      hb_vmEvalBlockV( block, 2, p0, p1 );
      hb_itemRelease( p0 );
      hb_itemRelease( p1 );
      hb_vmRequestRestore();
   }
   QWidget::resizeEvent( event );
   hb_threadMutexUnlock( s_mutex );
}
void MyMainWindow::closeEvent( QCloseEvent * event )
{
#if defined( __HB_DEBUG__ )
hbqt_debug( "               close event(%i)", threadID );
#endif
   hb_threadMutexLock( s_mutex );
   if( hb_vmRequestReenter() )
   {
      PHB_ITEM p0  = hb_itemPutNI( NULL, QEvent::Close );
      PHB_ITEM p1  = hb_itemPutPtr( NULL, event );
      hb_vmEvalBlockV( block, 2, p0, p1 );
      hb_itemRelease( p0 );
      hb_itemRelease( p1 );
      hb_vmRequestRestore();
   }
   hb_threadMutexUnlock( s_mutex );
}

HB_FUNC( QT_MYMAINWINDOW )
{
   PHB_ITEM bBlock = hb_itemNew( ( PHB_ITEM ) hb_param( 1, HB_IT_BLOCK ) );
   hb_retptr( ( MyMainWindow * ) new MyMainWindow( bBlock, hb_parni( 2 ) ) );
}

HB_FUNC( QT_MYMAINWINDOW_DESTROY )
{
   hbqt_par_MyMainWindow( 1 )->~MyMainWindow();
}

HB_FUNC( QT_MUTEXCREATE )
{
   if( s_mutex == NULL )
   {
      s_mutex = hb_threadMutexCreate();
   }
}

HB_FUNC( QT_MUTEXDESTROY )
{
   if( s_mutex != NULL )
   {
      hb_itemRelease( s_mutex );
      s_mutex = NULL;
   }
}

/*----------------------------------------------------------------------*/

#endif
