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

#define hb_getQtEventFilter()       ( ( PHB_EVENTS ) hb_stackGetTSD( &s_events ) )

typedef struct
{
   Slots * slot;
} HB_SLOTS, * PHB_SLOTS;

static HB_TSD_NEW( s_slots, sizeof( HB_SLOTS ), NULL, NULL );

#define hb_getQtEventSlots()       ( ( PHB_SLOTS ) hb_stackGetTSD( &s_slots ) )

/*----------------------------------------------------------------------*/

void qt_setEventFilter()
{
   Events * s_e = hb_getQtEventFilter()->events;
   if( !s_e )
      hb_getQtEventFilter()->events = new Events();
}

Events * qt_getEventFilter( void )
{
   return hb_getQtEventFilter()->events;
}


void qt_setEventSlots()
{
   Slots * s_s = hb_getQtEventSlots()->slot;
   if( !s_s )
      hb_getQtEventSlots()->slot = new Slots();
}

Slots * qt_getEventSlots( void )
{
   return hb_getQtEventSlots()->slot;
}

/*----------------------------------------------------------------------*/

Slots::Slots( QObject* parent ) : QObject( parent )
{
}
Slots::~Slots()
{
}

static void SlotsExec( QObject* object, char* event )
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
      }
   }
}

static void SlotsExecBool( QObject* object, char* event, bool bBool )
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
      }
   }
}

static void SlotsExecInt( QObject* object, char* event, int iValue )
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
      }
   }
}

static void SlotsExecIntInt( QObject* object, char* event, int iValue1, int iValue2 )
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
      }
   }
}

static void SlotsExecIntIntInt( QObject* object, char* event, int iValue1, int iValue2, int iValue3 )
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
      }
   }
}

static void SlotsExecIntIntIntInt( QObject* object, char* event, int iValue1, int iValue2, int iValue3, int iValue4 )
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
      }
   }
}

static void SlotsExecIntIntRect( QObject* object, char* event, int iValue1, int iValue2, const QRect & rect )
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
         PHB_ITEM pValue3 = hb_itemPutPtr( NULL, new QRect( rect ) );
         hb_vmEvalBlockV( s_s->listBlock.at( i - 1 ), 4, pObject, pValue1, pValue2, pValue3 );
         hb_itemRelease( pObject );
         hb_itemRelease( pValue1 );
         hb_itemRelease( pValue2 );
         delete ( ( QRect * ) hb_itemGetPtr( pValue3 ) );
         hb_itemRelease( pValue3 );
      }
   }
}

static void SlotsExecString( QObject* object, char* event, const QString & string )
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
      }
   }
}
#if 0
static void SlotsExecString2( QObject* object, char* event, const QString & s1, const QString & s2 )
{
   if( object )
   {
      Slots * s_s = qt_getEventSlots();
      int i = object->property( event ).toInt();
      if( i > 0 && i <= s_s->listBlock.size() && hb_vmRequestReenter() )
      {
         PHB_ITEM pObject = hb_itemPutPtr( NULL, object );
         PHB_ITEM pS1 = hb_itemPutC( NULL, s1.toAscii().data() );
         PHB_ITEM pS2 = hb_itemPutC( NULL, s2.toAscii().data() );
         hb_vmEvalBlockV( s_s->listBlock.at( i - 1 ), 3, pObject, pS1, pS2 );
         hb_itemRelease( pObject );
         hb_itemRelease( pS1 );
         hb_itemRelease( pS2 );
      }
   }
}
#endif
static void SlotsExecString3( QObject* object, char* event, const QString & s1, const QString & s2, const QString & s3 )
{
   if( object )
   {
      Slots * s_s = qt_getEventSlots();
      int i = object->property( event ).toInt();
      if( i > 0 && i <= s_s->listBlock.size() && hb_vmRequestReenter() )
      {
         PHB_ITEM pObject = hb_itemPutPtr( NULL, object );
         PHB_ITEM pS1 = hb_itemPutC( NULL, s1.toAscii().data() );
         PHB_ITEM pS2 = hb_itemPutC( NULL, s2.toAscii().data() );
         PHB_ITEM pS3 = hb_itemPutC( NULL, s3.toAscii().data() );
         hb_vmEvalBlockV( s_s->listBlock.at( i - 1 ), 4, pObject, pS1, pS2, pS3 );
         hb_itemRelease( pObject );
         hb_itemRelease( pS1 );
         hb_itemRelease( pS2 );
         hb_itemRelease( pS3 );
      }
   }
}

static void SlotsExecModel( QObject* object, char* event, const QModelIndex & index )
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
      }
   }
}

static void SlotsExecRect( QObject* object, char* event, const QRect & rect )
{
   if( object )
   {
      Slots * s_s = qt_getEventSlots();
      int i = object->property( event ).toInt();
      if( i > 0 && i <= s_s->listBlock.size() && hb_vmRequestReenter() )
      {
         PHB_ITEM pObject = hb_itemPutPtr( NULL, object );
         PHB_ITEM p1 = hb_itemPutPtr( NULL, new QRect( rect ) );
         hb_vmEvalBlockV( s_s->listBlock.at( i - 1 ), 2, pObject, p1 );
         hb_itemRelease( pObject );
         delete ( ( QRect * ) hb_itemGetPtr( p1 ) );
         hb_itemRelease( p1 );
      }
   }
}

static void SlotsExecUrl( QObject* object, char* event, const QUrl & url )
{
   if( object )
   {
      Slots * s_s = qt_getEventSlots();
      int i = object->property( event ).toInt();
      if( i > 0 && i <= s_s->listBlock.size() && hb_vmRequestReenter() )
      {
         PHB_ITEM pObject = hb_itemPutPtr( NULL, object );
         PHB_ITEM p1 = hb_itemPutPtr( NULL, new QUrl( url ) );
         hb_vmEvalBlockV( s_s->listBlock.at( i - 1 ), 2, pObject, p1 );
         hb_itemRelease( pObject );
         delete ( ( QUrl * ) hb_itemGetPtr( p1 ) );
         hb_itemRelease( p1 );
      }
   }
}

static void SlotsExecTextCharFormat( QObject* object, char* event, const QTextCharFormat & f )
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
      }
   }
}

static void SlotsExecFont( QObject* object, char* event, const QFont & font )
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
      }
   }
}

static void SlotsExecStringList( QObject* object, char* event, const QStringList & stringList )
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
      }
   }
}

static void SlotsExecNetworkRequest( QObject* object, char* event, const QNetworkRequest & request )
{
   if( object )
   {
      Slots * s_s = qt_getEventSlots();
      int i = object->property( event ).toInt();
      if( i > 0 && i <= s_s->listBlock.size() && hb_vmRequestReenter() )
      {
         PHB_ITEM pObject = hb_itemPutPtr( NULL, object );
         PHB_ITEM p1 = hb_itemPutPtr( NULL, ( QNetworkRequest* ) new QNetworkRequest( request ) );
         hb_vmEvalBlockV( s_s->listBlock.at( i - 1 ), 2, pObject, p1 );
         hb_itemRelease( pObject );
         delete ( ( QNetworkRequest * ) hb_itemGetPtr( p1 ) );
         hb_itemRelease( p1 );
      }
   }
}

static void SlotsExecPointer( QObject* object, char* event, void * p1 )
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
      }
   }
}

static void SlotsExecPointerString( QObject* object, char* event, void * p1, QString s1 )
{
   if( object )
   {
      Slots * s_s = qt_getEventSlots();
      int i = object->property( event ).toInt();
      if( i > 0 && i <= s_s->listBlock.size() && hb_vmRequestReenter() )
      {
         PHB_ITEM pObject = hb_itemPutPtr( NULL, object );
         PHB_ITEM pP1 = hb_itemPutPtr( NULL, p1 );
         PHB_ITEM pS1 = hb_itemPutC( NULL, s1.toAscii().data() );
         hb_vmEvalBlockV( s_s->listBlock.at( i - 1 ), 3, pObject, pP1, pS1 );
         hb_itemRelease( pObject );
         hb_itemRelease( pP1 );
         hb_itemRelease( pS1 );
      }
   }
}

static void SlotsExecPointerInt( QObject* object, char* event, void * p1, int iInt )
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
      }
   }
}

static void SlotsExecPointerPointer( QObject* object, char* event, void * p1, void * p2 )
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
      }
   }
}

/* Generic Key and Mouse Events emitted by subclass objects */
void Slots::keyPressEvent( QKeyEvent * event )
{
   QObject *object = qobject_cast<QObject *>( sender() );
   SlotsExecPointer( object, ( char* ) "keyPressEvent()", event );
}
void Slots::keyReleaseEvent( QKeyEvent * event )
{
   QObject *object = qobject_cast<QObject *>( sender() );
   SlotsExecPointer( object, ( char* ) "keyReleaseEvent()", event );
}
void Slots::mouseMoveEvent( QMouseEvent * event )
{
   QObject *object = qobject_cast<QObject *>( sender() );
   SlotsExecPointer( object, ( char* ) "mouseMoveEvent()", event );
}
void Slots::mouseDoubleClickEvent( QMouseEvent * event )
{
   QObject *object = qobject_cast<QObject *>( sender() );
   SlotsExecPointer( object, ( char* ) "mouseDoubleClickEvent()", event );
}
void Slots::mousePressEvent( QMouseEvent * event )
{
   QObject *object = qobject_cast<QObject *>( sender() );
   SlotsExecPointer( object, ( char* ) "mousePressEvent()", event );
}
void Slots::mouseReleaseEvent( QMouseEvent * event )
{
   QObject *object = qobject_cast<QObject *>( sender() );
   SlotsExecPointer( object, ( char* ) "mouseReleaseEvent()", event );
}
void Slots::wheelEvent( QWheelEvent * event )
{
   QObject *object = qobject_cast<QObject *>( sender() );
   SlotsExecPointer( object, ( char* ) "wheelEvent()", event );
}
void Slots::resizeEvent( QResizeEvent * event )
{
   QObject *object = qobject_cast<QObject *>( sender() );
   SlotsExecPointer( object, ( char* ) "resizeEvent()", event );
}
/* ........................................................ */


void Slots::triggered( bool checked )
{
   QObject *object = qobject_cast<QObject *>( sender() );
   SlotsExecBool( object, ( char* ) "triggered(bool)", checked );
}
void Slots::hovered( QAction * action )
{
   QObject *object = qobject_cast<QObject *>( sender() );
   SlotsExecPointer( object, ( char* ) "hovered(action)", action );
}
void Slots::clicked()
{
   QObject *object = qobject_cast<QObject *>( sender() );
   SlotsExec( object, ( char* ) "clicked()" );
}
void Slots::returnPressed()
{
   QObject *object = qobject_cast<QObject *>( sender() );
   SlotsExec( object, ( char* ) "returnPressed()" );
}
void Slots::viewportEntered()
{
   QObject *object = qobject_cast<QObject *>( sender() );
   SlotsExec( object, ( char* ) "viewportEntered()" );
}
void Slots::pressed()
{
   QObject *object = qobject_cast<QObject *>( sender() );
   SlotsExec( object, ( char* ) "pressed()" );
}
void Slots::released()
{
   QObject *object = qobject_cast<QObject *>( sender() );
   SlotsExec( object, ( char* ) "released()" );
}
void Slots::triggered()
{
   QObject *object = qobject_cast<QObject *>( sender() );
   SlotsExec( object, ( char* ) "triggered()" );
}
void Slots::hovered()
{
   QAction *object = qobject_cast<QAction *>( sender() );
   SlotsExec( object, ( char* ) "hovered()" );
}
void Slots::stateChanged( int state )
{
   QObject *object = qobject_cast<QObject *>( sender() );
   SlotsExecInt( object, ( char* ) "stateChanged(int)", state );
}
void Slots::activated( int index )
{
   QObject *object = qobject_cast<QObject *>( sender() );
   SlotsExecInt( object, ( char* ) "activated(int)", index );
}
void Slots::currentIndexChanged( int index )
{
   QObject *object = qobject_cast<QObject *>( sender() );
   SlotsExecInt( object, ( char* ) "currentIndexChanged(int)", index );
}
void Slots::currentChanged( int index )
{
   QObject *object = qobject_cast<QObject *>( sender() );
   SlotsExecInt( object, ( char* ) "currentChanged(int)", index );
}
void Slots::highlighted( int index )
{
   QObject *object = qobject_cast<QObject *>( sender() );
   SlotsExecInt( object, ( char* ) "highlighted(int)", index );
}

void Slots::clicked( const QModelIndex & index )
{
   QObject *object = qobject_cast<QObject *>( sender() );
   SlotsExecModel( object, ( char* ) "clicked(QModelIndex)", index );
}
void Slots::doubleClicked( const QModelIndex & index )
{
   QObject *object = qobject_cast<QObject *>( sender() );
   SlotsExecModel( object, ( char* ) "doubleClicked(QModelIndex)", index );
}
void Slots::entered( const QModelIndex & index )
{
   QObject *object = qobject_cast<QObject *>( sender() );
   SlotsExecModel( object, ( char* ) "entered(QModelIndex)", index );
}

void Slots::actionTriggered( int action )
{
   QObject *object = qobject_cast<QObject *>( sender() );
   SlotsExecInt( object, ( char* ) "actionTriggered(int)", action );
}
void Slots::rangeChanged( int min, int max )
{
   QObject *object = qobject_cast<QObject *>( sender() );
   SlotsExecIntInt( object, ( char* ) "rangeChanged(int)", min, max );
}
void Slots::sliderMoved( int value )
{
   QObject *object = qobject_cast<QObject *>( sender() );
   SlotsExecInt( object, ( char* ) "sliderMoved(int)", value );
}
void Slots::sliderPressed()
{
   QObject *object = qobject_cast<QObject *>( sender() );
   SlotsExec( object, ( char* ) "sliderPressed()" );
}
void Slots::sliderReleased()
{
   QObject *object = qobject_cast<QObject *>( sender() );
   SlotsExec( object, ( char* ) "sliderReleased()" );
}
void Slots::valueChanged( int value )
{
   QObject *object = qobject_cast<QObject *>( sender() );
   SlotsExecInt( object, ( char* ) "valueChanged(int)", value );
}
void Slots::cursorPositionChanged( int iOld, int iNew )
{
   QObject *object = qobject_cast<QObject *>( sender() );
   SlotsExecIntInt( object, ( char* ) "cursorPositionChanged(int,int)", iOld, iNew );
}
void Slots::editingFinished()
{
   QObject *object = qobject_cast<QObject *>( sender() );
   SlotsExec( object, ( char* ) "editingFinished()" );
}
void Slots::selectionChanged()
{
   QObject *object = qobject_cast<QObject *>( sender() );
   SlotsExec( object, ( char* ) "selectionChanged()" );
}
void Slots::textChanged( const QString & text )
{
   QObject *object = qobject_cast<QObject *>( sender() );
   SlotsExecString( object, ( char* ) "textChanged(QString)", text );
}
void Slots::textEdited( const QString & text )
{
   QObject *object = qobject_cast<QObject *>( sender() );
   SlotsExecString( object, ( char* ) "textEdited(QString)", text );
}
/*  TreeViewobject */
void Slots::currentItemChanged( QTreeWidgetItem * current, QTreeWidgetItem * previous )
{
   QObject *object = qobject_cast<QObject *>( sender() );
   SlotsExecPointerPointer( object, ( char* ) "currentItemChanged(QTWItem)", current, previous );
}
void Slots::itemActivated( QTreeWidgetItem * item, int column )
{
   QObject *object = qobject_cast<QObject *>( sender() );
   SlotsExecPointerInt( object, ( char* ) "itemActivated(QTWItem)", item, column );
}
void Slots::itemChanged( QTreeWidgetItem * item, int column )
{
   QObject *object = qobject_cast<QObject *>( sender() );
   SlotsExecPointerInt( object, ( char* ) "itemChanged(QTWItem)", item, column );
}
void Slots::itemClicked( QTreeWidgetItem * item, int column )
{
   QObject *object = qobject_cast<QObject *>( sender() );
   SlotsExecPointerInt( object, ( char* ) "itemClicked(QTWItem)", item, column );
}
void Slots::itemDoubleClicked( QTreeWidgetItem * item, int column )
{
   QObject *object = qobject_cast<QObject *>( sender() );
   SlotsExecPointerInt( object, ( char* ) "itemDoubleClicked(QTWItem)", item, column );
}
void Slots::itemEntered( QTreeWidgetItem * item, int column )
{
   QObject *object = qobject_cast<QObject *>( sender() );
   SlotsExecPointerInt( object, ( char* ) "itemEntered(QTWItem)", item, column );
}
void Slots::itemPressed( QTreeWidgetItem * item, int column )
{
   QObject *object = qobject_cast<QObject *>( sender() );
   SlotsExecPointerInt( object, ( char* ) "itemPressed(QTWItem)", item, column );
}
void Slots::itemExpanded( QTreeWidgetItem * item )
{
   QObject *object = qobject_cast<QObject *>( sender() );
   SlotsExecPointer( object, ( char* ) "itemExpanded(QTWItem)", item );
}
void Slots::itemCollapsed( QTreeWidgetItem * item )
{
   QObject *object = qobject_cast<QObject *>( sender() );
   SlotsExecPointer( object, ( char* ) "itemCollapsed(QTWItem)", item );
}
void Slots::itemSelectionChanged()
{
   QObject *object = qobject_cast<QObject *>( sender() );
   SlotsExec( object, ( char* ) "itemSelectionChanged()" );
}
/* QWebPage */
void Slots::contentsChanged()
{
   QObject *object = qobject_cast<QObject *>( sender() );
   SlotsExec( object, ( char* ) "contentsChanged()" );
}
void Slots::databaseQuotaExceeded( QWebFrame * frame, QString databaseName )
{
   QObject *object = qobject_cast<QObject *>( sender() );
   SlotsExecPointerString( object, ( char* ) "databaseQuotaExceeded(QWebFrame,QString)", frame, databaseName );
}
void Slots::downloadRequested( const QNetworkRequest & request )
{
   QObject *object = qobject_cast<QObject *>( sender() );
   SlotsExecNetworkRequest( object, ( char* ) "downloadRequested(QNetworkRequest)", request );
}
void Slots::frameCreated( QWebFrame * frame )
{
   QObject *object = qobject_cast<QObject *>( sender() );
   SlotsExecPointer( object, ( char* ) "frameCreated(QWebFrame)", frame );
}
void Slots::geometryChangeRequested( const QRect & geom )
{
   QObject *object = qobject_cast<QObject *>( sender() );
   SlotsExecRect( object, ( char* ) "geometryChangeRequested(QRect)", geom );
}
void Slots::linkClicked( const QUrl & url )
{
   QObject *object = qobject_cast<QObject *>( sender() );
   SlotsExecUrl( object, ( char* ) "linkClicked(QUrl)", url );
}
void Slots::linkHovered( const QString & link, const QString & title, const QString & textContent )
{
   QObject *object = qobject_cast<QObject *>( sender() );
   SlotsExecString3( object, ( char* ) "linkHovered(QString,QString,QString)", link, title, textContent );
}
void Slots::loadFinished( bool ok )
{
   QObject *object = qobject_cast<QObject *>( sender() );
   SlotsExecBool( object, ( char* ) "loadFinished(bool)", ok );
}
void Slots::loadProgress( int progress )
{
   QObject *object = qobject_cast<QObject *>( sender() );
   SlotsExecInt( object, ( char* ) "loadProgress(int)", progress );
}
void Slots::loadStarted()
{
   QObject *object = qobject_cast<QObject *>( sender() );
   SlotsExec( object, ( char* ) "loadStarted()" );
}
void Slots::menuBarVisibilityChangeRequested( bool visible )
{
   QObject *object = qobject_cast<QObject *>( sender() );
   SlotsExecBool( object, ( char* ) "menuBarVisibilityChangeRequested(bool)", visible );
}
void Slots::microFocusChanged()
{
   QObject *object = qobject_cast<QObject *>( sender() );
   SlotsExec( object, ( char* ) "microFocusChanged()" );
}
void Slots::printRequested( QWebFrame * frame )
{
   QObject *object = qobject_cast<QObject *>( sender() );
   SlotsExecPointer( object, ( char* ) "printRequested(QWebFrame)", frame );
}
void Slots::repaintRequested( const QRect & dirtyRect )
{
   QObject *object = qobject_cast<QObject *>( sender() );
   SlotsExecRect( object, ( char* ) "repaintRequested(QRect)", dirtyRect );
}
void Slots::restoreFrameStateRequested( QWebFrame * frame )
{
   QObject *object = qobject_cast<QObject *>( sender() );
   SlotsExecPointer( object, ( char* ) "restoreFrameStateRequested(QWebFrame)", frame );
}
void Slots::saveFrameStateRequested( QWebFrame * frame, QWebHistoryItem * item )
{
   QObject *object = qobject_cast<QObject *>( sender() );
   SlotsExecPointerPointer( object, ( char* ) "saveFrameStateRequested(QWebFrame,QWebHistoryItem)", frame, item );
}
void Slots::scrollRequested( int dx, int dy, const QRect & rectToScroll )
{
   QObject *object = qobject_cast<QObject *>( sender() );
   SlotsExecIntIntRect( object, ( char* ) "scrollRequested(int,int,QRect)", dx, dy, rectToScroll );
}
void Slots::statusBarMessage( const QString & text )
{
   QObject *object = qobject_cast<QObject *>( sender() );
   SlotsExecString( object, ( char* ) "statusBarMessage(QString)", text );
}
void Slots::statusBarVisibilityChangeRequested( bool visible )
{
   QObject *object = qobject_cast<QObject *>( sender() );
   SlotsExecBool( object, ( char* ) "statusBarVisibilityChangeRequested(bool)", visible );
}
void Slots::toolBarVisibilityChangeRequested( bool visible )
{
   QObject *object = qobject_cast<QObject *>( sender() );
   SlotsExecBool( object, ( char* ) "toolBarVisibilityChangeRequested(bool)", visible );
}
void Slots::unsupportedContent( QNetworkReply * reply )
{
   QObject *object = qobject_cast<QObject *>( sender() );
   SlotsExecPointer( object, ( char* ) "unsupportedContent(QNetworkReply)", reply );
}
void Slots::windowCloseRequested()
{
   QObject *object = qobject_cast<QObject *>( sender() );
   SlotsExec( object, ( char* ) "windowCloseRequested()" );
}
/* QWebView */
void Slots::iconChanged()
{
   QObject *object = qobject_cast<QObject *>( sender() );
   SlotsExec( object, ( char* ) "iconChanged()" );
}
void Slots::titleChanged( const QString & title )
{
   QObject *object = qobject_cast<QObject *>( sender() );
   SlotsExecString( object, ( char* ) "titleChanged(QString)", title );
}
void Slots::urlChanged( const QUrl & url )
{
   QObject *object = qobject_cast<QObject *>( sender() );
   SlotsExecUrl( object, ( char* ) "urlChanged(QUrl)", url );
}
/* QDialog (s)*/
void Slots::currentFontChanged( const QFont & font )
{
   QObject *object = qobject_cast<QObject *>( sender() );
   SlotsExecFont( object, ( char* ) "currentFontChanged(QFont)", font );
}
void Slots::fontSelected( const QFont & font )
{
   QObject *object = qobject_cast<QObject *>( sender() );
   SlotsExecFont( object, ( char* ) "fontSelected(QFont)", font );
}
void Slots::accepted()
{
   QObject *object = qobject_cast<QObject *>( sender() );
   SlotsExec( object, ( char* ) "accepted()" );
}
void Slots::finished( int result )
{
   QObject *object = qobject_cast<QObject *>( sender() );
   SlotsExecInt( object, ( char* ) "finished(int)", result );
}
void Slots::rejected()
{
   QObject *object = qobject_cast<QObject *>( sender() );
   SlotsExec( object, ( char* ) "rejected()" );
}
void Slots::currentChanged( const QString & path )
{
   QObject *object = qobject_cast<QObject *>( sender() );
   SlotsExecString( object, ( char* ) "currentChanged(QString)", path );
}
void Slots::directoryEntered( const QString & directory )
{
   QObject *object = qobject_cast<QObject *>( sender() );
   SlotsExecString( object, ( char* ) "directoryEntered(QString)", directory );
}
void Slots::fileSelected( const QString & file )
{
   QObject *object = qobject_cast<QObject *>( sender() );
   SlotsExecString( object, ( char* ) "fileSelected(QString)", file );
}
void Slots::filesSelected( const QStringList & selected )
{
   QObject *object = qobject_cast<QObject *>( sender() );
   SlotsExecStringList( object, ( char* ) "filesSelected(QStringList)", selected );
}
void Slots::filterSelected( const QString & filter )
{
   QObject *object = qobject_cast<QObject *>( sender() );
   SlotsExecString( object, ( char* ) "filterSelected(QString)", filter );
}
/* QPrintDialog */
void Slots::accepted( QPrinter * printer )
{
   QObject *object = qobject_cast<QObject *>( sender() );
   SlotsExecPointer( object, ( char* ) "accepted(QPrinter)", printer );
}
/* QTextEdit */
void Slots::copyAvailable( bool yes )
{
   QObject *object = qobject_cast<QObject *>( sender() );
   SlotsExecBool( object, ( char* ) "copyAvailable(bool)", yes );
}
void Slots::currentCharFormatChanged( const QTextCharFormat & f )
{
   QObject *object = qobject_cast<QObject *>( sender() );
   SlotsExecTextCharFormat( object, ( char* ) "currentCharFormatChanged(QTextCharFormat)", f );
}
void Slots::cursorPositionChanged()
{
   QObject *object = qobject_cast<QObject *>( sender() );
   SlotsExec( object, ( char* ) "cursorPositionChanged()" );
}
void Slots::redoAvailable( bool available )
{
   QObject *object = qobject_cast<QObject *>( sender() );
   SlotsExecBool( object, ( char* ) "redoAvailable(bool)", available );
}
void Slots::textChanged()
{
   QObject *object = qobject_cast<QObject *>( sender() );
   SlotsExec( object, ( char* ) "textChanged()" );
}
void Slots::undoAvailable( bool available )
{
   QObject *object = qobject_cast<QObject *>( sender() );
   SlotsExecBool( object, ( char* ) "undoAvailable(available)", available );
}
void Slots::timeout()
{
   QObject *object = qobject_cast<QObject *>( sender() );
   SlotsExec( object, ( char* ) "timeout()" );
}
void Slots::scrollContentsBy( int x, int y )
{
   QObject *object = qobject_cast<QObject *>( sender() );
   SlotsExecIntInt( object, ( char* ) "scrollContentsBy(int,int)", x, y );
}
void Slots::geometriesChanged()
{
   QObject *object = qobject_cast<QObject *>( sender() );
   SlotsExec( object, ( char* ) "geometriesChanged()" );
}
void Slots::sectionAutoResize( int logicalIndex, QHeaderView::ResizeMode mode )
{
   QObject *object = qobject_cast<QObject *>( sender() );
   SlotsExecIntInt( object, ( char* ) "sectionAutoResize(int,int)", logicalIndex, mode );
}
void Slots::sectionClicked( int logicalIndex )
{
   QObject *object = qobject_cast<QObject *>( sender() );
   SlotsExecInt( object, ( char* ) "sectionClicked(int)", logicalIndex );
}
void Slots::sectionCountChanged( int oldCount, int newCount )
{
   QObject *object = qobject_cast<QObject *>( sender() );
   SlotsExecIntInt( object, ( char* ) "sectionCountChanged(int,int)", oldCount, newCount );
}
void Slots::sectionDoubleClicked( int logicalIndex )
{
   QObject *object = qobject_cast<QObject *>( sender() );
   SlotsExecInt( object, ( char* ) "sectionDoubleClicked(int)", logicalIndex );
}
void Slots::sectionEntered( int logicalIndex )
{
   QObject *object = qobject_cast<QObject *>( sender() );
   SlotsExecInt( object, ( char* ) "sectionEntered(int)", logicalIndex );
}
void Slots::sectionHandleDoubleClicked( int logicalIndex )
{
   QObject *object = qobject_cast<QObject *>( sender() );
   SlotsExecInt( object, ( char* ) "sectionHandleDoubleClicked(int)", logicalIndex );
}
void Slots::sectionMoved( int logicalIndex, int oldVisualIndex, int newVisualIndex )
{
   QObject *object = qobject_cast<QObject *>( sender() );
   SlotsExecIntIntInt( object, ( char* ) "sectionMoved(int,int,int)", logicalIndex, oldVisualIndex, newVisualIndex );
}
void Slots::sectionPressed( int logicalIndex )
{
   QObject *object = qobject_cast<QObject *>( sender() );
   SlotsExecInt( object, ( char* ) "sectionPressed(int)", logicalIndex );
}
void Slots::sectionResized( int logicalIndex, int oldSize, int newSize )
{
   QObject *object = qobject_cast<QObject *>( sender() );
   SlotsExecIntIntInt( object, ( char* ) "sectionResized(int,int,int)", logicalIndex, oldSize, newSize );
}
void Slots::sortIndicatorChanged( int logicalIndex, Qt::SortOrder order )
{
   QObject *object = qobject_cast<QObject *>( sender() );
   SlotsExecIntInt( object, ( char* ) "sortIndicatorChanged(int,int)", logicalIndex, order );
}
void Slots::buttonClicked( int id )
{
   QObject *object = qobject_cast<QObject *>( sender() );
   SlotsExecInt( object, ( char* ) "buttonClicked(int)", id );
}
void Slots::buttonPressed( int id )
{
   QObject *object = qobject_cast<QObject *>( sender() );
   SlotsExecInt( object, ( char* ) "buttonPressed(int)", id );
}
void Slots::buttonReleased( int id )
{
   QObject *object = qobject_cast<QObject *>( sender() );
   SlotsExecInt( object, ( char* ) "buttonReleased(int)", id );
}
void Slots::linkActivated( const QString & link )
{
   QObject *object = qobject_cast<QObject *>( sender() );
   SlotsExecString( object, ( char* ) "linkActivated(QString)", link );
}
void Slots::linkHovered( const QString & link )
{
   QObject *object = qobject_cast<QObject *>( sender() );
   SlotsExecString( object, ( char* ) "linkHovered(QString)", link );
}
void Slots::cellActivated( int row, int column )
{
   QObject *object = qobject_cast<QObject *>( sender() );
   SlotsExecIntInt( object, ( char* ) "cellActivated(int,int)", row, column );
}
void Slots::cellChanged( int row, int column )
{
   QObject *object = qobject_cast<QObject *>( sender() );
   SlotsExecIntInt( object, ( char* ) "cellChanged(int,int)", row, column );
}
void Slots::cellClicked( int row, int column )
{
   QObject *object = qobject_cast<QObject *>( sender() );
   SlotsExecIntInt( object, ( char* ) "cellClicked(int,int)", row, column );
}
void Slots::cellDoubleClicked( int row, int column )
{
   QObject *object = qobject_cast<QObject *>( sender() );
   SlotsExecIntInt( object, ( char* ) "cellDoubleClicked(int,int)", row, column );
}
void Slots::cellEntered( int row, int column )
{
   QObject *object = qobject_cast<QObject *>( sender() );
   SlotsExecIntInt( object, ( char* ) "cellEntered(int,int)", row, column );
}
void Slots::cellPressed( int row, int column )
{
   QObject *object = qobject_cast<QObject *>( sender() );
   SlotsExecIntInt( object, ( char* ) "cellEntered(int,int)", row, column );
}
void Slots::currentCellChanged( int currentRow, int currentColumn, int previousRow, int previousColumn )
{
   QObject *object = qobject_cast<QObject *>( sender() );
   SlotsExecIntIntIntInt( object, ( char* ) "currentCellChanged(int,int,int,int)", currentRow, currentColumn, previousRow, previousColumn );
}


/*
 * harbour function to connect signals with slots
 */
HB_FUNC( QT_CONNECT_SIGNAL )
{
   QString   signal    = hb_parcx( 2 );                             /* get signal    */
   PHB_ITEM  codeblock = hb_itemNew( hb_param( 3, HB_IT_BLOCK ) );  /* get codeblock */
   bool      ret       = false;                                     /* return value  */
   QObject * object    = ( QObject* ) hbqt_gcpointer( 1 );          /* get sender    */

   qt_setEventSlots();
   Slots   * s_s       = qt_getEventSlots();

   if( signal == ( QString ) "clicked()" )
   {
      ret = object->connect( object, SIGNAL( clicked() )          , s_s, SLOT( clicked() )          , Qt::AutoConnection );
   }
   if( signal == ( QString ) "returnPressed()" )
   {
      ret = object->connect( object, SIGNAL( returnPressed() )    , s_s, SLOT( returnPressed() )    , Qt::AutoConnection );
   }
   if( signal == ( QString ) "triggered()" )
   {
      ret = object->connect( object, SIGNAL( triggered() )        , s_s, SLOT( triggered() )        , Qt::AutoConnection );
   }
   if( signal == ( QString ) "hovered()" )
   {
      ret = object->connect( object, SIGNAL( hovered() )          , s_s, SLOT( hovered() )          , Qt::AutoConnection );
   }
   if( signal == ( QString ) "viewportEntered()" )
   {
      ret = object->connect( object, SIGNAL( viewportEntered() )  , s_s, SLOT( viewportEntered() ) , Qt::AutoConnection );
   }
   if( signal == ( QString ) "pressed()" )
   {
      ret = object->connect( object, SIGNAL( pressed() )          , s_s, SLOT( pressed() )          , Qt::AutoConnection );
   }
   if( signal == ( QString ) "released()" )
   {
      ret = object->connect( object, SIGNAL( released() )         ,
                             s_s, SLOT( released() ), Qt::AutoConnection );
   }
   if( signal == ( QString ) "stateChanged(int)" )
   {
      ret = object->connect( object, SIGNAL( stateChanged( int ) ),
                             s_s, SLOT( stateChanged( int ) ), Qt::AutoConnection );
   }
   if( signal == ( QString ) "activated(int)" )
   {
      ret = object->connect( object, SIGNAL( activated( int ) ),
                             s_s, SLOT( activated( int ) )   , Qt::AutoConnection );
   }
   if( signal == ( QString ) "currentIndexChanged(int)" )
   {
      ret = object->connect( object, SIGNAL( currentIndexChanged( int ) ),
                             s_s, SLOT( currentIndexChanged( int ) ), Qt::AutoConnection );
   }
   if( signal == ( QString ) "highlighted(int)" )
   {
      ret = object->connect( object, SIGNAL( highlighted( int ) ) ,
                             s_s, SLOT( highlighted( int ) ) , Qt::AutoConnection );
   }
   if( signal == ( QString ) "triggered(bool)" )
   {
      ret = object->connect( object, SIGNAL( triggered( bool ) ),
                             s_s, SLOT( triggered( bool ) )  , Qt::AutoConnection );
   }
   if( signal == ( QString ) "clicked(QModelIndex)" )
   {
      ret = object->connect( object, SIGNAL( clicked( const QModelIndex & ) ),
                             s_s, SLOT( clicked( const QModelIndex & ) ), Qt::AutoConnection );
   }
   if( signal == ( QString ) "doubleClicked(QModelIndex)" )
   {
      ret = object->connect( object, SIGNAL( doubleClicked( const QModelIndex & ) ),
                             s_s, SLOT( doubleClicked( const QModelIndex & ) ), Qt::AutoConnection );
   }
   if( signal == ( QString ) "entered(QModelIndex)" )
   {
      ret = object->connect( object, SIGNAL( entered( const QModelIndex & ) ),
                             s_s, SLOT( entered( const QModelIndex & ) ), Qt::AutoConnection );
   }
   if( signal == ( QString ) "hovered(action)" )
   {
      ret = object->connect( object, SIGNAL( hovered( QAction * ) ),
                             s_s, SLOT( hovered( QAction * ) ), Qt::AutoConnection );
   }
   if( signal == ( QString ) "currentChanged(int)" )
   {
      ret = object->connect( object, SIGNAL( currentChanged( int ) ),
                             s_s, SLOT( currentChanged( int ) ), Qt::AutoConnection );
   }
   if( signal == ( QString ) "actionTriggered(int)" )
   {
      ret = object->connect( object,  SIGNAL( actionTriggered(int) ),
                             s_s, SLOT( actionTriggered(int) ), Qt::AutoConnection );
   }
   if( signal == ( QString ) "rangeChanged(int,int)" )
   {
      ret = object->connect( object,  SIGNAL( rangeChanged(int,int) ),
                             s_s, SLOT( rangeChanged(int,int) ), Qt::AutoConnection );
   }
   if( signal == ( QString ) "sliderMoved(int)" )
   {
      ret = object->connect( object,  SIGNAL( sliderMoved(int) ),
                             s_s, SLOT( sliderMoved(int) ), Qt::AutoConnection );
   }
   if( signal == ( QString ) "sliderPressed()" )
   {
      ret = object->connect( object,  SIGNAL( sliderPressed() ),
                             s_s, SLOT( sliderPressed() ), Qt::AutoConnection );
   }
   if( signal == ( QString ) "sliderReleased()" )
   {
      ret = object->connect( object,  SIGNAL( sliderReleased() ),
                             s_s, SLOT( sliderReleased() ), Qt::AutoConnection );
   }
   if( signal == ( QString ) "valueChanged(int)" )
   {
      ret = object->connect( object,  SIGNAL( valueChanged(int) ),
                             s_s, SLOT( valueChanged(int) ), Qt::AutoConnection );
   }
   if( signal == ( QString ) "cursorPositionChanged(int,int)" )
   {
      ret = object->connect( object,  SIGNAL( cursorPositionChanged(int,int) ),
                             s_s, SLOT( cursorPositionChanged(int,int) ), Qt::AutoConnection );
   }
   if( signal == ( QString ) "editingFinished()" )
   {
      ret = object->connect( object,  SIGNAL( editingFinished() ),
                             s_s, SLOT( editingFinished() ), Qt::AutoConnection );
   }
   if( signal == ( QString ) "returnPressed()" )
   {
      ret = object->connect( object,  SIGNAL( returnPressed() ),
                             s_s, SLOT( returnPressed() ), Qt::AutoConnection );
   }
   if( signal == ( QString ) "selectionChanged()" )
   {
      ret = object->connect( object,  SIGNAL( selectionChanged() ),
                             s_s, SLOT( selectionChanged() ), Qt::AutoConnection );
   }
   if( signal == ( QString ) "textChanged(QString)" )
   {
      ret = object->connect( object,  SIGNAL( textChanged( const QString &) ),
                             s_s, SLOT( textChanged( const QString & ) ), Qt::AutoConnection );
   }
   if( signal == ( QString ) "textEdited(QString)" )
   {
      ret = object->connect( object,  SIGNAL( textEdited( const QString &) ),
                             s_s, SLOT( textEdited( const QString & ) ), Qt::AutoConnection );
   }
   /* QTreeViewWidget */
   if( signal == ( QString ) "currentItemChanged(QTWItem)" )
   {
      ret = object->connect( object,  SIGNAL( currentItemChanged( QTreeWidgetItem *, QTreeWidgetItem * ) ),
                             s_s, SLOT( currentItemChanged( QTreeWidgetItem *, QTreeWidgetItem * ) ), Qt::AutoConnection );
   }
   if( signal == ( QString ) "itemActivated(QTWItem)" )
   {
      ret = object->connect( object,  SIGNAL( itemActivated( QTreeWidgetItem *, int ) ),
                             s_s, SLOT( itemActivated( QTreeWidgetItem *, int ) ), Qt::AutoConnection );
   }
   if( signal == ( QString ) "itemChanged(QTWItem)" )
   {
      ret = object->connect( object,  SIGNAL( itemChanged( QTreeWidgetItem *, int ) ),
                             s_s, SLOT( itemChanged( QTreeWidgetItem *, int ) ), Qt::AutoConnection );
   }
   if( signal == ( QString ) "itemClicked(QTWItem)" )
   {
      ret = object->connect( object,  SIGNAL( itemClicked( QTreeWidgetItem *, int ) ),
                             s_s, SLOT( itemClicked( QTreeWidgetItem *, int ) ), Qt::AutoConnection );
   }
   if( signal == ( QString ) "itemCollapsed(QTWItem)" )
   {
      ret = object->connect( object,  SIGNAL( itemCollapsed( QTreeWidgetItem * ) ),
                             s_s, SLOT( itemCollapsed( QTreeWidgetItem * ) ), Qt::AutoConnection );
   }
   if( signal == ( QString ) "itemDoubleClicked(QTWItem)" )
   {
      ret = object->connect( object,  SIGNAL( itemDoubleClicked( QTreeWidgetItem *, int ) ),
                             s_s, SLOT( itemDoubleClicked( QTreeWidgetItem *, int ) ), Qt::AutoConnection );
   }
   if( signal == ( QString ) "itemEntered(QTWItem)" )
   {
      ret = object->connect( object,  SIGNAL( itemEntered( QTreeWidgetItem *, int ) ),
                             s_s, SLOT( itemEntered( QTreeWidgetItem *, int ) ), Qt::AutoConnection );
   }
   if( signal == ( QString ) "itemExpanded(QTWItem)" )
   {
      ret = object->connect( object,  SIGNAL( itemExpanded( QTreeWidgetItem * ) ),
                             s_s, SLOT( itemExpanded( QTreeWidgetItem * ) ), Qt::AutoConnection );
   }
   if( signal == ( QString ) "itemPressed(QTWItem)" )
   {
      ret = object->connect( object,  SIGNAL( itemPressed( QTreeWidgetItem *, int ) ),
                             s_s, SLOT( itemPressed( QTreeWidgetItem *, int ) ), Qt::AutoConnection );
   }
   if( signal == ( QString ) "itemSelectionChanged()" )
   {
      ret = object->connect( object,  SIGNAL( itemSelectionChanged() ),
                             s_s, SLOT( itemSelectionChanged() ), Qt::AutoConnection );
   }
   /* QWebView */
   if( signal == ( QString ) "iconChanged()" )
   {
      ret = object->connect( object,  SIGNAL( iconChanged() ),
                             s_s, SLOT( iconChanged() ), Qt::AutoConnection );
   }
   if( signal == ( QString ) "titleChanged(QString)" )
   {
      ret = object->connect( object,  SIGNAL( titleChanged( const QString & ) ),
                             s_s, SLOT( titleChanged( const QString & ) ), Qt::AutoConnection );
   }
   if( signal == ( QString ) "urlChanged(QUrl)" )
   {
      ret = object->connect( object,  SIGNAL( urlChanged( const QUrl & ) ),
                             s_s, SLOT( urlChanged( const QUrl & ) ), Qt::AutoConnection );
   }
   /*  QWebPage */
   if( signal == ( QString ) "contentsChanged()" )
   {
      ret = object->connect( object,  SIGNAL( contentsChanged() ),
                             s_s, SLOT( contentsChanged() ), Qt::AutoConnection );
   }
   if( signal == ( QString ) "databaseQuotaExceeded(QWebFrame,QString)" )
   {
      ret = object->connect( object,  SIGNAL( databaseQuotaExceeded( QWebFrame, QString ) ),
                             s_s, SLOT( databaseQuotaExceeded( QWebFrame, QString ) ), Qt::AutoConnection );
   }
   if( signal == ( QString ) "downloadRequested(QNetworkRequest)" )
   {
      ret = object->connect( object,  SIGNAL( downloadRequested( const QNetworkRequest & ) ),
                             s_s, SLOT( downloadRequested( const QNetworkRequest & ) ), Qt::AutoConnection );
   }
   if( signal == ( QString ) "frameCreated(QWebFrame)" )
   {
      ret = object->connect( object,  SIGNAL( frameCreated( QWebFrame * ) ),
                             s_s, SLOT( frameCreated( QWebFrame * ) ), Qt::AutoConnection );
   }
   if( signal == ( QString ) "geometryChangeRequested(QRect)" )
   {
      ret = object->connect( object,  SIGNAL( geometryChangeRequested( const QRect & ) ),
                             s_s, SLOT( geometryChangeRequested( const QRect & ) ), Qt::AutoConnection );
   }
   if( signal == ( QString ) "linkClicked(QUrl)" )
   {
      ret = object->connect( object,  SIGNAL( linkClicked( const QUrl & ) ),
                             s_s, SLOT( linkClicked( const QUrl & ) ), Qt::AutoConnection );
   }
   if( signal == ( QString ) "linkHovered(QString,QString,QString)" )
   {
      ret = object->connect( object,  SIGNAL( linkHovered( const QString &, const QString &, const QString & ) ),
                             s_s, SLOT( linkHovered( const QString &, const QString &, const QString & ) ), Qt::AutoConnection );
   }
   if( signal == ( QString ) "loadFinished(bool)" )
   {
      ret = object->connect( object,  SIGNAL( loadFinished( bool ) ),
                             s_s, SLOT( loadFinished( bool ) ), Qt::AutoConnection );
   }
   if( signal == ( QString ) "loadProgress(int)" )
   {
      ret = object->connect( object,  SIGNAL( loadProgress( int ) ),
                             s_s, SLOT( loadProgress( int ) ), Qt::AutoConnection );
   }
   if( signal == ( QString ) "loadStarted()" )
   {
      ret = object->connect( object,  SIGNAL( loadStarted() ),
                             s_s, SLOT( loadStarted() ), Qt::AutoConnection );
   }
   if( signal == ( QString ) "menuBarVisibilityChangeRequested(bool)" )
   {
      ret = object->connect( object,  SIGNAL( menuBarVisibilityChangeRequested( bool ) ),
                             s_s, SLOT( menuBarVisibilityChangeRequested( bool ) ), Qt::AutoConnection );
   }
   if( signal == ( QString ) "microFocusChanged()" )
   {
      ret = object->connect( object,  SIGNAL( microFocusChanged() ),
                             s_s, SLOT( microFocusChanged() ), Qt::AutoConnection );
   }
   if( signal == ( QString ) "printRequested(QWebFrame)" )
   {
      ret = object->connect( object,  SIGNAL( printRequested( QWebFrame * ) ),
                             s_s, SLOT( printRequested( QWebFrame * ) ), Qt::AutoConnection );
   }
   if( signal == ( QString ) "repaintRequested(QRect)" )
   {
      ret = object->connect( object,  SIGNAL( repaintRequested( const QRect & ) ),
                             s_s, SLOT( repaintRequested( const QRect & ) ), Qt::AutoConnection );
   }
   if( signal == ( QString ) "restoreFrameStateRequested(QWebFrame)" )
   {
      ret = object->connect( object,  SIGNAL( restoreFrameStateRequested( QWebFrame * ) ),
                             s_s, SLOT( restoreFrameStateRequested( QWebFrame * ) ), Qt::AutoConnection );
   }
   if( signal == ( QString ) "saveFrameStateRequested(QWebFrame,QWebHistoryItem)" )
   {
      ret = object->connect( object,  SIGNAL( saveFrameStateRequested( QWebFrame *, QWebHistoryItem * ) ),
                             s_s, SLOT( saveFrameStateRequested( QWebFrame *, QWebHistoryItem * ) ), Qt::AutoConnection );
   }
   if( signal == ( QString ) "scrollRequested(int,int,QRect)" )
   {
      ret = object->connect( object,  SIGNAL( scrollRequested( int dx, int dy, const QRect & ) ),
                             s_s, SLOT( scrollRequested( int dx, int dy, const QRect & ) ), Qt::AutoConnection );
   }
   if( signal == ( QString ) "statusBarMessage(QString)" )
   {
      ret = object->connect( object,  SIGNAL( statusBarMessage( const QString & ) ),
                             s_s, SLOT( statusBarMessage( const QString & ) ), Qt::AutoConnection );
   }
   if( signal == ( QString ) "statusBarVisibilityChangeRequested(bool)" )
   {
      ret = object->connect( object,  SIGNAL( statusBarVisibilityChangeRequested( bool ) ),
                             s_s, SLOT( statusBarVisibilityChangeRequested( bool ) ), Qt::AutoConnection );
   }
   if( signal == ( QString ) "toolBarVisibilityChangeRequested(bool)" )
   {
      ret = object->connect( object,  SIGNAL( toolBarVisibilityChangeRequested( bool ) ),
                             s_s, SLOT( toolBarVisibilityChangeRequested( bool ) ), Qt::AutoConnection );
   }
   if( signal == ( QString ) "unsupportedContent(QNetworkReply)" )
   {
      ret = object->connect( object,  SIGNAL( unsupportedContent( QNetworkReply * ) ),
                             s_s, SLOT( unsupportedContent( QNetworkReply * ) ), Qt::AutoConnection );
   }
   if( signal == ( QString ) "windowCloseRequested()" )
   {
      ret = object->connect( object,  SIGNAL( windowCloseRequested() ),
                             s_s, SLOT( windowCloseRequested() ), Qt::AutoConnection );
   }
   /* QDialog (s) QFontDialog, QFileDialog */
   if( signal == ( QString ) "currentFontChanged(QFont)" )
   {
      ret = object->connect( object,  SIGNAL( currentFontChanged( const QFont & ) ),
                             s_s, SLOT( currentFontChanged( const QFont & ) ), Qt::AutoConnection );
   }
   if( signal == ( QString ) "fontSelected(QFont)" )
   {
      ret = object->connect( object,  SIGNAL( fontSelected( const QFont & ) ),
                             s_s, SLOT( fontSelected( const QFont & ) ), Qt::AutoConnection );
   }
   if( signal == ( QString ) "accepted()" )
   {
      ret = object->connect( object,  SIGNAL( accepted() ),
                             s_s, SLOT( accepted() ), Qt::AutoConnection );
   }
   if( signal == ( QString ) "finished(int)" )
   {
      ret = object->connect( object,  SIGNAL( finished( int ) ),
                             s_s, SLOT( finished( int ) ), Qt::AutoConnection );
   }
   if( signal == ( QString ) "rejected()" )
   {
      ret = object->connect( object,  SIGNAL( rejected() ),
                             s_s, SLOT( rejected() ), Qt::AutoConnection );
   }
   if( signal == ( QString ) "currentChanged(QString)" )
   {
      ret = object->connect( object,  SIGNAL( currentChanged( const QString & ) ),
                             s_s, SLOT( currentChanged( const QString & ) ), Qt::AutoConnection );
   }
   if( signal == ( QString ) "directoryEntered(QString)" )
   {
      ret = object->connect( object,  SIGNAL( directoryEntered( const QString & ) ),
                             s_s, SLOT( directoryEntered( const QString & ) ), Qt::AutoConnection );
   }
   if( signal == ( QString ) "fileSelected(QString)" )
   {
      ret = object->connect( object,  SIGNAL( fileSelected( const QString & ) ),
                             s_s, SLOT( fileSelected( const QString & ) ), Qt::AutoConnection );
   }
   if( signal == ( QString ) "filesSelected(QStringList)" )
   {
      ret = object->connect( object,  SIGNAL( filesSelected( const QStringList & ) ),
                             s_s, SLOT( filesSelected( const QStringList & ) ), Qt::AutoConnection );
   }
   if( signal == ( QString ) "filterSelected(QString)" )
   {
      ret = object->connect( object,  SIGNAL( filterSelected( const QString & ) ),
                             s_s, SLOT( filterSelected( const QString & ) ), Qt::AutoConnection );
   }
   if( signal == ( QString ) "accepted(QPrinter)" )
   {
      ret = object->connect( object,  SIGNAL( accepted( QPrinter * ) ),
                             s_s, SLOT( accepted( QPrinter * ) ), Qt::AutoConnection );
   }
   if( signal == ( QString ) "copyAvailable(bool)" )
   {
      ret = object->connect( object,  SIGNAL( copyAvailable( bool ) ),
                             s_s, SLOT( copyAvailable( bool ) ), Qt::AutoConnection );
   }
   if( signal == ( QString ) "currentCharFormatChanged(QTextCharFormat)" )
   {
      ret = object->connect( object,  SIGNAL( currentCharFormatChanged( const QTextCharFormat & ) ),
                             s_s, SLOT( currentCharFormatChanged( const QTextCharFormat & ) ), Qt::AutoConnection );
   }
   if( signal == ( QString ) "cursorPositionChanged()" )
   {
      ret = object->connect( object,  SIGNAL( cursorPositionChanged() ),
                             s_s, SLOT( cursorPositionChanged() ), Qt::AutoConnection );
   }
   if( signal == ( QString ) "redoAvailable(bool)" )
   {
      ret = object->connect( object,  SIGNAL( redoAvailable( bool ) ),
                             s_s, SLOT( redoAvailable( bool ) ), Qt::AutoConnection );
   }
   if( signal == ( QString ) "textChanged()" )
   {
      ret = object->connect( object,  SIGNAL( textChanged() ),
                             s_s, SLOT( textChanged() ), Qt::AutoConnection );
   }
   if( signal == ( QString ) "undoAvailable(available)" )
   {
      ret = object->connect( object,  SIGNAL( undoAvailable( bool ) ),
                             s_s, SLOT( undoAvailable( bool ) ), Qt::AutoConnection );
   }
   if( signal == ( QString ) "timeout()" )
   {
      ret = object->connect( object,  SIGNAL( timeout() ),
                             s_s, SLOT( timeout() ), Qt::AutoConnection );
   }
   /* Generic purpose mechanism to receive key and mouse events off subclasses */
   if( signal == ( QString ) "keyPressEvent()" )
   {
      ret = object->connect( object, SIGNAL( sg_keyPressEvent( QKeyEvent * ) ),
                             s_s, SLOT( keyPressEvent( QKeyEvent * ) ), Qt::AutoConnection );
   }
   if( signal == ( QString ) "keyReleaseEvent()" )
   {
      ret = object->connect( object, SIGNAL( sg_keyReleaseEvent( QKeyEvent * ) ),
                             s_s, SLOT( keyReleaseEvent( QKeyEvent * ) ), Qt::AutoConnection );
   }
   if( signal == ( QString ) "mouseMoveEvent()" )
   {
      ret = object->connect( object, SIGNAL( sg_mouseMoveEvent( QMouseEvent * ) ),
                             s_s, SLOT( mouseMoveEvent( QMouseEvent * ) ), Qt::AutoConnection );
   }
   if( signal == ( QString ) "mouseDoubleClickEvent()" )
   {
      ret = object->connect( object, SIGNAL( sg_mouseDoubleClickEvent( QMouseEvent * ) ),
                             s_s, SLOT( mouseDoubleClickEvent( QMouseEvent * ) ), Qt::AutoConnection );
   }
   if( signal == ( QString ) "mousePressEvent()" )
   {
      ret = object->connect( object, SIGNAL( sg_mousePressEvent( QMouseEvent * ) ),
                             s_s, SLOT( mousePressEvent( QMouseEvent * ) ), Qt::AutoConnection );
   }
   if( signal == ( QString ) "mouseReleaseEvent()" )
   {
      ret = object->connect( object, SIGNAL( sg_mouseReleaseEvent( QMouseEvent * ) ),
                             s_s, SLOT( mouseReleaseEvent( QMouseEvent * ) ), Qt::AutoConnection );
   }
   if( signal == ( QString ) "wheelEvent()" )
   {
      ret = object->connect( object, SIGNAL( sg_wheelEvent( QWheelEvent * ) ),
                             s_s, SLOT( wheelEvent( QWheelEvent * ) ), Qt::AutoConnection );
   }
   if( signal == ( QString ) "resizeEvent()" )
   {
      ret = object->connect( object, SIGNAL( sg_resizeEvent( QResizeEvent * ) ),
                             s_s, SLOT( resizeEvent( QResizeEvent * ) ), Qt::AutoConnection );
   }
   if( signal == ( QString ) "scrollContentsBy(int,int)" )
   {
      ret = object->connect( object, SIGNAL( sg_scrollContentsBy( int, int ) ),
                             s_s, SLOT( scrollContentsBy( int, int ) ), Qt::AutoConnection );
   }
   if( signal == ( QString ) "geometriesChanged()" )
   {
      ret = object->connect( object, SIGNAL( geometriesChanged() ),
                             s_s, SLOT( geometriesChanged() ), Qt::AutoConnection );
   }
   if( signal == ( QString ) "sectionAutoResize(int,int)" )
   {
      ret = object->connect( object, SIGNAL( sectionAutoResize( int, QHeaderView::ResizeMode ) ),
                             s_s, SLOT( sectionAutoResize( int, QHeaderView::ResizeMode ) ), Qt::AutoConnection );
   }
   if( signal == ( QString ) "sectionClicked(int)" )
   {
      ret = object->connect( object, SIGNAL( sectionClicked( int ) ),
                             s_s, SLOT( sectionClicked( int ) ), Qt::AutoConnection );
   }
   if( signal == ( QString ) "sectionCountChanged(int,int)" )
   {
      ret = object->connect( object, SIGNAL( sectionCountChanged( int, int ) ),
                             s_s, SLOT( sectionCountChanged( int, int ) ), Qt::AutoConnection );
   }
   if( signal == ( QString ) "sectionDoubleClicked(int)" )
   {
      ret = object->connect( object, SIGNAL( sectionDoubleClicked( int ) ),
                             s_s, SLOT( sectionDoubleClicked( int ) ), Qt::AutoConnection );
   }
   if( signal == ( QString ) "sectionEntered(int)" )
   {
      ret = object->connect( object, SIGNAL( sectionEntered( int ) ),
                             s_s, SLOT( sectionEntered( int ) ), Qt::AutoConnection );
   }
   if( signal == ( QString ) "sectionHandleDoubleClicked(int)" )
   {
      ret = object->connect( object, SIGNAL( sectionHandleDoubleClicked( int ) ),
                             s_s, SLOT( sectionHandleDoubleClicked( int ) ), Qt::AutoConnection );
   }
   if( signal == ( QString ) "sectionMoved(int,int,int)" )
   {
      ret = object->connect( object, SIGNAL( sectionMoved( int, int, int ) ),
                             s_s, SLOT( sectionMoved( int, int, int ) ), Qt::AutoConnection );
   }
   if( signal == ( QString ) "sectionPressed(int)" )
   {
      ret = object->connect( object, SIGNAL( sectionPressed( int ) ),
                             s_s, SLOT( sectionPressed( int ) ), Qt::AutoConnection );
   }
   if( signal == ( QString ) "sectionResized(int,int,int)" )
   {
      ret = object->connect( object, SIGNAL( sectionResized( int, int, int ) ),
                             s_s, SLOT( sectionResized( int, int, int ) ), Qt::AutoConnection );
   }
   if( signal == ( QString ) "sortIndicatorChanged(int,int)" )
   {
      ret = object->connect( object, SIGNAL( sortIndicatorChanged( int, Qt::SortOrder ) ),
                             s_s, SLOT( sortIndicatorChanged( int, Qt::SortOrder ) ), Qt::AutoConnection );
   }
   if( signal == ( QString ) "buttonClicked(int)" )
   {
      ret = object->connect( object, SIGNAL( buttonClicked( int ) ),
                             s_s, SLOT( buttonClicked( int ) ), Qt::AutoConnection );
   }
   if( signal == ( QString ) "buttonPressed(int)" )
   {
      ret = object->connect( object, SIGNAL( buttonPressed( int ) ),
                             s_s, SLOT( buttonPressed( int ) ), Qt::AutoConnection );
   }
   if( signal == ( QString ) "buttonReleased(int)" )
   {
      ret = object->connect( object, SIGNAL( buttonReleased( int ) ),
                             s_s, SLOT( buttonReleased( int ) ), Qt::AutoConnection );
   }
   if( signal == ( QString ) "linkActivated(QString)" )
   {
      ret = object->connect( object, SIGNAL( linkActivated( const QString & ) ),
                             s_s, SLOT( linkActivated( const QString & ) ), Qt::AutoConnection );
   }
   if( signal == ( QString ) "linkHovered(QString)" )
   {
      ret = object->connect( object, SIGNAL( linkHovered( const QString & ) ),
                             s_s, SLOT( linkHovered( const QString & ) ), Qt::AutoConnection );
   }
   if( signal == ( QString ) "cellActivated(int,int)" )
   {
      ret = object->connect( object, SIGNAL( cellActivated( int, int ) ),
                             s_s, SLOT( cellActivated( int, int ) ), Qt::AutoConnection );
   }
   if( signal == ( QString ) "cellChanged(int,int)" )
   {
      ret = object->connect( object, SIGNAL( cellChanged( int, int ) ),
                             s_s, SLOT( cellChanged( int, int ) ), Qt::AutoConnection );
   }
   if( signal == ( QString ) "cellClicked(int,int)" )
   {
      ret = object->connect( object, SIGNAL( cellClicked( int, int ) ),
                             s_s, SLOT( cellClicked( int, int ) ), Qt::AutoConnection );
   }
   if( signal == ( QString ) "cellDoubleClicked(int,int)" )
   {
      ret = object->connect( object, SIGNAL( cellDoubleClicked( int, int ) ),
                             s_s, SLOT( cellDoubleClicked( int, int ) ), Qt::AutoConnection );
   }
   if( signal == ( QString ) "cellEntered(int,int)" )
   {
      ret = object->connect( object, SIGNAL( cellEntered( int, int ) ),
                             s_s, SLOT( cellEntered( int, int ) ), Qt::AutoConnection );
   }
   if( signal == ( QString ) "cellPressed(int,int)" )
   {
      ret = object->connect( object, SIGNAL( cellPressed( int, int ) ),
                             s_s, SLOT( cellPressed( int, int ) ), Qt::AutoConnection );
   }
   if( signal == ( QString ) "currentCellChanged(int,int,int,int)" )
   {
      ret = object->connect( object, SIGNAL( currentCellChanged( int, int, int, int ) ),
                             s_s, SLOT( currentCellChanged( int, int, int, int ) ), Qt::AutoConnection );
   }


   hb_retl( ret );

   if( ret == true )
   {
      s_s->listBlock  << codeblock;
      s_s->listActv   << true;
      object->setProperty( hb_parcx( 2 ), ( int ) s_s->listBlock.size() );
   }
}

/*
 * harbour function to disconnect signals
 */
HB_FUNC( QT_DISCONNECT_SIGNAL )
{
   QObject * object = ( QObject* ) hbqt_gcpointer( 1 );

   if( object )
   {
      Slots * s_s = qt_getEventSlots();
      const char * event = hb_parcx( 2 );
      int i = object->property( event ).toInt();

      if( i > 0 && i <= s_s->listBlock.size() )
      {
         hb_itemRelease( s_s->listBlock.at( i - 1 ) );
         s_s->listBlock[ i - 1 ] = NULL;
         s_s->listActv[ i - 1 ] = false;
      }
   }
}


/*
 * harbour function to release all codeblocks storeds
 */
HB_FUNC( RELEASE_CODEBLOCKS )
{
   Slots * s_s = qt_getEventSlots();

   if( s_s )
   {
      for( int i = 0; i < s_s->listBlock.size(); ++i )
      {
         if( ( bool ) s_s->listActv.at( i ) == true )
         {
            hb_itemRelease( s_s->listBlock.at( i ) );
            s_s->listBlock[ i ] = NULL;
            s_s->listActv[ i ] = false;
         }
      }
   }
}

/*
 * C function to release all codeblocks storeds
 * called at end of the program
 * see qapplication.cpp
 */
void release_codeblocks( void )
{
   Slots * s_s = qt_getEventSlots();
   if( s_s )
   {
      for( int i = 0; i < s_s->listBlock.size(); ++i )
      {
         if( ( bool ) s_s->listActv.at( i ) == true )
         {
            hb_itemRelease( s_s->listBlock.at( i ) );
         }
      }
   }
}

/*----------------------------------------------------------------------*/
Events::Events( QObject * parent ) : QObject( parent )
{
}

Events::~Events()
{
}

bool Events::eventFilter( QObject * object, QEvent * event )
{
   QEvent::Type eventtype = event->type();
#if defined(__debug__)
//hb_snprintf( str, sizeof( str ), "0 Events::eventFilter = %i", ( int ) eventtype ); OutputDebugString( str );
#endif
   if( ( int ) eventtype == 0 )
   {
//hb_snprintf( str, sizeof( str ), "x Events::eventFilter =            0" ); OutputDebugString( str );
      return false;
   }

   char prop[ 10 ];
   hb_snprintf( prop, sizeof( prop ), "%s%i%s", "P", eventtype, "P" );
   int found = object->property( prop ).toInt();
   if( found == 0 )
   {
//hb_snprintf( str, sizeof( str ), "f Events::eventFilter = %s  %i", "       found=0", ( int ) eventtype ); OutputDebugString( str );
      return false;
   }

   //bool ret = false;
   bool ret = true;
   if( found <= listBlock.size() && listObj.at( found - 1 ) == object && hb_vmRequestReenter() )
   {
      PHB_ITEM pObject = hb_itemPutPtr( NULL, object );
      PHB_ITEM pEvent  = hb_itemPutPtr( NULL, event  );
//hb_snprintf( str, sizeof( str ), "0 Events::eventFilter = %i", ( int ) eventtype ); OutputDebugString( str );
      ret = hb_itemGetL( hb_vmEvalBlockV( ( PHB_ITEM ) listBlock.at( found - 1 ), 2, pObject, pEvent ) );
//hb_snprintf( str, sizeof( str ), "1 Events::eventFilter = %s", ret ? "   yes" : "   no" ); OutputDebugString( str );
      hb_itemRelease( pObject );
      hb_itemRelease( pEvent  );

      if( eventtype == QEvent::Close )
      {
         #if 0
         if( ret == true )
            event->accept();
         else
            event->ignore();
         #endif
         event->ignore();
      }
   }
//hb_snprintf( str, sizeof( str ), "1 Events::eventFilter = %i", ( int ) eventtype ); OutputDebugString( str );
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
   s_e->listActv  << true;
   s_e->listObj   << object;

   object->setProperty( prop, ( int ) s_e->listBlock.size() );

   hb_retl( true );
}

HB_FUNC( QT_DISCONNECT_EVENT )
{
   int       type   = hb_parni( 2 );
   bool      bRet   = false;
   Events  * s_e    = qt_getEventFilter();
   QObject * object    = ( QObject* ) hbqt_gcpointer( 1 );

   char prop[ 10 ];
   hb_snprintf( prop, sizeof( prop ), "%s%i%s", "P", type, "P" );    /* Make it a unique identifier */

   int i = object->property( prop ).toInt();
   if( i > 0 && i <= s_e->listBlock.size() )
   {
      hb_itemRelease( s_e->listBlock.at( i - 1 ) );
      s_e->listBlock[ i - 1 ] = NULL;
      s_e->listActv[ i - 1 ] = false;
      bRet = true;
   }
   hb_retl( bRet );
}

/*----------------------------------------------------------------------*/

HB_FUNC( QT_QDEBUG )
{
   qDebug( "%s", hb_parcx( 1 ) );
}

/*----------------------------------------------------------------------*/

HbTableView::HbTableView( QWidget * parent ) : QTableView( parent )
{
}
HbTableView::~HbTableView()
{
}
void HbTableView::keyPressEvent( QKeyEvent * event )
{
   emit sg_keyPressEvent( event );
}
void HbTableView::mouseDoubleClickEvent( QMouseEvent * event )
{
   emit sg_mouseDoubleClickEvent( event );
}
void HbTableView::mouseMoveEvent( QMouseEvent * event )
{
   emit sg_mouseMoveEvent( event );
}
void HbTableView::mousePressEvent( QMouseEvent * event )
{
   emit sg_mousePressEvent( event );
}
void HbTableView::mouseReleaseEvent( QMouseEvent * event )
{
   emit sg_mouseReleaseEvent( event );
}
void HbTableView::wheelEvent( QWheelEvent * event )
{
   emit sg_wheelEvent( event );
}
void HbTableView::resizeEvent( QResizeEvent * event )
{
   emit sg_resizeEvent( event );
}
QModelIndex HbTableView::moveCursor( HbTableView::CursorAction cursorAction, Qt::KeyboardModifiers modifiers )
{
// char str[ 50 ]; hb_snprintf( str, sizeof( str ), "HbTableView: action=%i %i", cursorAction, QAbstractItemView::MoveDown );  OutputDebugString( str );

   //emit sg_moveCursor( cursorAction, modifiers );
   return QTableView::moveCursor( cursorAction, modifiers );
}
QModelIndex HbTableView::navigate( int cursorAction )
{
   return moveCursor( ( HbTableView::CursorAction ) cursorAction, ( Qt::KeyboardModifiers ) 0 );
}
void HbTableView::scrollContentsBy( int x, int y )
{
   emit sg_scrollContentsBy( x, y );
}
void HbTableView::scrollTo( const QModelIndex & index, QAbstractItemView::ScrollHint hint )
{
//char str[ 50 ]; hb_snprintf( str, sizeof( str ), "HbTableView:scrollTo row = %i col = %i", index.row(),index.column() );  OutputDebugString( str );
   QTableView::scrollTo( index, hint );
}

/*----------------------------------------------------------------------*/

#if 0
Qt_DisplayRole              0       Qt_FontRole                    6
Qt_DecorationRole           1       Qt_TextAlignmentRole           7
Qt_EditRole                 2       Qt_BackgroundRole              8
Qt_ToolTipRole              3       Qt_ForegroundRole              9
Qt_StatusTipRole            4       Qt_CheckStateRole              10
Qt_WhatsThisRole            5       Qt_SizeHintRole                13
#endif

#define HBQT_BRW_CELLVALUE                        1001

#define HBQT_BRW_COLCOUNT                         1002
#define HBQT_BRW_ROWCOUNT                         1003

#define HBQT_BRW_COLHEADER                        1004
#define HBQT_BRW_COLALIGN                         1006
#define HBQT_BRW_COLFGCOLOR                       1007
#define HBQT_BRW_COLBGCOLOR                       1008
#define HBQT_BRW_COLHEIGHT                        1011

#define HBQT_BRW_ROWHEADER                        1005
#define HBQT_BRW_DATFGCOLOR                       1009
#define HBQT_BRW_DATBGCOLOR                       1010
#define HBQT_BRW_DATHEIGHT                        1012
#define HBQT_BRW_DATALIGN                         1013
#define HBQT_BRW_CELLDECORATION                   1014

QVariant fetchRole( PHB_ITEM block, int what, int par1, int par2 )
{
   QVariant vv;
   if( hb_vmRequestReenter() )
   {
      PHB_ITEM p0  = hb_itemPutNI( NULL, what );
      PHB_ITEM p1  = hb_itemPutNI( NULL, par1 );
      PHB_ITEM p2  = hb_itemPutNI( NULL, par2 );

      PHB_ITEM ret = hb_vmEvalBlockV( block, 3, p0, p1, p2 );

      hb_itemRelease( p0 );
      hb_itemRelease( p1 );
      hb_itemRelease( p2 );

      if( hb_itemType( ret ) & HB_IT_STRING )
         vv = hb_itemGetCPtr( ret );
      else if( hb_itemType( ret ) & HB_IT_LOGICAL )
         vv = hb_itemGetL( ret );
      else if( hb_itemType( ret ) & HB_IT_DOUBLE  )
         vv = hb_itemGetND( ret );
      else if( hb_itemType( ret ) & HB_IT_NUMERIC )
         vv = hb_itemGetNI( ret );
   }
   return vv;
}

HbDbfModel::HbDbfModel( PHB_ITEM pBlock ) : QAbstractItemModel()
{
   block = hb_itemNew( pBlock );
   iRows = 0;
   iCols = 0;
}
HbDbfModel::~HbDbfModel()
{
   hb_itemRelease( block );
}
Qt::ItemFlags HbDbfModel::flags( const QModelIndex & index ) const
{
   if( ! index.isValid() )
      return 0;

   return( Qt::ItemIsEnabled | Qt::ItemIsSelectable );
}

QVariant HbDbfModel::data( const QModelIndex & index, int role ) const
{
//hb_snprintf( str, sizeof( str ), "data - row=%i col=%i role=%i", index.row(), index.column(), role );  OutputDebugString( str );
   if( !index.isValid() )
      return( QVariant() );

   switch( role )
   {
      case Qt::SizeHintRole:
      {
         int iHeight = fetchRole( block, HBQT_BRW_DATHEIGHT, index.row()+1, index.column()+1 ).toInt();
         return( QSize( 20, iHeight ) );
      }
      case Qt::TextAlignmentRole:
      {
         return( fetchRole( block, HBQT_BRW_DATALIGN, index.row()+1, index.column()+1 ) );
      }
      case Qt::BackgroundRole:
      {
         int iClr = fetchRole( block, HBQT_BRW_DATBGCOLOR, index.row()+1, index.column()+1 ).toInt();
         if( iClr < 25 )
            return( QColor( ( Qt::GlobalColor ) iClr ) );
         else
            return( QColor( ( QRgb ) iClr ) );
      }
      case Qt::ForegroundRole:
      {
         int iClr = fetchRole( block, HBQT_BRW_DATFGCOLOR, index.row()+1, index.column()+1 ).toInt();
         if( iClr < 25 )
            return( QColor( ( Qt::GlobalColor ) iClr ) );
         else
            return( QColor( ( QRgb ) iClr ) );
      }
      case Qt::DecorationRole:
      {
         QVariant image = fetchRole( block, HBQT_BRW_CELLDECORATION, index.row()+1, index.column()+1 );
         if( image.toString() == ( QString ) "" )
            return( QVariant() );
         return( QIcon( image.toString() ) );
         //return( QPixmap( image.toString() ) );
      }
      case Qt::DisplayRole:
      {
         return( fetchRole( block, HBQT_BRW_CELLVALUE, index.row()+1, index.column()+1 ) );
      }
   }
   return( QVariant() );
}

QVariant HbDbfModel::headerData( int section, Qt::Orientation orientation, int role ) const
{
//hb_snprintf( str, sizeof( str ), "headerData - section=%i orient=%i role=%i name=%s", section, orientation, role, objectName() );  OutputDebugString( str );

   if( orientation == Qt::Horizontal )
   {
      switch( role )
      {
         case Qt::TextAlignmentRole:
         {
            return( fetchRole( block, HBQT_BRW_COLALIGN, 0, section+1 ).toInt() );
         }
         case Qt::SizeHintRole:
         {
            int iHeight = fetchRole( block, HBQT_BRW_COLHEIGHT, 0, section+1 ).toInt();
            return( QSize( 20, iHeight ) );
         }
         case Qt::BackgroundRole:
         {
            int iClr = fetchRole( block, HBQT_BRW_COLBGCOLOR, 0, section+1 ).toInt();
            if( iClr < 25 )
               return( QColor( ( Qt::GlobalColor ) iClr ) );
            else
               return( QColor( ( QRgb ) iClr ) );
         }
         case Qt::ForegroundRole:
         {
            int iClr = fetchRole( block, HBQT_BRW_COLFGCOLOR, 0, section+1 ).toInt();
            if( iClr < 25 )
               return( QColor( ( Qt::GlobalColor ) iClr ) );
            else
               return( QColor( ( QRgb ) iClr ) );
         }
         case Qt::DisplayRole:
         {
            return( fetchRole( block, HBQT_BRW_COLHEADER, 0, section+1 ) );
         }
         case Qt::FontRole:
         case Qt::DecorationRole:
            break;
      }
   }

   if( orientation == Qt::Vertical && role == Qt::DisplayRole )
   {
      return( section + 1 );
   }

   return QVariant();
}

int HbDbfModel::rowCount( const QModelIndex & /*parent = QModelIndex()*/ ) const
{
   #if 1
   if( hb_vmRequestReenter() )
   {
      PHB_ITEM p0 = hb_itemPutNI( NULL, HBQT_BRW_ROWCOUNT );

      int result = hb_itemGetNI( hb_vmEvalBlockV( block, 1, p0 ) );

      hb_itemRelease( p0 );

      hb_vmRequestRestore();
      return result;
   }
   else
   {
      return 0;
   }
   #else
   return iRows;
   #endif
}

int HbDbfModel::columnCount( const QModelIndex & /*parent = QModelIndex()*/ ) const
{
   #if 1
   if( hb_vmRequestReenter() )
   {
      PHB_ITEM p0 = hb_itemPutNI( NULL, HBQT_BRW_COLCOUNT );

      int result = hb_itemGetNI( hb_vmEvalBlockV( block, 1, p0 ) );

      hb_itemRelease( p0 );
      hb_vmRequestRestore();
      return result;
   }
   else
   {
      return 0;
   }
   #else
   return iCols;
   #endif
}

QModelIndex HbDbfModel::index( int row, int column, const QModelIndex & parent ) const
{
//hb_snprintf( str, sizeof( str ), "index:  row=%i col=%i", row, column );  OutputDebugString( str );
   HB_SYMBOL_UNUSED( parent );

   return( createIndex( row, column, row * column ) );
}

QModelIndex HbDbfModel::parent( const QModelIndex & /* child */ ) const
{
   return QModelIndex();
}

void HbDbfModel::reset()
{
   QAbstractItemModel::reset();
}

void HbDbfModel::hbSetRowColumns( int rows, int cols )
{
   iRows = rows;
   iCols = cols;
}

void HbDbfModel::destroy()
{
   hb_itemRelease( block );
   //HbDbfModel::~HbDbfModel();
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
   activated = true;
   painter   = new QPainter( this );
}
MyMainWindow::~MyMainWindow( void )
{
   destroy();
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

   // QWidget::paintEvent( event );
   // QWidget::render( painter, QPoint(), event->region(), QWidget::DrawWindowBackground | QWidget::DrawChildren );
   // QWidget::render( painter, QPoint(), event->region(), QWidget::DrawChildren );

   /* This is an ugly hack to control the way Qt executes its Paint Engine
    * which appears to be "not reentrant". If paint message is received by two
    * dialogs executing in different Harbour threads, appln GPF's.
    * If current thread is forced to release CPU time then everything seems to be
    * working fine. But then this slows down the navigation considerably.
    * Until this issue is resolved, it seems, we cannot implement multi-window, multi-thread
    * interface in Harbour. So sad...
    */
   #if defined( QT_EXECUTE_IN_THREADS )
      hb_idleSleep( 0.2 );
   #endif

   hb_threadMutexUnlock( s_mutex );
}
bool MyMainWindow::event( QEvent * event )
{
   int type = event->type();
//hb_snprintf( str, sizeof( str ), "                      event(%i) %i", threadID , type );  OutputDebugString( str );
   if( type == QEvent::WindowActivate || type == QEvent::Enter )
   {
      activated = true;
   }
   hb_threadMutexLock( s_mutex );
   #if 0
   if( hb_vmRequestReenter() )
   {
      PHB_ITEM p0  = hb_itemPutNI( NULL, type );
      PHB_ITEM p1  = hb_itemPutPtr( NULL, event );
      hb_vmEvalBlockV( block, 2, p0, p1 );
      hb_itemRelease( p0 );
      hb_itemRelease( p1 );
      hb_vmRequestRestore();
   }
   #endif
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
   activated = true;
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

HB_FUNC( QT_MYMAINWINDOW )
{
   PHB_ITEM bBlock = hb_itemNew( ( PHB_ITEM ) hb_param( 1, HB_IT_BLOCK ) );
   hb_retptr( ( MyMainWindow * ) new MyMainWindow( bBlock, hb_parni( 2 ) ) );
}
HB_FUNC( QT_MYMAINWINDOW_DESTROY )
{
   hb_itemRelease( hbqt_par_MyMainWindow( 1 )->block );
}
HB_FUNC( QT_MUTEXCREATE )
{
   if( s_mutex == NULL )
   {
      s_mutex = hb_threadMutexCreate( FALSE );
   }
}
/*----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*/

MyDrawingArea::MyDrawingArea(QWidget *parent)
    : QWidget(parent)
{
   setAttribute( Qt::WA_StaticContents );
   setAttribute( Qt::WA_PaintOnScreen );
   setAttribute( Qt::WA_DeleteOnClose );
   setAttribute( Qt::WA_WindowPropagation );

   setFocusPolicy( Qt::StrongFocus );
   setMouseTracking( true );

   setAttribute( Qt::WA_InputMethodEnabled, true );
}
MyDrawingArea::~MyDrawingArea( void )
{
}
void MyDrawingArea::mouseMoveEvent( QMouseEvent * event )
{
   emit sg_mouseMoveEvent( event );
}
void MyDrawingArea::keyPressEvent( QKeyEvent * event )
{
   emit sg_keyPressEvent( event );
}
HB_FUNC( QT_MYDRAWINGAREA )
{
   hb_retptr( ( MyDrawingArea * ) new MyDrawingArea() );
}

/*----------------------------------------------------------------------*/

#endif
