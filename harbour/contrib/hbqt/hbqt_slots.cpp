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

#include "hbqt.h"

#if QT_VERSION >= 0x040500

#include "hbqt_slots.h"

//             #include <windows.h>   ////////////////////////////////////////////////////

#include <QWidget>
#include <QString>
#include <QList>
#include <QKeyEvent>
#include <QAction>
#include <QObject>
#include <QEvent>

/*----------------------------------------------------------------------*/

static Slots  *s_s = NULL;
static Events *s_e = NULL;

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
      int i = object->property( event ).toInt();
      if( ( i > 0 ) && ( s_s->listActv.at( i - 1 ) == true ) )
      {
         PHB_ITEM pObject = hb_itemPutPtr( NULL, ( QObject* ) object );
         hb_vmEvalBlockV( ( PHB_ITEM ) s_s->listBlock.at( i - 1 ), 1, pObject );
         hb_itemRelease( pObject );
      }
   }
}

static void SlotsExecBool( QObject* object, char* event, bool bBool )
{
   if( object )
   {
      int i = object->property( event ).toInt();
      if( ( i > 0 ) && ( s_s->listActv.at( i - 1 ) == true ) )
      {
         PHB_ITEM pObject = hb_itemPutPtr( NULL, object );
         PHB_ITEM pBool = hb_itemPutL( NULL, bBool );
         hb_vmEvalBlockV( ( PHB_ITEM ) s_s->listBlock.at( i - 1 ), 2, pObject, pBool );
         hb_itemRelease( pObject );
         hb_itemRelease( pBool );
      }
   }
}

static void SlotsExecInt( QObject* object, char* event, int iValue )
{
   if( object )
   {
      int i = object->property( event ).toInt();
      if( ( i > 0 ) && ( s_s->listActv.at( i - 1 ) == true ) )
      {
         PHB_ITEM pObject = hb_itemPutPtr( NULL, ( QObject* ) object );
         PHB_ITEM pState = hb_itemPutNI( NULL, iValue );
         hb_vmEvalBlockV( ( PHB_ITEM ) s_s->listBlock.at( i - 1 ), 2, pObject, pState );
         hb_itemRelease( pObject );
         hb_itemRelease( pState );
      }
   }
}

static void SlotsExecIntInt( QObject* object, char* event, int iValue1, int iValue2 )
{
   if( object )
   {
      int i = object->property( event ).toInt();
      if( i > 0 && ( s_s->listActv.at( i - 1 ) == true ) )
      {
         PHB_ITEM pObject = hb_itemPutPtr( NULL, ( QObject* ) object );
         PHB_ITEM pValue1 = hb_itemPutNI( NULL, iValue1 );
         PHB_ITEM pValue2 = hb_itemPutNI( NULL, iValue2 );
         hb_vmEvalBlockV( ( PHB_ITEM ) s_s->listBlock.at( i - 1 ), 3, pObject, pValue1, pValue2 );
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
      int i = object->property( event ).toInt();
      if( i > 0 && ( s_s->listActv.at( i - 1 ) == true ) )
      {
         PHB_ITEM pObject = hb_itemPutPtr( NULL, ( QObject* ) object );
         PHB_ITEM pValue1 = hb_itemPutNI( NULL, iValue1 );
         PHB_ITEM pValue2 = hb_itemPutNI( NULL, iValue2 );
         PHB_ITEM pValue3 = hb_itemPutNI( NULL, iValue3 );
         hb_vmEvalBlockV( ( PHB_ITEM ) s_s->listBlock.at( i - 1 ), 4, pObject, pValue1, pValue2, pValue3 );
         hb_itemRelease( pObject );
         hb_itemRelease( pValue1 );
         hb_itemRelease( pValue2 );
         hb_itemRelease( pValue3 );
      }
   }
}

static void SlotsExecIntIntRect( QObject* object, char* event, int iValue1, int iValue2, const QRect & rect )
{
   if( object )
   {
      int i = object->property( event ).toInt();
      if( i > 0 && ( s_s->listActv.at( i - 1 ) == true ) )
      {
         PHB_ITEM pObject = hb_itemPutPtr( NULL, ( QObject* ) object );
         PHB_ITEM pValue1 = hb_itemPutNI( NULL, iValue1 );
         PHB_ITEM pValue2 = hb_itemPutNI( NULL, iValue2 );
         PHB_ITEM pValue3 = hb_itemPutPtr( NULL, new QRect( rect ) );
         hb_vmEvalBlockV( ( PHB_ITEM ) s_s->listBlock.at( i - 1 ), 4, pObject, pValue1, pValue2, pValue3 );
         hb_itemRelease( pObject );
         hb_itemRelease( pValue1 );
         hb_itemRelease( pValue2 );
         hb_itemRelease( pValue3 );
      }
   }
}

static void SlotsExecString( QObject* object, char* event, const QString & string )
{
   if( object )
   {
      int i = object->property( event ).toInt();
      if( i > 0 && ( s_s->listActv.at( i - 1 ) == true ) )
      {
         PHB_ITEM pObject = hb_itemPutPtr( NULL, ( QObject* ) object );
         PHB_ITEM pString = hb_itemPutC( NULL, string.toLatin1().data() );
         hb_vmEvalBlockV( ( PHB_ITEM ) s_s->listBlock.at( i - 1 ), 2, pObject, pString );
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
      int i = object->property( event ).toInt();
      if( i > 0 && ( s_s->listActv.at( i - 1 ) == true ) )
      {
         PHB_ITEM pObject = hb_itemPutPtr( NULL, ( QObject* ) object );
         PHB_ITEM pS1 = hb_itemPutC( NULL, s1.toLatin1().data() );
         PHB_ITEM pS2 = hb_itemPutC( NULL, s2.toLatin1().data() );
         hb_vmEvalBlockV( ( PHB_ITEM ) s_s->listBlock.at( i - 1 ), 3, pObject, pS1, pS2 );
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
      int i = object->property( event ).toInt();
      if( i > 0 && ( s_s->listActv.at( i - 1 ) == true ) )
      {
         PHB_ITEM pObject = hb_itemPutPtr( NULL, ( QObject* ) object );
         PHB_ITEM pS1 = hb_itemPutC( NULL, s1.toLatin1().data() );
         PHB_ITEM pS2 = hb_itemPutC( NULL, s2.toLatin1().data() );
         PHB_ITEM pS3 = hb_itemPutC( NULL, s3.toLatin1().data() );
         hb_vmEvalBlockV( ( PHB_ITEM ) s_s->listBlock.at( i - 1 ), 4, pObject, pS1, pS2, pS3 );
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
      int i = object->property( event ).toInt();
      if( ( i > 0 ) && ( s_s->listActv.at( i - 1 ) == true ) )
      {
         PHB_ITEM pObject = hb_itemPutPtr( NULL, ( QObject* ) object );
         PHB_ITEM pState = hb_itemPutPtr( NULL, ( QModelIndex * ) new QModelIndex( index ) );
         hb_vmEvalBlockV( ( PHB_ITEM ) s_s->listBlock.at( i - 1 ), 2, pObject, pState );
         hb_itemRelease( pObject );
         hb_itemRelease( pState );
      }
   }
}

static void SlotsExecRect( QObject* object, char* event, const QRect & rect )
{
   if( object )
   {
      int i = object->property( event ).toInt();
      if( ( i > 0 ) && ( s_s->listActv.at( i - 1 ) == true ) )
      {
         PHB_ITEM pObject = hb_itemPutPtr( NULL, ( QObject* ) object );
         PHB_ITEM p1 = hb_itemPutPtr( NULL, new QRect( rect ) );
         hb_vmEvalBlockV( ( PHB_ITEM ) s_s->listBlock.at( i - 1 ), 2, pObject, p1 );
         hb_itemRelease( pObject );
         hb_itemRelease( p1 );
      }
   }
}

static void SlotsExecUrl( QObject* object, char* event, const QUrl & url )
{
   if( object )
   {
      int i = object->property( event ).toInt();
      if( ( i > 0 ) && ( s_s->listActv.at( i - 1 ) == true ) )
      {
         PHB_ITEM pObject = hb_itemPutPtr( NULL, ( QObject* ) object );
         PHB_ITEM p1 = hb_itemPutPtr( NULL, new QUrl( url ) );
         hb_vmEvalBlockV( ( PHB_ITEM ) s_s->listBlock.at( i - 1 ), 2, pObject, p1 );
         hb_itemRelease( pObject );
         hb_itemRelease( p1 );
      }
   }
}

static void SlotsExecTextCharFormat( QObject* object, char* event, const QTextCharFormat & f )
{
   if( object )
   {
      int i = object->property( event ).toInt();
      if( ( i > 0 ) && ( s_s->listActv.at( i - 1 ) == true ) )
      {
         PHB_ITEM pObject = hb_itemPutPtr( NULL, ( QObject* ) object );
         PHB_ITEM p1 = hb_itemPutPtr( NULL, new QTextCharFormat( f ) );
         hb_vmEvalBlockV( ( PHB_ITEM ) s_s->listBlock.at( i - 1 ), 2, pObject, p1 );
         hb_itemRelease( pObject );
         hb_itemRelease( p1 );
      }
   }
}

static void SlotsExecFont( QObject* object, char* event, const QFont & font )
{
   if( object )
   {
      int i = object->property( event ).toInt();
      if( ( i > 0 ) && ( s_s->listActv.at( i - 1 ) == true ) )
      {
         PHB_ITEM pObject = hb_itemPutPtr( NULL, ( QObject* ) object );
         PHB_ITEM p1 = hb_itemPutPtr( NULL, new QFont( font ) );
         hb_vmEvalBlockV( ( PHB_ITEM ) s_s->listBlock.at( i - 1 ), 2, pObject, p1 );
         hb_itemRelease( pObject );
         hb_itemRelease( p1 );
      }
   }
}

static void SlotsExecStringList( QObject* object, char* event, const QStringList & stringList )
{
   if( object )
   {
      int i = object->property( event ).toInt();
      if( ( i > 0 ) && ( s_s->listActv.at( i - 1 ) == true ) )
      {
         PHB_ITEM pObject = hb_itemPutPtr( NULL, ( QObject* ) object );
         PHB_ITEM p1 = hb_itemPutPtr( NULL, new QStringList( stringList ) );
         hb_vmEvalBlockV( ( PHB_ITEM ) s_s->listBlock.at( i - 1 ), 2, pObject, p1 );
         hb_itemRelease( pObject );
         hb_itemRelease( p1 );
      }
   }
}

static void SlotsExecNetworkRequest( QObject* object, char* event, const QNetworkRequest & request )
{
   if( object )
   {
      int i = object->property( event ).toInt();
      if( ( i > 0 ) && ( s_s->listActv.at( i - 1 ) == true ) )
      {
         PHB_ITEM pObject = hb_itemPutPtr( NULL, ( QObject* ) object );
         PHB_ITEM p1 = hb_itemPutPtr( NULL, ( QNetworkRequest* ) new QNetworkRequest( request ) );
         hb_vmEvalBlockV( ( PHB_ITEM ) s_s->listBlock.at( i - 1 ), 2, pObject, p1 );
         hb_itemRelease( pObject );
         hb_itemRelease( p1 );
      }
   }
}

static void SlotsExecPointer( QObject* object, char* event, void * p1 )
{
   if( object )
   {
      int i = object->property( event ).toInt();
      if( ( i > 0 ) && ( s_s->listActv.at( i - 1 ) == true ) )
      {
         PHB_ITEM pObject = hb_itemPutPtr( NULL, object );
         PHB_ITEM pP1 = hb_itemPutPtr( NULL, p1 );
         hb_vmEvalBlockV( ( PHB_ITEM ) s_s->listBlock.at( i - 1 ), 2, pObject, pP1 );
         hb_itemRelease( pObject );
         hb_itemRelease( pP1 );
      }
   }
}

static void SlotsExecPointerString( QObject* object, char* event, void * p1, QString s1 )
{
   if( object )
   {
      int i = object->property( event ).toInt();
      if( ( i > 0 ) && ( s_s->listActv.at( i - 1 ) == true ) )
      {
         PHB_ITEM pObject = hb_itemPutPtr( NULL, object );
         PHB_ITEM pP1 = hb_itemPutPtr( NULL, p1 );
         PHB_ITEM pS1 = hb_itemPutC( NULL, s1.toLatin1().data() );
         hb_vmEvalBlockV( ( PHB_ITEM ) s_s->listBlock.at( i - 1 ), 3, pObject, pP1, pS1 );
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
      int i = object->property( event ).toInt();
      if( ( i > 0 ) && ( s_s->listActv.at( i - 1 ) == true ) )
      {
         PHB_ITEM pObject = hb_itemPutPtr( NULL, object );
         PHB_ITEM pP1 = hb_itemPutPtr( NULL, p1 );
         PHB_ITEM pI1 = hb_itemPutNI( NULL, iInt );
         hb_vmEvalBlockV( ( PHB_ITEM ) s_s->listBlock.at( i - 1 ), 3, pObject, pP1, pI1 );
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
      int i = object->property( event ).toInt();
      if( ( i > 0 ) && ( s_s->listActv.at( i - 1 ) == true ) )
      {
         PHB_ITEM pObject = hb_itemPutPtr( NULL, object );
         PHB_ITEM pP1 = hb_itemPutPtr( NULL, p1 );
         PHB_ITEM pP2 = hb_itemPutPtr( NULL, p2 );
         hb_vmEvalBlockV( ( PHB_ITEM ) s_s->listBlock.at( i - 1 ), 3, pP1, pP2 );
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

/*
 * harbour function to connect signals with slots
 */
HB_FUNC( QT_CONNECT_SIGNAL )
{
   QObject * object    = ( QObject* ) hb_parptr( 1 ); /* get sender */
   QString   signal    = hb_parcx( 2 );               /* get signal */
   PHB_ITEM  codeblock = hb_itemNew( hb_param( 3, HB_IT_BLOCK | HB_IT_BYREF ) ); /* get codeblock */
   bool      ret       = false;                       /* return value */

   if( s_s == NULL )
      s_s = new Slots();

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


   hb_retl( ret );

   if( ret == true )
   {
      bool bFound = false;
      int  i;

      for( i = 0; i < s_s->listBlock.size(); i++ )
      {
         if( s_s->listBlock.at( i ) == NULL )
         {
            bFound = true;
            break;
         }
      }
      if( bFound )
      {
         s_s->listBlock[ i ] = codeblock;
         s_s->listActv[ i ] = true;
         object->setProperty( hb_parcx( 2 ), ( int ) i + 1 );
      }
      else
      {
         s_s->listBlock  << codeblock;
         s_s->listActv   << true;
         object->setProperty( hb_parcx( 2 ), ( int ) s_s->listBlock.size() );
      }
   }
}

/*
 * harbour function to disconnect signals
 */
HB_FUNC( QT_DISCONNECT_SIGNAL )
{
   QObject * object = ( QObject* ) hb_parptr( 1 );
   if( object )
   {
      const char * event = hb_parcx( 2 );
      int i = object->property( event ).toInt();

      if( i > 0 && i <= s_s->listBlock.size() )
      {
         if( s_s->listBlock.at( i - 1 ) != NULL )
         {
            hb_itemRelease( s_s->listBlock.at( i - 1 ) );
            s_s->listBlock[ i - 1 ] = NULL;
            s_s->listActv[ i - 1 ] = false;
         }
      }
   }
}


/*
 * harbour function to release all codeblocks storeds
 */
HB_FUNC( RELEASE_CODEBLOCKS )
{
   if( s_s )
   {
      for( int i = 0; i < s_s->listBlock.size(); ++i )
      {
         if( ( bool ) s_s->listActv.at( i ) == true )
         {
            hb_itemRelease( ( PHB_ITEM ) s_s->listBlock.at( i ) );
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
   if( s_s )
   {
      for( int i = 0; i < s_s->listBlock.size(); ++i )
      {
         if( ( bool ) s_s->listActv.at( i ) == true )
         {
            hb_itemRelease( ( PHB_ITEM ) s_s->listBlock.at( i ) );
         }
      }
   }
}

/*----------------------------------------------------------------------*/

MyMainWindow::MyMainWindow()
{
   Qt::WindowFlags flags = Qt::WindowCloseButtonHint    | Qt::WindowMaximizeButtonHint |
                           Qt::WindowMinimizeButtonHint | Qt::WindowSystemMenuHint     |
                           Qt::CustomizeWindowHint      | Qt::WindowTitleHint          |
                           Qt::Window;
   setWindowFlags( flags );
   setFocusPolicy( Qt::StrongFocus );
   setAttribute( Qt::WA_DeleteOnClose );
}
MyMainWindow::~MyMainWindow( void )
{
   destroy();
}
HB_FUNC( QT_MYMAINWINDOW )
{
   hb_retptr( ( MyMainWindow * ) new MyMainWindow() );
}

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

Events::Events( QObject * parent ) : QObject( parent )
{
}

Events::~Events()
{
}

bool Events::eventFilter( QObject * object, QEvent * event )
{
   QEvent::Type eventtype = event->type();
   int          found;;
   bool         ret;

   if( ( int ) eventtype == 0 )
      return true;

   char str[ 10 ];
   hb_snprintf( str, sizeof( str ), "%s%i%s", "P", eventtype, "P" );
   found = object->property( str ).toInt();
   if( found == 0 )
      return false;

   PHB_ITEM pObject = hb_itemPutPtr( NULL, object );
   PHB_ITEM pEvent  = hb_itemPutPtr( NULL, event  );

   ret = hb_itemGetL( hb_vmEvalBlockV( ( PHB_ITEM ) listBlock.at( found - 1 ), 2, pObject, pEvent ) );

   hb_itemRelease( pObject );
   hb_itemRelease( pEvent  );

   if( eventtype == QEvent::Close )
   {
      if( ret == true )
         event->accept();
      else
         event->ignore();
   }
   return ret; //true;
}

HB_FUNC( QT_QEVENTFILTER )
{
   if( s_e == NULL )
      s_e = new Events();

   hb_retptr( ( Events * ) s_e );
}

HB_FUNC( QT_CONNECT_EVENT )
{
   QObject * object    = ( QObject * ) hb_parptr( 1 );
   int       type      = hb_parni( 2 );
   PHB_ITEM  codeblock = hb_itemNew( hb_param( 3, HB_IT_BLOCK | HB_IT_BYREF ) );

   if( s_e == NULL )
      s_e = new Events();

   bool bFound = false;
   int  i;
   char str[ 10 ];
   hb_snprintf( str, sizeof( str ), "%s%i%s", "P", type, "P" );    /* Make it a unique identifier */

   for( i = 0; i < s_e->listBlock.size(); i++ )
   {
      if( s_e->listBlock.at( i ) == NULL )
      {
         bFound = true;
         break;
      }
   }
   if( bFound )
   {
      s_e->listBlock[ i ] = codeblock;
      s_e->listActv[ i ] = true;
      object->setProperty( str, ( int ) i + 1 );
   }
   else
   {
      s_e->listBlock << codeblock;
      s_e->listActv  << true;
      object->setProperty( str, ( int ) s_e->listBlock.size() );
   }

   hb_retl( true );
}

HB_FUNC( QT_DISCONNECT_EVENT )
{
   QObject * object    = ( QObject * ) hb_parptr( 1 );
   int       type      = hb_parni( 2 );
   bool      bRet = false;

   char str[ 10 ];
   hb_snprintf( str, sizeof( str ), "%s%i%s", "P", type, "P" );    /* Make it a unique identifier */

   int i = object->property( str ).toInt();
   if( i > 0 && i <= s_e->listBlock.size() )
   {
      if( s_e->listBlock.at( i - 1 ) != NULL )
      {
         hb_itemRelease( s_e->listBlock.at( i - 1 ) );
         s_e->listBlock[ i - 1 ] = NULL;
         s_e->listActv[ i - 1 ] = false;
         bRet = true;
      }
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
#define HBQT_BRW_ROWHEADER                        1005
#define HBQT_BRW_COLALIGN                         1006
#define HBQT_BRW_COLFGCOLOR                       1007
#define HBQT_BRW_COLBGCOLOR                       1008
#define HBQT_BRW_DATFGCOLOR                       1009
#define HBQT_BRW_DATBGCOLOR                       1010
#define HBQT_BRW_COLHEIGHT                        1011
#define HBQT_BRW_DATHEIGHT                        1012
#define HBQT_BRW_DATALIGN                         1013
#define HBQT_BRW_CELLDECORATION                   1014

QVariant fetchRole( PHB_ITEM block, int what, int par1, int par2 )
{
   PHB_ITEM p0  = hb_itemPutNI( NULL, what );
   PHB_ITEM p1  = hb_itemPutNI( NULL, par1 );
   PHB_ITEM p2  = hb_itemPutNI( NULL, par2 );

   PHB_ITEM ret = hb_vmEvalBlockV( block, 3, p0, p1, p2 );

   hb_itemRelease( p0 );
   hb_itemRelease( p1 );
   hb_itemRelease( p2 );

   if( hb_itemType( ret ) & HB_IT_STRING )
      return( hb_itemGetC( ret ) );
   else if( hb_itemType( ret ) & HB_IT_LOGICAL )
      return( hb_itemGetL( ret ) );
   else if( hb_itemType( ret ) & HB_IT_DOUBLE  )
      return( hb_itemGetND( ret ) );
   else if( hb_itemType( ret ) & HB_IT_NUMERIC )
      return( hb_itemGetNI( ret ) );
   #if 0
   else if( hb_itemType( ret ) & HB_IT_POINTER )
   {
      QPixmap pixmap = qobject_cast<QPixmap>( (QPixmap*) hb_itemGetPtr( ret ) );
   }
   #endif
   else
      return QVariant();
}

HbDbfModel::HbDbfModel( PHB_ITEM pBlock ) : QAbstractItemModel()
{
   block = hb_itemNew( pBlock );
}
HbDbfModel::~HbDbfModel()
{
}
Qt::ItemFlags HbDbfModel::flags( const QModelIndex & index ) const
{
   if( ! index.isValid() )
      return 0;

   return( Qt::ItemIsEnabled | Qt::ItemIsSelectable );
}

QVariant HbDbfModel::data( const QModelIndex & index, int role ) const
{
//char str[ 50 ]; hb_snprintf( str, sizeof( str ), "data - row=%i col=%i role=%i", index.row(), index.column(), role );  OutputDebugString( str );
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
//char str[ 50 ]; hb_snprintf( str, sizeof( str ), "headerData - section=%i orient=%i role=%i", section, orientation, role );  OutputDebugString( str );

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
   PHB_ITEM p0 = hb_itemPutNI( NULL, HBQT_BRW_ROWCOUNT );
   PHB_ITEM ret = hb_vmEvalBlockV( block, 1, p0 );
   hb_itemRelease( p0 );

   return hb_itemGetNI( ret );
   hb_itemRelease( ret );
}

int HbDbfModel::columnCount( const QModelIndex & /*parent = QModelIndex()*/ ) const
{
   PHB_ITEM p0 = hb_itemPutNI( NULL, HBQT_BRW_COLCOUNT );
   PHB_ITEM ret = hb_vmEvalBlockV( block, 1, p0 );
   hb_itemRelease( p0 );

   return hb_itemGetNI( ret );
   hb_itemRelease( ret );
}

QModelIndex HbDbfModel::index( int row, int column, const QModelIndex & parent ) const
{
//char str[ 50 ]; hb_snprintf( str, sizeof( str ), "index:  row=%i col=%i", row, column );  OutputDebugString( str );
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

/*----------------------------------------------------------------------*/
#endif
