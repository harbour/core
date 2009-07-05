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

#include "hbapi.h"
#include "hbvm.h"
#include "hbapiitm.h"

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

/*----------------------------------------------------------------------*/
#if 0                                      /* Just for Quick References */

static void SlotsExec( QWidget* widget, char* event )
static void SlotsExecAction( QAction* widget, char* event )
static void SlotsExecInt( QWidget* widget, char* event, int iValue )
static void SlotsExecIntInt( QWidget* widget, char* event, int iValue1, int iValue2 )
static void SlotsExecString( QWidget* widget, char* event, const QString & string )
static void SlotsExecModel( QWidget* widget, char* event, const QModelIndex & index )
static void SlotsExecMouseEvent( QWidget* widget, char* event, QMouseEvent* mevent )

#endif

static Slots  *s_s = NULL;
static Events *s_e = NULL;

/*----------------------------------------------------------------------*/

Slots::Slots( QObject* parent ) : QObject( parent )
{
}
Slots::~Slots()
{
}

static void SlotsExec( QWidget* widget, char* event )
{
   if( widget )
   {
      int i = widget->property( event ).toInt();
      if( ( i > 0 ) && ( s_s->listActv.at( i - 1 ) == true ) )
      {
         PHB_ITEM pWidget = hb_itemPutPtr( NULL, ( QWidget* ) widget );
         hb_vmEvalBlockV( ( PHB_ITEM ) s_s->listBlock.at( i - 1 ), 1, pWidget );
         hb_itemRelease( pWidget );
      }
   }
}

static void SlotsExecBool( QWidget* widget, char* event, bool bBool )
{
   if( widget )
   {
      int i = widget->property( event ).toInt();
      if( ( i > 0 ) && ( s_s->listActv.at( i - 1 ) == true ) )
      {
         PHB_ITEM pWidget = hb_itemPutPtr( NULL, widget );
         PHB_ITEM pBool = hb_itemPutL( NULL, bBool );
         hb_vmEvalBlockV( ( PHB_ITEM ) s_s->listBlock.at( i - 1 ), 2, pWidget, pBool );
         hb_itemRelease( pWidget );
         hb_itemRelease( pBool );
      }
   }
}

static void SlotsExecAction( QAction* widget, char* event )
{
   if( widget )
   {
      int i = widget->property( event ).toInt();
      if( ( i > 0 ) && ( s_s->listActv.at( i - 1 ) == true ) )
      {
         PHB_ITEM pWidget = hb_itemPutPtr( NULL, ( QAction* ) widget );
         hb_vmEvalBlockV( ( PHB_ITEM ) s_s->listBlock.at( i - 1 ), 1, pWidget );
         hb_itemRelease( pWidget );
      }
   }
}

static void SlotsExecInt( QWidget* widget, char* event, int iValue )
{
   if( widget )
   {
      int i = widget->property( event ).toInt();
      if( ( i > 0 ) && ( s_s->listActv.at( i - 1 ) == true ) )
      {
         PHB_ITEM pWidget = hb_itemPutPtr( NULL, ( QWidget* ) widget );
         PHB_ITEM pState = hb_itemPutNI( NULL, iValue );
         hb_vmEvalBlockV( ( PHB_ITEM ) s_s->listBlock.at( i - 1 ), 2, pWidget, pState );
         hb_itemRelease( pWidget );
         hb_itemRelease( pState );
      }
   }
}

static void SlotsExecIntInt( QWidget* widget, char* event, int iValue1, int iValue2 )
{
   if( widget )
   {
      int i = widget->property( event ).toInt();
      if( i > 0 && ( s_s->listActv.at( i - 1 ) == true ) )
      {
         PHB_ITEM pWidget = hb_itemPutPtr( NULL, ( QWidget* ) widget );
         PHB_ITEM pValue1 = hb_itemPutNI( NULL, iValue1 );
         PHB_ITEM pValue2 = hb_itemPutNI( NULL, iValue2 );
         hb_vmEvalBlockV( ( PHB_ITEM ) s_s->listBlock.at( i - 1 ), 3, pWidget, pValue1, pValue2 );
         hb_itemRelease( pWidget );
         hb_itemRelease( pValue1 );
         hb_itemRelease( pValue2 );
      }
   }
}

static void SlotsExecIntIntRect( QWidget* widget, char* event, int iValue1, int iValue2, const QRect & rect )
{
   if( widget )
   {
      int i = widget->property( event ).toInt();
      if( i > 0 && ( s_s->listActv.at( i - 1 ) == true ) )
      {
         PHB_ITEM pWidget = hb_itemPutPtr( NULL, ( QWidget* ) widget );
         PHB_ITEM pValue1 = hb_itemPutNI( NULL, iValue1 );
         PHB_ITEM pValue2 = hb_itemPutNI( NULL, iValue2 );
         PHB_ITEM pValue3 = hb_itemPutPtr( NULL, new QRect( rect ) );
         hb_vmEvalBlockV( ( PHB_ITEM ) s_s->listBlock.at( i - 1 ), 4, pWidget, pValue1, pValue2, pValue3 );
         hb_itemRelease( pWidget );
         hb_itemRelease( pValue1 );
         hb_itemRelease( pValue2 );
         hb_itemRelease( pValue3 );
      }
   }
}

static void SlotsExecString( QWidget* widget, char* event, const QString & string )
{
   if( widget )
   {
      int i = widget->property( event ).toInt();
      if( i > 0 && ( s_s->listActv.at( i - 1 ) == true ) )
      {
         PHB_ITEM pWidget = hb_itemPutPtr( NULL, ( QWidget* ) widget );
         PHB_ITEM pString = hb_itemPutC( NULL, string.toLatin1().data() );
         hb_vmEvalBlockV( ( PHB_ITEM ) s_s->listBlock.at( i - 1 ), 2, pWidget, pString );
         hb_itemRelease( pWidget );
         hb_itemRelease( pString );
      }
   }
}
#if 0
static void SlotsExecString2( QWidget* widget, char* event, const QString & s1, const QString & s2 )
{
   if( widget )
   {
      int i = widget->property( event ).toInt();
      if( i > 0 && ( s_s->listActv.at( i - 1 ) == true ) )
      {
         PHB_ITEM pWidget = hb_itemPutPtr( NULL, ( QWidget* ) widget );
         PHB_ITEM pS1 = hb_itemPutC( NULL, s1.toLatin1().data() );
         PHB_ITEM pS2 = hb_itemPutC( NULL, s2.toLatin1().data() );
         hb_vmEvalBlockV( ( PHB_ITEM ) s_s->listBlock.at( i - 1 ), 3, pWidget, pS1, pS2 );
         hb_itemRelease( pWidget );
         hb_itemRelease( pS1 );
         hb_itemRelease( pS2 );
      }
   }
}
#endif
static void SlotsExecString3( QWidget* widget, char* event, const QString & s1, const QString & s2, const QString & s3 )
{
   if( widget )
   {
      int i = widget->property( event ).toInt();
      if( i > 0 && ( s_s->listActv.at( i - 1 ) == true ) )
      {
         PHB_ITEM pWidget = hb_itemPutPtr( NULL, ( QWidget* ) widget );
         PHB_ITEM pS1 = hb_itemPutC( NULL, s1.toLatin1().data() );
         PHB_ITEM pS2 = hb_itemPutC( NULL, s2.toLatin1().data() );
         PHB_ITEM pS3 = hb_itemPutC( NULL, s3.toLatin1().data() );
         hb_vmEvalBlockV( ( PHB_ITEM ) s_s->listBlock.at( i - 1 ), 4, pWidget, pS1, pS2, pS3 );
         hb_itemRelease( pWidget );
         hb_itemRelease( pS1 );
         hb_itemRelease( pS2 );
         hb_itemRelease( pS3 );
      }
   }
}

static void SlotsExecModel( QWidget* widget, char* event, const QModelIndex & index )
{
   if( widget )
   {
      int i = widget->property( event ).toInt();
      if( ( i > 0 ) && ( s_s->listActv.at( i - 1 ) == true ) )
      {
         PHB_ITEM pWidget = hb_itemPutPtr( NULL, ( QWidget* ) widget );
         PHB_ITEM pState = hb_itemPutPtr( NULL, ( QModelIndex * ) new QModelIndex( index ) );
         hb_vmEvalBlockV( ( PHB_ITEM ) s_s->listBlock.at( i - 1 ), 2, pWidget, pState );
         hb_itemRelease( pWidget );
         hb_itemRelease( pState );
      }
   }
}

static void SlotsExecRect( QWidget* widget, char* event, const QRect & rect )
{
   if( widget )
   {
      int i = widget->property( event ).toInt();
      if( ( i > 0 ) && ( s_s->listActv.at( i - 1 ) == true ) )
      {
         PHB_ITEM pWidget = hb_itemPutPtr( NULL, ( QWidget* ) widget );
         PHB_ITEM p1 = hb_itemPutPtr( NULL, new QRect( rect ) );
         hb_vmEvalBlockV( ( PHB_ITEM ) s_s->listBlock.at( i - 1 ), 2, pWidget, p1 );
         hb_itemRelease( pWidget );
         hb_itemRelease( p1 );
      }
   }
}

static void SlotsExecUrl( QWidget* widget, char* event, const QUrl & url )
{
   if( widget )
   {
      int i = widget->property( event ).toInt();
      if( ( i > 0 ) && ( s_s->listActv.at( i - 1 ) == true ) )
      {
         PHB_ITEM pWidget = hb_itemPutPtr( NULL, ( QWidget* ) widget );
         PHB_ITEM p1 = hb_itemPutPtr( NULL, new QUrl( url ) );
         hb_vmEvalBlockV( ( PHB_ITEM ) s_s->listBlock.at( i - 1 ), 2, pWidget, p1 );
         hb_itemRelease( pWidget );
         hb_itemRelease( p1 );
      }
   }
}

static void SlotsExecFont( QWidget* widget, char* event, const QFont & font )
{
   if( widget )
   {
      int i = widget->property( event ).toInt();
      if( ( i > 0 ) && ( s_s->listActv.at( i - 1 ) == true ) )
      {
         PHB_ITEM pWidget = hb_itemPutPtr( NULL, ( QWidget* ) widget );
         PHB_ITEM p1 = hb_itemPutPtr( NULL, new QFont( font ) );
         hb_vmEvalBlockV( ( PHB_ITEM ) s_s->listBlock.at( i - 1 ), 2, pWidget, p1 );
         hb_itemRelease( pWidget );
         hb_itemRelease( p1 );
      }
   }
}

static void SlotsExecStringList( QWidget* widget, char* event, const QStringList & stringList )
{
   if( widget )
   {
      int i = widget->property( event ).toInt();
      if( ( i > 0 ) && ( s_s->listActv.at( i - 1 ) == true ) )
      {
         PHB_ITEM pWidget = hb_itemPutPtr( NULL, ( QWidget* ) widget );
         PHB_ITEM p1 = hb_itemPutPtr( NULL, new QStringList( stringList ) );
         hb_vmEvalBlockV( ( PHB_ITEM ) s_s->listBlock.at( i - 1 ), 2, pWidget, p1 );
         hb_itemRelease( pWidget );
         hb_itemRelease( p1 );
      }
   }
}

static void SlotsExecNetworkRequest( QWidget* widget, char* event, const QNetworkRequest & request )
{
   if( widget )
   {
      int i = widget->property( event ).toInt();
      if( ( i > 0 ) && ( s_s->listActv.at( i - 1 ) == true ) )
      {
         PHB_ITEM pWidget = hb_itemPutPtr( NULL, ( QWidget* ) widget );
         PHB_ITEM p1 = hb_itemPutPtr( NULL, ( QNetworkRequest* ) new QNetworkRequest( request ) );
         hb_vmEvalBlockV( ( PHB_ITEM ) s_s->listBlock.at( i - 1 ), 2, pWidget, p1 );
         hb_itemRelease( pWidget );
         hb_itemRelease( p1 );
      }
   }
}

static void SlotsExecPointerString( QWidget* widget, char* event, void * p1, QString s1 )
{
   if( widget )
   {
      int i = widget->property( event ).toInt();
      if( ( i > 0 ) && ( s_s->listActv.at( i - 1 ) == true ) )
      {
         PHB_ITEM pWidget = hb_itemPutPtr( NULL, widget );
         PHB_ITEM pP1 = hb_itemPutPtr( NULL, p1 );
         PHB_ITEM pS1 = hb_itemPutC( NULL, s1.toLatin1().data() );
         hb_vmEvalBlockV( ( PHB_ITEM ) s_s->listBlock.at( i - 1 ), 3, pWidget, pP1, pS1 );
         hb_itemRelease( pWidget );
         hb_itemRelease( pP1 );
         hb_itemRelease( pS1 );
      }
   }
}

static void SlotsExecPointer( QWidget* widget, char* event, void * p1 )
{
   if( widget )
   {
      int i = widget->property( event ).toInt();
      if( ( i > 0 ) && ( s_s->listActv.at( i - 1 ) == true ) )
      {
         PHB_ITEM pWidget = hb_itemPutPtr( NULL, widget );
         PHB_ITEM pP1 = hb_itemPutPtr( NULL, p1 );
         hb_vmEvalBlockV( ( PHB_ITEM ) s_s->listBlock.at( i - 1 ), 2, pWidget, pP1 );
         hb_itemRelease( pWidget );
         hb_itemRelease( pP1 );
      }
   }
}

static void SlotsExecPointerInt( QWidget* widget, char* event, void * p1, int iInt )
{
   if( widget )
   {
      int i = widget->property( event ).toInt();
      if( ( i > 0 ) && ( s_s->listActv.at( i - 1 ) == true ) )
      {
         PHB_ITEM pWidget = hb_itemPutPtr( NULL, widget );
         PHB_ITEM pP1 = hb_itemPutPtr( NULL, p1 );
         PHB_ITEM pI1 = hb_itemPutNI( NULL, iInt );
         hb_vmEvalBlockV( ( PHB_ITEM ) s_s->listBlock.at( i - 1 ), 3, pWidget, pP1, pI1 );
         hb_itemRelease( pWidget );
         hb_itemRelease( pP1 );
         hb_itemRelease( pI1 );
      }
   }
}

static void SlotsExecPointerPointer( QWidget* widget, char* event, void * p1, void * p2 )
{
   if( widget )
   {
      int i = widget->property( event ).toInt();
      if( ( i > 0 ) && ( s_s->listActv.at( i - 1 ) == true ) )
      {
         PHB_ITEM pWidget = hb_itemPutPtr( NULL, widget );
         PHB_ITEM pP1 = hb_itemPutPtr( NULL, p1 );
         PHB_ITEM pP2 = hb_itemPutPtr( NULL, p2 );
         hb_vmEvalBlockV( ( PHB_ITEM ) s_s->listBlock.at( i - 1 ), 3, pP1, pP2 );
         hb_itemRelease( pWidget );
         hb_itemRelease( pP1 );
         hb_itemRelease( pP2 );
      }
   }
}

static void SlotsExecMouseEvent( QWidget* widget, char* event, QMouseEvent* mevent )
{
   if( widget )
   {
      int i = widget->property( event ).toInt();
      if( ( i > 0 ) && ( s_s->listActv.at( i - 1 ) == true ) )
      {
         PHB_ITEM pWidget  = hb_itemPutPtr( NULL, ( QWidget * ) widget );
         PHB_ITEM pEvent   = hb_itemPutPtr( NULL, mevent );
         hb_vmEvalBlockV( ( PHB_ITEM ) s_s->listBlock.at( i - 1 ), 2, pWidget, pEvent );
         hb_itemRelease( pWidget );
         hb_itemRelease( pEvent );
      }
   }
}

void Slots::keyPressEvent( QKeyEvent * event )
{
   QObject *widget = qobject_cast<QObject *>( sender() );
   if( widget )
   {
      int i = widget->property( "keyPressEvent()" ).toInt();
      if( ( i > 0 ) && ( s_s->listActv.at( i - 1 ) == true ) )
      {
         PHB_ITEM pWidget  = hb_itemPutPtr( NULL, ( QWidget * ) widget );
         PHB_ITEM pEvent   = hb_itemPutPtr( NULL, event );
         hb_vmEvalBlockV( ( PHB_ITEM ) listBlock.at( i - 1 ), 2, pWidget, pEvent );
         hb_itemRelease( pWidget );
         hb_itemRelease( pEvent );
      }
   }
}

void Slots::triggered( bool checked )
{
   QObject *widget = qobject_cast<QObject *>( sender() );
   if( widget )
   {
      int i = widget->property( "triggered(bool)" ).toInt();
      if( ( i > 0 ) && ( s_s->listActv.at( i - 1 ) == true ) )
      {
         PHB_ITEM pWidget  = hb_itemPutPtr( NULL, ( QObject * ) widget );
         PHB_ITEM pChecked = hb_itemPutL( NULL, checked );
         hb_vmEvalBlockV( ( PHB_ITEM ) listBlock.at( i - 1 ), 2, pWidget, pChecked );
         hb_itemRelease( pWidget );
         hb_itemRelease( pChecked );
      }
   }
}

void Slots::hovered( QAction * action )
{
   QObject *widget = qobject_cast<QObject *>( sender() );
   if( widget )
   {
      int i = widget->property( "hovered(action)" ).toInt();
      if( ( i > 0 ) && ( s_s->listActv.at( i - 1 ) == true ) )
      {
         PHB_ITEM pWidget  = hb_itemPutPtr( NULL, ( QWidget * ) widget );
         PHB_ITEM pEvent   = hb_itemPutPtr( NULL, action );
         hb_vmEvalBlockV( ( PHB_ITEM ) listBlock.at( i - 1 ), 2, pWidget, pEvent );
         hb_itemRelease( pWidget );
         hb_itemRelease( pEvent );
      }
   }
}

void Slots::mouseMoveEvent( QMouseEvent * event )
{
   QWidget *widget = qobject_cast<QWidget *>( sender() );
   SlotsExecMouseEvent( widget, ( char* ) "mouseMoveEvent()", event );
}

void Slots::clicked()
{
   QWidget *widget = qobject_cast<QWidget *>( sender() );
   SlotsExec( widget, ( char* ) "clicked()" );
}
void Slots::returnPressed()
{
   QWidget *widget = qobject_cast<QWidget *>( sender() );
   SlotsExec( widget, ( char* ) "returnPressed()" );
}
void Slots::viewportEntered()
{
   QWidget *widget = qobject_cast<QWidget *>( sender() );
   SlotsExec( widget, ( char* ) "viewportEntered()" );
}
void Slots::pressed()
{
   QWidget *widget = qobject_cast<QWidget *>( sender() );
   SlotsExec( widget, ( char* ) "pressed()" );
}
void Slots::released()
{
   QWidget *widget = qobject_cast<QWidget *>( sender() );
   SlotsExec( widget, ( char* ) "released()" );
}
void Slots::triggered()
{
   QWidget *widget = qobject_cast<QWidget *>( sender() );
   SlotsExec( widget, ( char* ) "triggered()" );
}
void Slots::hovered()
{
   QAction *widget = qobject_cast<QAction *>( sender() );
   SlotsExecAction( widget, ( char* ) "hovered()" );
}

void Slots::stateChanged( int state )
{
   QWidget *widget = qobject_cast<QWidget *>( sender() );
   SlotsExecInt( widget, ( char* ) "stateChanged(int)", state );
}
void Slots::activated( int index )
{
   QWidget *widget = qobject_cast<QWidget *>( sender() );
   SlotsExecInt( widget, ( char* ) "activated(int)", index );
}
void Slots::currentIndexChanged( int index )
{
   QWidget *widget = qobject_cast<QWidget *>( sender() );
   SlotsExecInt( widget, ( char* ) "currentIndexChanged(int)", index );
}
void Slots::currentChanged( int index )
{
   QWidget *widget = qobject_cast<QWidget *>( sender() );
   SlotsExecInt( widget, ( char* ) "currentChanged(int)", index );
}
void Slots::highlighted( int index )
{
   QWidget *widget = qobject_cast<QWidget *>( sender() );
   SlotsExecInt( widget, ( char* ) "highlighted(int)", index );
}

void Slots::clicked( const QModelIndex & index )
{
   QWidget * widget = qobject_cast<QWidget *>( sender() );
   SlotsExecModel( widget, ( char* ) "clicked(QModelIndex)", index );
}
void Slots::doubleClicked( const QModelIndex & index )
{
   QWidget * widget = qobject_cast<QWidget *>( sender() );
   SlotsExecModel( widget, ( char* ) "doubleClicked(QModelIndex)", index );
}
void Slots::entered( const QModelIndex & index )
{
   QWidget * widget = qobject_cast<QWidget *>( sender() );
   SlotsExecModel( widget, ( char* ) "entered(QModelIndex)", index );
}

void Slots::actionTriggered( int action )
{
   QWidget *widget = qobject_cast<QWidget *>( sender() );
   SlotsExecInt( widget, ( char* ) "actionTriggered(int)", action );
}
void Slots::rangeChanged( int min, int max )
{
   QWidget *widget = qobject_cast<QWidget *>( sender() );
   SlotsExecIntInt( widget, ( char* ) "rangeChanged(int)", min, max );
}
void Slots::sliderMoved( int value )
{
   QWidget *widget = qobject_cast<QWidget *>( sender() );
   SlotsExecInt( widget, ( char* ) "sliderMoved(int)", value );
}
void Slots::sliderPressed()
{
   QWidget *widget = qobject_cast<QWidget *>( sender() );
   SlotsExec( widget, ( char* ) "sliderPressed()" );
}
void Slots::sliderReleased()
{
   QWidget *widget = qobject_cast<QWidget *>( sender() );
   SlotsExec( widget, ( char* ) "sliderReleased()" );
}
void Slots::valueChanged( int value )
{
   QWidget *widget = qobject_cast<QWidget *>( sender() );
   SlotsExecInt( widget, ( char* ) "valueChanged(int)", value );
}
void Slots::cursorPositionChanged( int iOld, int iNew )
{
   QWidget *widget = qobject_cast<QWidget *>( sender() );
   SlotsExecIntInt( widget, ( char* ) "cursorPositionChanged(int,int)", iOld, iNew );
}
void Slots::editingFinished()
{
   QWidget *widget = qobject_cast<QWidget *>( sender() );
   SlotsExec( widget, ( char* ) "editingFinished()" );
}
void Slots::selectionChanged()
{
   QWidget *widget = qobject_cast<QWidget *>( sender() );
   SlotsExec( widget, ( char* ) "selectionChanged()" );
}
void Slots::textChanged( const QString & text )
{
   QWidget *widget = qobject_cast<QWidget *>( sender() );
   SlotsExecString( widget, ( char* ) "textChanged(QString)", text );
}
void Slots::textEdited( const QString & text )
{
   QWidget *widget = qobject_cast<QWidget *>( sender() );
   SlotsExecString( widget, ( char* ) "textEdited(QString)", text );
}
/*  TreeViewWidget */
void Slots::currentItemChanged( QTreeWidgetItem * current, QTreeWidgetItem * previous )
{
   QWidget *widget = qobject_cast<QWidget *>( sender() );
   SlotsExecPointerPointer( widget, ( char* ) "currentItemChanged(QTWItem)", current, previous );
}
void Slots::itemActivated( QTreeWidgetItem * item, int column )
{
   QWidget *widget = qobject_cast<QWidget *>( sender() );
   SlotsExecPointerInt( widget, ( char* ) "itemActivated(QTWItem)", item, column );
}
void Slots::itemChanged( QTreeWidgetItem * item, int column )
{
   QWidget *widget = qobject_cast<QWidget *>( sender() );
   SlotsExecPointerInt( widget, ( char* ) "itemChanged(QTWItem)", item, column );
}
void Slots::itemClicked( QTreeWidgetItem * item, int column )
{
   QWidget *widget = qobject_cast<QWidget *>( sender() );
   SlotsExecPointerInt( widget, ( char* ) "itemClicked(QTWItem)", item, column );
}
void Slots::itemDoubleClicked( QTreeWidgetItem * item, int column )
{
   QWidget *widget = qobject_cast<QWidget *>( sender() );
   SlotsExecPointerInt( widget, ( char* ) "itemDoubleClicked(QTWItem)", item, column );
}
void Slots::itemEntered( QTreeWidgetItem * item, int column )
{
   QWidget *widget = qobject_cast<QWidget *>( sender() );
   SlotsExecPointerInt( widget, ( char* ) "itemEntered(QTWItem)", item, column );
}
void Slots::itemPressed( QTreeWidgetItem * item, int column )
{
   QWidget *widget = qobject_cast<QWidget *>( sender() );
   SlotsExecPointerInt( widget, ( char* ) "itemPressed(QTWItem)", item, column );
}
void Slots::itemExpanded( QTreeWidgetItem * item )
{
   QWidget *widget = qobject_cast<QWidget *>( sender() );
   SlotsExecPointer( widget, ( char* ) "itemExpanded(QTWItem)", item );
}
void Slots::itemCollapsed( QTreeWidgetItem * item )
{
   QWidget *widget = qobject_cast<QWidget *>( sender() );
   SlotsExecPointer( widget, ( char* ) "itemCollapsed(QTWItem)", item );
}
void Slots::itemSelectionChanged()
{
   QWidget *widget = qobject_cast<QWidget *>( sender() );
   SlotsExec( widget, ( char* ) "itemSelectionChanged()" );
}
/* QWebPage */
void Slots::contentsChanged()
{
   QWidget *object = qobject_cast<QWidget *>( sender() );
   SlotsExec( object, ( char* ) "contentsChanged()" );
}
void Slots::databaseQuotaExceeded( QWebFrame * frame, QString databaseName )
{
   QWidget *object = qobject_cast<QWidget *>( sender() );
   SlotsExecPointerString( object, ( char* ) "databaseQuotaExceeded(QWebFrame,QString)", frame, databaseName );
}
void Slots::downloadRequested( const QNetworkRequest & request )
{
   QWidget *object = qobject_cast<QWidget *>( sender() );
   SlotsExecNetworkRequest( object, ( char* ) "downloadRequested(QNetworkRequest)", request );
}
void Slots::frameCreated( QWebFrame * frame )
{
   QWidget *object = qobject_cast<QWidget *>( sender() );
   SlotsExecPointer( object, ( char* ) "frameCreated(QWebFrame)", frame );
}
void Slots::geometryChangeRequested( const QRect & geom )
{
   QWidget *object = qobject_cast<QWidget *>( sender() );
   SlotsExecRect( object, ( char* ) "geometryChangeRequested(QRect)", geom );
}
void Slots::linkClicked( const QUrl & url )
{
   QWidget *object = qobject_cast<QWidget *>( sender() );
   SlotsExecUrl( object, ( char* ) "linkClicked(QUrl)", url );
}
void Slots::linkHovered( const QString & link, const QString & title, const QString & textContent )
{
   QWidget *object = qobject_cast<QWidget *>( sender() );
   SlotsExecString3( object, ( char* ) "linkHovered(QString,QString,QString)", link, title, textContent );
}
void Slots::loadFinished( bool ok )
{
   QWidget *object = qobject_cast<QWidget *>( sender() );
   SlotsExecBool( object, ( char* ) "loadFinished(bool)", ok );
}
void Slots::loadProgress( int progress )
{
   QWidget *object = qobject_cast<QWidget *>( sender() );
   SlotsExecInt( object, ( char* ) "loadProgress(int)", progress );
}
void Slots::loadStarted()
{
   QWidget *object = qobject_cast<QWidget *>( sender() );
   SlotsExec( object, ( char* ) "loadStarted()" );
}
void Slots::menuBarVisibilityChangeRequested( bool visible )
{
   QWidget *object = qobject_cast<QWidget *>( sender() );
   SlotsExecBool( object, ( char* ) "menuBarVisibilityChangeRequested(bool)", visible );
}
void Slots::microFocusChanged()
{
   QWidget *object = qobject_cast<QWidget *>( sender() );
   SlotsExec( object, ( char* ) "microFocusChanged()" );
}
void Slots::printRequested( QWebFrame * frame )
{
   QWidget *object = qobject_cast<QWidget *>( sender() );
   SlotsExecPointer( object, ( char* ) "printRequested(QWebFrame)", frame );
}
void Slots::repaintRequested( const QRect & dirtyRect )
{
   QWidget *object = qobject_cast<QWidget *>( sender() );
   SlotsExecRect( object, ( char* ) "repaintRequested(QRect)", dirtyRect );
}
void Slots::restoreFrameStateRequested( QWebFrame * frame )
{
   QWidget *object = qobject_cast<QWidget *>( sender() );
   SlotsExecPointer( object, ( char* ) "restoreFrameStateRequested(QWebFrame)", frame );
}
void Slots::saveFrameStateRequested( QWebFrame * frame, QWebHistoryItem * item )
{
   QWidget *object = qobject_cast<QWidget *>( sender() );
   SlotsExecPointerPointer( object, ( char* ) "saveFrameStateRequested(QWebFrame,QWebHistoryItem)", frame, item );
}
void Slots::scrollRequested( int dx, int dy, const QRect & rectToScroll )
{
   QWidget *object = qobject_cast<QWidget *>( sender() );
   SlotsExecIntIntRect( object, ( char* ) "scrollRequested(int,int,QRect)", dx, dy, rectToScroll );
}
void Slots::statusBarMessage( const QString & text )
{
   QWidget *object = qobject_cast<QWidget *>( sender() );
   SlotsExecString( object, ( char* ) "statusBarMessage(QString)", text );
}
void Slots::statusBarVisibilityChangeRequested( bool visible )
{
   QWidget *object = qobject_cast<QWidget *>( sender() );
   SlotsExecBool( object, ( char* ) "statusBarVisibilityChangeRequested(bool)", visible );
}
void Slots::toolBarVisibilityChangeRequested( bool visible )
{
   QWidget *object = qobject_cast<QWidget *>( sender() );
   SlotsExecBool( object, ( char* ) "toolBarVisibilityChangeRequested(bool)", visible );
}
void Slots::unsupportedContent( QNetworkReply * reply )
{
   QWidget *object = qobject_cast<QWidget *>( sender() );
   SlotsExecPointer( object, ( char* ) "unsupportedContent(QNetworkReply)", reply );
}
void Slots::windowCloseRequested()
{
   QWidget *object = qobject_cast<QWidget *>( sender() );
   SlotsExec( object, ( char* ) "windowCloseRequested()" );
}
/* QWebView */
void Slots::iconChanged()
{
   QWidget *object = qobject_cast<QWidget *>( sender() );
   SlotsExec( object, ( char* ) "iconChanged()" );
}
void Slots::titleChanged( const QString & title )
{
   QWidget *object = qobject_cast<QWidget *>( sender() );
   SlotsExecString( object, ( char* ) "titleChanged(QString)", title );
}
void Slots::urlChanged( const QUrl & url )
{
   QWidget *object = qobject_cast<QWidget *>( sender() );
   SlotsExecUrl( object, ( char* ) "urlChanged(QUrl)", url );
}
/* QDialog (s)*/
void Slots::currentFontChanged( const QFont & font )
{
   QWidget *oWidget = qobject_cast<QWidget *>( sender() );
   SlotsExecFont( oWidget, ( char* ) "currentFontChanged(QFont)", font );
}
void Slots::fontSelected( const QFont & font )
{
   QWidget *oWidget = qobject_cast<QWidget *>( sender() );
   SlotsExecFont( oWidget, ( char* ) "fontSelected(QFont)", font );
}
void Slots::accepted()
{
   QWidget *oWidget = qobject_cast<QWidget *>( sender() );
   SlotsExec( oWidget, ( char* ) "accepted()" );
}
void Slots::finished( int result )
{
   QWidget *oWidget = qobject_cast<QWidget *>( sender() );
   SlotsExecInt( oWidget, ( char* ) "finished(int)", result );
}
void Slots::rejected()
{
   QWidget *oWidget = qobject_cast<QWidget *>( sender() );
   SlotsExec( oWidget, ( char* ) "rejected()" );
}
void Slots::currentChanged( const QString & path )
{
   QWidget *oWidget = qobject_cast<QWidget *>( sender() );
   SlotsExecString( oWidget, ( char* ) "currentChanged(QString)", path );
}
void Slots::directoryEntered( const QString & directory )
{
   QWidget *oWidget = qobject_cast<QWidget *>( sender() );
   SlotsExecString( oWidget, ( char* ) "directoryEntered(QString)", directory );
}
void Slots::fileSelected( const QString & file )
{
   QWidget *oWidget = qobject_cast<QWidget *>( sender() );
   SlotsExecString( oWidget, ( char* ) "fileSelected(QString)", file );
}
void Slots::filesSelected( const QStringList & selected )
{
   QWidget *oWidget = qobject_cast<QWidget *>( sender() );
   SlotsExecStringList( oWidget, ( char* ) "filesSelected(QStringList)", selected );
}
void Slots::filterSelected( const QString & filter )
{
   QWidget *oWidget = qobject_cast<QWidget *>( sender() );
   SlotsExecString( oWidget, ( char* ) "filterSelected(QString)", filter );
}


/*
 * harbour function to connect signals with slots
 */
HB_FUNC( QT_CONNECT_SIGNAL )
{
   QWidget * widget    = ( QWidget* ) hb_parptr( 1 ); /* get sender */
   QString   signal    = hb_parcx( 2 );               /* get signal */
   PHB_ITEM  codeblock = hb_itemNew( hb_param( 3, HB_IT_BLOCK | HB_IT_BYREF ) ); /* get codeblock */
   bool      ret       = false;                       /* return value */

   if( s_s == NULL )
      s_s = new Slots();

   if( signal == ( QString ) "clicked()" )
   {
      ret = widget->connect( widget, SIGNAL( clicked() )          , s_s, SLOT( clicked() )          , Qt::AutoConnection );
   }
   if( signal == ( QString ) "returnPressed()" )
   {
      ret = widget->connect( widget, SIGNAL( returnPressed() )    , s_s, SLOT( returnPressed() )    , Qt::AutoConnection );
   }
   if( signal == ( QString ) "triggered()" )
   {
      ret = widget->connect( widget, SIGNAL( triggered() )        , s_s, SLOT( triggered() )        , Qt::AutoConnection );
   }
   if( signal == ( QString ) "hovered()" )
   {
      ret = widget->connect( widget, SIGNAL( hovered() )          , s_s, SLOT( hovered() )          , Qt::AutoConnection );
   }
   if( signal == ( QString ) "viewportEntered()" )
   {
      ret = widget->connect( widget, SIGNAL( viewportEntered() )  , s_s, SLOT( viewportEntered() ) , Qt::AutoConnection );
   }
   if( signal == ( QString ) "pressed()" )
   {
      ret = widget->connect( widget, SIGNAL( pressed() )          , s_s, SLOT( pressed() )          , Qt::AutoConnection );
   }
   if( signal == ( QString ) "released()" )
   {
      ret = widget->connect( widget, SIGNAL( released() )         ,
                             s_s, SLOT( released() ), Qt::AutoConnection );
   }
   if( signal == ( QString ) "stateChanged(int)" )
   {
      ret = widget->connect( widget, SIGNAL( stateChanged( int ) ),
                             s_s, SLOT( stateChanged( int ) ), Qt::AutoConnection );
   }
   if( signal == ( QString ) "activated(int)" )
   {
      ret = widget->connect( widget, SIGNAL( activated( int ) ),
                             s_s, SLOT( activated( int ) )   , Qt::AutoConnection );
   }
   if( signal == ( QString ) "currentIndexChanged(int)" )
   {
      ret = widget->connect( widget, SIGNAL( currentIndexChanged( int ) ),
                             s_s, SLOT( currentIndexChanged( int ) ), Qt::AutoConnection );
   }
   if( signal == ( QString ) "highlighted(int)" )
   {
      ret = widget->connect( widget, SIGNAL( highlighted( int ) ) ,
                             s_s, SLOT( highlighted( int ) ) , Qt::AutoConnection );
   }
   if( signal == ( QString ) "triggered(bool)" )
   {
      ret = widget->connect( widget, SIGNAL( triggered( bool ) ),
                             s_s, SLOT( triggered( bool ) )  , Qt::AutoConnection );
   }
   if( signal == ( QString ) "clicked(QModelIndex)" )
   {
      ret = widget->connect( widget, SIGNAL( clicked( const QModelIndex & ) ),
                             s_s, SLOT( clicked( const QModelIndex & ) ), Qt::AutoConnection );
   }
   if( signal == ( QString ) "doubleClicked(QModelIndex)" )
   {
      ret = widget->connect( widget, SIGNAL( doubleClicked( const QModelIndex & ) ),
                             s_s, SLOT( doubleClicked( const QModelIndex & ) ), Qt::AutoConnection );
   }
   if( signal == ( QString ) "entered(QModelIndex)" )
   {
      ret = widget->connect( widget, SIGNAL( entered( const QModelIndex & ) ),
                             s_s, SLOT( entered( const QModelIndex & ) ), Qt::AutoConnection );
   }
   if( signal == ( QString ) "keyPressEvent()" )
   {
      ret = widget->connect( widget, SIGNAL( sg_keyPressEvent( QKeyEvent * ) ),
                             s_s, SLOT( keyPressEvent( QKeyEvent * ) ), Qt::AutoConnection );
   }
   if( signal == ( QString ) "mouseMoveEvent()" )
   {
      ret = widget->connect( widget, SIGNAL( sg_mouseMoveEvent( QMouseEvent * ) ),
                             s_s, SLOT( mouseMoveEvent( QMouseEvent * ) ), Qt::AutoConnection );
   }
   if( signal == ( QString ) "hovered(action)" )
   {
      ret = widget->connect( widget, SIGNAL( hovered( QAction * ) ),
                             s_s, SLOT( hovered( QAction * ) ), Qt::AutoConnection );
   }
   if( signal == ( QString ) "currentChanged(int)" )
   {
      ret = widget->connect( widget, SIGNAL( currentChanged( int ) ),
                             s_s, SLOT( currentChanged( int ) ), Qt::AutoConnection );
   }
   if( signal == ( QString ) "actionTriggered(int)" )
   {
      ret = widget->connect( widget,  SIGNAL( actionTriggered(int) ),
                             s_s, SLOT( actionTriggered(int) ), Qt::AutoConnection );
   }
   if( signal == ( QString ) "rangeChanged(int,int)" )
   {
      ret = widget->connect( widget,  SIGNAL( rangeChanged(int,int) ),
                             s_s, SLOT( rangeChanged(int,int) ), Qt::AutoConnection );
   }
   if( signal == ( QString ) "sliderMoved(int)" )
   {
      ret = widget->connect( widget,  SIGNAL( sliderMoved(int) ),
                             s_s, SLOT( sliderMoved(int) ), Qt::AutoConnection );
   }
   if( signal == ( QString ) "sliderPressed()" )
   {
      ret = widget->connect( widget,  SIGNAL( sliderPressed() ),
                             s_s, SLOT( sliderPressed() ), Qt::AutoConnection );
   }
   if( signal == ( QString ) "sliderReleased()" )
   {
      ret = widget->connect( widget,  SIGNAL( sliderReleased() ),
                             s_s, SLOT( sliderReleased() ), Qt::AutoConnection );
   }
   if( signal == ( QString ) "valueChanged(int)" )
   {
      ret = widget->connect( widget,  SIGNAL( valueChanged(int) ),
                             s_s, SLOT( valueChanged(int) ), Qt::AutoConnection );
   }
   if( signal == ( QString ) "cursorPositionChanged(int,int)" )
   {
      ret = widget->connect( widget,  SIGNAL( cursorPositionChanged(int,int) ),
                             s_s, SLOT( cursorPositionChanged(int,int) ), Qt::AutoConnection );
   }
   if( signal == ( QString ) "editingFinished()" )
   {
      ret = widget->connect( widget,  SIGNAL( editingFinished() ),
                             s_s, SLOT( editingFinished() ), Qt::AutoConnection );
   }
   if( signal == ( QString ) "returnPressed()" )
   {
      ret = widget->connect( widget,  SIGNAL( returnPressed() ),
                             s_s, SLOT( returnPressed() ), Qt::AutoConnection );
   }
   if( signal == ( QString ) "selectionChanged()" )
   {
      ret = widget->connect( widget,  SIGNAL( selectionChanged() ),
                             s_s, SLOT( selectionChanged() ), Qt::AutoConnection );
   }
   if( signal == ( QString ) "textChanged(QString)" )
   {
      ret = widget->connect( widget,  SIGNAL( textChanged( const QString &) ),
                             s_s, SLOT( textChanged( const QString & ) ), Qt::AutoConnection );
   }
   if( signal == ( QString ) "textEdited(QString)" )
   {
      ret = widget->connect( widget,  SIGNAL( textEdited( const QString &) ),
                             s_s, SLOT( textEdited( const QString & ) ), Qt::AutoConnection );
   }
   /* QTreeViewWidget */
   if( signal == ( QString ) "currentItemChanged(QTWItem)" )
   {
      ret = widget->connect( widget,  SIGNAL( currentItemChanged( QTreeWidgetItem *, QTreeWidgetItem * ) ),
                             s_s, SLOT( currentItemChanged( QTreeWidgetItem *, QTreeWidgetItem * ) ), Qt::AutoConnection );
   }
   if( signal == ( QString ) "itemActivated(QTWItem)" )
   {
      ret = widget->connect( widget,  SIGNAL( itemActivated( QTreeWidgetItem *, int ) ),
                             s_s, SLOT( itemActivated( QTreeWidgetItem *, int ) ), Qt::AutoConnection );
   }
   if( signal == ( QString ) "itemChanged(QTWItem)" )
   {
      ret = widget->connect( widget,  SIGNAL( itemChanged( QTreeWidgetItem *, int ) ),
                             s_s, SLOT( itemChanged( QTreeWidgetItem *, int ) ), Qt::AutoConnection );
   }
   if( signal == ( QString ) "itemClicked(QTWItem)" )
   {
      ret = widget->connect( widget,  SIGNAL( itemClicked( QTreeWidgetItem *, int ) ),
                             s_s, SLOT( itemClicked( QTreeWidgetItem *, int ) ), Qt::AutoConnection );
   }
   if( signal == ( QString ) "itemCollapsed(QTWItem)" )
   {
      ret = widget->connect( widget,  SIGNAL( itemCollapsed( QTreeWidgetItem * ) ),
                             s_s, SLOT( itemCollapsed( QTreeWidgetItem * ) ), Qt::AutoConnection );
   }
   if( signal == ( QString ) "itemDoubleClicked(QTWItem)" )
   {
      ret = widget->connect( widget,  SIGNAL( itemDoubleClicked( QTreeWidgetItem *, int ) ),
                             s_s, SLOT( itemDoubleClicked( QTreeWidgetItem *, int ) ), Qt::AutoConnection );
   }
   if( signal == ( QString ) "itemEntered(QTWItem)" )
   {
      ret = widget->connect( widget,  SIGNAL( itemEntered( QTreeWidgetItem *, int ) ),
                             s_s, SLOT( itemEntered( QTreeWidgetItem *, int ) ), Qt::AutoConnection );
   }
   if( signal == ( QString ) "itemExpanded(QTWItem)" )
   {
      ret = widget->connect( widget,  SIGNAL( itemExpanded( QTreeWidgetItem * ) ),
                             s_s, SLOT( itemExpanded( QTreeWidgetItem * ) ), Qt::AutoConnection );
   }
   if( signal == ( QString ) "itemPressed(QTWItem)" )
   {
      ret = widget->connect( widget,  SIGNAL( itemPressed( QTreeWidgetItem *, int ) ),
                             s_s, SLOT( itemPressed( QTreeWidgetItem *, int ) ), Qt::AutoConnection );
   }
   if( signal == ( QString ) "itemSelectionChanged()" )
   {
      ret = widget->connect( widget,  SIGNAL( itemSelectionChanged() ),
                             s_s, SLOT( itemSelectionChanged() ), Qt::AutoConnection );
   }
   /* QWebView */
   if( signal == ( QString ) "iconChanged()" )
   {
      ret = widget->connect( widget,  SIGNAL( iconChanged() ),
                             s_s, SLOT( iconChanged() ), Qt::AutoConnection );
   }
   if( signal == ( QString ) "titleChanged(QString)" )
   {
      ret = widget->connect( widget,  SIGNAL( titleChanged( const QString & ) ),
                             s_s, SLOT( titleChanged( const QString & ) ), Qt::AutoConnection );
   }
   if( signal == ( QString ) "urlChanged(QUrl)" )
   {
      ret = widget->connect( widget,  SIGNAL( urlChanged( const QUrl & ) ),
                             s_s, SLOT( urlChanged( const QUrl & ) ), Qt::AutoConnection );
   }
   /*  QWebPage */
   if( signal == ( QString ) "contentsChanged()" )
   {
      ret = widget->connect( widget,  SIGNAL( contentsChanged() ),
                             s_s, SLOT( contentsChanged() ), Qt::AutoConnection );
   }
   if( signal == ( QString ) "databaseQuotaExceeded(QWebFrame,QString)" )
   {
      ret = widget->connect( widget,  SIGNAL( databaseQuotaExceeded( QWebFrame, QString ) ),
                             s_s, SLOT( databaseQuotaExceeded( QWebFrame, QString ) ), Qt::AutoConnection );
   }
   if( signal == ( QString ) "downloadRequested(QNetworkRequest)" )
   {
      ret = widget->connect( widget,  SIGNAL( downloadRequested( const QNetworkRequest & ) ),
                             s_s, SLOT( downloadRequested( const QNetworkRequest & ) ), Qt::AutoConnection );
   }
   if( signal == ( QString ) "frameCreated(QWebFrame)" )
   {
      ret = widget->connect( widget,  SIGNAL( frameCreated( QWebFrame * ) ),
                             s_s, SLOT( frameCreated( QWebFrame * ) ), Qt::AutoConnection );
   }
   if( signal == ( QString ) "geometryChangeRequested(QRect)" )
   {
      ret = widget->connect( widget,  SIGNAL( geometryChangeRequested( const QRect & ) ),
                             s_s, SLOT( geometryChangeRequested( const QRect & ) ), Qt::AutoConnection );
   }
   if( signal == ( QString ) "linkClicked(QUrl)" )
   {
      ret = widget->connect( widget,  SIGNAL( linkClicked( const QUrl & ) ),
                             s_s, SLOT( linkClicked( const QUrl & ) ), Qt::AutoConnection );
   }
   if( signal == ( QString ) "linkHovered(QString,QString,QString)" )
   {
      ret = widget->connect( widget,  SIGNAL( linkHovered( const QString &, const QString &, const QString & ) ),
                             s_s, SLOT( linkHovered( const QString &, const QString &, const QString & ) ), Qt::AutoConnection );
   }
   if( signal == ( QString ) "loadFinished(bool)" )
   {
      ret = widget->connect( widget,  SIGNAL( loadFinished( bool ) ),
                             s_s, SLOT( loadFinished( bool ) ), Qt::AutoConnection );
   }
   if( signal == ( QString ) "loadProgress(int)" )
   {
      ret = widget->connect( widget,  SIGNAL( loadProgress( int ) ),
                             s_s, SLOT( loadProgress( int ) ), Qt::AutoConnection );
   }
   if( signal == ( QString ) "loadStarted()" )
   {
      ret = widget->connect( widget,  SIGNAL( loadStarted() ),
                             s_s, SLOT( loadStarted() ), Qt::AutoConnection );
   }
   if( signal == ( QString ) "menuBarVisibilityChangeRequested(bool)" )
   {
      ret = widget->connect( widget,  SIGNAL( menuBarVisibilityChangeRequested( bool ) ),
                             s_s, SLOT( menuBarVisibilityChangeRequested( bool ) ), Qt::AutoConnection );
   }
   if( signal == ( QString ) "microFocusChanged()" )
   {
      ret = widget->connect( widget,  SIGNAL( microFocusChanged() ),
                             s_s, SLOT( microFocusChanged() ), Qt::AutoConnection );
   }
   if( signal == ( QString ) "printRequested(QWebFrame)" )
   {
      ret = widget->connect( widget,  SIGNAL( printRequested( QWebFrame * ) ),
                             s_s, SLOT( printRequested( QWebFrame * ) ), Qt::AutoConnection );
   }
   if( signal == ( QString ) "repaintRequested(QRect)" )
   {
      ret = widget->connect( widget,  SIGNAL( repaintRequested( const QRect & ) ),
                             s_s, SLOT( repaintRequested( const QRect & ) ), Qt::AutoConnection );
   }
   if( signal == ( QString ) "restoreFrameStateRequested(QWebFrame)" )
   {
      ret = widget->connect( widget,  SIGNAL( restoreFrameStateRequested( QWebFrame * ) ),
                             s_s, SLOT( restoreFrameStateRequested( QWebFrame * ) ), Qt::AutoConnection );
   }
   if( signal == ( QString ) "saveFrameStateRequested(QWebFrame,QWebHistoryItem)" )
   {
      ret = widget->connect( widget,  SIGNAL( saveFrameStateRequested( QWebFrame *, QWebHistoryItem * ) ),
                             s_s, SLOT( saveFrameStateRequested( QWebFrame *, QWebHistoryItem * ) ), Qt::AutoConnection );
   }
   if( signal == ( QString ) "scrollRequested(int,int,QRect)" )
   {
      ret = widget->connect( widget,  SIGNAL( scrollRequested( int dx, int dy, const QRect & ) ),
                             s_s, SLOT( scrollRequested( int dx, int dy, const QRect & ) ), Qt::AutoConnection );
   }
   if( signal == ( QString ) "statusBarMessage(QString)" )
   {
      ret = widget->connect( widget,  SIGNAL( statusBarMessage( const QString & ) ),
                             s_s, SLOT( statusBarMessage( const QString & ) ), Qt::AutoConnection );
   }
   if( signal == ( QString ) "statusBarVisibilityChangeRequested(bool)" )
   {
      ret = widget->connect( widget,  SIGNAL( statusBarVisibilityChangeRequested( bool ) ),
                             s_s, SLOT( statusBarVisibilityChangeRequested( bool ) ), Qt::AutoConnection );
   }
   if( signal == ( QString ) "toolBarVisibilityChangeRequested(bool)" )
   {
      ret = widget->connect( widget,  SIGNAL( toolBarVisibilityChangeRequested( bool ) ),
                             s_s, SLOT( toolBarVisibilityChangeRequested( bool ) ), Qt::AutoConnection );
   }
   if( signal == ( QString ) "unsupportedContent(QNetworkReply)" )
   {
      ret = widget->connect( widget,  SIGNAL( unsupportedContent( QNetworkReply * ) ),
                             s_s, SLOT( unsupportedContent( QNetworkReply * ) ), Qt::AutoConnection );
   }
   if( signal == ( QString ) "windowCloseRequested()" )
   {
      ret = widget->connect( widget,  SIGNAL( windowCloseRequested() ),
                             s_s, SLOT( windowCloseRequested() ), Qt::AutoConnection );
   }
   /* QDialog (s) QFontDialog, QFileDialog */
   if( signal == ( QString ) "currentFontChanged(QFont)" )
   {
      ret = widget->connect( widget,  SIGNAL( currentFontChanged( const QFont & ) ),
                             s_s, SLOT( currentFontChanged( const QFont & ) ), Qt::AutoConnection );
   }
   if( signal == ( QString ) "fontSelected(QFont)" )
   {
      ret = widget->connect( widget,  SIGNAL( fontSelected( const QFont & ) ),
                             s_s, SLOT( fontSelected( const QFont & ) ), Qt::AutoConnection );
   }
   if( signal == ( QString ) "accepted()" )
   {
      ret = widget->connect( widget,  SIGNAL( accepted() ),
                             s_s, SLOT( accepted() ), Qt::AutoConnection );
   }
   if( signal == ( QString ) "finished(int)" )
   {
      ret = widget->connect( widget,  SIGNAL( finished( int ) ),
                             s_s, SLOT( finished( int ) ), Qt::AutoConnection );
   }
   if( signal == ( QString ) "rejected()" )
   {
      ret = widget->connect( widget,  SIGNAL( rejected() ),
                             s_s, SLOT( rejected() ), Qt::AutoConnection );
   }
   if( signal == ( QString ) "currentChanged(QString)" )
   {
      ret = widget->connect( widget,  SIGNAL( currentChanged( const QString & ) ),
                             s_s, SLOT( currentChanged( const QString & ) ), Qt::AutoConnection );
   }
   if( signal == ( QString ) "directoryEntered(QString)" )
   {
      ret = widget->connect( widget,  SIGNAL( directoryEntered( const QString & ) ),
                             s_s, SLOT( directoryEntered( const QString & ) ), Qt::AutoConnection );
   }
   if( signal == ( QString ) "fileSelected(QString)" )
   {
      ret = widget->connect( widget,  SIGNAL( fileSelected( const QString & ) ),
                             s_s, SLOT( fileSelected( const QString & ) ), Qt::AutoConnection );
   }
   if( signal == ( QString ) "filesSelected(QStringList)" )
   {
      ret = widget->connect( widget,  SIGNAL( filesSelected( const QStringList & ) ),
                             s_s, SLOT( filesSelected( const QStringList & ) ), Qt::AutoConnection );
   }
   if( signal == ( QString ) "filterSelected(QString)" )
   {
      ret = widget->connect( widget,  SIGNAL( filterSelected( const QString & ) ),
                             s_s, SLOT( filterSelected( const QString & ) ), Qt::AutoConnection );
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
         widget->setProperty( hb_parcx( 2 ), ( int ) i + 1 );
      }
      else
      {
         s_s->listBlock  << codeblock;
         s_s->listActv   << true;
         widget->setProperty( hb_parcx( 2 ), ( int ) s_s->listBlock.size() );
      }
   }
}

/*
 * harbour function to disconnect signals
 */
HB_FUNC( QT_DISCONNECT_SIGNAL )
{
   QWidget * widget = ( QWidget* ) hb_parptr( 1 );
   if( widget )
   {
      const char * event = hb_parcx( 2 );
      int i = widget->property( event ).toInt();

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

bool Events::eventFilter( QObject * obj, QEvent * event )
{
   QEvent::Type eventtype = event->type();
   int          found;;
   bool         ret;

   if( ( int ) eventtype == 0 )
      return true;

   char str[ 10 ];
   hb_snprintf( str, sizeof( str ), "%s%i%s", "P", eventtype, "P" );
   found = obj->property( str ).toInt();
   if( found == 0 )
      return false;

   PHB_ITEM pObject = hb_itemPutPtr( NULL, obj );
   PHB_ITEM pEvent  = hb_itemPutPtr( NULL, event );

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

#endif
