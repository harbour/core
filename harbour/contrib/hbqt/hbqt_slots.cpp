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

#if 0
static void SlotsExecFocusEvent( QWidget* widget, char* event, QFocusEvent* mevent )
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
#endif

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
   if( widget )
   {
      int i = widget->property( "currentItemChanged(QTWItem)" ).toInt();
      if( ( i > 0 ) && ( s_s->listActv.at( i - 1 ) == true ) )
      {
         PHB_ITEM pWidget = hb_itemPutPtr( NULL, widget );
         PHB_ITEM pItemC  = hb_itemPutPtr( NULL, current );
         PHB_ITEM pItemP  = hb_itemPutPtr( NULL, previous );
         hb_vmEvalBlockV( ( PHB_ITEM ) s_s->listBlock.at( i - 1 ), 3, pWidget, pItemC, pItemP );
         hb_itemRelease( pWidget );
         hb_itemRelease( pItemC );
         hb_itemRelease( pItemP );
      }
   }
}
static void SlotsExecTreeWidgetItemInt( QWidget * widget, char* event, QTreeWidgetItem * item, int column )
{
   if( widget )
   {
      int i = widget->property( event ).toInt();
      if( ( i > 0 ) && ( s_s->listActv.at( i - 1 ) == true ) )
      {
         PHB_ITEM pWidget = hb_itemPutPtr( NULL, widget );
         PHB_ITEM pItem   = hb_itemPutPtr( NULL, item );
         if( column != -1001 )
         {
            PHB_ITEM pColumn = hb_itemPutNI( NULL, column );
            hb_vmEvalBlockV( ( PHB_ITEM ) s_s->listBlock.at( i - 1 ), 3, pWidget, pItem, pColumn );
            hb_itemRelease( pColumn );
         }
         else
         {
            hb_vmEvalBlockV( ( PHB_ITEM ) s_s->listBlock.at( i - 1 ), 2, pWidget, pItem );
         }
         hb_itemRelease( pWidget );
         hb_itemRelease( pItem );
      }
   }
}
void Slots::itemActivated( QTreeWidgetItem * item, int column )
{
   QWidget *widget = qobject_cast<QWidget *>( sender() );
   SlotsExecTreeWidgetItemInt( widget, ( char* ) "itemActivated(QTWItem)", item, column );
}
void Slots::itemChanged( QTreeWidgetItem * item, int column )
{
   QWidget *widget = qobject_cast<QWidget *>( sender() );
   SlotsExecTreeWidgetItemInt( widget, ( char* ) "itemChanged(QTWItem)", item, column );
}
void Slots::itemClicked( QTreeWidgetItem * item, int column )
{
   QWidget *widget = qobject_cast<QWidget *>( sender() );
   SlotsExecTreeWidgetItemInt( widget, ( char* ) "itemClicked(QTWItem)", item, column );
}
void Slots::itemDoubleClicked( QTreeWidgetItem * item, int column )
{
   QWidget *widget = qobject_cast<QWidget *>( sender() );
   SlotsExecTreeWidgetItemInt( widget, ( char* ) "itemDoubleClicked(QTWItem)", item, column );
}
void Slots::itemEntered( QTreeWidgetItem * item, int column )
{
   QWidget *widget = qobject_cast<QWidget *>( sender() );
   SlotsExecTreeWidgetItemInt( widget, ( char* ) "itemEntered(QTWItem)", item, column );
}
void Slots::itemPressed( QTreeWidgetItem * item, int column )
{
   QWidget *widget = qobject_cast<QWidget *>( sender() );
   SlotsExecTreeWidgetItemInt( widget, ( char* ) "itemPressed(QTWItem)", item, column );
}
void Slots::itemExpanded( QTreeWidgetItem * item )
{
   QWidget *widget = qobject_cast<QWidget *>( sender() );
   SlotsExecTreeWidgetItemInt( widget, ( char* ) "itemExpanded(QTWItem)", item, -1001 );
}
void Slots::itemCollapsed( QTreeWidgetItem * item )
{
   QWidget *widget = qobject_cast<QWidget *>( sender() );
   SlotsExecTreeWidgetItemInt( widget, ( char* ) "itemCollapsed(QTWItem)", item, -1001 );
}
void Slots::itemSelectionChanged()
{
   QWidget *widget = qobject_cast<QWidget *>( sender() );
   SlotsExec( widget, ( char* ) "itemSelectionChanged()" );
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
      char * event = hb_parcx( 2 );
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
                           Qt::Window ;
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
   return true;
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
   qDebug( hb_parcx( 1 ) );
}

/*----------------------------------------------------------------------*/

#endif
