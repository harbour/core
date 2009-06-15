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

#define HBQT_EVT_CLICKED          1
#define HBQT_EVT_TRIGGERED        2
#define HBQT_EVT_TRIGGERED_BOOL   3
#define HBQT_EVT_HOVERED          4
#define HBQT_EVT_STATECHANGED     5
#define HBQT_EVT_PRESSED          6
#define HBQT_EVT_RELEASED         7

/*----------------------------------------------------------------------*/

static Slots *s = NULL;

/*----------------------------------------------------------------------*/

static void SlotsExec( QWidget* widget, QString event, PHB_ITEM pItem )
{
   for( int i = 0; i < s->list1.size(); ++i )
   {
      if( ( QWidget* ) s->list1.at( i ) == widget )
      {
        if( ( ( QString ) s->list2.at( i ) == event ) && ( ( bool ) s->list4.at( i ) == true ) )
        {
           PHB_ITEM pWidget = hb_itemPutPtr( NULL, ( QWidget* ) widget );
           hb_vmEvalBlockV( ( PHB_ITEM ) s->list3.at( i ), 1, pWidget );
           hb_itemRelease( pWidget );
        }
      }
   }
   if( pItem != NULL )
   {
      hb_itemRelease( pItem );
   }
}

static void SlotsExecAction( QAction* widget, QString event, PHB_ITEM pItem )
{
   for( int i = 0; i < s->list1.size(); ++i )
   {
      if( ( QAction* ) s->list5.at( i ) == widget )
      {
        if( ( ( QString ) s->list2.at( i ) == event ) && ( ( bool ) s->list4.at( i ) == true ) )
        {
           PHB_ITEM pWidget = hb_itemPutPtr( NULL, ( QAction* ) widget );
           hb_vmEvalBlockV( ( PHB_ITEM ) s->list3.at( i ), 1, pWidget );
           hb_itemRelease( pWidget );
        }
      }
   }
   if( pItem != NULL )
   {
      hb_itemRelease( pItem );
   }
}

static void SlotsExecInt( QWidget* widget, QString event, PHB_ITEM pItem, int iValue )
{
   for( int i = 0; i < s->list1.size(); ++i )
   {
      if( ( QWidget* ) s->list1.at( i ) == widget )
      {
         if( ( ( QString ) s->list2.at( i ) == event ) && ( ( bool ) s->list4.at( i ) == true ) )
         {
            PHB_ITEM pWidget = hb_itemPutPtr( NULL, ( QWidget* ) widget );
            PHB_ITEM pState = hb_itemPutNI( NULL, iValue );
            hb_vmEvalBlockV( ( PHB_ITEM ) s->list3.at( i ), 2, pWidget, pState );
            hb_itemRelease( pWidget );
            hb_itemRelease( pState );
         }
      }
   }
   if( pItem != NULL )
   {
      hb_itemRelease( pItem );
   }
}

static void SlotsExecModel( QWidget* widget, QString event, PHB_ITEM pItem, const QModelIndex & index )
{
   for( int i = 0; i < s->list1.size(); ++i )
   {
      if( ( QWidget* ) s->list1.at( i ) == widget )
      {
         if( ( ( QString ) s->list2.at( i ) == event ) && ( ( bool ) s->list4.at( i ) == true ) )
         {
            PHB_ITEM pWidget = hb_itemPutPtr( NULL, ( QWidget* ) widget );
            PHB_ITEM pState = hb_itemPutPtr( NULL, ( QModelIndex * ) new QModelIndex( index ) );
            hb_vmEvalBlockV( ( PHB_ITEM ) s->list3.at( i ), 2, pWidget, pState );
            hb_itemRelease( pWidget );
            hb_itemRelease( pState );
         }
      }
   }
   if( pItem != NULL )
   {
      hb_itemRelease( pItem );
   }
}

Slots::Slots( QObject* parent ) : QObject( parent )
{
}

Slots::~Slots()
{
}

bool Slots::event( QEvent * event )
{
   bool bAccepted = true;
   QObject *widget = qobject_cast<QObject *>( sender() );
   for( int i = 0; i < list1.size(); ++i )
   {
      if( ( QObject* ) list1.at( i ) == ( QObject* ) widget )
      {
         if( ( ( QString ) list2.at( i ) == ( QString ) "event()" ) && ( ( bool ) list4.at( i ) == true ) )
         {
            PHB_ITEM pWidget  = hb_itemPutPtr( NULL, ( QWidget * ) widget );
            PHB_ITEM pEvent   = hb_itemPutPtr( NULL, event );
            bAccepted = hb_vmEvalBlockV( ( PHB_ITEM ) list3.at( i ), 2, pWidget, pEvent );
            hb_itemRelease( pWidget );
            hb_itemRelease( pEvent );
         }
      }
   }
   return bAccepted;
}

void Slots::keyPressEvent( QKeyEvent * event )
{
   QObject *widget = qobject_cast<QObject *>( sender() );
   for( int i = 0; i < list1.size(); ++i )
   {
      if( ( QObject* ) list1.at( i ) == ( QObject* ) widget )
      {
         if( ( ( QString ) list2.at( i ) == ( QString ) "keyPressEvent()" ) && ( ( bool ) list4.at( i ) == true ) )
         {
            PHB_ITEM pWidget  = hb_itemPutPtr( NULL, ( QWidget * ) widget );
            PHB_ITEM pEvent   = hb_itemPutPtr( NULL, event );
            hb_vmEvalBlockV( ( PHB_ITEM ) list3.at( i ), 2, pWidget, pEvent );
            hb_itemRelease( pWidget );
            hb_itemRelease( pEvent );
         }
      }
   }
}

void Slots::mouseMoveEvent( QMouseEvent * event )
{
   QObject *widget = qobject_cast<QObject *>( sender() );
   for( int i = 0; i < list1.size(); ++i )
   {
      if( ( QObject* ) list1.at( i ) == ( QObject* ) widget )
      {
         if( ( ( QString ) list2.at( i ) == ( QString ) "mouseMoveEvent()" ) && ( ( bool ) list4.at( i ) == true ) )
         {
            PHB_ITEM pWidget  = hb_itemPutPtr( NULL, ( QWidget * ) widget );
            PHB_ITEM pEvent   = hb_itemPutPtr( NULL, event );
            hb_vmEvalBlockV( ( PHB_ITEM ) list3.at( i ), 2, pWidget, pEvent );
            hb_itemRelease( pWidget );
            hb_itemRelease( pEvent );
         }
      }
   }
}

void Slots::hovered( QAction * action )
{
   QObject *widget = qobject_cast<QObject *>( sender() );
   for( int i = 0; i < list1.size(); ++i )
   {
      if( ( QObject* ) list1.at( i ) == ( QObject* ) widget )
      {
         if( ( ( QString ) list2.at( i ) == ( QString ) "hovered(action)" ) && ( ( bool ) list4.at( i ) == true ) )
         {
            PHB_ITEM pWidget  = hb_itemPutPtr( NULL, ( QWidget * ) widget );
            PHB_ITEM pEvent   = hb_itemPutPtr( NULL, action );
            hb_vmEvalBlockV( ( PHB_ITEM ) list3.at( i ), 2, pWidget, pEvent );
            hb_itemRelease( pWidget );
            hb_itemRelease( pEvent );
         }
      }
   }
}

void Slots::clicked()
{
   QWidget *widget = qobject_cast<QWidget *>( sender() );
   SlotsExec( widget, ( QString ) "clicked()", NULL );
}

void Slots::returnPressed()
{
   QWidget *widget = qobject_cast<QWidget *>( sender() );
   SlotsExec( widget, ( QString ) "returnPressed()", NULL );
}

void Slots::viewportEntered()
{
   QWidget *widget = qobject_cast<QWidget *>( sender() );
   SlotsExec( widget, ( QString ) "viewportEntered()", NULL );
}

void Slots::pressed()
{
   QWidget *widget = qobject_cast<QWidget *>( sender() );
   SlotsExec( widget, ( QString ) "pressed()", NULL );
}

void Slots::released()
{
   QWidget *widget = qobject_cast<QWidget *>( sender() );
   SlotsExec( widget, ( QString ) "released()", NULL );
}

void Slots::hovered()
{
   //QWidget *widget = qobject_cast<QWidget *>( sender() );
   QAction *widget = qobject_cast<QAction *>( sender() );
   if( widget )
   {
      SlotsExecAction( widget, ( QString ) "hovered()", NULL );
   }
}

void Slots::triggered()
{
   QWidget *widget = qobject_cast<QWidget *>( sender() );
   SlotsExec( widget, ( QString ) "triggered()", NULL );
}

void Slots::triggered( bool checked )
{
   QObject *widget = qobject_cast<QObject *>( sender() );
   for( int i = 0; i < list1.size(); ++i )
   {
      if( ( QObject* ) list1.at( i ) == ( QObject* ) widget )
      {
         if( ( ( QString ) list2.at( i ) == ( QString ) "triggered(bool)" ) && ( ( bool ) list4.at( i ) == true ) )
         {
            PHB_ITEM pWidget  = hb_itemPutPtr( NULL, ( QObject * ) widget );
            PHB_ITEM pChecked = hb_itemPutL( NULL, checked );

            hb_vmEvalBlockV( ( PHB_ITEM ) list3.at( i ), 2, pWidget, pChecked );
            hb_itemRelease( pWidget );
            hb_itemRelease( pChecked );
         }
      }
   }
}

void Slots::stateChanged( int state )
{
   QWidget *widget = qobject_cast<QWidget *>( sender() );
   SlotsExecInt( widget, ( QString ) "stateChanged(int)", NULL, state );
}

void Slots::activated( int index )
{
   QWidget *widget = qobject_cast<QWidget *>( sender() );
   SlotsExecInt( widget, ( QString ) "activated(int)", NULL, index );
}

void Slots::currentIndexChanged( int index )
{
   QWidget *widget = qobject_cast<QWidget *>( sender() );
   SlotsExecInt( widget, ( QString ) "currentIndexChanged(int)", NULL, index );
}

void Slots::currentChanged( int index )
{
   QWidget *widget = qobject_cast<QWidget *>( sender() );
   SlotsExecInt( widget, ( QString ) "currentChanged(int)", NULL, index );
}

void Slots::highlighted( int index )
{
   QWidget *widget = qobject_cast<QWidget *>( sender() );
   SlotsExecInt( widget, ( QString ) "highlighted(int)", NULL, index );
}

void Slots::clicked( const QModelIndex & index )
{
   QWidget * widget = qobject_cast<QWidget *>( sender() );
   SlotsExecModel( widget, ( QString ) "clicked(QModelIndex)", NULL, index );
}
void Slots::doubleClicked( const QModelIndex & index )
{
   QWidget * widget = qobject_cast<QWidget *>( sender() );
   SlotsExecModel( widget, ( QString ) "doubleClicked(QModelIndex)", NULL, index );
}
void Slots::entered( const QModelIndex & index )
{
   QWidget * widget = qobject_cast<QWidget *>( sender() );
   SlotsExecModel( widget, ( QString ) "entered(QModelIndex)", NULL, index );
}

/*
 * harbour function to connect signals with slots
 */
HB_FUNC( QT_CONNECT_SIGNAL )
{
   QWidget * widget    = ( QWidget* ) hb_parptr( 1 ); /* get sender */
   QString   signal    = hb_parc( 2 );                /* get signal */
   PHB_ITEM  codeblock = hb_itemNew( hb_param( 3, HB_IT_BLOCK | HB_IT_BYREF ) ); /* get codeblock */
   bool      ret       = false;                       /* return value */
   bool      bAct      = false;

   /* create object s, if not created yet */
   if( s == NULL )
   {
      s = new Slots();
   }

   /* connect signal with slot
    * if the list become to long, more classes can be created
    * TODO: parameter Qt::AutoConnection
    */

   /*                    Events with no parameters                  */
   if( signal == ( QString ) "clicked()" )
   {
      ret = widget->connect( widget, SIGNAL( clicked() )          , s, SLOT( clicked() )          , Qt::AutoConnection );
   }
   if( signal == ( QString ) "returnPressed()" )
   {
      ret = widget->connect( widget, SIGNAL( returnPressed() )    , s, SLOT( returnPressed() )    , Qt::AutoConnection );
   }
   if( signal == ( QString ) "triggered()" )
   {
      ret = widget->connect( widget, SIGNAL( triggered() )        , s, SLOT( triggered() )        , Qt::AutoConnection );
   }
   if( signal == ( QString ) "hovered()" )
   {
      ret = widget->connect( widget, SIGNAL( hovered() )          , s, SLOT( hovered() )          , Qt::AutoConnection );
      bAct = true;
   }
   if( signal == ( QString ) "viewportEntered()" )
   {
      ret = widget->connect( widget, SIGNAL( viewportEntered() )  , s, SLOT( viewportEntered() ) , Qt::AutoConnection );
   }
   if( signal == ( QString ) "pressed()" )
   {
      ret = widget->connect( widget, SIGNAL( pressed() )          , s, SLOT( pressed() )          , Qt::AutoConnection );
   }
   if( signal == ( QString ) "released()" )
   {
      ret = widget->connect( widget, SIGNAL( released() )         ,
                             s, SLOT( released() ), Qt::AutoConnection );
   }
   /*                   Events with int parameter                 */
   if( signal == ( QString ) "stateChanged(int)" )
   {
      ret = widget->connect( widget, SIGNAL( stateChanged( int ) ),
                             s, SLOT( stateChanged( int ) ), Qt::AutoConnection );
   }
   if( signal == ( QString ) "activated(int)" )
   {
      ret = widget->connect( widget, SIGNAL( activated( int ) ),
                             s, SLOT( activated( int ) )   , Qt::AutoConnection );
   }
   if( signal == ( QString ) "currentIndexChanged(int)" )
   {
      ret = widget->connect( widget, SIGNAL( currentIndexChanged( int ) ),
                             s, SLOT( currentIndexChanged( int ) ), Qt::AutoConnection );
   }
   if( signal == ( QString ) "highlighted(int)" )
   {
      ret = widget->connect( widget, SIGNAL( highlighted( int ) ) ,
                             s, SLOT( highlighted( int ) ) , Qt::AutoConnection );
   }
   /*                    Events with bool parameter               */
   if( signal == ( QString ) "triggered(bool)" )
   {
      ret = widget->connect( widget, SIGNAL( triggered( bool ) ),
                             s, SLOT( triggered( bool ) )  , Qt::AutoConnection );
   }
   /*                    Events with miscellaneous parameters     */
   if( signal == ( QString ) "clicked(QModelIndex)" )
   {
      ret = widget->connect( widget, SIGNAL( clicked( const QModelIndex & ) ),
                             s, SLOT( clicked( const QModelIndex & ) ), Qt::AutoConnection );
   }
   if( signal == ( QString ) "doubleClicked(QModelIndex)" )
   {
      ret = widget->connect( widget, SIGNAL( doubleClicked( const QModelIndex & ) ),
                             s, SLOT( doubleClicked( const QModelIndex & ) ), Qt::AutoConnection );
   }
   if( signal == ( QString ) "entered(QModelIndex)" )
   {
      ret = widget->connect( widget, SIGNAL( entered( const QModelIndex & ) ),
                             s, SLOT( entered( const QModelIndex & ) ), Qt::AutoConnection );
   }
   if( signal == ( QString ) "event()" )
   {
      ret = widget->connect( widget, SIGNAL( event( QEvent * ) ),
                             s, SLOT( event( QEvent * ) ) , Qt::AutoConnection );
   }
   if( signal == ( QString ) "keyPressEvent()" )
   {
      ret = widget->connect( widget, SIGNAL( sg_keyPressEvent( QKeyEvent * ) ),
                             s, SLOT( keyPressEvent( QKeyEvent * ) ), Qt::AutoConnection );
   }
   if( signal == ( QString ) "mouseMoveEvent()" )
   {
      ret = widget->connect( widget, SIGNAL( sg_mouseMoveEvent( QMouseEvent * ) ),
                             s, SLOT( mouseMoveEvent( QMouseEvent * ) ), Qt::AutoConnection );
   }
   if( signal == ( QString ) "hovered(action)" )
   {
      ret = widget->connect( widget, SIGNAL( hovered( QAction * ) ),
                             s, SLOT( hovered( QAction * ) ), Qt::AutoConnection );
   }
   if( signal == ( QString ) "currentChanged(int)" )
   {
      ret = widget->connect( widget, SIGNAL( currentChanged( int ) ),
                             s, SLOT( currentChanged( int ) ), Qt::AutoConnection );
   }

   /* return connect result */
   hb_retl( ret );

   /* if connected: store widget, signal, codeblock and flag
    * TODO: locate a inactive entry and use it
    */
   if( ret == true )
   {
      if( bAct )
      {
         s->list5 << ( QAction* ) hb_parptr( 1 );
         s->list1 << NULL;
      }
      else
      {
         s->list5 << NULL;
         s->list1 << widget;
      }
      s->list2 << signal;
      s->list3 << codeblock;
      s->list4 << true;
   }
}


/*
 * harbour function to disconnect signals
 */
HB_FUNC( QT_DISCONNECT_SIGNAL )
{
   /* TODO */
}


/*
 * harbour function to release all codeblocks storeds
 */
#if 0
HB_FUNC( RELEASE_CODEBLOCKS )
{
   if( s )
   {
      for( int i = 0; i < s->list1.size(); ++i )
      {
         if( ( bool ) s->list4.at( i ) == true )
         {
            hb_itemRelease( ( PHB_ITEM ) s->list3.at( i ) );
         }
      }
   }
}
#endif


/*
 * C function to release all codeblocks storeds
 * called at end of the program
 * see qapplication.cpp
 */
void release_codeblocks( void )
{
   if( s )
   {
      for( int i = 0; i < s->list1.size(); ++i )
      {
         if( ( bool ) s->list4.at( i ) == true )
         {
            hb_itemRelease( ( PHB_ITEM ) s->list3.at( i ) );
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
}

void MyMainWindow::closeEvent( QCloseEvent *event )
{
   event->accept();
}

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

HB_FUNC( QT_MYMAINWINDOW )
{
   hb_retptr( ( MyMainWindow * ) new MyMainWindow() );
}

HB_FUNC( QT_MYDRAWINGAREA )
{
   hb_retptr( ( MyDrawingArea * ) new MyDrawingArea() );
}

/*----------------------------------------------------------------------*/
#endif

