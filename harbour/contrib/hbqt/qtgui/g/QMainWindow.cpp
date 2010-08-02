/*
 * $Id$
 */

/* -------------------------------------------------------------------- */
/* WARNING: Automatically generated source file. DO NOT EDIT!           */
/*          Instead, edit corresponding .qth file,                      */
/*          or the generator tool itself, and run regenarate.           */
/* -------------------------------------------------------------------- */

/*
 * Harbour Project source code:
 * QT wrapper main header
 *
 * Copyright 2009-2010 Pritpal Bedi <pritpal@vouchcac.com>
 *
 * Copyright 2009 Marcos Antonio Gambeta <marcosgambeta at gmail dot com>
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
#include "hbqtgui_garbage.h"
#include "hbqtgui.h"
#include "hbqtcore_garbage.h"
#include "hbqtcore.h"

/*----------------------------------------------------------------------*/
#if QT_VERSION >= 0x040500
/*----------------------------------------------------------------------*/

/*
 *  enum DockOption { AnimatedDocks, AllowNestedDocks, AllowTabbedDocks, ForceTabbedDocks, VerticalTabs }
 *  flags DockOptions
 */

/*
 *  Constructed[ 49/50 [ 98.00% ] ]
 *
 *  *** Unconvered Prototypes ***
 *  -----------------------------
 *
 *  }
 */

#include <QtCore/QPointer>

#include <QtGui/QMainWindow>
#include <QtCore/QSettings>


/*
 * QMainWindow ( QWidget * parent = 0, Qt::WindowFlags flags = 0 )
 * ~QMainWindow ()
 */

HB_FUNC( HBQT_QMAINWINDOW_SAVESETTINGS )
{
   QSettings qSet( hbqt_par_QString( 1 ), QSettings::IniFormat );

   qSet.setValue( hbqt_par_QString( 2 ), hbqt_par_QMainWindow( 3 )->saveState() );
}

HB_FUNC( HBQT_QMAINWINDOW_RESTSETTINGS )
{
   QSettings qSet( hbqt_par_QString( 1 ), QSettings::IniFormat );

   hbqt_par_QMainWindow( 3 )->restoreState( qSet.value( hbqt_par_QString( 2 ) ).toByteArray() );
}

typedef struct
{
   QPointer< QMainWindow > ph;
   bool bNew;
   QT_G_FUNC_PTR func;
   int type;
} QGC_POINTER_QMainWindow;

QT_G_FUNC( hbqt_gcRelease_QMainWindow )
{
   QMainWindow  * ph = NULL ;
   QGC_POINTER_QMainWindow * p = ( QGC_POINTER_QMainWindow * ) Cargo;

   if( p && p->bNew && p->ph )
   {
      ph = p->ph;
      if( ph )
      {
         const QMetaObject * m = ( ph )->metaObject();
         if( ( QString ) m->className() != ( QString ) "QObject" )
         {
            HB_TRACE( HB_TR_DEBUG, ( "ph=%p %p YES_rel_QMainWindow   /.\\   ", (void*) ph, (void*) p->ph ) );
            delete ( p->ph );
            HB_TRACE( HB_TR_DEBUG, ( "ph=%p %p YES_rel_QMainWindow   \\./   ", (void*) ph, (void*) p->ph ) );
            p->ph = NULL;
         }
         else
         {
            HB_TRACE( HB_TR_DEBUG, ( "ph=%p NO__rel_QMainWindow          ", ph ) );
            p->ph = NULL;
         }
      }
      else
      {
         HB_TRACE( HB_TR_DEBUG, ( "ph=%p DEL_rel_QMainWindow    :     Object already deleted!", ph ) );
         p->ph = NULL;
      }
   }
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p PTR_rel_QMainWindow    :    Object not created with new=true", ph ) );
      p->ph = NULL;
   }
}

void * hbqt_gcAllocate_QMainWindow( void * pObj, bool bNew )
{
   QGC_POINTER_QMainWindow * p = ( QGC_POINTER_QMainWindow * ) hb_gcAllocate( sizeof( QGC_POINTER_QMainWindow ), hbqt_gcFuncs() );

   new( & p->ph ) QPointer< QMainWindow >( ( QMainWindow * ) pObj );
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QMainWindow;
   p->type = HBQT_TYPE_QMainWindow;

   if( bNew )
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p    _new_QMainWindow  under p->pq", pObj ) );
   }
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p NOT_new_QMainWindow", pObj ) );
   }
   return p;
}

HB_FUNC( QT_QMAINWINDOW )
{
   QMainWindow * pObj = NULL;

   pObj =  new QMainWindow( hbqt_par_QWidget( 1 ), ( Qt::WindowFlags ) hb_parni( 2 ) ) ;

   hb_retptrGC( hbqt_gcAllocate_QMainWindow( ( void * ) pObj, true ) );
}

/*
 * void addDockWidget ( Qt::DockWidgetArea area, QDockWidget * dockwidget )
 */
HB_FUNC( QT_QMAINWINDOW_ADDDOCKWIDGET )
{
   QMainWindow * p = hbqt_par_QMainWindow( 1 );
   if( p )
      ( p )->addDockWidget( ( Qt::DockWidgetArea ) hb_parni( 2 ), hbqt_par_QDockWidget( 3 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QMAINWINDOW_ADDDOCKWIDGET FP=( p )->addDockWidget( ( Qt::DockWidgetArea ) hb_parni( 2 ), hbqt_par_QDockWidget( 3 ) ); p is NULL" ) );
   }
}

/*
 * void addDockWidget ( Qt::DockWidgetArea area, QDockWidget * dockwidget, Qt::Orientation orientation )
 */
HB_FUNC( QT_QMAINWINDOW_ADDDOCKWIDGET_1 )
{
   QMainWindow * p = hbqt_par_QMainWindow( 1 );
   if( p )
      ( p )->addDockWidget( ( Qt::DockWidgetArea ) hb_parni( 2 ), hbqt_par_QDockWidget( 3 ), ( Qt::Orientation ) hb_parni( 4 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QMAINWINDOW_ADDDOCKWIDGET_1 FP=( p )->addDockWidget( ( Qt::DockWidgetArea ) hb_parni( 2 ), hbqt_par_QDockWidget( 3 ), ( Qt::Orientation ) hb_parni( 4 ) ); p is NULL" ) );
   }
}

/*
 * void addToolBar ( Qt::ToolBarArea area, QToolBar * toolbar )
 */
HB_FUNC( QT_QMAINWINDOW_ADDTOOLBAR )
{
   QGC_POINTER_QMainWindow * q = ( QGC_POINTER_QMainWindow * ) hb_parptrGC( hbqt_gcFuncs(), 1 );
   QGC_POINTER * p = ( QGC_POINTER * ) hb_parptrGC( hbqt_gcFuncs(), 3 );

   HB_TRACE( HB_TR_DEBUG, ("QMAINWINDOW_ADDTOOLBAR" ) );
   HB_TRACE( HB_TR_DEBUG, ( "QT_QMAINWINDOW_ADDTOOLBAR() Qt object: %p  to: %p", (void *) p, (void *) q) );
   HB_TRACE( HB_TR_DEBUG, ( "QT_QMAINWINDOW_ADDTOOLBAR() Qt object: %p  to: %p", (void *) p->ph, (void *) q->ph) );

   if ( p && p->ph && q && q->ph )
   {
      HB_TRACE( HB_TR_DEBUG, ( "QT_QMAINWINDOW_ADDTOOLBAR() Qt object: %p is attached to: %p", (void *) p->ph, (void *) q->ph) );
      p->bNew = HB_FALSE;
      if ( q && q->ph )
         ( q->ph )->addToolBar( ( Qt::ToolBarArea ) hb_parni( 2 ), ( ( QToolBar *) p->ph ));
      else
      {
         HB_TRACE( HB_TR_DEBUG, ( "F=QT_QTOOLBAR_ADDACTION FP=( p )->addAction( hbqt_par_QAction( 2 ) ); p is NULL" ));
      }
   }
}

/*
 * void addToolBar ( QToolBar * toolbar )
 */
HB_FUNC( QT_QMAINWINDOW_ADDTOOLBAR_1 )
{
   QMainWindow * p = hbqt_par_QMainWindow( 1 );
   if( p )
      ( p )->addToolBar( hbqt_par_QToolBar( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QMAINWINDOW_ADDTOOLBAR_1 FP=( p )->addToolBar( hbqt_par_QToolBar( 2 ) ); p is NULL" ) );
   }
}

/*
 * QToolBar * addToolBar ( const QString & title )
 */
HB_FUNC( QT_QMAINWINDOW_ADDTOOLBAR_2 )
{
   QMainWindow * p = hbqt_par_QMainWindow( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QToolBar( ( p )->addToolBar( QMainWindow::tr( hb_parc( 2 ) ) ), false ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QMAINWINDOW_ADDTOOLBAR_2 FP=hb_retptrGC( hbqt_gcAllocate_QToolBar( ( p )->addToolBar( QMainWindow::tr( hb_parc( 2 ) ) ), false ) ); p is NULL" ) );
   }
}

/*
 * void addToolBarBreak ( Qt::ToolBarArea area = Qt::TopToolBarArea )
 */
HB_FUNC( QT_QMAINWINDOW_ADDTOOLBARBREAK )
{
   QMainWindow * p = hbqt_par_QMainWindow( 1 );
   if( p )
      ( p )->addToolBarBreak( ( HB_ISNUM( 2 ) ? ( Qt::ToolBarArea ) hb_parni( 2 ) : ( Qt::ToolBarArea ) Qt::TopToolBarArea ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QMAINWINDOW_ADDTOOLBARBREAK FP=( p )->addToolBarBreak( ( HB_ISNUM( 2 ) ? ( Qt::ToolBarArea ) hb_parni( 2 ) : ( Qt::ToolBarArea ) Qt::TopToolBarArea ) ); p is NULL" ) );
   }
}

/*
 * QWidget * centralWidget () const
 */
HB_FUNC( QT_QMAINWINDOW_CENTRALWIDGET )
{
   QMainWindow * p = hbqt_par_QMainWindow( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QWidget( ( p )->centralWidget(), false ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QMAINWINDOW_CENTRALWIDGET FP=hb_retptrGC( hbqt_gcAllocate_QWidget( ( p )->centralWidget(), false ) ); p is NULL" ) );
   }
}

/*
 * Qt::DockWidgetArea corner ( Qt::Corner corner ) const
 */
HB_FUNC( QT_QMAINWINDOW_CORNER )
{
   QMainWindow * p = hbqt_par_QMainWindow( 1 );
   if( p )
      hb_retni( ( Qt::DockWidgetArea ) ( p )->corner( ( Qt::Corner ) hb_parni( 2 ) ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QMAINWINDOW_CORNER FP=hb_retni( ( Qt::DockWidgetArea ) ( p )->corner( ( Qt::Corner ) hb_parni( 2 ) ) ); p is NULL" ) );
   }
}

/*
 * virtual QMenu * createPopupMenu ()
 */
HB_FUNC( QT_QMAINWINDOW_CREATEPOPUPMENU )
{
   QMainWindow * p = hbqt_par_QMainWindow( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QMenu( ( p )->createPopupMenu(), false ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QMAINWINDOW_CREATEPOPUPMENU FP=hb_retptrGC( hbqt_gcAllocate_QMenu( ( p )->createPopupMenu(), false ) ); p is NULL" ) );
   }
}

/*
 * DockOptions dockOptions () const
 */
HB_FUNC( QT_QMAINWINDOW_DOCKOPTIONS )
{
   QMainWindow * p = hbqt_par_QMainWindow( 1 );
   if( p )
      hb_retni( ( QMainWindow::DockOptions ) ( p )->dockOptions() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QMAINWINDOW_DOCKOPTIONS FP=hb_retni( ( QMainWindow::DockOptions ) ( p )->dockOptions() ); p is NULL" ) );
   }
}

/*
 * Qt::DockWidgetArea dockWidgetArea ( QDockWidget * dockwidget ) const
 */
HB_FUNC( QT_QMAINWINDOW_DOCKWIDGETAREA )
{
   QMainWindow * p = hbqt_par_QMainWindow( 1 );
   if( p )
      hb_retni( ( Qt::DockWidgetArea ) ( p )->dockWidgetArea( hbqt_par_QDockWidget( 2 ) ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QMAINWINDOW_DOCKWIDGETAREA FP=hb_retni( ( Qt::DockWidgetArea ) ( p )->dockWidgetArea( hbqt_par_QDockWidget( 2 ) ) ); p is NULL" ) );
   }
}

/*
 * bool documentMode () const
 */
HB_FUNC( QT_QMAINWINDOW_DOCUMENTMODE )
{
   QMainWindow * p = hbqt_par_QMainWindow( 1 );
   if( p )
      hb_retl( ( p )->documentMode() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QMAINWINDOW_DOCUMENTMODE FP=hb_retl( ( p )->documentMode() ); p is NULL" ) );
   }
}

/*
 * QSize iconSize () const
 */
HB_FUNC( QT_QMAINWINDOW_ICONSIZE )
{
   QMainWindow * p = hbqt_par_QMainWindow( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QSize( new QSize( ( p )->iconSize() ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QMAINWINDOW_ICONSIZE FP=hb_retptrGC( hbqt_gcAllocate_QSize( new QSize( ( p )->iconSize() ), true ) ); p is NULL" ) );
   }
}

/*
 * void insertToolBar ( QToolBar * before, QToolBar * toolbar )
 */
HB_FUNC( QT_QMAINWINDOW_INSERTTOOLBAR )
{
   QMainWindow * p = hbqt_par_QMainWindow( 1 );
   if( p )
      ( p )->insertToolBar( hbqt_par_QToolBar( 2 ), hbqt_par_QToolBar( 3 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QMAINWINDOW_INSERTTOOLBAR FP=( p )->insertToolBar( hbqt_par_QToolBar( 2 ), hbqt_par_QToolBar( 3 ) ); p is NULL" ) );
   }
}

/*
 * void insertToolBarBreak ( QToolBar * before )
 */
HB_FUNC( QT_QMAINWINDOW_INSERTTOOLBARBREAK )
{
   QMainWindow * p = hbqt_par_QMainWindow( 1 );
   if( p )
      ( p )->insertToolBarBreak( hbqt_par_QToolBar( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QMAINWINDOW_INSERTTOOLBARBREAK FP=( p )->insertToolBarBreak( hbqt_par_QToolBar( 2 ) ); p is NULL" ) );
   }
}

/*
 * bool isAnimated () const
 */
HB_FUNC( QT_QMAINWINDOW_ISANIMATED )
{
   QMainWindow * p = hbqt_par_QMainWindow( 1 );
   if( p )
      hb_retl( ( p )->isAnimated() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QMAINWINDOW_ISANIMATED FP=hb_retl( ( p )->isAnimated() ); p is NULL" ) );
   }
}

/*
 * bool isDockNestingEnabled () const
 */
HB_FUNC( QT_QMAINWINDOW_ISDOCKNESTINGENABLED )
{
   QMainWindow * p = hbqt_par_QMainWindow( 1 );
   if( p )
      hb_retl( ( p )->isDockNestingEnabled() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QMAINWINDOW_ISDOCKNESTINGENABLED FP=hb_retl( ( p )->isDockNestingEnabled() ); p is NULL" ) );
   }
}

/*
 * QMenuBar * menuBar () const
 */
HB_FUNC( QT_QMAINWINDOW_MENUBAR )
{
   QMainWindow * p = hbqt_par_QMainWindow( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QMenuBar( ( p )->menuBar(), false ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QMAINWINDOW_MENUBAR FP=hb_retptrGC( hbqt_gcAllocate_QMenuBar( ( p )->menuBar(), false ) ); p is NULL" ) );
   }
}

/*
 * QWidget * menuWidget () const
 */
HB_FUNC( QT_QMAINWINDOW_MENUWIDGET )
{
   QMainWindow * p = hbqt_par_QMainWindow( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QWidget( ( p )->menuWidget(), false ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QMAINWINDOW_MENUWIDGET FP=hb_retptrGC( hbqt_gcAllocate_QWidget( ( p )->menuWidget(), false ) ); p is NULL" ) );
   }
}

/*
 * void removeDockWidget ( QDockWidget * dockwidget )
 */
HB_FUNC( QT_QMAINWINDOW_REMOVEDOCKWIDGET )
{
   QMainWindow * p = hbqt_par_QMainWindow( 1 );
   if( p )
      ( p )->removeDockWidget( hbqt_par_QDockWidget( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QMAINWINDOW_REMOVEDOCKWIDGET FP=( p )->removeDockWidget( hbqt_par_QDockWidget( 2 ) ); p is NULL" ) );
   }
}

/*
 * void removeToolBar ( QToolBar * toolbar )
 */
HB_FUNC( QT_QMAINWINDOW_REMOVETOOLBAR )
{
   QMainWindow * p = hbqt_par_QMainWindow( 1 );
   if( p )
      ( p )->removeToolBar( hbqt_par_QToolBar( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QMAINWINDOW_REMOVETOOLBAR FP=( p )->removeToolBar( hbqt_par_QToolBar( 2 ) ); p is NULL" ) );
   }
}

/*
 * void removeToolBarBreak ( QToolBar * before )
 */
HB_FUNC( QT_QMAINWINDOW_REMOVETOOLBARBREAK )
{
   QMainWindow * p = hbqt_par_QMainWindow( 1 );
   if( p )
      ( p )->removeToolBarBreak( hbqt_par_QToolBar( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QMAINWINDOW_REMOVETOOLBARBREAK FP=( p )->removeToolBarBreak( hbqt_par_QToolBar( 2 ) ); p is NULL" ) );
   }
}

/*
 * bool restoreDockWidget ( QDockWidget * dockwidget )
 */
HB_FUNC( QT_QMAINWINDOW_RESTOREDOCKWIDGET )
{
   QMainWindow * p = hbqt_par_QMainWindow( 1 );
   if( p )
      hb_retl( ( p )->restoreDockWidget( hbqt_par_QDockWidget( 2 ) ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QMAINWINDOW_RESTOREDOCKWIDGET FP=hb_retl( ( p )->restoreDockWidget( hbqt_par_QDockWidget( 2 ) ) ); p is NULL" ) );
   }
}

/*
 * bool restoreState ( const QByteArray & state, int version = 0 )
 */
HB_FUNC( QT_QMAINWINDOW_RESTORESTATE )
{
   QMainWindow * p = hbqt_par_QMainWindow( 1 );
   if( p )
      hb_retl( ( p )->restoreState( *hbqt_par_QByteArray( 2 ), hb_parni( 3 ) ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QMAINWINDOW_RESTORESTATE FP=hb_retl( ( p )->restoreState( *hbqt_par_QByteArray( 2 ), hb_parni( 3 ) ) ); p is NULL" ) );
   }
}

/*
 * QByteArray saveState ( int version = 0 ) const
 */
HB_FUNC( QT_QMAINWINDOW_SAVESTATE )
{
   QMainWindow * p = hbqt_par_QMainWindow( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QByteArray( new QByteArray( ( p )->saveState( hb_parni( 2 ) ) ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QMAINWINDOW_SAVESTATE FP=hb_retptrGC( hbqt_gcAllocate_QByteArray( new QByteArray( ( p )->saveState( hb_parni( 2 ) ) ), true ) ); p is NULL" ) );
   }
}

/*
 * void setCentralWidget ( QWidget * widget )
 */
HB_FUNC( QT_QMAINWINDOW_SETCENTRALWIDGET )
{
   QMainWindow * p = hbqt_par_QMainWindow( 1 );
   if( p )
      ( p )->setCentralWidget( hbqt_par_QWidget( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QMAINWINDOW_SETCENTRALWIDGET FP=( p )->setCentralWidget( hbqt_par_QWidget( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setCorner ( Qt::Corner corner, Qt::DockWidgetArea area )
 */
HB_FUNC( QT_QMAINWINDOW_SETCORNER )
{
   QMainWindow * p = hbqt_par_QMainWindow( 1 );
   if( p )
      ( p )->setCorner( ( Qt::Corner ) hb_parni( 2 ), ( Qt::DockWidgetArea ) hb_parni( 3 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QMAINWINDOW_SETCORNER FP=( p )->setCorner( ( Qt::Corner ) hb_parni( 2 ), ( Qt::DockWidgetArea ) hb_parni( 3 ) ); p is NULL" ) );
   }
}

/*
 * void setDockOptions ( DockOptions options )
 */
HB_FUNC( QT_QMAINWINDOW_SETDOCKOPTIONS )
{
   QMainWindow * p = hbqt_par_QMainWindow( 1 );
   if( p )
      ( p )->setDockOptions( ( QMainWindow::DockOptions ) hb_parni( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QMAINWINDOW_SETDOCKOPTIONS FP=( p )->setDockOptions( ( QMainWindow::DockOptions ) hb_parni( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setDocumentMode ( bool enabled )
 */
HB_FUNC( QT_QMAINWINDOW_SETDOCUMENTMODE )
{
   QMainWindow * p = hbqt_par_QMainWindow( 1 );
   if( p )
      ( p )->setDocumentMode( hb_parl( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QMAINWINDOW_SETDOCUMENTMODE FP=( p )->setDocumentMode( hb_parl( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setIconSize ( const QSize & iconSize )
 */
HB_FUNC( QT_QMAINWINDOW_SETICONSIZE )
{
   QMainWindow * p = hbqt_par_QMainWindow( 1 );
   if( p )
      ( p )->setIconSize( *hbqt_par_QSize( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QMAINWINDOW_SETICONSIZE FP=( p )->setIconSize( *hbqt_par_QSize( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setMenuBar ( QMenuBar * menuBar )
 */
HB_FUNC( QT_QMAINWINDOW_SETMENUBAR )
{
   QMainWindow * p = hbqt_par_QMainWindow( 1 );
   if( p )
      ( p )->setMenuBar( hbqt_par_QMenuBar( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QMAINWINDOW_SETMENUBAR FP=( p )->setMenuBar( hbqt_par_QMenuBar( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setMenuWidget ( QWidget * menuBar )
 */
HB_FUNC( QT_QMAINWINDOW_SETMENUWIDGET )
{
   QMainWindow * p = hbqt_par_QMainWindow( 1 );
   if( p )
      ( p )->setMenuWidget( hbqt_par_QWidget( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QMAINWINDOW_SETMENUWIDGET FP=( p )->setMenuWidget( hbqt_par_QWidget( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setStatusBar ( QStatusBar * statusbar )
 */
HB_FUNC( QT_QMAINWINDOW_SETSTATUSBAR )
{
   QMainWindow * p = hbqt_par_QMainWindow( 1 );
   if( p )
      ( p )->setStatusBar( hbqt_par_QStatusBar( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QMAINWINDOW_SETSTATUSBAR FP=( p )->setStatusBar( hbqt_par_QStatusBar( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setTabPosition ( Qt::DockWidgetAreas areas, QTabWidget::TabPosition tabPosition )
 */
HB_FUNC( QT_QMAINWINDOW_SETTABPOSITION )
{
   QMainWindow * p = hbqt_par_QMainWindow( 1 );
   if( p )
      ( p )->setTabPosition( ( Qt::DockWidgetAreas ) hb_parni( 2 ), ( QTabWidget::TabPosition ) hb_parni( 3 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QMAINWINDOW_SETTABPOSITION FP=( p )->setTabPosition( ( Qt::DockWidgetAreas ) hb_parni( 2 ), ( QTabWidget::TabPosition ) hb_parni( 3 ) ); p is NULL" ) );
   }
}

/*
 * void setTabShape ( QTabWidget::TabShape tabShape )
 */
HB_FUNC( QT_QMAINWINDOW_SETTABSHAPE )
{
   QMainWindow * p = hbqt_par_QMainWindow( 1 );
   if( p )
      ( p )->setTabShape( ( QTabWidget::TabShape ) hb_parni( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QMAINWINDOW_SETTABSHAPE FP=( p )->setTabShape( ( QTabWidget::TabShape ) hb_parni( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setToolButtonStyle ( Qt::ToolButtonStyle toolButtonStyle )
 */
HB_FUNC( QT_QMAINWINDOW_SETTOOLBUTTONSTYLE )
{
   QMainWindow * p = hbqt_par_QMainWindow( 1 );
   if( p )
      ( p )->setToolButtonStyle( ( Qt::ToolButtonStyle ) hb_parni( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QMAINWINDOW_SETTOOLBUTTONSTYLE FP=( p )->setToolButtonStyle( ( Qt::ToolButtonStyle ) hb_parni( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setUnifiedTitleAndToolBarOnMac ( bool set )
 */
HB_FUNC( QT_QMAINWINDOW_SETUNIFIEDTITLEANDTOOLBARONMAC )
{
   QMainWindow * p = hbqt_par_QMainWindow( 1 );
   if( p )
      ( p )->setUnifiedTitleAndToolBarOnMac( hb_parl( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QMAINWINDOW_SETUNIFIEDTITLEANDTOOLBARONMAC FP=( p )->setUnifiedTitleAndToolBarOnMac( hb_parl( 2 ) ); p is NULL" ) );
   }
}

/*
 * void splitDockWidget ( QDockWidget * first, QDockWidget * second, Qt::Orientation orientation )
 */
HB_FUNC( QT_QMAINWINDOW_SPLITDOCKWIDGET )
{
   QMainWindow * p = hbqt_par_QMainWindow( 1 );
   if( p )
      ( p )->splitDockWidget( hbqt_par_QDockWidget( 2 ), hbqt_par_QDockWidget( 3 ), ( Qt::Orientation ) hb_parni( 4 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QMAINWINDOW_SPLITDOCKWIDGET FP=( p )->splitDockWidget( hbqt_par_QDockWidget( 2 ), hbqt_par_QDockWidget( 3 ), ( Qt::Orientation ) hb_parni( 4 ) ); p is NULL" ) );
   }
}

/*
 * QStatusBar * statusBar () const
 */
HB_FUNC( QT_QMAINWINDOW_STATUSBAR )
{
   QMainWindow * p = hbqt_par_QMainWindow( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QStatusBar( ( p )->statusBar(), false ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QMAINWINDOW_STATUSBAR FP=hb_retptrGC( hbqt_gcAllocate_QStatusBar( ( p )->statusBar(), false ) ); p is NULL" ) );
   }
}

/*
 * QTabWidget::TabPosition tabPosition ( Qt::DockWidgetArea area ) const
 */
HB_FUNC( QT_QMAINWINDOW_TABPOSITION )
{
   QMainWindow * p = hbqt_par_QMainWindow( 1 );
   if( p )
      hb_retni( ( QTabWidget::TabPosition ) ( p )->tabPosition( ( Qt::DockWidgetArea ) hb_parni( 2 ) ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QMAINWINDOW_TABPOSITION FP=hb_retni( ( QTabWidget::TabPosition ) ( p )->tabPosition( ( Qt::DockWidgetArea ) hb_parni( 2 ) ) ); p is NULL" ) );
   }
}

/*
 * QTabWidget::TabShape tabShape () const
 */
HB_FUNC( QT_QMAINWINDOW_TABSHAPE )
{
   QMainWindow * p = hbqt_par_QMainWindow( 1 );
   if( p )
      hb_retni( ( QTabWidget::TabShape ) ( p )->tabShape() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QMAINWINDOW_TABSHAPE FP=hb_retni( ( QTabWidget::TabShape ) ( p )->tabShape() ); p is NULL" ) );
   }
}

/*
 * QList<QDockWidget *> tabifiedDockWidgets ( QDockWidget * dockwidget ) const
 */
HB_FUNC( QT_QMAINWINDOW_TABIFIEDDOCKWIDGETS )
{
   QMainWindow * p = hbqt_par_QMainWindow( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QList( new QList<QDockWidget *>( ( p )->tabifiedDockWidgets( hbqt_par_QDockWidget( 2 ) ) ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QMAINWINDOW_TABIFIEDDOCKWIDGETS FP=hb_retptrGC( hbqt_gcAllocate_QList( new QList<QDockWidget *>( ( p )->tabifiedDockWidgets( hbqt_par_QDockWidget( 2 ) ) ), true ) ); p is NULL" ) );
   }
}

/*
 * void tabifyDockWidget ( QDockWidget * first, QDockWidget * second )
 */
HB_FUNC( QT_QMAINWINDOW_TABIFYDOCKWIDGET )
{
   QMainWindow * p = hbqt_par_QMainWindow( 1 );
   if( p )
      ( p )->tabifyDockWidget( hbqt_par_QDockWidget( 2 ), hbqt_par_QDockWidget( 3 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QMAINWINDOW_TABIFYDOCKWIDGET FP=( p )->tabifyDockWidget( hbqt_par_QDockWidget( 2 ), hbqt_par_QDockWidget( 3 ) ); p is NULL" ) );
   }
}

/*
 * Qt::ToolBarArea toolBarArea ( QToolBar * toolbar ) const
 */
HB_FUNC( QT_QMAINWINDOW_TOOLBARAREA )
{
   QMainWindow * p = hbqt_par_QMainWindow( 1 );
   if( p )
      hb_retni( ( Qt::ToolBarArea ) ( p )->toolBarArea( hbqt_par_QToolBar( 2 ) ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QMAINWINDOW_TOOLBARAREA FP=hb_retni( ( Qt::ToolBarArea ) ( p )->toolBarArea( hbqt_par_QToolBar( 2 ) ) ); p is NULL" ) );
   }
}

/*
 * bool toolBarBreak ( QToolBar * toolbar ) const
 */
HB_FUNC( QT_QMAINWINDOW_TOOLBARBREAK )
{
   QMainWindow * p = hbqt_par_QMainWindow( 1 );
   if( p )
      hb_retl( ( p )->toolBarBreak( hbqt_par_QToolBar( 2 ) ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QMAINWINDOW_TOOLBARBREAK FP=hb_retl( ( p )->toolBarBreak( hbqt_par_QToolBar( 2 ) ) ); p is NULL" ) );
   }
}

/*
 * Qt::ToolButtonStyle toolButtonStyle () const
 */
HB_FUNC( QT_QMAINWINDOW_TOOLBUTTONSTYLE )
{
   QMainWindow * p = hbqt_par_QMainWindow( 1 );
   if( p )
      hb_retni( ( Qt::ToolButtonStyle ) ( p )->toolButtonStyle() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QMAINWINDOW_TOOLBUTTONSTYLE FP=hb_retni( ( Qt::ToolButtonStyle ) ( p )->toolButtonStyle() ); p is NULL" ) );
   }
}

/*
 * bool unifiedTitleAndToolBarOnMac () const
 */
HB_FUNC( QT_QMAINWINDOW_UNIFIEDTITLEANDTOOLBARONMAC )
{
   QMainWindow * p = hbqt_par_QMainWindow( 1 );
   if( p )
      hb_retl( ( p )->unifiedTitleAndToolBarOnMac() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QMAINWINDOW_UNIFIEDTITLEANDTOOLBARONMAC FP=hb_retl( ( p )->unifiedTitleAndToolBarOnMac() ); p is NULL" ) );
   }
}

/*
 * void setAnimated ( bool enabled )
 */
HB_FUNC( QT_QMAINWINDOW_SETANIMATED )
{
   QMainWindow * p = hbqt_par_QMainWindow( 1 );
   if( p )
      ( p )->setAnimated( hb_parl( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QMAINWINDOW_SETANIMATED FP=( p )->setAnimated( hb_parl( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setDockNestingEnabled ( bool enabled )
 */
HB_FUNC( QT_QMAINWINDOW_SETDOCKNESTINGENABLED )
{
   QMainWindow * p = hbqt_par_QMainWindow( 1 );
   if( p )
      ( p )->setDockNestingEnabled( hb_parl( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QMAINWINDOW_SETDOCKNESTINGENABLED FP=( p )->setDockNestingEnabled( hb_parl( 2 ) ); p is NULL" ) );
   }
}


/*----------------------------------------------------------------------*/
#endif             /* #if QT_VERSION >= 0x040500 */
/*----------------------------------------------------------------------*/
