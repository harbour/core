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

#include "../hbqt.h"

/*----------------------------------------------------------------------*/
#if QT_VERSION >= 0x040500
/*----------------------------------------------------------------------*/

/*
 *  enum AreaOption { DontMaximizeSubWindowOnActivation }
 *  flags AreaOptions
 *  enum ViewMode { SubWindowView, TabbedView }
 *  enum WindowOrder { CreationOrder, StackingOrder, ActivationHistoryOrder }
 */

#include <QtCore/QPointer>

#include <QtGui/QMdiArea>


/*
 * QMdiArea ( QWidget * parent = 0 )
 * ~QMdiArea ()
 *
 */

typedef struct
{
   QPointer< QMdiArea > ph;
   bool bNew;
   QT_G_FUNC_PTR func;
} QGC_POINTER_QMdiArea;

QT_G_FUNC( hbqt_gcRelease_QMdiArea )
{
   QMdiArea  * ph = NULL ;
   QGC_POINTER_QMdiArea * p = ( QGC_POINTER_QMdiArea * ) Cargo;

   if( p && p->bNew && p->ph )
   {
      ph = p->ph;
      if( ph )
      {
         const QMetaObject * m = ( ph )->metaObject();
         if( ( QString ) m->className() != ( QString ) "QObject" )
         {
            HB_TRACE( HB_TR_DEBUG, ( "ph=%p %p YES_rel_QMdiArea   /.\\   ", (void*) ph, (void*) p->ph ) );
            delete ( p->ph );
            HB_TRACE( HB_TR_DEBUG, ( "ph=%p %p YES_rel_QMdiArea   \\./   ", (void*) ph, (void*) p->ph ) );
            p->ph = NULL;
         }
         else
         {
            HB_TRACE( HB_TR_DEBUG, ( "ph=%p NO__rel_QMdiArea          ", ph ) );
            p->ph = NULL;
         }
      }
      else
      {
         HB_TRACE( HB_TR_DEBUG, ( "ph=%p DEL_rel_QMdiArea    :     Object already deleted!", ph ) );
         p->ph = NULL;
      }
   }
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p PTR_rel_QMdiArea    :    Object not created with new=true", ph ) );
      p->ph = NULL;
   }
}

void * hbqt_gcAllocate_QMdiArea( void * pObj, bool bNew )
{
   QGC_POINTER_QMdiArea * p = ( QGC_POINTER_QMdiArea * ) hb_gcAllocate( sizeof( QGC_POINTER_QMdiArea ), hbqt_gcFuncs() );

   new( & p->ph ) QPointer< QMdiArea >( ( QMdiArea * ) pObj );
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QMdiArea;

   if( bNew )
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p    _new_QMdiArea  under p->pq", pObj ) );
   }
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p NOT_new_QMdiArea", pObj ) );
   }
   return p;
}

HB_FUNC( QT_QMDIAREA )
{
   QMdiArea * pObj = NULL;

   if( hb_pcount() == 1 && HB_ISPOINTER( 1 ) )
   {
      pObj = new QMdiArea( hbqt_par_QWidget( 1 ) ) ;
   }
   else
   {
      pObj = new QMdiArea() ;
   }

   hb_retptrGC( hbqt_gcAllocate_QMdiArea( ( void * ) pObj, true ) );
}

/*
 * WindowOrder activationOrder () const
 */
HB_FUNC( QT_QMDIAREA_ACTIVATIONORDER )
{
   QMdiArea * p = hbqt_par_QMdiArea( 1 );
   if( p )
      hb_retni( ( QMdiArea::WindowOrder ) ( p )->activationOrder() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QMDIAREA_ACTIVATIONORDER FP=hb_retni( ( QMdiArea::WindowOrder ) ( p )->activationOrder() ); p is NULL" ) );
   }
}

/*
 * QMdiSubWindow * activeSubWindow () const
 */
HB_FUNC( QT_QMDIAREA_ACTIVESUBWINDOW )
{
   QMdiArea * p = hbqt_par_QMdiArea( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QMdiSubWindow( ( p )->activeSubWindow(), false ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QMDIAREA_ACTIVESUBWINDOW FP=hb_retptrGC( hbqt_gcAllocate_QMdiSubWindow( ( p )->activeSubWindow(), false ) ); p is NULL" ) );
   }
}

/*
 * QMdiSubWindow * addSubWindow ( QWidget * widget, Qt::WindowFlags windowFlags = 0 )
 */
HB_FUNC( QT_QMDIAREA_ADDSUBWINDOW )
{
   QMdiArea * p = hbqt_par_QMdiArea( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QMdiSubWindow( ( p )->addSubWindow( hbqt_par_QWidget( 2 ), ( Qt::WindowFlags ) hb_parni( 3 ) ), false ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QMDIAREA_ADDSUBWINDOW FP=hb_retptrGC( hbqt_gcAllocate_QMdiSubWindow( ( p )->addSubWindow( hbqt_par_QWidget( 2 ), ( Qt::WindowFlags ) hb_parni( 3 ) ), false ) ); p is NULL" ) );
   }
}

/*
 * QBrush background () const
 */
HB_FUNC( QT_QMDIAREA_BACKGROUND )
{
   QMdiArea * p = hbqt_par_QMdiArea( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QBrush( new QBrush( ( p )->background() ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QMDIAREA_BACKGROUND FP=hb_retptrGC( hbqt_gcAllocate_QBrush( new QBrush( ( p )->background() ), true ) ); p is NULL" ) );
   }
}

/*
 * QMdiSubWindow * currentSubWindow () const
 */
HB_FUNC( QT_QMDIAREA_CURRENTSUBWINDOW )
{
   QMdiArea * p = hbqt_par_QMdiArea( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QMdiSubWindow( ( p )->currentSubWindow(), false ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QMDIAREA_CURRENTSUBWINDOW FP=hb_retptrGC( hbqt_gcAllocate_QMdiSubWindow( ( p )->currentSubWindow(), false ) ); p is NULL" ) );
   }
}

/*
 * bool documentMode () const
 */
HB_FUNC( QT_QMDIAREA_DOCUMENTMODE )
{
   QMdiArea * p = hbqt_par_QMdiArea( 1 );
   if( p )
      hb_retl( ( p )->documentMode() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QMDIAREA_DOCUMENTMODE FP=hb_retl( ( p )->documentMode() ); p is NULL" ) );
   }
}

/*
 * void removeSubWindow ( QWidget * widget )
 */
HB_FUNC( QT_QMDIAREA_REMOVESUBWINDOW )
{
   QMdiArea * p = hbqt_par_QMdiArea( 1 );
   if( p )
      ( p )->removeSubWindow( hbqt_par_QWidget( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QMDIAREA_REMOVESUBWINDOW FP=( p )->removeSubWindow( hbqt_par_QWidget( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setActivationOrder ( WindowOrder order )
 */
HB_FUNC( QT_QMDIAREA_SETACTIVATIONORDER )
{
   QMdiArea * p = hbqt_par_QMdiArea( 1 );
   if( p )
      ( p )->setActivationOrder( ( QMdiArea::WindowOrder ) hb_parni( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QMDIAREA_SETACTIVATIONORDER FP=( p )->setActivationOrder( ( QMdiArea::WindowOrder ) hb_parni( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setBackground ( const QBrush & background )
 */
HB_FUNC( QT_QMDIAREA_SETBACKGROUND )
{
   QMdiArea * p = hbqt_par_QMdiArea( 1 );
   if( p )
      ( p )->setBackground( *hbqt_par_QBrush( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QMDIAREA_SETBACKGROUND FP=( p )->setBackground( *hbqt_par_QBrush( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setDocumentMode ( bool enabled )
 */
HB_FUNC( QT_QMDIAREA_SETDOCUMENTMODE )
{
   QMdiArea * p = hbqt_par_QMdiArea( 1 );
   if( p )
      ( p )->setDocumentMode( hb_parl( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QMDIAREA_SETDOCUMENTMODE FP=( p )->setDocumentMode( hb_parl( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setOption ( AreaOption option, bool on = true )
 */
HB_FUNC( QT_QMDIAREA_SETOPTION )
{
   QMdiArea * p = hbqt_par_QMdiArea( 1 );
   if( p )
      ( p )->setOption( ( QMdiArea::AreaOption ) hb_parni( 2 ), hb_parl( 3 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QMDIAREA_SETOPTION FP=( p )->setOption( ( QMdiArea::AreaOption ) hb_parni( 2 ), hb_parl( 3 ) ); p is NULL" ) );
   }
}

/*
 * void setTabPosition ( QTabWidget::TabPosition position )
 */
HB_FUNC( QT_QMDIAREA_SETTABPOSITION )
{
   QMdiArea * p = hbqt_par_QMdiArea( 1 );
   if( p )
      ( p )->setTabPosition( ( QTabWidget::TabPosition ) hb_parni( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QMDIAREA_SETTABPOSITION FP=( p )->setTabPosition( ( QTabWidget::TabPosition ) hb_parni( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setTabShape ( QTabWidget::TabShape shape )
 */
HB_FUNC( QT_QMDIAREA_SETTABSHAPE )
{
   QMdiArea * p = hbqt_par_QMdiArea( 1 );
   if( p )
      ( p )->setTabShape( ( QTabWidget::TabShape ) hb_parni( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QMDIAREA_SETTABSHAPE FP=( p )->setTabShape( ( QTabWidget::TabShape ) hb_parni( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setViewMode ( ViewMode mode )
 */
HB_FUNC( QT_QMDIAREA_SETVIEWMODE )
{
   QMdiArea * p = hbqt_par_QMdiArea( 1 );
   if( p )
      ( p )->setViewMode( ( QMdiArea::ViewMode ) hb_parni( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QMDIAREA_SETVIEWMODE FP=( p )->setViewMode( ( QMdiArea::ViewMode ) hb_parni( 2 ) ); p is NULL" ) );
   }
}

/*
 * QList<QMdiSubWindow *> subWindowList ( WindowOrder order = CreationOrder ) const
 */
HB_FUNC( QT_QMDIAREA_SUBWINDOWLIST )
{
   QMdiArea * p = hbqt_par_QMdiArea( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QList( new QList<QMdiSubWindow *>( ( p )->subWindowList( ( HB_ISNUM( 2 ) ? ( QMdiArea::WindowOrder ) hb_parni( 2 ) : ( QMdiArea::WindowOrder ) QMdiArea::CreationOrder ) ) ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QMDIAREA_SUBWINDOWLIST FP=hb_retptrGC( hbqt_gcAllocate_QList( new QList<QMdiSubWindow *>( ( p )->subWindowList( ( HB_ISNUM( 2 ) ? ( QMdiArea::WindowOrder ) hb_parni( 2 ) : ( QMdiArea::WindowOrder ) QMdiArea::CreationOrder ) ) ), true ) ); p is NULL" ) );
   }
}

/*
 * QTabWidget::TabPosition tabPosition () const
 */
HB_FUNC( QT_QMDIAREA_TABPOSITION )
{
   QMdiArea * p = hbqt_par_QMdiArea( 1 );
   if( p )
      hb_retni( ( QTabWidget::TabPosition ) ( p )->tabPosition() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QMDIAREA_TABPOSITION FP=hb_retni( ( QTabWidget::TabPosition ) ( p )->tabPosition() ); p is NULL" ) );
   }
}

/*
 * QTabWidget::TabShape tabShape () const
 */
HB_FUNC( QT_QMDIAREA_TABSHAPE )
{
   QMdiArea * p = hbqt_par_QMdiArea( 1 );
   if( p )
      hb_retni( ( QTabWidget::TabShape ) ( p )->tabShape() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QMDIAREA_TABSHAPE FP=hb_retni( ( QTabWidget::TabShape ) ( p )->tabShape() ); p is NULL" ) );
   }
}

/*
 * bool testOption ( AreaOption option ) const
 */
HB_FUNC( QT_QMDIAREA_TESTOPTION )
{
   QMdiArea * p = hbqt_par_QMdiArea( 1 );
   if( p )
      hb_retl( ( p )->testOption( ( QMdiArea::AreaOption ) hb_parni( 2 ) ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QMDIAREA_TESTOPTION FP=hb_retl( ( p )->testOption( ( QMdiArea::AreaOption ) hb_parni( 2 ) ) ); p is NULL" ) );
   }
}

/*
 * ViewMode viewMode () const
 */
HB_FUNC( QT_QMDIAREA_VIEWMODE )
{
   QMdiArea * p = hbqt_par_QMdiArea( 1 );
   if( p )
      hb_retni( ( QMdiArea::ViewMode ) ( p )->viewMode() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QMDIAREA_VIEWMODE FP=hb_retni( ( QMdiArea::ViewMode ) ( p )->viewMode() ); p is NULL" ) );
   }
}

/*
 * void activateNextSubWindow ()
 */
HB_FUNC( QT_QMDIAREA_ACTIVATENEXTSUBWINDOW )
{
   QMdiArea * p = hbqt_par_QMdiArea( 1 );
   if( p )
      ( p )->activateNextSubWindow();
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QMDIAREA_ACTIVATENEXTSUBWINDOW FP=( p )->activateNextSubWindow(); p is NULL" ) );
   }
}

/*
 * void activatePreviousSubWindow ()
 */
HB_FUNC( QT_QMDIAREA_ACTIVATEPREVIOUSSUBWINDOW )
{
   QMdiArea * p = hbqt_par_QMdiArea( 1 );
   if( p )
      ( p )->activatePreviousSubWindow();
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QMDIAREA_ACTIVATEPREVIOUSSUBWINDOW FP=( p )->activatePreviousSubWindow(); p is NULL" ) );
   }
}

/*
 * void cascadeSubWindows ()
 */
HB_FUNC( QT_QMDIAREA_CASCADESUBWINDOWS )
{
   QMdiArea * p = hbqt_par_QMdiArea( 1 );
   if( p )
      ( p )->cascadeSubWindows();
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QMDIAREA_CASCADESUBWINDOWS FP=( p )->cascadeSubWindows(); p is NULL" ) );
   }
}

/*
 * void closeActiveSubWindow ()
 */
HB_FUNC( QT_QMDIAREA_CLOSEACTIVESUBWINDOW )
{
   QMdiArea * p = hbqt_par_QMdiArea( 1 );
   if( p )
      ( p )->closeActiveSubWindow();
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QMDIAREA_CLOSEACTIVESUBWINDOW FP=( p )->closeActiveSubWindow(); p is NULL" ) );
   }
}

/*
 * void closeAllSubWindows ()
 */
HB_FUNC( QT_QMDIAREA_CLOSEALLSUBWINDOWS )
{
   QMdiArea * p = hbqt_par_QMdiArea( 1 );
   if( p )
      ( p )->closeAllSubWindows();
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QMDIAREA_CLOSEALLSUBWINDOWS FP=( p )->closeAllSubWindows(); p is NULL" ) );
   }
}

/*
 * void setActiveSubWindow ( QMdiSubWindow * window )
 */
HB_FUNC( QT_QMDIAREA_SETACTIVESUBWINDOW )
{
   QMdiArea * p = hbqt_par_QMdiArea( 1 );
   if( p )
      ( p )->setActiveSubWindow( hbqt_par_QMdiSubWindow( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QMDIAREA_SETACTIVESUBWINDOW FP=( p )->setActiveSubWindow( hbqt_par_QMdiSubWindow( 2 ) ); p is NULL" ) );
   }
}

/*
 * void tileSubWindows ()
 */
HB_FUNC( QT_QMDIAREA_TILESUBWINDOWS )
{
   QMdiArea * p = hbqt_par_QMdiArea( 1 );
   if( p )
      ( p )->tileSubWindows();
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QMDIAREA_TILESUBWINDOWS FP=( p )->tileSubWindows(); p is NULL" ) );
   }
}


/*----------------------------------------------------------------------*/
#endif             /* #if QT_VERSION >= 0x040500 */
/*----------------------------------------------------------------------*/
