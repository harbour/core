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

#include "hbqtcore.h"
#include "hbqtgui.h"

/*----------------------------------------------------------------------*/
#if QT_VERSION >= 0x040500
/*----------------------------------------------------------------------*/

/*
 *  enum AreaOption { DontMaximizeSubWindowOnActivation }
 *  flags AreaOptions
 *  enum ViewMode { SubWindowView, TabbedView }
 *  enum WindowOrder { CreationOrder, StackingOrder, ActivationHistoryOrder }
 */

/*
 *  Constructed[ 26/26 [ 100.00% ] ]
 *
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
   PHBQT_GC_FUNC func;
   int type;
} HBQT_GC_T_QMdiArea;

HBQT_GC_FUNC( hbqt_gcRelease_QMdiArea )
{
   QMdiArea  * ph = NULL ;
   HBQT_GC_T_QMdiArea * p = ( HBQT_GC_T_QMdiArea * ) Cargo;

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
   HBQT_GC_T_QMdiArea * p = ( HBQT_GC_T_QMdiArea * ) hb_gcAllocate( sizeof( HBQT_GC_T_QMdiArea ), hbqt_gcFuncs() );

   new( & p->ph ) QPointer< QMdiArea >( ( QMdiArea * ) pObj );
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QMdiArea;
   p->type = HBQT_TYPE_QMdiArea;

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
   {
      hb_retni( ( QMdiArea::WindowOrder ) ( p )->activationOrder() );
   }
}

/*
 * QMdiSubWindow * activeSubWindow () const
 */
HB_FUNC( QT_QMDIAREA_ACTIVESUBWINDOW )
{
   QMdiArea * p = hbqt_par_QMdiArea( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QMdiSubWindow( ( p )->activeSubWindow(), false ) );
   }
}

/*
 * QMdiSubWindow * addSubWindow ( QWidget * widget, Qt::WindowFlags windowFlags = 0 )
 */
HB_FUNC( QT_QMDIAREA_ADDSUBWINDOW )
{
   QMdiArea * p = hbqt_par_QMdiArea( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QMdiSubWindow( ( p )->addSubWindow( hbqt_par_QWidget( 2 ), ( Qt::WindowFlags ) hb_parni( 3 ) ), false ) );
   }
}

/*
 * QBrush background () const
 */
HB_FUNC( QT_QMDIAREA_BACKGROUND )
{
   QMdiArea * p = hbqt_par_QMdiArea( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QBrush( new QBrush( ( p )->background() ), true ) );
   }
}

/*
 * QMdiSubWindow * currentSubWindow () const
 */
HB_FUNC( QT_QMDIAREA_CURRENTSUBWINDOW )
{
   QMdiArea * p = hbqt_par_QMdiArea( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QMdiSubWindow( ( p )->currentSubWindow(), false ) );
   }
}

/*
 * bool documentMode () const
 */
HB_FUNC( QT_QMDIAREA_DOCUMENTMODE )
{
   QMdiArea * p = hbqt_par_QMdiArea( 1 );
   if( p )
   {
      hb_retl( ( p )->documentMode() );
   }
}

/*
 * void removeSubWindow ( QWidget * widget )
 */
HB_FUNC( QT_QMDIAREA_REMOVESUBWINDOW )
{
   QMdiArea * p = hbqt_par_QMdiArea( 1 );
   if( p )
   {
      ( p )->removeSubWindow( hbqt_par_QWidget( 2 ) );
   }
}

/*
 * void setActivationOrder ( WindowOrder order )
 */
HB_FUNC( QT_QMDIAREA_SETACTIVATIONORDER )
{
   QMdiArea * p = hbqt_par_QMdiArea( 1 );
   if( p )
   {
      ( p )->setActivationOrder( ( QMdiArea::WindowOrder ) hb_parni( 2 ) );
   }
}

/*
 * void setBackground ( const QBrush & background )
 */
HB_FUNC( QT_QMDIAREA_SETBACKGROUND )
{
   QMdiArea * p = hbqt_par_QMdiArea( 1 );
   if( p )
   {
      ( p )->setBackground( *hbqt_par_QBrush( 2 ) );
   }
}

/*
 * void setDocumentMode ( bool enabled )
 */
HB_FUNC( QT_QMDIAREA_SETDOCUMENTMODE )
{
   QMdiArea * p = hbqt_par_QMdiArea( 1 );
   if( p )
   {
      ( p )->setDocumentMode( hb_parl( 2 ) );
   }
}

/*
 * void setOption ( AreaOption option, bool on = true )
 */
HB_FUNC( QT_QMDIAREA_SETOPTION )
{
   QMdiArea * p = hbqt_par_QMdiArea( 1 );
   if( p )
   {
      ( p )->setOption( ( QMdiArea::AreaOption ) hb_parni( 2 ), hb_parl( 3 ) );
   }
}

/*
 * void setTabPosition ( QTabWidget::TabPosition position )
 */
HB_FUNC( QT_QMDIAREA_SETTABPOSITION )
{
   QMdiArea * p = hbqt_par_QMdiArea( 1 );
   if( p )
   {
      ( p )->setTabPosition( ( QTabWidget::TabPosition ) hb_parni( 2 ) );
   }
}

/*
 * void setTabShape ( QTabWidget::TabShape shape )
 */
HB_FUNC( QT_QMDIAREA_SETTABSHAPE )
{
   QMdiArea * p = hbqt_par_QMdiArea( 1 );
   if( p )
   {
      ( p )->setTabShape( ( QTabWidget::TabShape ) hb_parni( 2 ) );
   }
}

/*
 * void setViewMode ( ViewMode mode )
 */
HB_FUNC( QT_QMDIAREA_SETVIEWMODE )
{
   QMdiArea * p = hbqt_par_QMdiArea( 1 );
   if( p )
   {
      ( p )->setViewMode( ( QMdiArea::ViewMode ) hb_parni( 2 ) );
   }
}

/*
 * QList<QMdiSubWindow *> subWindowList ( WindowOrder order = CreationOrder ) const
 */
HB_FUNC( QT_QMDIAREA_SUBWINDOWLIST )
{
   QMdiArea * p = hbqt_par_QMdiArea( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QList( new QList<QMdiSubWindow *>( ( p )->subWindowList( ( HB_ISNUM( 2 ) ? ( QMdiArea::WindowOrder ) hb_parni( 2 ) : ( QMdiArea::WindowOrder ) QMdiArea::CreationOrder ) ) ), true ) );
   }
}

/*
 * QTabWidget::TabPosition tabPosition () const
 */
HB_FUNC( QT_QMDIAREA_TABPOSITION )
{
   QMdiArea * p = hbqt_par_QMdiArea( 1 );
   if( p )
   {
      hb_retni( ( QTabWidget::TabPosition ) ( p )->tabPosition() );
   }
}

/*
 * QTabWidget::TabShape tabShape () const
 */
HB_FUNC( QT_QMDIAREA_TABSHAPE )
{
   QMdiArea * p = hbqt_par_QMdiArea( 1 );
   if( p )
   {
      hb_retni( ( QTabWidget::TabShape ) ( p )->tabShape() );
   }
}

/*
 * bool testOption ( AreaOption option ) const
 */
HB_FUNC( QT_QMDIAREA_TESTOPTION )
{
   QMdiArea * p = hbqt_par_QMdiArea( 1 );
   if( p )
   {
      hb_retl( ( p )->testOption( ( QMdiArea::AreaOption ) hb_parni( 2 ) ) );
   }
}

/*
 * ViewMode viewMode () const
 */
HB_FUNC( QT_QMDIAREA_VIEWMODE )
{
   QMdiArea * p = hbqt_par_QMdiArea( 1 );
   if( p )
   {
      hb_retni( ( QMdiArea::ViewMode ) ( p )->viewMode() );
   }
}

/*
 * void activateNextSubWindow ()
 */
HB_FUNC( QT_QMDIAREA_ACTIVATENEXTSUBWINDOW )
{
   QMdiArea * p = hbqt_par_QMdiArea( 1 );
   if( p )
   {
      ( p )->activateNextSubWindow();
   }
}

/*
 * void activatePreviousSubWindow ()
 */
HB_FUNC( QT_QMDIAREA_ACTIVATEPREVIOUSSUBWINDOW )
{
   QMdiArea * p = hbqt_par_QMdiArea( 1 );
   if( p )
   {
      ( p )->activatePreviousSubWindow();
   }
}

/*
 * void cascadeSubWindows ()
 */
HB_FUNC( QT_QMDIAREA_CASCADESUBWINDOWS )
{
   QMdiArea * p = hbqt_par_QMdiArea( 1 );
   if( p )
   {
      ( p )->cascadeSubWindows();
   }
}

/*
 * void closeActiveSubWindow ()
 */
HB_FUNC( QT_QMDIAREA_CLOSEACTIVESUBWINDOW )
{
   QMdiArea * p = hbqt_par_QMdiArea( 1 );
   if( p )
   {
      ( p )->closeActiveSubWindow();
   }
}

/*
 * void closeAllSubWindows ()
 */
HB_FUNC( QT_QMDIAREA_CLOSEALLSUBWINDOWS )
{
   QMdiArea * p = hbqt_par_QMdiArea( 1 );
   if( p )
   {
      ( p )->closeAllSubWindows();
   }
}

/*
 * void setActiveSubWindow ( QMdiSubWindow * window )
 */
HB_FUNC( QT_QMDIAREA_SETACTIVESUBWINDOW )
{
   QMdiArea * p = hbqt_par_QMdiArea( 1 );
   if( p )
   {
      ( p )->setActiveSubWindow( hbqt_par_QMdiSubWindow( 2 ) );
   }
}

/*
 * void tileSubWindows ()
 */
HB_FUNC( QT_QMDIAREA_TILESUBWINDOWS )
{
   QMdiArea * p = hbqt_par_QMdiArea( 1 );
   if( p )
   {
      ( p )->tileSubWindows();
   }
}


/*----------------------------------------------------------------------*/
#endif             /* #if QT_VERSION >= 0x040500 */
/*----------------------------------------------------------------------*/
