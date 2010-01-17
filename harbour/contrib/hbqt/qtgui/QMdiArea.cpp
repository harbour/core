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

/*
 *  Constructed[ 25/26 [ 96.15% ] ]
 *
 *  *** Unconvered Prototypes ***
 *  -----------------------------
 *
 *  QList<QMdiSubWindow *> subWindowList ( WindowOrder order = CreationOrder ) const
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
  void * ph;
  bool bNew;
  QT_G_FUNC_PTR func;
  QPointer< QMdiArea > pq;
} QGC_POINTER_QMdiArea;

QT_G_FUNC( hbqt_gcRelease_QMdiArea )
{
   QGC_POINTER_QMdiArea * p = ( QGC_POINTER_QMdiArea * ) Cargo;

   if( p && p->bNew )
   {
      if( p->ph && p->pq )
      {
         const QMetaObject * m = ( ( QObject * ) p->ph )->metaObject();
         if( ( QString ) m->className() != ( QString ) "QObject" )
         {
            delete ( ( QMdiArea * ) p->ph );
            HB_TRACE( HB_TR_DEBUG, ( "YES_rel_QMdiArea                   ph=%p pq=%p %i B %i KB", p->ph, (void *)(p->pq), ( int ) hb_xquery( 1001 ), hbqt_getmemused() ) );
            p->ph = NULL;
         }
         else
         {
            HB_TRACE( HB_TR_DEBUG, ( "NO__rel_QMdiArea                   ph=%p pq=%p %i B %i KB", p->ph, (void *)(p->pq), ( int ) hb_xquery( 1001 ), hbqt_getmemused() ) );
         }
      }
      else
      {
         HB_TRACE( HB_TR_DEBUG, ( "DEL_rel_QMdiArea                    Object already deleted!" ) );
      }
   }
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "PTR_rel_QMdiArea                    Object not created with - new" ) );
      p->ph = NULL;
   }
}

void * hbqt_gcAllocate_QMdiArea( void * pObj, bool bNew )
{
   QGC_POINTER_QMdiArea * p = ( QGC_POINTER_QMdiArea * ) hb_gcAllocate( sizeof( QGC_POINTER_QMdiArea ), hbqt_gcFuncs() );

   p->ph = pObj;
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QMdiArea;

   if( bNew )
   {
      new( & p->pq ) QPointer< QMdiArea >( ( QMdiArea * ) pObj );
      HB_TRACE( HB_TR_DEBUG, ( "   _new_QMdiArea                   ph=%p %i B %i KB", pObj, ( int ) hb_xquery( 1001 ), hbqt_getmemused() ) );
   }
   return p;
}

HB_FUNC( QT_QMDIAREA )
{
   void * pObj = NULL;

   if( hb_pcount() == 1 && HB_ISPOINTER( 1 ) )
   {
      pObj = new QMdiArea( hbqt_par_QWidget( 1 ) ) ;
   }
   else
   {
      pObj = new QMdiArea() ;
   }

   hb_retptrGC( hbqt_gcAllocate_QMdiArea( pObj, true ) );
}
/*
 * WindowOrder activationOrder () const
 */
HB_FUNC( QT_QMDIAREA_ACTIVATIONORDER )
{
   hb_retni( ( QMdiArea::WindowOrder ) hbqt_par_QMdiArea( 1 )->activationOrder() );
}

/*
 * QMdiSubWindow * activeSubWindow () const
 */
HB_FUNC( QT_QMDIAREA_ACTIVESUBWINDOW )
{
   hb_retptrGC( hbqt_gcAllocate_QMdiSubWindow( hbqt_par_QMdiArea( 1 )->activeSubWindow(), false ) );
}

/*
 * QMdiSubWindow * addSubWindow ( QWidget * widget, Qt::WindowFlags windowFlags = 0 )
 */
HB_FUNC( QT_QMDIAREA_ADDSUBWINDOW )
{
   hb_retptrGC( hbqt_gcAllocate_QMdiSubWindow( hbqt_par_QMdiArea( 1 )->addSubWindow( hbqt_par_QWidget( 2 ), ( Qt::WindowFlags ) hb_parni( 3 ) ), false ) );
}

/*
 * QBrush background () const
 */
HB_FUNC( QT_QMDIAREA_BACKGROUND )
{
   hb_retptrGC( hbqt_gcAllocate_QBrush( new QBrush( hbqt_par_QMdiArea( 1 )->background() ), true ) );
}

/*
 * QMdiSubWindow * currentSubWindow () const
 */
HB_FUNC( QT_QMDIAREA_CURRENTSUBWINDOW )
{
   hb_retptrGC( hbqt_gcAllocate_QMdiSubWindow( hbqt_par_QMdiArea( 1 )->currentSubWindow(), false ) );
}

/*
 * bool documentMode () const
 */
HB_FUNC( QT_QMDIAREA_DOCUMENTMODE )
{
   hb_retl( hbqt_par_QMdiArea( 1 )->documentMode() );
}

/*
 * void removeSubWindow ( QWidget * widget )
 */
HB_FUNC( QT_QMDIAREA_REMOVESUBWINDOW )
{
   hbqt_par_QMdiArea( 1 )->removeSubWindow( hbqt_par_QWidget( 2 ) );
}

/*
 * void setActivationOrder ( WindowOrder order )
 */
HB_FUNC( QT_QMDIAREA_SETACTIVATIONORDER )
{
   hbqt_par_QMdiArea( 1 )->setActivationOrder( ( QMdiArea::WindowOrder ) hb_parni( 2 ) );
}

/*
 * void setBackground ( const QBrush & background )
 */
HB_FUNC( QT_QMDIAREA_SETBACKGROUND )
{
   hbqt_par_QMdiArea( 1 )->setBackground( *hbqt_par_QBrush( 2 ) );
}

/*
 * void setDocumentMode ( bool enabled )
 */
HB_FUNC( QT_QMDIAREA_SETDOCUMENTMODE )
{
   hbqt_par_QMdiArea( 1 )->setDocumentMode( hb_parl( 2 ) );
}

/*
 * void setOption ( AreaOption option, bool on = true )
 */
HB_FUNC( QT_QMDIAREA_SETOPTION )
{
   hbqt_par_QMdiArea( 1 )->setOption( ( QMdiArea::AreaOption ) hb_parni( 2 ), hb_parl( 3 ) );
}

/*
 * void setTabPosition ( QTabWidget::TabPosition position )
 */
HB_FUNC( QT_QMDIAREA_SETTABPOSITION )
{
   hbqt_par_QMdiArea( 1 )->setTabPosition( ( QTabWidget::TabPosition ) hb_parni( 2 ) );
}

/*
 * void setTabShape ( QTabWidget::TabShape shape )
 */
HB_FUNC( QT_QMDIAREA_SETTABSHAPE )
{
   hbqt_par_QMdiArea( 1 )->setTabShape( ( QTabWidget::TabShape ) hb_parni( 2 ) );
}

/*
 * void setViewMode ( ViewMode mode )
 */
HB_FUNC( QT_QMDIAREA_SETVIEWMODE )
{
   hbqt_par_QMdiArea( 1 )->setViewMode( ( QMdiArea::ViewMode ) hb_parni( 2 ) );
}

/*
 * QTabWidget::TabPosition tabPosition () const
 */
HB_FUNC( QT_QMDIAREA_TABPOSITION )
{
   hb_retni( ( QTabWidget::TabPosition ) hbqt_par_QMdiArea( 1 )->tabPosition() );
}

/*
 * QTabWidget::TabShape tabShape () const
 */
HB_FUNC( QT_QMDIAREA_TABSHAPE )
{
   hb_retni( ( QTabWidget::TabShape ) hbqt_par_QMdiArea( 1 )->tabShape() );
}

/*
 * bool testOption ( AreaOption option ) const
 */
HB_FUNC( QT_QMDIAREA_TESTOPTION )
{
   hb_retl( hbqt_par_QMdiArea( 1 )->testOption( ( QMdiArea::AreaOption ) hb_parni( 2 ) ) );
}

/*
 * ViewMode viewMode () const
 */
HB_FUNC( QT_QMDIAREA_VIEWMODE )
{
   hb_retni( ( QMdiArea::ViewMode ) hbqt_par_QMdiArea( 1 )->viewMode() );
}

/*
 * void activateNextSubWindow ()
 */
HB_FUNC( QT_QMDIAREA_ACTIVATENEXTSUBWINDOW )
{
   hbqt_par_QMdiArea( 1 )->activateNextSubWindow();
}

/*
 * void activatePreviousSubWindow ()
 */
HB_FUNC( QT_QMDIAREA_ACTIVATEPREVIOUSSUBWINDOW )
{
   hbqt_par_QMdiArea( 1 )->activatePreviousSubWindow();
}

/*
 * void cascadeSubWindows ()
 */
HB_FUNC( QT_QMDIAREA_CASCADESUBWINDOWS )
{
   hbqt_par_QMdiArea( 1 )->cascadeSubWindows();
}

/*
 * void closeActiveSubWindow ()
 */
HB_FUNC( QT_QMDIAREA_CLOSEACTIVESUBWINDOW )
{
   hbqt_par_QMdiArea( 1 )->closeActiveSubWindow();
}

/*
 * void closeAllSubWindows ()
 */
HB_FUNC( QT_QMDIAREA_CLOSEALLSUBWINDOWS )
{
   hbqt_par_QMdiArea( 1 )->closeAllSubWindows();
}

/*
 * void setActiveSubWindow ( QMdiSubWindow * window )
 */
HB_FUNC( QT_QMDIAREA_SETACTIVESUBWINDOW )
{
   hbqt_par_QMdiArea( 1 )->setActiveSubWindow( hbqt_par_QMdiSubWindow( 2 ) );
}

/*
 * void tileSubWindows ()
 */
HB_FUNC( QT_QMDIAREA_TILESUBWINDOWS )
{
   hbqt_par_QMdiArea( 1 )->tileSubWindows();
}


/*----------------------------------------------------------------------*/
#endif             /* #if QT_VERSION >= 0x040500 */
/*----------------------------------------------------------------------*/
