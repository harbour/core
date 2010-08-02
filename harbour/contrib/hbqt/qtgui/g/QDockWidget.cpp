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
 *  enum DockWidgetFeature { DockWidgetClosable, DockWidgetMovable, DockWidgetFloatable, DockWidgetVerticalTitleBar, AllDockWidgetFeatures, NoDockWidgetFeatures }
 *  flags DockWidgetFeatures
 */

#include <QtCore/QPointer>

#include <QtGui/QDockWidget>


/*
 * QDockWidget ( const QString & title, QWidget * parent = 0, Qt::WindowFlags flags = 0 )
 * QDockWidget ( QWidget * parent = 0, Qt::WindowFlags flags = 0 )
 * ~QDockWidget ()
 */

typedef struct
{
   QPointer< QDockWidget > ph;
   bool bNew;
   QT_G_FUNC_PTR func;
   int type;
} QGC_POINTER_QDockWidget;

QT_G_FUNC( hbqt_gcRelease_QDockWidget )
{
   QDockWidget  * ph = NULL ;
   QGC_POINTER_QDockWidget * p = ( QGC_POINTER_QDockWidget * ) Cargo;

   if( p && p->bNew && p->ph )
   {
      ph = p->ph;
      if( ph )
      {
         const QMetaObject * m = ( ph )->metaObject();
         if( ( QString ) m->className() != ( QString ) "QObject" )
         {
            HB_TRACE( HB_TR_DEBUG, ( "ph=%p %p YES_rel_QDockWidget   /.\\   ", (void*) ph, (void*) p->ph ) );
            delete ( p->ph );
            HB_TRACE( HB_TR_DEBUG, ( "ph=%p %p YES_rel_QDockWidget   \\./   ", (void*) ph, (void*) p->ph ) );
            p->ph = NULL;
         }
         else
         {
            HB_TRACE( HB_TR_DEBUG, ( "ph=%p NO__rel_QDockWidget          ", ph ) );
            p->ph = NULL;
         }
      }
      else
      {
         HB_TRACE( HB_TR_DEBUG, ( "ph=%p DEL_rel_QDockWidget    :     Object already deleted!", ph ) );
         p->ph = NULL;
      }
   }
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p PTR_rel_QDockWidget    :    Object not created with new=true", ph ) );
      p->ph = NULL;
   }
}

void * hbqt_gcAllocate_QDockWidget( void * pObj, bool bNew )
{
   QGC_POINTER_QDockWidget * p = ( QGC_POINTER_QDockWidget * ) hb_gcAllocate( sizeof( QGC_POINTER_QDockWidget ), hbqt_gcFuncs() );

   new( & p->ph ) QPointer< QDockWidget >( ( QDockWidget * ) pObj );
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QDockWidget;
   p->type = HBQT_TYPE_QDockWidget;

   if( bNew )
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p    _new_QDockWidget  under p->pq", pObj ) );
   }
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p NOT_new_QDockWidget", pObj ) );
   }
   return p;
}

HB_FUNC( QT_QDOCKWIDGET )
{
   QDockWidget * pObj = NULL;

   pObj = new QDockWidget( hbqt_par_QWidget( 1 ), ( Qt::WindowFlags ) hb_parni( 2 ) ) ;

   hb_retptrGC( hbqt_gcAllocate_QDockWidget( ( void * ) pObj, true ) );
}

/*
 * Qt::DockWidgetAreas allowedAreas () const
 */
HB_FUNC( QT_QDOCKWIDGET_ALLOWEDAREAS )
{
   QDockWidget * p = hbqt_par_QDockWidget( 1 );
   if( p )
      hb_retni( ( Qt::DockWidgetAreas ) ( p )->allowedAreas() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QDOCKWIDGET_ALLOWEDAREAS FP=hb_retni( ( Qt::DockWidgetAreas ) ( p )->allowedAreas() ); p is NULL" ) );
   }
}

/*
 * DockWidgetFeatures features () const
 */
HB_FUNC( QT_QDOCKWIDGET_FEATURES )
{
   QDockWidget * p = hbqt_par_QDockWidget( 1 );
   if( p )
      hb_retni( ( QDockWidget::DockWidgetFeatures ) ( p )->features() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QDOCKWIDGET_FEATURES FP=hb_retni( ( QDockWidget::DockWidgetFeatures ) ( p )->features() ); p is NULL" ) );
   }
}

/*
 * bool isAreaAllowed ( Qt::DockWidgetArea area ) const
 */
HB_FUNC( QT_QDOCKWIDGET_ISAREAALLOWED )
{
   QDockWidget * p = hbqt_par_QDockWidget( 1 );
   if( p )
      hb_retl( ( p )->isAreaAllowed( ( Qt::DockWidgetArea ) hb_parni( 2 ) ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QDOCKWIDGET_ISAREAALLOWED FP=hb_retl( ( p )->isAreaAllowed( ( Qt::DockWidgetArea ) hb_parni( 2 ) ) ); p is NULL" ) );
   }
}

/*
 * bool isFloating () const
 */
HB_FUNC( QT_QDOCKWIDGET_ISFLOATING )
{
   QDockWidget * p = hbqt_par_QDockWidget( 1 );
   if( p )
      hb_retl( ( p )->isFloating() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QDOCKWIDGET_ISFLOATING FP=hb_retl( ( p )->isFloating() ); p is NULL" ) );
   }
}

/*
 * void setAllowedAreas ( Qt::DockWidgetAreas areas )
 */
HB_FUNC( QT_QDOCKWIDGET_SETALLOWEDAREAS )
{
   QDockWidget * p = hbqt_par_QDockWidget( 1 );
   if( p )
      ( p )->setAllowedAreas( ( Qt::DockWidgetAreas ) hb_parni( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QDOCKWIDGET_SETALLOWEDAREAS FP=( p )->setAllowedAreas( ( Qt::DockWidgetAreas ) hb_parni( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setFeatures ( DockWidgetFeatures features )
 */
HB_FUNC( QT_QDOCKWIDGET_SETFEATURES )
{
   QDockWidget * p = hbqt_par_QDockWidget( 1 );
   if( p )
      ( p )->setFeatures( ( QDockWidget::DockWidgetFeatures ) hb_parni( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QDOCKWIDGET_SETFEATURES FP=( p )->setFeatures( ( QDockWidget::DockWidgetFeatures ) hb_parni( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setFloating ( bool floating )
 */
HB_FUNC( QT_QDOCKWIDGET_SETFLOATING )
{
   QDockWidget * p = hbqt_par_QDockWidget( 1 );
   if( p )
      ( p )->setFloating( hb_parl( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QDOCKWIDGET_SETFLOATING FP=( p )->setFloating( hb_parl( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setTitleBarWidget ( QWidget * widget )
 */
HB_FUNC( QT_QDOCKWIDGET_SETTITLEBARWIDGET )
{
   QDockWidget * p = hbqt_par_QDockWidget( 1 );
   if( p )
      ( p )->setTitleBarWidget( hbqt_par_QWidget( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QDOCKWIDGET_SETTITLEBARWIDGET FP=( p )->setTitleBarWidget( hbqt_par_QWidget( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setWidget ( QWidget * widget )
 */
HB_FUNC( QT_QDOCKWIDGET_SETWIDGET )
{
   QDockWidget * p = hbqt_par_QDockWidget( 1 );
   if( p )
      ( p )->setWidget( hbqt_par_QWidget( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QDOCKWIDGET_SETWIDGET FP=( p )->setWidget( hbqt_par_QWidget( 2 ) ); p is NULL" ) );
   }
}

/*
 * QWidget * titleBarWidget () const
 */
HB_FUNC( QT_QDOCKWIDGET_TITLEBARWIDGET )
{
   QDockWidget * p = hbqt_par_QDockWidget( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QWidget( ( p )->titleBarWidget(), false ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QDOCKWIDGET_TITLEBARWIDGET FP=hb_retptrGC( hbqt_gcAllocate_QWidget( ( p )->titleBarWidget(), false ) ); p is NULL" ) );
   }
}

/*
 * QAction * toggleViewAction () const
 */
HB_FUNC( QT_QDOCKWIDGET_TOGGLEVIEWACTION )
{
   QDockWidget * p = hbqt_par_QDockWidget( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QAction( ( p )->toggleViewAction(), false ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QDOCKWIDGET_TOGGLEVIEWACTION FP=hb_retptrGC( hbqt_gcAllocate_QAction( ( p )->toggleViewAction(), false ) ); p is NULL" ) );
   }
}

/*
 * QWidget * widget () const
 */
HB_FUNC( QT_QDOCKWIDGET_WIDGET )
{
   QDockWidget * p = hbqt_par_QDockWidget( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QWidget( ( p )->widget(), false ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QDOCKWIDGET_WIDGET FP=hb_retptrGC( hbqt_gcAllocate_QWidget( ( p )->widget(), false ) ); p is NULL" ) );
   }
}


/*----------------------------------------------------------------------*/
#endif             /* #if QT_VERSION >= 0x040500 */
/*----------------------------------------------------------------------*/
