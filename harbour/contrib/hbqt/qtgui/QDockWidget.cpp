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
 * Copyright 2009 Pritpal Bedi <pritpal@vouchcac.com>
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
  void * ph;
  QT_G_FUNC_PTR func;
  QPointer< QDockWidget > pq;
} QGC_POINTER_QDockWidget;

QT_G_FUNC( release_QDockWidget )
{
   QGC_POINTER_QDockWidget * p = ( QGC_POINTER_QDockWidget * ) Cargo;

   HB_TRACE( HB_TR_DEBUG, ( "release_QDockWidget                  p=%p", p));
   HB_TRACE( HB_TR_DEBUG, ( "release_QDockWidget                 ph=%p pq=%p", p->ph, (void *)(p->pq)));

   if( p && p->ph && p->pq )
   {
      const QMetaObject * m = ( ( QObject * ) p->ph )->metaObject();
      if( ( QString ) m->className() != ( QString ) "QObject" )
      {
         switch( hbqt_get_object_release_method() )
         {
         case HBQT_RELEASE_WITH_DELETE:
            delete ( ( QDockWidget * ) p->ph );
            break;
         case HBQT_RELEASE_WITH_DESTRUTOR:
            ( ( QDockWidget * ) p->ph )->~QDockWidget();
            break;
         case HBQT_RELEASE_WITH_DELETE_LATER:
            ( ( QDockWidget * ) p->ph )->deleteLater();
            break;
         }
         p->ph = NULL;
         HB_TRACE( HB_TR_DEBUG, ( "release_QDockWidget                 Object deleted! %i B %i KB", ( int ) hb_xquery( 1001 ), hbqt_getmemused() ) );
      }
      else
      {
         HB_TRACE( HB_TR_DEBUG, ( "NO release_QDockWidget                 Object Name Missing!" ) );
      }
   }
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "DEL release_QDockWidget                 Object Allready deleted!" ) );
   }
}

void * gcAllocate_QDockWidget( void * pObj )
{
   QGC_POINTER_QDockWidget * p = ( QGC_POINTER_QDockWidget * ) hb_gcAllocate( sizeof( QGC_POINTER_QDockWidget ), gcFuncs() );

   p->ph = pObj;
   p->func = release_QDockWidget;
   new( & p->pq ) QPointer< QDockWidget >( ( QDockWidget * ) pObj );
   HB_TRACE( HB_TR_DEBUG, ( "          new_QDockWidget                 %i B %i KB", ( int ) hb_xquery( 1001 ), hbqt_getmemused() ) );
   return( p );
}

HB_FUNC( QT_QDOCKWIDGET )
{
   void * pObj = NULL;

   pObj = new QDockWidget( hbqt_par_QWidget( 1 ), ( Qt::WindowFlags ) hb_parni( 2 ) ) ;

   hb_retptrGC( gcAllocate_QDockWidget( pObj ) );
}
/*
 * Qt::DockWidgetAreas allowedAreas () const
 */
HB_FUNC( QT_QDOCKWIDGET_ALLOWEDAREAS )
{
   hb_retni( ( Qt::DockWidgetAreas ) hbqt_par_QDockWidget( 1 )->allowedAreas() );
}

/*
 * DockWidgetFeatures features () const
 */
HB_FUNC( QT_QDOCKWIDGET_FEATURES )
{
   hb_retni( ( QDockWidget::DockWidgetFeatures ) hbqt_par_QDockWidget( 1 )->features() );
}

/*
 * bool isAreaAllowed ( Qt::DockWidgetArea area ) const
 */
HB_FUNC( QT_QDOCKWIDGET_ISAREAALLOWED )
{
   hb_retl( hbqt_par_QDockWidget( 1 )->isAreaAllowed( ( Qt::DockWidgetArea ) hb_parni( 2 ) ) );
}

/*
 * bool isFloating () const
 */
HB_FUNC( QT_QDOCKWIDGET_ISFLOATING )
{
   hb_retl( hbqt_par_QDockWidget( 1 )->isFloating() );
}

/*
 * void setAllowedAreas ( Qt::DockWidgetAreas areas )
 */
HB_FUNC( QT_QDOCKWIDGET_SETALLOWEDAREAS )
{
   hbqt_par_QDockWidget( 1 )->setAllowedAreas( ( Qt::DockWidgetAreas ) hb_parni( 2 ) );
}

/*
 * void setFeatures ( DockWidgetFeatures features )
 */
HB_FUNC( QT_QDOCKWIDGET_SETFEATURES )
{
   hbqt_par_QDockWidget( 1 )->setFeatures( ( QDockWidget::DockWidgetFeatures ) hb_parni( 2 ) );
}

/*
 * void setFloating ( bool floating )
 */
HB_FUNC( QT_QDOCKWIDGET_SETFLOATING )
{
   hbqt_par_QDockWidget( 1 )->setFloating( hb_parl( 2 ) );
}

/*
 * void setTitleBarWidget ( QWidget * widget )
 */
HB_FUNC( QT_QDOCKWIDGET_SETTITLEBARWIDGET )
{
   hbqt_par_QDockWidget( 1 )->setTitleBarWidget( hbqt_par_QWidget( 2 ) );
}

/*
 * void setWidget ( QWidget * widget )
 */
HB_FUNC( QT_QDOCKWIDGET_SETWIDGET )
{
   hbqt_par_QDockWidget( 1 )->setWidget( hbqt_par_QWidget( 2 ) );
}

/*
 * QWidget * titleBarWidget () const
 */
HB_FUNC( QT_QDOCKWIDGET_TITLEBARWIDGET )
{
   hb_retptr( ( QWidget* ) hbqt_par_QDockWidget( 1 )->titleBarWidget() );
}

/*
 * QAction * toggleViewAction () const
 */
HB_FUNC( QT_QDOCKWIDGET_TOGGLEVIEWACTION )
{
   hb_retptr( ( QAction* ) hbqt_par_QDockWidget( 1 )->toggleViewAction() );
}

/*
 * QWidget * widget () const
 */
HB_FUNC( QT_QDOCKWIDGET_WIDGET )
{
   hb_retptr( ( QWidget* ) hbqt_par_QDockWidget( 1 )->widget() );
}


/*----------------------------------------------------------------------*/
#endif             /* #if QT_VERSION >= 0x040500 */
/*----------------------------------------------------------------------*/
