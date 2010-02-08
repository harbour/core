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

#include <QtCore/QPointer>

#include <QtGui/QStackedWidget>


/*
 * QStackedWidget ( QWidget * parent = 0 )
 * ~QStackedWidget ()
 *
 */

typedef struct
{
   void * ph;
   bool bNew;
   QT_G_FUNC_PTR func;
   QPointer< QStackedWidget > pq;
} QGC_POINTER_QStackedWidget;

QT_G_FUNC( hbqt_gcRelease_QStackedWidget )
{
   QGC_POINTER_QStackedWidget * p = ( QGC_POINTER_QStackedWidget * ) Cargo;

   if( p && p->bNew )
   {
      if( p->ph && p->pq )
      {
         const QMetaObject * m = ( ( QObject * ) p->ph )->metaObject();
         if( ( QString ) m->className() != ( QString ) "QObject" )
         {
            HB_TRACE( HB_TR_DEBUG, ( "YES_rel_QStackedWidget   /.\\   ph=%p pq=%p", p->ph, (void *)(p->pq) ) );
            delete ( ( QStackedWidget * ) p->ph );
            HB_TRACE( HB_TR_DEBUG, ( "YES_rel_QStackedWidget   \\./   ph=%p pq=%p", p->ph, (void *)(p->pq) ) );
            p->ph = NULL;
         }
         else
         {
            HB_TRACE( HB_TR_DEBUG, ( "NO__rel_QStackedWidgetph=%p pq=%p", p->ph, (void *)(p->pq) ) );
         }
      }
      else
      {
         HB_TRACE( HB_TR_DEBUG, ( "DEL_rel_QStackedWidget    :     Object already deleted!" ) );
      }
   }
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "PTR_rel_QStackedWidget    :    Object not created with new()" ) );
      p->ph = NULL;
   }
}

void * hbqt_gcAllocate_QStackedWidget( void * pObj, bool bNew )
{
   QGC_POINTER_QStackedWidget * p = ( QGC_POINTER_QStackedWidget * ) hb_gcAllocate( sizeof( QGC_POINTER_QStackedWidget ), hbqt_gcFuncs() );

   p->ph = pObj;
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QStackedWidget;

   if( bNew )
   {
      new( & p->pq ) QPointer< QStackedWidget >( ( QStackedWidget * ) pObj );
      HB_TRACE( HB_TR_DEBUG, ( "   _new_QStackedWidget             ph=%p %i B %i KB", pObj, ( int ) hb_xquery( 1001 ), hbqt_getmemused() ) );
   }
   return p;
}

HB_FUNC( QT_QSTACKEDWIDGET )
{
   void * pObj = NULL;

   pObj = new QStackedWidget( hbqt_par_QWidget( 1 ) ) ;

   hb_retptrGC( hbqt_gcAllocate_QStackedWidget( pObj, true ) );
}

/*
 * int addWidget ( QWidget * widget )
 */
HB_FUNC( QT_QSTACKEDWIDGET_ADDWIDGET )
{
   hb_retni( hbqt_par_QStackedWidget( 1 )->addWidget( hbqt_par_QWidget( 2 ) ) );
}

/*
 * int count () const
 */
HB_FUNC( QT_QSTACKEDWIDGET_COUNT )
{
   hb_retni( hbqt_par_QStackedWidget( 1 )->count() );
}

/*
 * int currentIndex () const
 */
HB_FUNC( QT_QSTACKEDWIDGET_CURRENTINDEX )
{
   hb_retni( hbqt_par_QStackedWidget( 1 )->currentIndex() );
}

/*
 * QWidget * currentWidget () const
 */
HB_FUNC( QT_QSTACKEDWIDGET_CURRENTWIDGET )
{
   hb_retptrGC( hbqt_gcAllocate_QWidget( hbqt_par_QStackedWidget( 1 )->currentWidget(), false ) );
}

/*
 * int indexOf ( QWidget * widget ) const
 */
HB_FUNC( QT_QSTACKEDWIDGET_INDEXOF )
{
   hb_retni( hbqt_par_QStackedWidget( 1 )->indexOf( hbqt_par_QWidget( 2 ) ) );
}

/*
 * int insertWidget ( int index, QWidget * widget )
 */
HB_FUNC( QT_QSTACKEDWIDGET_INSERTWIDGET )
{
   hb_retni( hbqt_par_QStackedWidget( 1 )->insertWidget( hb_parni( 2 ), hbqt_par_QWidget( 3 ) ) );
}

/*
 * void removeWidget ( QWidget * widget )
 */
HB_FUNC( QT_QSTACKEDWIDGET_REMOVEWIDGET )
{
   hbqt_par_QStackedWidget( 1 )->removeWidget( hbqt_par_QWidget( 2 ) );
}

/*
 * QWidget * widget ( int index ) const
 */
HB_FUNC( QT_QSTACKEDWIDGET_WIDGET )
{
   hb_retptrGC( hbqt_gcAllocate_QWidget( hbqt_par_QStackedWidget( 1 )->widget( hb_parni( 2 ) ), false ) );
}

/*
 * void setCurrentIndex ( int index )
 */
HB_FUNC( QT_QSTACKEDWIDGET_SETCURRENTINDEX )
{
   hbqt_par_QStackedWidget( 1 )->setCurrentIndex( hb_parni( 2 ) );
}

/*
 * void setCurrentWidget ( QWidget * widget )
 */
HB_FUNC( QT_QSTACKEDWIDGET_SETCURRENTWIDGET )
{
   hbqt_par_QStackedWidget( 1 )->setCurrentWidget( hbqt_par_QWidget( 2 ) );
}


/*----------------------------------------------------------------------*/
#endif             /* #if QT_VERSION >= 0x040500 */
/*----------------------------------------------------------------------*/
