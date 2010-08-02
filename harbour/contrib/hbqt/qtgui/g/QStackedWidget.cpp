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

#include <QtCore/QPointer>

#include <QtGui/QStackedWidget>


/*
 * QStackedWidget ( QWidget * parent = 0 )
 * ~QStackedWidget ()
 *
 */

typedef struct
{
   QPointer< QStackedWidget > ph;
   bool bNew;
   QT_G_FUNC_PTR func;
   int type;
} QGC_POINTER_QStackedWidget;

QT_G_FUNC( hbqt_gcRelease_QStackedWidget )
{
   QStackedWidget  * ph = NULL ;
   QGC_POINTER_QStackedWidget * p = ( QGC_POINTER_QStackedWidget * ) Cargo;

   if( p && p->bNew && p->ph )
   {
      ph = p->ph;
      if( ph )
      {
         const QMetaObject * m = ( ph )->metaObject();
         if( ( QString ) m->className() != ( QString ) "QObject" )
         {
            HB_TRACE( HB_TR_DEBUG, ( "ph=%p %p YES_rel_QStackedWidget   /.\\   ", (void*) ph, (void*) p->ph ) );
            delete ( p->ph );
            HB_TRACE( HB_TR_DEBUG, ( "ph=%p %p YES_rel_QStackedWidget   \\./   ", (void*) ph, (void*) p->ph ) );
            p->ph = NULL;
         }
         else
         {
            HB_TRACE( HB_TR_DEBUG, ( "ph=%p NO__rel_QStackedWidget          ", ph ) );
            p->ph = NULL;
         }
      }
      else
      {
         HB_TRACE( HB_TR_DEBUG, ( "ph=%p DEL_rel_QStackedWidget    :     Object already deleted!", ph ) );
         p->ph = NULL;
      }
   }
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p PTR_rel_QStackedWidget    :    Object not created with new=true", ph ) );
      p->ph = NULL;
   }
}

void * hbqt_gcAllocate_QStackedWidget( void * pObj, bool bNew )
{
   QGC_POINTER_QStackedWidget * p = ( QGC_POINTER_QStackedWidget * ) hb_gcAllocate( sizeof( QGC_POINTER_QStackedWidget ), hbqt_gcFuncs() );

   new( & p->ph ) QPointer< QStackedWidget >( ( QStackedWidget * ) pObj );
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QStackedWidget;
   p->type = HBQT_TYPE_QStackedWidget;

   if( bNew )
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p    _new_QStackedWidget  under p->pq", pObj ) );
   }
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p NOT_new_QStackedWidget", pObj ) );
   }
   return p;
}

HB_FUNC( QT_QSTACKEDWIDGET )
{
   QStackedWidget * pObj = NULL;

   pObj = new QStackedWidget( hbqt_par_QWidget( 1 ) ) ;

   hb_retptrGC( hbqt_gcAllocate_QStackedWidget( ( void * ) pObj, true ) );
}

/*
 * int addWidget ( QWidget * widget )
 */
HB_FUNC( QT_QSTACKEDWIDGET_ADDWIDGET )
{
   QStackedWidget * p = hbqt_par_QStackedWidget( 1 );
   if( p )
      hb_retni( ( p )->addWidget( hbqt_par_QWidget( 2 ) ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QSTACKEDWIDGET_ADDWIDGET FP=hb_retni( ( p )->addWidget( hbqt_par_QWidget( 2 ) ) ); p is NULL" ) );
   }
}

/*
 * int count () const
 */
HB_FUNC( QT_QSTACKEDWIDGET_COUNT )
{
   QStackedWidget * p = hbqt_par_QStackedWidget( 1 );
   if( p )
      hb_retni( ( p )->count() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QSTACKEDWIDGET_COUNT FP=hb_retni( ( p )->count() ); p is NULL" ) );
   }
}

/*
 * int currentIndex () const
 */
HB_FUNC( QT_QSTACKEDWIDGET_CURRENTINDEX )
{
   QStackedWidget * p = hbqt_par_QStackedWidget( 1 );
   if( p )
      hb_retni( ( p )->currentIndex() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QSTACKEDWIDGET_CURRENTINDEX FP=hb_retni( ( p )->currentIndex() ); p is NULL" ) );
   }
}

/*
 * QWidget * currentWidget () const
 */
HB_FUNC( QT_QSTACKEDWIDGET_CURRENTWIDGET )
{
   QStackedWidget * p = hbqt_par_QStackedWidget( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QWidget( ( p )->currentWidget(), false ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QSTACKEDWIDGET_CURRENTWIDGET FP=hb_retptrGC( hbqt_gcAllocate_QWidget( ( p )->currentWidget(), false ) ); p is NULL" ) );
   }
}

/*
 * int indexOf ( QWidget * widget ) const
 */
HB_FUNC( QT_QSTACKEDWIDGET_INDEXOF )
{
   QStackedWidget * p = hbqt_par_QStackedWidget( 1 );
   if( p )
      hb_retni( ( p )->indexOf( hbqt_par_QWidget( 2 ) ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QSTACKEDWIDGET_INDEXOF FP=hb_retni( ( p )->indexOf( hbqt_par_QWidget( 2 ) ) ); p is NULL" ) );
   }
}

/*
 * int insertWidget ( int index, QWidget * widget )
 */
HB_FUNC( QT_QSTACKEDWIDGET_INSERTWIDGET )
{
   QStackedWidget * p = hbqt_par_QStackedWidget( 1 );
   if( p )
      hb_retni( ( p )->insertWidget( hb_parni( 2 ), hbqt_par_QWidget( 3 ) ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QSTACKEDWIDGET_INSERTWIDGET FP=hb_retni( ( p )->insertWidget( hb_parni( 2 ), hbqt_par_QWidget( 3 ) ) ); p is NULL" ) );
   }
}

/*
 * void removeWidget ( QWidget * widget )
 */
HB_FUNC( QT_QSTACKEDWIDGET_REMOVEWIDGET )
{
   QStackedWidget * p = hbqt_par_QStackedWidget( 1 );
   if( p )
      ( p )->removeWidget( hbqt_par_QWidget( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QSTACKEDWIDGET_REMOVEWIDGET FP=( p )->removeWidget( hbqt_par_QWidget( 2 ) ); p is NULL" ) );
   }
}

/*
 * QWidget * widget ( int index ) const
 */
HB_FUNC( QT_QSTACKEDWIDGET_WIDGET )
{
   QStackedWidget * p = hbqt_par_QStackedWidget( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QWidget( ( p )->widget( hb_parni( 2 ) ), false ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QSTACKEDWIDGET_WIDGET FP=hb_retptrGC( hbqt_gcAllocate_QWidget( ( p )->widget( hb_parni( 2 ) ), false ) ); p is NULL" ) );
   }
}

/*
 * void setCurrentIndex ( int index )
 */
HB_FUNC( QT_QSTACKEDWIDGET_SETCURRENTINDEX )
{
   QStackedWidget * p = hbqt_par_QStackedWidget( 1 );
   if( p )
      ( p )->setCurrentIndex( hb_parni( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QSTACKEDWIDGET_SETCURRENTINDEX FP=( p )->setCurrentIndex( hb_parni( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setCurrentWidget ( QWidget * widget )
 */
HB_FUNC( QT_QSTACKEDWIDGET_SETCURRENTWIDGET )
{
   QStackedWidget * p = hbqt_par_QStackedWidget( 1 );
   if( p )
      ( p )->setCurrentWidget( hbqt_par_QWidget( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QSTACKEDWIDGET_SETCURRENTWIDGET FP=( p )->setCurrentWidget( hbqt_par_QWidget( 2 ) ); p is NULL" ) );
   }
}


/*----------------------------------------------------------------------*/
#endif             /* #if QT_VERSION >= 0x040500 */
/*----------------------------------------------------------------------*/
