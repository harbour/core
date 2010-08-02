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

#include <QtGui/QWidgetAction>


/*
 * QWidgetAction ( QObject * parent )
 * virtual ~QWidgetAction ()
 */

typedef struct
{
   QPointer< QWidgetAction > ph;
   bool bNew;
   QT_G_FUNC_PTR func;
   int type;
} QGC_POINTER_QWidgetAction;

QT_G_FUNC( hbqt_gcRelease_QWidgetAction )
{
   QWidgetAction  * ph = NULL ;
   QGC_POINTER_QWidgetAction * p = ( QGC_POINTER_QWidgetAction * ) Cargo;

   if( p && p->bNew && p->ph )
   {
      ph = p->ph;
      if( ph )
      {
         const QMetaObject * m = ( ph )->metaObject();
         if( ( QString ) m->className() != ( QString ) "QObject" )
         {
            HB_TRACE( HB_TR_DEBUG, ( "ph=%p %p YES_rel_QWidgetAction   /.\\   ", (void*) ph, (void*) p->ph ) );
            delete ( p->ph );
            HB_TRACE( HB_TR_DEBUG, ( "ph=%p %p YES_rel_QWidgetAction   \\./   ", (void*) ph, (void*) p->ph ) );
            p->ph = NULL;
         }
         else
         {
            HB_TRACE( HB_TR_DEBUG, ( "ph=%p NO__rel_QWidgetAction          ", ph ) );
            p->ph = NULL;
         }
      }
      else
      {
         HB_TRACE( HB_TR_DEBUG, ( "ph=%p DEL_rel_QWidgetAction    :     Object already deleted!", ph ) );
         p->ph = NULL;
      }
   }
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p PTR_rel_QWidgetAction    :    Object not created with new=true", ph ) );
      p->ph = NULL;
   }
}

void * hbqt_gcAllocate_QWidgetAction( void * pObj, bool bNew )
{
   QGC_POINTER_QWidgetAction * p = ( QGC_POINTER_QWidgetAction * ) hb_gcAllocate( sizeof( QGC_POINTER_QWidgetAction ), hbqt_gcFuncs() );

   new( & p->ph ) QPointer< QWidgetAction >( ( QWidgetAction * ) pObj );
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QWidgetAction;
   p->type = HBQT_TYPE_QWidgetAction;

   if( bNew )
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p    _new_QWidgetAction  under p->pq", pObj ) );
   }
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p NOT_new_QWidgetAction", pObj ) );
   }
   return p;
}

HB_FUNC( QT_QWIDGETACTION )
{
   QWidgetAction * pObj = NULL;

   pObj =  new QWidgetAction( hbqt_par_QObject( 1 ) ) ;

   hb_retptrGC( hbqt_gcAllocate_QWidgetAction( ( void * ) pObj, true ) );
}

/*
 * QWidget * defaultWidget () const
 */
HB_FUNC( QT_QWIDGETACTION_DEFAULTWIDGET )
{
   QWidgetAction * p = hbqt_par_QWidgetAction( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QWidget( ( p )->defaultWidget(), false ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QWIDGETACTION_DEFAULTWIDGET FP=hb_retptrGC( hbqt_gcAllocate_QWidget( ( p )->defaultWidget(), false ) ); p is NULL" ) );
   }
}

/*
 * void releaseWidget ( QWidget * widget )
 */
HB_FUNC( QT_QWIDGETACTION_RELEASEWIDGET )
{
   QWidgetAction * p = hbqt_par_QWidgetAction( 1 );
   if( p )
      ( p )->releaseWidget( hbqt_par_QWidget( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QWIDGETACTION_RELEASEWIDGET FP=( p )->releaseWidget( hbqt_par_QWidget( 2 ) ); p is NULL" ) );
   }
}

/*
 * QWidget * requestWidget ( QWidget * parent )
 */
HB_FUNC( QT_QWIDGETACTION_REQUESTWIDGET )
{
   QWidgetAction * p = hbqt_par_QWidgetAction( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QWidget( ( p )->requestWidget( hbqt_par_QWidget( 2 ) ), false ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QWIDGETACTION_REQUESTWIDGET FP=hb_retptrGC( hbqt_gcAllocate_QWidget( ( p )->requestWidget( hbqt_par_QWidget( 2 ) ), false ) ); p is NULL" ) );
   }
}

/*
 * void setDefaultWidget ( QWidget * widget )
 */
HB_FUNC( QT_QWIDGETACTION_SETDEFAULTWIDGET )
{
   QWidgetAction * p = hbqt_par_QWidgetAction( 1 );
   if( p )
      ( p )->setDefaultWidget( hbqt_par_QWidget( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QWIDGETACTION_SETDEFAULTWIDGET FP=( p )->setDefaultWidget( hbqt_par_QWidget( 2 ) ); p is NULL" ) );
   }
}


/*----------------------------------------------------------------------*/
#endif             /* #if QT_VERSION >= 0x040500 */
/*----------------------------------------------------------------------*/
