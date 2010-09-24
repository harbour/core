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
 *  enum TickPosition { NoTicks, TicksBothSides, TicksAbove, TicksBelow, TicksLeft, TicksRight }
 */

/*
 *  Constructed[ 4/4 [ 100.00% ] ]
 *
 */

#include <QtCore/QPointer>

#include <QtGui/QSlider>


/*
 * QSlider ( QWidget * parent = 0 )
 * QSlider ( Qt::Orientation orientation, QWidget * parent = 0 )
 * ~QSlider ()
 */

typedef struct
{
   QPointer< QSlider > ph;
   bool bNew;
   PHBQT_GC_FUNC func;
   int type;
} HBQT_GC_T_QSlider;

HBQT_GC_FUNC( hbqt_gcRelease_QSlider )
{
   QSlider  * ph = NULL ;
   HBQT_GC_T_QSlider * p = ( HBQT_GC_T_QSlider * ) Cargo;

   if( p && p->bNew && p->ph )
   {
      ph = p->ph;
      if( ph )
      {
         const QMetaObject * m = ( ph )->metaObject();
         if( ( QString ) m->className() != ( QString ) "QObject" )
         {
            HB_TRACE( HB_TR_DEBUG, ( "ph=%p %p YES_rel_QSlider   /.\\   ", (void*) ph, (void*) p->ph ) );
            delete ( p->ph );
            HB_TRACE( HB_TR_DEBUG, ( "ph=%p %p YES_rel_QSlider   \\./   ", (void*) ph, (void*) p->ph ) );
            p->ph = NULL;
         }
         else
         {
            HB_TRACE( HB_TR_DEBUG, ( "ph=%p NO__rel_QSlider          ", ph ) );
            p->ph = NULL;
         }
      }
      else
      {
         HB_TRACE( HB_TR_DEBUG, ( "ph=%p DEL_rel_QSlider    :     Object already deleted!", ph ) );
         p->ph = NULL;
      }
   }
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p PTR_rel_QSlider    :    Object not created with new=true", ph ) );
      p->ph = NULL;
   }
}

void * hbqt_gcAllocate_QSlider( void * pObj, bool bNew )
{
   HBQT_GC_T_QSlider * p = ( HBQT_GC_T_QSlider * ) hb_gcAllocate( sizeof( HBQT_GC_T_QSlider ), hbqt_gcFuncs() );

   new( & p->ph ) QPointer< QSlider >( ( QSlider * ) pObj );
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QSlider;
   p->type = HBQT_TYPE_QSlider;

   if( bNew )
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p    _new_QSlider  under p->pq", pObj ) );
   }
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p NOT_new_QSlider", pObj ) );
   }
   return p;
}

HB_FUNC( QT_QSLIDER )
{
   QSlider * pObj = NULL;

   if( hb_pcount() >= 1 && HB_ISNUM( 1 ) )
      pObj =  new QSlider( ( Qt::Orientation ) hb_parni( 1 ), hbqt_par_QWidget( 2 ) ) ;
   else
      pObj =  new QSlider( hbqt_par_QWidget( 1 ) ) ;

   hb_retptrGC( hbqt_gcAllocate_QSlider( ( void * ) pObj, true ) );
}

/*
 * void setTickInterval ( int ti )
 */
HB_FUNC( QT_QSLIDER_SETTICKINTERVAL )
{
   QSlider * p = hbqt_par_QSlider( 1 );
   if( p )
   {
      ( p )->setTickInterval( hb_parni( 2 ) );
   }
}

/*
 * void setTickPosition ( TickPosition position )
 */
HB_FUNC( QT_QSLIDER_SETTICKPOSITION )
{
   QSlider * p = hbqt_par_QSlider( 1 );
   if( p )
   {
      ( p )->setTickPosition( ( QSlider::TickPosition ) hb_parni( 2 ) );
   }
}

/*
 * int tickInterval () const
 */
HB_FUNC( QT_QSLIDER_TICKINTERVAL )
{
   QSlider * p = hbqt_par_QSlider( 1 );
   if( p )
   {
      hb_retni( ( p )->tickInterval() );
   }
}

/*
 * TickPosition tickPosition () const
 */
HB_FUNC( QT_QSLIDER_TICKPOSITION )
{
   QSlider * p = hbqt_par_QSlider( 1 );
   if( p )
   {
      hb_retni( ( QSlider::TickPosition ) ( p )->tickPosition() );
   }
}


/*----------------------------------------------------------------------*/
#endif             /* #if QT_VERSION >= 0x040500 */
/*----------------------------------------------------------------------*/
