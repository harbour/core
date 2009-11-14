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
 *  enum TickPosition { NoTicks, TicksBothSides, TicksAbove, TicksBelow, TicksLeft, TicksRight }
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
  void * ph;
  QT_G_FUNC_PTR func;
  QPointer< QSlider > pq;
} QGC_POINTER_QSlider;

QT_G_FUNC( release_QSlider )
{
   QGC_POINTER_QSlider * p = ( QGC_POINTER_QSlider * ) Cargo;

   HB_TRACE( HB_TR_DEBUG, ( "release_QSlider                      p=%p", p));
   HB_TRACE( HB_TR_DEBUG, ( "release_QSlider                     ph=%p pq=%p", p->ph, (void *)(p->pq)));

   if( p && p->ph && p->pq )
   {
      const QMetaObject * m = ( ( QObject * ) p->ph )->metaObject();
      if( ( QString ) m->className() != ( QString ) "QObject" )
      {
         ( ( QSlider * ) p->ph )->~QSlider();
         p->ph = NULL;
         HB_TRACE( HB_TR_DEBUG, ( "release_QSlider                     Object deleted!" ) );
         #if defined(__debug__)
            just_debug( "  YES release_QSlider                     %i B %i KB", ( int ) hb_xquery( 1001 ), hb_getMemUsed() );
         #endif
      }
      else
      {
         HB_TRACE( HB_TR_DEBUG, ( "release_QSlider                     Object Name Missing!" ) );
         #if defined(__debug__)
            just_debug( "  NO  release_QSlider" );
         #endif
      }
   }
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "release_QSlider                     Object Allready deleted!" ) );
      #if defined(__debug__)
         just_debug( "  DEL release_QSlider" );
      #endif
   }
}

void * gcAllocate_QSlider( void * pObj )
{
   QGC_POINTER_QSlider * p = ( QGC_POINTER_QSlider * ) hb_gcAllocate( sizeof( QGC_POINTER_QSlider ), gcFuncs() );

   p->ph = pObj;
   p->func = release_QSlider;
   new( & p->pq ) QPointer< QSlider >( ( QSlider * ) pObj );
   #if defined(__debug__)
      just_debug( "          new_QSlider                     %i B %i KB", ( int ) hb_xquery( 1001 ), hb_getMemUsed() );
   #endif
   return( p );
}

HB_FUNC( QT_QSLIDER )
{
   void * pObj = NULL;

   if( hb_pcount() >= 1 && HB_ISNUM( 1 ) )
      pObj = ( QSlider* ) new QSlider( ( Qt::Orientation ) hb_parni( 1 ), hbqt_par_QWidget( 2 ) ) ;
   else
      pObj = ( QSlider* ) new QSlider( hbqt_par_QWidget( 1 ) ) ;

   hb_retptrGC( gcAllocate_QSlider( pObj ) );
}
/*
 * void setTickInterval ( int ti )
 */
HB_FUNC( QT_QSLIDER_SETTICKINTERVAL )
{
   hbqt_par_QSlider( 1 )->setTickInterval( hb_parni( 2 ) );
}

/*
 * void setTickPosition ( TickPosition position )
 */
HB_FUNC( QT_QSLIDER_SETTICKPOSITION )
{
   hbqt_par_QSlider( 1 )->setTickPosition( ( QSlider::TickPosition ) hb_parni( 2 ) );
}

/*
 * int tickInterval () const
 */
HB_FUNC( QT_QSLIDER_TICKINTERVAL )
{
   hb_retni( hbqt_par_QSlider( 1 )->tickInterval() );
}

/*
 * TickPosition tickPosition () const
 */
HB_FUNC( QT_QSLIDER_TICKPOSITION )
{
   hb_retni( ( QSlider::TickPosition ) hbqt_par_QSlider( 1 )->tickPosition() );
}


/*----------------------------------------------------------------------*/
#endif             /* #if QT_VERSION >= 0x040500 */
/*----------------------------------------------------------------------*/
