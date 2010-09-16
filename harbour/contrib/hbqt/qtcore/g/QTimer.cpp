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

/*----------------------------------------------------------------------*/
#if QT_VERSION >= 0x040500
/*----------------------------------------------------------------------*/

#include <QtCore/QPointer>

#include <QtCore/QTimer>


/*
 * QTimer ( QObject * parent = 0 )
 * ~QTimer ()
 */

typedef struct
{
   QPointer< QTimer > ph;
   bool bNew;
   PHBQT_GC_FUNC func;
   int type;
} HBQT_GC_T_QTimer;

HBQT_GC_FUNC( hbqt_gcRelease_QTimer )
{
   QTimer  * ph = NULL ;
   HBQT_GC_T_QTimer * p = ( HBQT_GC_T_QTimer * ) Cargo;

   if( p && p->bNew && p->ph )
   {
      ph = p->ph;
      if( ph )
      {
         const QMetaObject * m = ( ph )->metaObject();
         if( ( QString ) m->className() != ( QString ) "QObject" )
         {
            HB_TRACE( HB_TR_DEBUG, ( "ph=%p %p YES_rel_QTimer   /.\\   ", (void*) ph, (void*) p->ph ) );
            delete ( p->ph );
            HB_TRACE( HB_TR_DEBUG, ( "ph=%p %p YES_rel_QTimer   \\./   ", (void*) ph, (void*) p->ph ) );
            p->ph = NULL;
         }
         else
         {
            HB_TRACE( HB_TR_DEBUG, ( "ph=%p NO__rel_QTimer          ", ph ) );
            p->ph = NULL;
         }
      }
      else
      {
         HB_TRACE( HB_TR_DEBUG, ( "ph=%p DEL_rel_QTimer    :     Object already deleted!", ph ) );
         p->ph = NULL;
      }
   }
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p PTR_rel_QTimer    :    Object not created with new=true", ph ) );
      p->ph = NULL;
   }
}

void * hbqt_gcAllocate_QTimer( void * pObj, bool bNew )
{
   HBQT_GC_T_QTimer * p = ( HBQT_GC_T_QTimer * ) hb_gcAllocate( sizeof( HBQT_GC_T_QTimer ), hbqt_gcFuncs() );

   new( & p->ph ) QPointer< QTimer >( ( QTimer * ) pObj );
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QTimer;
   p->type = HBQT_TYPE_QTimer;

   if( bNew )
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p    _new_QTimer  under p->pq", pObj ) );
   }
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p NOT_new_QTimer", pObj ) );
   }
   return p;
}

HB_FUNC( QT_QTIMER )
{
   QTimer * pObj = NULL;

   pObj = new QTimer( hbqt_par_QObject( 1 ) ) ;

   hb_retptrGC( hbqt_gcAllocate_QTimer( ( void * ) pObj, true ) );
}

/*
 * int interval () const
 */
HB_FUNC( QT_QTIMER_INTERVAL )
{
   QTimer * p = hbqt_par_QTimer( 1 );
   if( p )
   {
      hb_retni( ( p )->interval() );
   }
}

/*
 * bool isActive () const
 */
HB_FUNC( QT_QTIMER_ISACTIVE )
{
   QTimer * p = hbqt_par_QTimer( 1 );
   if( p )
   {
      hb_retl( ( p )->isActive() );
   }
}

/*
 * bool isSingleShot () const
 */
HB_FUNC( QT_QTIMER_ISSINGLESHOT )
{
   QTimer * p = hbqt_par_QTimer( 1 );
   if( p )
   {
      hb_retl( ( p )->isSingleShot() );
   }
}

/*
 * void setInterval ( int msec )
 */
HB_FUNC( QT_QTIMER_SETINTERVAL )
{
   QTimer * p = hbqt_par_QTimer( 1 );
   if( p )
   {
      ( p )->setInterval( hb_parni( 2 ) );
   }
}

/*
 * void setSingleShot ( bool singleShot )
 */
HB_FUNC( QT_QTIMER_SETSINGLESHOT )
{
   QTimer * p = hbqt_par_QTimer( 1 );
   if( p )
   {
      ( p )->setSingleShot( hb_parl( 2 ) );
   }
}

/*
 * int timerId () const
 */
HB_FUNC( QT_QTIMER_TIMERID )
{
   QTimer * p = hbqt_par_QTimer( 1 );
   if( p )
   {
      hb_retni( ( p )->timerId() );
   }
}

/*
 * void singleShot ( int msec, QObject * receiver, const char * member )
 */
HB_FUNC( QT_QTIMER_SINGLESHOT )
{
   QTimer * p = hbqt_par_QTimer( 1 );
   if( p )
   {
      ( p )->singleShot( hb_parni( 2 ), hbqt_par_QObject( 3 ), hbqt_par_char( 4 ) );
   }
}

/*
 * void start ()
 */
HB_FUNC( QT_QTIMER_START )
{
   QTimer * p = hbqt_par_QTimer( 1 );
   if( p )
   {
      ( p )->start();
   }
}

/*
 * void stop ()
 */
HB_FUNC( QT_QTIMER_STOP )
{
   QTimer * p = hbqt_par_QTimer( 1 );
   if( p )
   {
      ( p )->stop();
   }
}

/*
 * void start ( int msec )
 */
HB_FUNC( QT_QTIMER_START_1 )
{
   QTimer * p = hbqt_par_QTimer( 1 );
   if( p )
   {
      ( p )->start( hb_parni( 2 ) );
   }
}


/*----------------------------------------------------------------------*/
#endif             /* #if QT_VERSION >= 0x040500 */
/*----------------------------------------------------------------------*/
