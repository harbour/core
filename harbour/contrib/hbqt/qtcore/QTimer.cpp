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

#include <QtCore/QPointer>

#include <QtCore/QTimer>


/*
 * QTimer ( QObject * parent = 0 )
 * ~QTimer ()
 */

typedef struct
{
  void * ph;
  QT_G_FUNC_PTR func;
  QPointer< QTimer > pq;
} QGC_POINTER_QTimer;

QT_G_FUNC( release_QTimer )
{
   QGC_POINTER_QTimer * p = ( QGC_POINTER_QTimer * ) Cargo;

   HB_TRACE( HB_TR_DEBUG, ( "release_QTimer                       p=%p", p));
   HB_TRACE( HB_TR_DEBUG, ( "release_QTimer                      ph=%p pq=%p", p->ph, (void *)(p->pq)));

   if( p && p->ph && p->pq )
   {
      const QMetaObject * m = ( ( QObject * ) p->ph )->metaObject();
      if( ( QString ) m->className() != ( QString ) "QObject" )
      {
         switch( hbqt_get_object_release_method() )
         {
         case HBQT_RELEASE_WITH_DELETE:
            delete ( ( QTimer * ) p->ph );
            break;
         case HBQT_RELEASE_WITH_DESTRUTOR:
            ( ( QTimer * ) p->ph )->~QTimer();
            break;
         case HBQT_RELEASE_WITH_DELETE_LATER:
            ( ( QTimer * ) p->ph )->deleteLater();
            break;
         }
         p->ph = NULL;
         HB_TRACE( HB_TR_DEBUG, ( "release_QTimer                      Object deleted! %i B %i KB", ( int ) hb_xquery( 1001 ), hbqt_getmemused() ) );
      }
      else
      {
         HB_TRACE( HB_TR_DEBUG, ( "NO release_QTimer                      Object Name Missing!" ) );
      }
   }
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "DEL release_QTimer                      Object Allready deleted!" ) );
   }
}

void * gcAllocate_QTimer( void * pObj )
{
   QGC_POINTER_QTimer * p = ( QGC_POINTER_QTimer * ) hb_gcAllocate( sizeof( QGC_POINTER_QTimer ), gcFuncs() );

   p->ph = pObj;
   p->func = release_QTimer;
   new( & p->pq ) QPointer< QTimer >( ( QTimer * ) pObj );
   HB_TRACE( HB_TR_DEBUG, ( "          new_QTimer                      %i B %i KB", ( int ) hb_xquery( 1001 ), hbqt_getmemused() ) );
   return( p );
}

HB_FUNC( QT_QTIMER )
{
   void * pObj = NULL;

   pObj = new QTimer( hbqt_par_QObject( 1 ) ) ;

   hb_retptrGC( gcAllocate_QTimer( pObj ) );
}
/*
 * int interval () const
 */
HB_FUNC( QT_QTIMER_INTERVAL )
{
   hb_retni( hbqt_par_QTimer( 1 )->interval() );
}

/*
 * bool isActive () const
 */
HB_FUNC( QT_QTIMER_ISACTIVE )
{
   hb_retl( hbqt_par_QTimer( 1 )->isActive() );
}

/*
 * bool isSingleShot () const
 */
HB_FUNC( QT_QTIMER_ISSINGLESHOT )
{
   hb_retl( hbqt_par_QTimer( 1 )->isSingleShot() );
}

/*
 * void setInterval ( int msec )
 */
HB_FUNC( QT_QTIMER_SETINTERVAL )
{
   hbqt_par_QTimer( 1 )->setInterval( hb_parni( 2 ) );
}

/*
 * void setSingleShot ( bool singleShot )
 */
HB_FUNC( QT_QTIMER_SETSINGLESHOT )
{
   hbqt_par_QTimer( 1 )->setSingleShot( hb_parl( 2 ) );
}

/*
 * int timerId () const
 */
HB_FUNC( QT_QTIMER_TIMERID )
{
   hb_retni( hbqt_par_QTimer( 1 )->timerId() );
}

/*
 * void singleShot ( int msec, QObject * receiver, const char * member )
 */
HB_FUNC( QT_QTIMER_SINGLESHOT )
{
   hbqt_par_QTimer( 1 )->singleShot( hb_parni( 2 ), hbqt_par_QObject( 3 ), hbqt_par_char( 4 ) );
}

/*
 * void start ()
 */
HB_FUNC( QT_QTIMER_START )
{
   hbqt_par_QTimer( 1 )->start();
}

/*
 * void stop ()
 */
HB_FUNC( QT_QTIMER_STOP )
{
   hbqt_par_QTimer( 1 )->stop();
}

/*
 * void start ( int msec )
 */
HB_FUNC( QT_QTIMER_START_1 )
{
   hbqt_par_QTimer( 1 )->start( hb_parni( 2 ) );
}


/*----------------------------------------------------------------------*/
#endif             /* #if QT_VERSION >= 0x040500 */
/*----------------------------------------------------------------------*/
