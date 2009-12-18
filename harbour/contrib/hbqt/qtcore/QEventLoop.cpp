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
 *  enum ProcessEventsFlag { AllEvents, ExcludeUserInputEvents, ExcludeSocketNotifiers, WaitForMoreEvents, DeferredDeletion }
 *  flags ProcessEventsFlags
 */

#include <QtCore/QPointer>

#include <QtCore/QEventLoop>
#include <QtCore/QEvent>

/*
 * QEventLoop ( QObject * parent = 0 )
 * ~QEventLoop ()
 */

typedef struct
{
  void * ph;
  QT_G_FUNC_PTR func;
  QPointer< QEventLoop > pq;
} QGC_POINTER_QEventLoop;

QT_G_FUNC( release_QEventLoop )
{
   QGC_POINTER_QEventLoop * p = ( QGC_POINTER_QEventLoop * ) Cargo;

   HB_TRACE( HB_TR_DEBUG, ( "release_QEventLoop                   p=%p", p));
   HB_TRACE( HB_TR_DEBUG, ( "release_QEventLoop                  ph=%p pq=%p", p->ph, (void *)(p->pq)));

   if( p && p->ph && p->pq )
   {
      const QMetaObject * m = ( ( QObject * ) p->ph )->metaObject();
      if( ( QString ) m->className() != ( QString ) "QObject" )
      {
         switch( hbqt_get_object_release_method() )
         {
         case HBQT_RELEASE_WITH_DELETE:
            delete ( ( QEventLoop * ) p->ph );
            break;
         case HBQT_RELEASE_WITH_DESTRUTOR:
            ( ( QEventLoop * ) p->ph )->~QEventLoop();
            break;
         case HBQT_RELEASE_WITH_DELETE_LATER:
            ( ( QEventLoop * ) p->ph )->deleteLater();
            break;
         }
         p->ph = NULL;
         HB_TRACE( HB_TR_DEBUG, ( "release_QEventLoop                  Object deleted! %i B %i KB", ( int ) hb_xquery( 1001 ), hbqt_getmemused() ) );
      }
      else
      {
         HB_TRACE( HB_TR_DEBUG, ( "NO release_QEventLoop                  Object Name Missing!" ) );
      }
   }
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "DEL release_QEventLoop                  Object Already deleted!" ) );
   }
}

void * hbqt_gcAllocate_QEventLoop( void * pObj )
{
   QGC_POINTER_QEventLoop * p = ( QGC_POINTER_QEventLoop * ) hb_gcAllocate( sizeof( QGC_POINTER_QEventLoop ), hbqt_gcFuncs() );

   p->ph = pObj;
   p->func = release_QEventLoop;
   new( & p->pq ) QPointer< QEventLoop >( ( QEventLoop * ) pObj );
   HB_TRACE( HB_TR_DEBUG, ( "          new_QEventLoop                  %i B %i KB", ( int ) hb_xquery( 1001 ), hbqt_getmemused() ) );
   return( p );
}

HB_FUNC( QT_QEVENTLOOP )
{
   void * pObj = NULL;

   pObj = new QEventLoop( hbqt_par_QObject( 1 ) ) ;

   hb_retptrGC( hbqt_gcAllocate_QEventLoop( pObj ) );
}
/*
 * int exec ( ProcessEventsFlags flags = AllEvents )
 */
HB_FUNC( QT_QEVENTLOOP_EXEC )
{
   hb_retni( hbqt_par_QEventLoop( 1 )->exec( ( HB_ISNUM( 2 ) ? ( QEventLoop::ProcessEventsFlags ) hb_parni( 2 ) : ( QEventLoop::ProcessEventsFlags ) QEventLoop::AllEvents ) ) );
}

/*
 * void exit ( int returnCode = 0 )
 */
HB_FUNC( QT_QEVENTLOOP_EXIT )
{
   hbqt_par_QEventLoop( 1 )->exit( hb_parni( 2 ) );
}

/*
 * bool isRunning () const
 */
HB_FUNC( QT_QEVENTLOOP_ISRUNNING )
{
   hb_retl( hbqt_par_QEventLoop( 1 )->isRunning() );
}

/*
 * bool processEvents ( ProcessEventsFlags flags = AllEvents )
 */
HB_FUNC( QT_QEVENTLOOP_PROCESSEVENTS )
{
   hb_retl( hbqt_par_QEventLoop( 1 )->processEvents( ( HB_ISNUM( 2 ) ? ( QEventLoop::ProcessEventsFlags ) hb_parni( 2 ) : ( QEventLoop::ProcessEventsFlags ) QEventLoop::AllEvents ) ) );
}

/*
 * void processEvents ( ProcessEventsFlags flags, int maxTime )
 */
HB_FUNC( QT_QEVENTLOOP_PROCESSEVENTS_1 )
{
   hbqt_par_QEventLoop( 1 )->processEvents( ( QEventLoop::ProcessEventsFlags ) hb_parni( 2 ), hb_parni( 3 ) );
}

/*
 * void wakeUp ()
 */
HB_FUNC( QT_QEVENTLOOP_WAKEUP )
{
   hbqt_par_QEventLoop( 1 )->wakeUp();
}


/*----------------------------------------------------------------------*/
#endif             /* #if QT_VERSION >= 0x040500 */
/*----------------------------------------------------------------------*/
