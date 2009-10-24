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
 *  enum Priority { IdlePriority, LowestPriority, LowPriority, NormalPriority, ..., InheritPriority }
 */

#include <QtCore/QPointer>

#include <QtCore/QThread>


/* QThread ( QObject * parent = 0 )
 * ~QThread ()
 */

QT_G_FUNC( release_QThread )
{
#if defined(__debug__)
   hb_snprintf( str, sizeof(str), "release_QThread" );  OutputDebugString( str );
#endif
   void * ph = ( void * ) Cargo;
   if( ph )
   {
      const QMetaObject * m = ( ( QObject * ) ph )->metaObject();
      if( ( QString ) m->className() != ( QString ) "QObject" )
      {
         delete ( ( QThread * ) ph );
         ph = NULL;
      }
      else
      {
#if defined(__debug__)
   hb_snprintf( str, sizeof(str), "  Object Name Missing: QThread" );  OutputDebugString( str );
#endif
      }
   }
}

HB_FUNC( QT_QTHREAD )
{
   QGC_POINTER * p = ( QGC_POINTER * ) hb_gcAllocate( sizeof( QGC_POINTER ), gcFuncs() );
   QPointer< QThread > pObj = NULL;

   pObj = new QThread() ;

   p->ph = pObj;
   p->func = release_QThread;

   hb_retptrGC( p );
}
/*
 * void exit ( int returnCode = 0 )
 */
HB_FUNC( QT_QTHREAD_EXIT )
{
   hbqt_par_QThread( 1 )->exit( hb_parni( 2 ) );
}

/*
 * bool isFinished () const
 */
HB_FUNC( QT_QTHREAD_ISFINISHED )
{
   hb_retl( hbqt_par_QThread( 1 )->isFinished() );
}

/*
 * bool isRunning () const
 */
HB_FUNC( QT_QTHREAD_ISRUNNING )
{
   hb_retl( hbqt_par_QThread( 1 )->isRunning() );
}

/*
 * Priority priority () const
 */
HB_FUNC( QT_QTHREAD_PRIORITY )
{
   hb_retni( ( QThread::Priority ) hbqt_par_QThread( 1 )->priority() );
}

/*
 * void setPriority ( Priority priority )
 */
HB_FUNC( QT_QTHREAD_SETPRIORITY )
{
   hbqt_par_QThread( 1 )->setPriority( ( QThread::Priority ) hb_parni( 2 ) );
}

/*
 * void setStackSize ( uint stackSize )
 */
HB_FUNC( QT_QTHREAD_SETSTACKSIZE )
{
   hbqt_par_QThread( 1 )->setStackSize( hb_parni( 2 ) );
}

/*
 * uint stackSize () const
 */
HB_FUNC( QT_QTHREAD_STACKSIZE )
{
   hb_retni( hbqt_par_QThread( 1 )->stackSize() );
}

/*
 * bool wait ( unsigned long time = ULONG_MAX )
 */
HB_FUNC( QT_QTHREAD_WAIT )
{
   hb_retl( hbqt_par_QThread( 1 )->wait() );
}

/*
 * QThread * currentThread ()
 */
HB_FUNC( QT_QTHREAD_CURRENTTHREAD )
{
   hb_retptr( ( QThread* ) hbqt_par_QThread( 1 )->currentThread() );
}

/*
 * int idealThreadCount ()
 */
HB_FUNC( QT_QTHREAD_IDEALTHREADCOUNT )
{
   hb_retni( hbqt_par_QThread( 1 )->idealThreadCount() );
}

/*
 * void yieldCurrentThread ()
 */
HB_FUNC( QT_QTHREAD_YIELDCURRENTTHREAD )
{
   hbqt_par_QThread( 1 )->yieldCurrentThread();
}

/*
 * void quit ()
 */
HB_FUNC( QT_QTHREAD_QUIT )
{
   hbqt_par_QThread( 1 )->quit();
}

/*
 * void start ( Priority priority = InheritPriority )
 */
HB_FUNC( QT_QTHREAD_START )
{
   hbqt_par_QThread( 1 )->start( ( HB_ISNUM( 2 ) ? ( QThread::Priority ) hb_parni( 2 ) : ( QThread::Priority ) QThread::InheritPriority ) );
}

/*
 * void terminate ()
 */
HB_FUNC( QT_QTHREAD_TERMINATE )
{
   hbqt_par_QThread( 1 )->terminate();
}


/*----------------------------------------------------------------------*/
#endif             /* #if QT_VERSION >= 0x040500 */
/*----------------------------------------------------------------------*/
