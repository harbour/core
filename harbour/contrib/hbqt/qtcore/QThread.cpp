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

typedef struct
{
   QPointer< QThread > ph;
   bool bNew;
   QT_G_FUNC_PTR func;
   int type;
} QGC_POINTER_QThread;

QT_G_FUNC( hbqt_gcRelease_QThread )
{
   QThread  * ph = NULL ;
   QGC_POINTER_QThread * p = ( QGC_POINTER_QThread * ) Cargo;

   if( p && p->bNew && p->ph )
   {
      ph = p->ph;
      if( ph )
      {
         const QMetaObject * m = ( ph )->metaObject();
         if( ( QString ) m->className() != ( QString ) "QObject" )
         {
            HB_TRACE( HB_TR_DEBUG, ( "ph=%p %p YES_rel_QThread   /.\\   ", (void*) ph, (void*) p->ph ) );
            delete ( p->ph );
            HB_TRACE( HB_TR_DEBUG, ( "ph=%p %p YES_rel_QThread   \\./   ", (void*) ph, (void*) p->ph ) );
            p->ph = NULL;
         }
         else
         {
            HB_TRACE( HB_TR_DEBUG, ( "ph=%p NO__rel_QThread          ", ph ) );
            p->ph = NULL;
         }
      }
      else
      {
         HB_TRACE( HB_TR_DEBUG, ( "ph=%p DEL_rel_QThread    :     Object already deleted!", ph ) );
         p->ph = NULL;
      }
   }
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p PTR_rel_QThread    :    Object not created with new=true", ph ) );
      p->ph = NULL;
   }
}

void * hbqt_gcAllocate_QThread( void * pObj, bool bNew )
{
   QGC_POINTER_QThread * p = ( QGC_POINTER_QThread * ) hb_gcAllocate( sizeof( QGC_POINTER_QThread ), hbqt_gcFuncs() );

   new( & p->ph ) QPointer< QThread >( ( QThread * ) pObj );
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QThread;
   p->type = QT_TYPE_QThread;

   if( bNew )
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p    _new_QThread  under p->pq", pObj ) );
   }
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p NOT_new_QThread", pObj ) );
   }
   return p;
}

HB_FUNC( QT_QTHREAD )
{
   QThread * pObj = NULL;

   pObj = new QThread() ;

   hb_retptrGC( hbqt_gcAllocate_QThread( ( void * ) pObj, true ) );
}

/*
 * void exit ( int returnCode = 0 )
 */
HB_FUNC( QT_QTHREAD_EXIT )
{
   QThread * p = hbqt_par_QThread( 1 );
   if( p )
      ( p )->exit( hb_parni( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTHREAD_EXIT FP=( p )->exit( hb_parni( 2 ) ); p is NULL" ) );
   }
}

/*
 * bool isFinished () const
 */
HB_FUNC( QT_QTHREAD_ISFINISHED )
{
   QThread * p = hbqt_par_QThread( 1 );
   if( p )
      hb_retl( ( p )->isFinished() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTHREAD_ISFINISHED FP=hb_retl( ( p )->isFinished() ); p is NULL" ) );
   }
}

/*
 * bool isRunning () const
 */
HB_FUNC( QT_QTHREAD_ISRUNNING )
{
   QThread * p = hbqt_par_QThread( 1 );
   if( p )
      hb_retl( ( p )->isRunning() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTHREAD_ISRUNNING FP=hb_retl( ( p )->isRunning() ); p is NULL" ) );
   }
}

/*
 * Priority priority () const
 */
HB_FUNC( QT_QTHREAD_PRIORITY )
{
   QThread * p = hbqt_par_QThread( 1 );
   if( p )
      hb_retni( ( QThread::Priority ) ( p )->priority() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTHREAD_PRIORITY FP=hb_retni( ( QThread::Priority ) ( p )->priority() ); p is NULL" ) );
   }
}

/*
 * void setPriority ( Priority priority )
 */
HB_FUNC( QT_QTHREAD_SETPRIORITY )
{
   QThread * p = hbqt_par_QThread( 1 );
   if( p )
      ( p )->setPriority( ( QThread::Priority ) hb_parni( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTHREAD_SETPRIORITY FP=( p )->setPriority( ( QThread::Priority ) hb_parni( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setStackSize ( uint stackSize )
 */
HB_FUNC( QT_QTHREAD_SETSTACKSIZE )
{
   QThread * p = hbqt_par_QThread( 1 );
   if( p )
      ( p )->setStackSize( hb_parni( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTHREAD_SETSTACKSIZE FP=( p )->setStackSize( hb_parni( 2 ) ); p is NULL" ) );
   }
}

/*
 * uint stackSize () const
 */
HB_FUNC( QT_QTHREAD_STACKSIZE )
{
   QThread * p = hbqt_par_QThread( 1 );
   if( p )
      hb_retni( ( p )->stackSize() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTHREAD_STACKSIZE FP=hb_retni( ( p )->stackSize() ); p is NULL" ) );
   }
}

/*
 * bool wait ( ulong time = ULONG_MAX )
 */
HB_FUNC( QT_QTHREAD_WAIT )
{
   QThread * p = hbqt_par_QThread( 1 );
   if( p )
      hb_retl( ( p )->wait( ( ulong ) hb_parnintdef( 2, ULONG_MAX ) ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTHREAD_WAIT FP=hb_retl( ( p )->wait( ( ulong ) hb_parnintdef( 2, ULONG_MAX ) ) ); p is NULL" ) );
   }
}

/*
 * QThread * currentThread ()
 */
HB_FUNC( QT_QTHREAD_CURRENTTHREAD )
{
   QThread * p = hbqt_par_QThread( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QThread( ( p )->currentThread(), false ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTHREAD_CURRENTTHREAD FP=hb_retptrGC( hbqt_gcAllocate_QThread( ( p )->currentThread(), false ) ); p is NULL" ) );
   }
}

/*
 * int idealThreadCount ()
 */
HB_FUNC( QT_QTHREAD_IDEALTHREADCOUNT )
{
   QThread * p = hbqt_par_QThread( 1 );
   if( p )
      hb_retni( ( p )->idealThreadCount() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTHREAD_IDEALTHREADCOUNT FP=hb_retni( ( p )->idealThreadCount() ); p is NULL" ) );
   }
}

/*
 * void yieldCurrentThread ()
 */
HB_FUNC( QT_QTHREAD_YIELDCURRENTTHREAD )
{
   QThread * p = hbqt_par_QThread( 1 );
   if( p )
      ( p )->yieldCurrentThread();
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTHREAD_YIELDCURRENTTHREAD FP=( p )->yieldCurrentThread(); p is NULL" ) );
   }
}

/*
 * void quit ()
 */
HB_FUNC( QT_QTHREAD_QUIT )
{
   QThread * p = hbqt_par_QThread( 1 );
   if( p )
      ( p )->quit();
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTHREAD_QUIT FP=( p )->quit(); p is NULL" ) );
   }
}

/*
 * void start ( Priority priority = InheritPriority )
 */
HB_FUNC( QT_QTHREAD_START )
{
   QThread * p = hbqt_par_QThread( 1 );
   if( p )
      ( p )->start( ( HB_ISNUM( 2 ) ? ( QThread::Priority ) hb_parni( 2 ) : ( QThread::Priority ) QThread::InheritPriority ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTHREAD_START FP=( p )->start( ( HB_ISNUM( 2 ) ? ( QThread::Priority ) hb_parni( 2 ) : ( QThread::Priority ) QThread::InheritPriority ) ); p is NULL" ) );
   }
}

/*
 * void terminate ()
 */
HB_FUNC( QT_QTHREAD_TERMINATE )
{
   QThread * p = hbqt_par_QThread( 1 );
   if( p )
      ( p )->terminate();
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTHREAD_TERMINATE FP=( p )->terminate(); p is NULL" ) );
   }
}


/*----------------------------------------------------------------------*/
#endif             /* #if QT_VERSION >= 0x040500 */
/*----------------------------------------------------------------------*/
