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
 * Copyright 2009-2010 Pritpal Bedi <bedipritpal@hotmail.com>
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
/*                            C R E D I T S                             */
/*----------------------------------------------------------------------*/
/*
 * Marcos Antonio Gambeta
 *    for providing first ever prototype parsing methods. Though the current
 *    implementation is diametrically different then what he proposed, still
 *    current code shaped on those footsteps.
 *
 * Viktor Szakats
 *    for directing the project with futuristic vision;
 *    for designing and maintaining a complex build system for hbQT, hbIDE;
 *    for introducing many constructs on PRG and C++ levels;
 *    for streamlining signal/slots and events management classes;
 *
 * Istvan Bisz
 *    for introducing QPointer<> concept in the generator;
 *    for testing the library on numerous accounts;
 *    for showing a way how a GC pointer can be detached;
 *
 * Francesco Perillo
 *    for taking keen interest in hbQT development and peeking the code;
 *    for providing tips here and there to improve the code quality;
 *    for hitting bulls eye to describe why few objects need GC detachment;
 *
 * Carlos Bacco
 *    for implementing HBQT_TYPE_Q*Class enums;
 *    for peeking into the code and suggesting optimization points;
 *
 * Przemyslaw Czerpak
 *    for providing tips and trick to manipulate HVM internals to the best
 *    of its use and always showing a path when we get stuck;
 *    A true tradition of a MASTER...
*/
/*----------------------------------------------------------------------*/

#include "hbqtcore.h"

/*----------------------------------------------------------------------*/
#if QT_VERSION >= 0x040500
/*----------------------------------------------------------------------*/

/*
 *  enum Priority { IdlePriority, LowestPriority, LowPriority, NormalPriority, ..., InheritPriority }
 */

/*
 *  Constructed[ 14/14 [ 100.00% ] ]
 *
 *
 *  *** Commented out protostypes ***
 *
 *  // Qt::HANDLE currentThreadId ()
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
   PHBQT_GC_FUNC func;
   int type;
} HBQT_GC_T_QThread;

HBQT_GC_FUNC( hbqt_gcRelease_QThread )
{
   QThread  * ph = NULL ;
   HBQT_GC_T_QThread * p = ( HBQT_GC_T_QThread * ) Cargo;

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
   HBQT_GC_T_QThread * p = ( HBQT_GC_T_QThread * ) hb_gcAllocate( sizeof( HBQT_GC_T_QThread ), hbqt_gcFuncs() );

   new( & p->ph ) QPointer< QThread >( ( QThread * ) pObj );
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QThread;
   p->type = HBQT_TYPE_QThread;

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
   {
      ( p )->exit( hb_parni( 2 ) );
   }
}

/*
 * bool isFinished () const
 */
HB_FUNC( QT_QTHREAD_ISFINISHED )
{
   QThread * p = hbqt_par_QThread( 1 );
   if( p )
   {
      hb_retl( ( p )->isFinished() );
   }
}

/*
 * bool isRunning () const
 */
HB_FUNC( QT_QTHREAD_ISRUNNING )
{
   QThread * p = hbqt_par_QThread( 1 );
   if( p )
   {
      hb_retl( ( p )->isRunning() );
   }
}

/*
 * Priority priority () const
 */
HB_FUNC( QT_QTHREAD_PRIORITY )
{
   QThread * p = hbqt_par_QThread( 1 );
   if( p )
   {
      hb_retni( ( QThread::Priority ) ( p )->priority() );
   }
}

/*
 * void setPriority ( Priority priority )
 */
HB_FUNC( QT_QTHREAD_SETPRIORITY )
{
   QThread * p = hbqt_par_QThread( 1 );
   if( p )
   {
      ( p )->setPriority( ( QThread::Priority ) hb_parni( 2 ) );
   }
}

/*
 * void setStackSize ( uint stackSize )
 */
HB_FUNC( QT_QTHREAD_SETSTACKSIZE )
{
   QThread * p = hbqt_par_QThread( 1 );
   if( p )
   {
      ( p )->setStackSize( hb_parni( 2 ) );
   }
}

/*
 * uint stackSize () const
 */
HB_FUNC( QT_QTHREAD_STACKSIZE )
{
   QThread * p = hbqt_par_QThread( 1 );
   if( p )
   {
      hb_retni( ( p )->stackSize() );
   }
}

/*
 * bool wait ( ulong time = ULONG_MAX )
 */
HB_FUNC( QT_QTHREAD_WAIT )
{
   QThread * p = hbqt_par_QThread( 1 );
   if( p )
   {
      hb_retl( ( p )->wait( ( ulong ) hb_parnintdef( 2, ULONG_MAX ) ) );
   }
}

/*
 * QThread * currentThread ()
 */
HB_FUNC( QT_QTHREAD_CURRENTTHREAD )
{
   QThread * p = hbqt_par_QThread( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QThread( ( p )->currentThread(), false ) );
   }
}

/*
 * int idealThreadCount ()
 */
HB_FUNC( QT_QTHREAD_IDEALTHREADCOUNT )
{
   QThread * p = hbqt_par_QThread( 1 );
   if( p )
   {
      hb_retni( ( p )->idealThreadCount() );
   }
}

/*
 * void yieldCurrentThread ()
 */
HB_FUNC( QT_QTHREAD_YIELDCURRENTTHREAD )
{
   QThread * p = hbqt_par_QThread( 1 );
   if( p )
   {
      ( p )->yieldCurrentThread();
   }
}

/*
 * void quit ()
 */
HB_FUNC( QT_QTHREAD_QUIT )
{
   QThread * p = hbqt_par_QThread( 1 );
   if( p )
   {
      ( p )->quit();
   }
}

/*
 * void start ( Priority priority = InheritPriority )
 */
HB_FUNC( QT_QTHREAD_START )
{
   QThread * p = hbqt_par_QThread( 1 );
   if( p )
   {
      ( p )->start( ( HB_ISNUM( 2 ) ? ( QThread::Priority ) hb_parni( 2 ) : ( QThread::Priority ) QThread::InheritPriority ) );
   }
}

/*
 * void terminate ()
 */
HB_FUNC( QT_QTHREAD_TERMINATE )
{
   QThread * p = hbqt_par_QThread( 1 );
   if( p )
   {
      ( p )->terminate();
   }
}


/*----------------------------------------------------------------------*/
#endif             /* #if QT_VERSION >= 0x040500 */
/*----------------------------------------------------------------------*/
