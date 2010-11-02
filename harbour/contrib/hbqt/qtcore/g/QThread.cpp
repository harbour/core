/*
 * $Id$
 */

/* -------------------------------------------------------------------- */
/* WARNING: Automatically generated source file. DO NOT EDIT!           */
/*          Instead, edit corresponding .qth file,                      */
/*          or the generator tool itself, and run regenarate.           */
/* -------------------------------------------------------------------- */

/*
 * Harbour Project QT wrapper
 *
 * Copyright 2009-2010 Pritpal Bedi <bedipritpal@hotmail.com>
 * www - http://harbour-project.org
 *
 * For full copyright message and credits, see: CREDITS.txt
 *
 */

#include "hbqtcore.h"

#if QT_VERSION >= 0x040500

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
   QThread  * ph = NULL;
   HBQT_GC_T_QThread * p = ( HBQT_GC_T_QThread * ) Cargo;

   if( p && p->bNew && p->ph )
   {
      ph = p->ph;
      if( ph )
      {
         const QMetaObject * m = ( ph )->metaObject();
         if( ( QString ) m->className() != ( QString ) "QObject" )
         {
            delete ( p->ph );
            p->ph = NULL;
         }
         else
            p->ph = NULL;
      }
      else
         p->ph = NULL;
   }
   else
      p->ph = NULL;
}

void * hbqt_gcAllocate_QThread( void * pObj, bool bNew )
{
   HBQT_GC_T_QThread * p = ( HBQT_GC_T_QThread * ) hb_gcAllocate( sizeof( HBQT_GC_T_QThread ), hbqt_gcFuncs() );

   new( & p->ph ) QPointer< QThread >( ( QThread * ) pObj );
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QThread;
   p->type = HBQT_TYPE_QThread;

   return p;
}

HB_FUNC( QT_QTHREAD )
{
   QThread * pObj = NULL;

   pObj = new QThread() ;

   hb_retptrGC( hbqt_gcAllocate_QThread( ( void * ) pObj, true ) );
}

/* void exit ( int returnCode = 0 ) */
HB_FUNC( QT_QTHREAD_EXIT )
{
   QThread * p = hbqt_par_QThread( 1 );
   if( p )
      ( p )->exit( hb_parni( 2 ) );
}

/* bool isFinished () const */
HB_FUNC( QT_QTHREAD_ISFINISHED )
{
   QThread * p = hbqt_par_QThread( 1 );
   if( p )
      hb_retl( ( p )->isFinished() );
}

/* bool isRunning () const */
HB_FUNC( QT_QTHREAD_ISRUNNING )
{
   QThread * p = hbqt_par_QThread( 1 );
   if( p )
      hb_retl( ( p )->isRunning() );
}

/* Priority priority () const */
HB_FUNC( QT_QTHREAD_PRIORITY )
{
   QThread * p = hbqt_par_QThread( 1 );
   if( p )
      hb_retni( ( QThread::Priority ) ( p )->priority() );
}

/* void setPriority ( Priority priority ) */
HB_FUNC( QT_QTHREAD_SETPRIORITY )
{
   QThread * p = hbqt_par_QThread( 1 );
   if( p )
      ( p )->setPriority( ( QThread::Priority ) hb_parni( 2 ) );
}

/* void setStackSize ( uint stackSize ) */
HB_FUNC( QT_QTHREAD_SETSTACKSIZE )
{
   QThread * p = hbqt_par_QThread( 1 );
   if( p )
      ( p )->setStackSize( hb_parni( 2 ) );
}

/* uint stackSize () const */
HB_FUNC( QT_QTHREAD_STACKSIZE )
{
   QThread * p = hbqt_par_QThread( 1 );
   if( p )
      hb_retni( ( p )->stackSize() );
}

/* bool wait ( ulong time = ULONG_MAX ) */
HB_FUNC( QT_QTHREAD_WAIT )
{
   QThread * p = hbqt_par_QThread( 1 );
   if( p )
      hb_retl( ( p )->wait( ( ulong ) hb_parnintdef( 2, ULONG_MAX ) ) );
}

/* QThread * currentThread () */
HB_FUNC( QT_QTHREAD_CURRENTTHREAD )
{
   QThread * p = hbqt_par_QThread( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QThread( ( p )->currentThread(), false ) );
}

/* int idealThreadCount () */
HB_FUNC( QT_QTHREAD_IDEALTHREADCOUNT )
{
   QThread * p = hbqt_par_QThread( 1 );
   if( p )
      hb_retni( ( p )->idealThreadCount() );
}

/* void yieldCurrentThread () */
HB_FUNC( QT_QTHREAD_YIELDCURRENTTHREAD )
{
   QThread * p = hbqt_par_QThread( 1 );
   if( p )
      ( p )->yieldCurrentThread();
}

/* void quit () */
HB_FUNC( QT_QTHREAD_QUIT )
{
   QThread * p = hbqt_par_QThread( 1 );
   if( p )
      ( p )->quit();
}

/* void start ( Priority priority = InheritPriority ) */
HB_FUNC( QT_QTHREAD_START )
{
   QThread * p = hbqt_par_QThread( 1 );
   if( p )
      ( p )->start( ( HB_ISNUM( 2 ) ? ( QThread::Priority ) hb_parni( 2 ) : ( QThread::Priority ) QThread::InheritPriority ) );
}

/* void terminate () */
HB_FUNC( QT_QTHREAD_TERMINATE )
{
   QThread * p = hbqt_par_QThread( 1 );
   if( p )
      ( p )->terminate();
}


#endif /* #if QT_VERSION >= 0x040500 */
