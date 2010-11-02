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
 *  enum ProcessEventsFlag { AllEvents, ExcludeUserInputEvents, ExcludeSocketNotifiers, WaitForMoreEvents, DeferredDeletion }
 *  flags ProcessEventsFlags
 */

/*
 *  Constructed[ 6/6 [ 100.00% ] ]
 *
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
   QPointer< QEventLoop > ph;
   bool bNew;
   PHBQT_GC_FUNC func;
   int type;
} HBQT_GC_T_QEventLoop;

HBQT_GC_FUNC( hbqt_gcRelease_QEventLoop )
{
   HBQT_GC_T_QEventLoop * p = ( HBQT_GC_T_QEventLoop * ) Cargo;

   if( p )
   {
      if( p->bNew && p->ph )
      {
         QEventLoop * ph = p->ph;
         const QMetaObject * m = ( ph )->metaObject();
         if( ( QString ) m->className() != ( QString ) "QObject" )
            delete ( p->ph );
      }
      p->ph = NULL;
   }
}

void * hbqt_gcAllocate_QEventLoop( void * pObj, bool bNew )
{
   HBQT_GC_T_QEventLoop * p = ( HBQT_GC_T_QEventLoop * ) hb_gcAllocate( sizeof( HBQT_GC_T_QEventLoop ), hbqt_gcFuncs() );

   new( & p->ph ) QPointer< QEventLoop >( ( QEventLoop * ) pObj );
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QEventLoop;
   p->type = HBQT_TYPE_QEventLoop;

   return p;
}

HB_FUNC( QT_QEVENTLOOP )
{
   QEventLoop * pObj = NULL;

   pObj = new QEventLoop( hbqt_par_QObject( 1 ) ) ;

   hb_retptrGC( hbqt_gcAllocate_QEventLoop( ( void * ) pObj, true ) );
}

/* int exec ( ProcessEventsFlags flags = AllEvents ) */
HB_FUNC( QT_QEVENTLOOP_EXEC )
{
   QEventLoop * p = hbqt_par_QEventLoop( 1 );
   if( p )
      hb_retni( ( p )->exec( ( HB_ISNUM( 2 ) ? ( QEventLoop::ProcessEventsFlags ) hb_parni( 2 ) : ( QEventLoop::ProcessEventsFlags ) QEventLoop::AllEvents ) ) );
}

/* void exit ( int returnCode = 0 ) */
HB_FUNC( QT_QEVENTLOOP_EXIT )
{
   QEventLoop * p = hbqt_par_QEventLoop( 1 );
   if( p )
      ( p )->exit( hb_parni( 2 ) );
}

/* bool isRunning () const */
HB_FUNC( QT_QEVENTLOOP_ISRUNNING )
{
   QEventLoop * p = hbqt_par_QEventLoop( 1 );
   if( p )
      hb_retl( ( p )->isRunning() );
}

/* bool processEvents ( ProcessEventsFlags flags = AllEvents ) */
HB_FUNC( QT_QEVENTLOOP_PROCESSEVENTS )
{
   QEventLoop * p = hbqt_par_QEventLoop( 1 );
   if( p )
      hb_retl( ( p )->processEvents( ( HB_ISNUM( 2 ) ? ( QEventLoop::ProcessEventsFlags ) hb_parni( 2 ) : ( QEventLoop::ProcessEventsFlags ) QEventLoop::AllEvents ) ) );
}

/* void processEvents ( ProcessEventsFlags flags, int maxTime ) */
HB_FUNC( QT_QEVENTLOOP_PROCESSEVENTS_1 )
{
   QEventLoop * p = hbqt_par_QEventLoop( 1 );
   if( p )
      ( p )->processEvents( ( QEventLoop::ProcessEventsFlags ) hb_parni( 2 ), hb_parni( 3 ) );
}

/* void wakeUp () */
HB_FUNC( QT_QEVENTLOOP_WAKEUP )
{
   QEventLoop * p = hbqt_par_QEventLoop( 1 );
   if( p )
      ( p )->wakeUp();
}


#endif /* #if QT_VERSION >= 0x040500 */
