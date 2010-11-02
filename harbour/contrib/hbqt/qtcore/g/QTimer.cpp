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
 *  Constructed[ 10/10 [ 100.00% ] ]
 *
 */

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
   HBQT_GC_T_QTimer * p = ( HBQT_GC_T_QTimer * ) Cargo;

   if( p && p->bNew && p->ph )
   {
      QTimer * ph = p->ph;
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

void * hbqt_gcAllocate_QTimer( void * pObj, bool bNew )
{
   HBQT_GC_T_QTimer * p = ( HBQT_GC_T_QTimer * ) hb_gcAllocate( sizeof( HBQT_GC_T_QTimer ), hbqt_gcFuncs() );

   new( & p->ph ) QPointer< QTimer >( ( QTimer * ) pObj );
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QTimer;
   p->type = HBQT_TYPE_QTimer;

   return p;
}

HB_FUNC( QT_QTIMER )
{
   QTimer * pObj = NULL;

   pObj = new QTimer( hbqt_par_QObject( 1 ) ) ;

   hb_retptrGC( hbqt_gcAllocate_QTimer( ( void * ) pObj, true ) );
}

/* int interval () const */
HB_FUNC( QT_QTIMER_INTERVAL )
{
   QTimer * p = hbqt_par_QTimer( 1 );
   if( p )
      hb_retni( ( p )->interval() );
}

/* bool isActive () const */
HB_FUNC( QT_QTIMER_ISACTIVE )
{
   QTimer * p = hbqt_par_QTimer( 1 );
   if( p )
      hb_retl( ( p )->isActive() );
}

/* bool isSingleShot () const */
HB_FUNC( QT_QTIMER_ISSINGLESHOT )
{
   QTimer * p = hbqt_par_QTimer( 1 );
   if( p )
      hb_retl( ( p )->isSingleShot() );
}

/* void setInterval ( int msec ) */
HB_FUNC( QT_QTIMER_SETINTERVAL )
{
   QTimer * p = hbqt_par_QTimer( 1 );
   if( p )
      ( p )->setInterval( hb_parni( 2 ) );
}

/* void setSingleShot ( bool singleShot ) */
HB_FUNC( QT_QTIMER_SETSINGLESHOT )
{
   QTimer * p = hbqt_par_QTimer( 1 );
   if( p )
      ( p )->setSingleShot( hb_parl( 2 ) );
}

/* int timerId () const */
HB_FUNC( QT_QTIMER_TIMERID )
{
   QTimer * p = hbqt_par_QTimer( 1 );
   if( p )
      hb_retni( ( p )->timerId() );
}

/* void singleShot ( int msec, QObject * receiver, const char * member ) */
HB_FUNC( QT_QTIMER_SINGLESHOT )
{
   QTimer * p = hbqt_par_QTimer( 1 );
   if( p )
      ( p )->singleShot( hb_parni( 2 ), hbqt_par_QObject( 3 ), ( const char * ) hb_parc( 4 ) );
}

/* void start () */
HB_FUNC( QT_QTIMER_START )
{
   QTimer * p = hbqt_par_QTimer( 1 );
   if( p )
      ( p )->start();
}

/* void stop () */
HB_FUNC( QT_QTIMER_STOP )
{
   QTimer * p = hbqt_par_QTimer( 1 );
   if( p )
      ( p )->stop();
}

/* void start ( int msec ) */
HB_FUNC( QT_QTIMER_START_1 )
{
   QTimer * p = hbqt_par_QTimer( 1 );
   if( p )
      ( p )->start( hb_parni( 2 ) );
}


#endif /* #if QT_VERSION >= 0x040500 */
