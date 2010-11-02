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
 *  enum CurveShape { EaseInCurve, EaseOutCurve, EaseInOutCurve, LinearCurve, SineCurve, CosineCurve }
 *  enum Direction { Forward, Backward }
 *  enum State { NotRunning, Paused, Running }
 */

/*
 *  Constructed[ 27/27 [ 100.00% ] ]
 *
 */

#include <QtCore/QPointer>

#include <QtCore/QTimeLine>


/*
 * QTimeLine ( int duration = 1000, QObject * parent = 0 )
 * virtual ~QTimeLine ()
 */

typedef struct
{
   QPointer< QTimeLine > ph;
   bool bNew;
   PHBQT_GC_FUNC func;
   int type;
} HBQT_GC_T_QTimeLine;

HBQT_GC_FUNC( hbqt_gcRelease_QTimeLine )
{
   HBQT_GC_T_QTimeLine * p = ( HBQT_GC_T_QTimeLine * ) Cargo;

   if( p )
   {
      if( p->bNew && p->ph )
      {
         QTimeLine * ph = p->ph;
         const QMetaObject * m = ( ph )->metaObject();
         if( ( QString ) m->className() != ( QString ) "QObject" )
            delete ( p->ph );
      }
      p->ph = NULL;
   }
}

void * hbqt_gcAllocate_QTimeLine( void * pObj, bool bNew )
{
   HBQT_GC_T_QTimeLine * p = ( HBQT_GC_T_QTimeLine * ) hb_gcAllocate( sizeof( HBQT_GC_T_QTimeLine ), hbqt_gcFuncs() );

   new( & p->ph ) QPointer< QTimeLine >( ( QTimeLine * ) pObj );
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QTimeLine;
   p->type = HBQT_TYPE_QTimeLine;

   return p;
}

HB_FUNC( QT_QTIMELINE )
{
   QTimeLine * pObj = NULL;

   pObj = new QTimeLine() ;

   hb_retptrGC( hbqt_gcAllocate_QTimeLine( ( void * ) pObj, true ) );
}

/* int currentFrame () const */
HB_FUNC( QT_QTIMELINE_CURRENTFRAME )
{
   QTimeLine * p = hbqt_par_QTimeLine( 1 );
   if( p )
      hb_retni( ( p )->currentFrame() );
}

/* int currentTime () const */
HB_FUNC( QT_QTIMELINE_CURRENTTIME )
{
   QTimeLine * p = hbqt_par_QTimeLine( 1 );
   if( p )
      hb_retni( ( p )->currentTime() );
}

/* qreal currentValue () const */
HB_FUNC( QT_QTIMELINE_CURRENTVALUE )
{
   QTimeLine * p = hbqt_par_QTimeLine( 1 );
   if( p )
      hb_retnd( ( p )->currentValue() );
}

/* CurveShape curveShape () const */
HB_FUNC( QT_QTIMELINE_CURVESHAPE )
{
   QTimeLine * p = hbqt_par_QTimeLine( 1 );
   if( p )
      hb_retni( ( QTimeLine::CurveShape ) ( p )->curveShape() );
}

/* Direction direction () const */
HB_FUNC( QT_QTIMELINE_DIRECTION )
{
   QTimeLine * p = hbqt_par_QTimeLine( 1 );
   if( p )
      hb_retni( ( QTimeLine::Direction ) ( p )->direction() );
}

/* int duration () const */
HB_FUNC( QT_QTIMELINE_DURATION )
{
   QTimeLine * p = hbqt_par_QTimeLine( 1 );
   if( p )
      hb_retni( ( p )->duration() );
}

/* int endFrame () const */
HB_FUNC( QT_QTIMELINE_ENDFRAME )
{
   QTimeLine * p = hbqt_par_QTimeLine( 1 );
   if( p )
      hb_retni( ( p )->endFrame() );
}

/* int frameForTime ( int msec ) const */
HB_FUNC( QT_QTIMELINE_FRAMEFORTIME )
{
   QTimeLine * p = hbqt_par_QTimeLine( 1 );
   if( p )
      hb_retni( ( p )->frameForTime( hb_parni( 2 ) ) );
}

/* int loopCount () const */
HB_FUNC( QT_QTIMELINE_LOOPCOUNT )
{
   QTimeLine * p = hbqt_par_QTimeLine( 1 );
   if( p )
      hb_retni( ( p )->loopCount() );
}

/* void setCurveShape ( CurveShape shape ) */
HB_FUNC( QT_QTIMELINE_SETCURVESHAPE )
{
   QTimeLine * p = hbqt_par_QTimeLine( 1 );
   if( p )
      ( p )->setCurveShape( ( QTimeLine::CurveShape ) hb_parni( 2 ) );
}

/* void setDirection ( Direction direction ) */
HB_FUNC( QT_QTIMELINE_SETDIRECTION )
{
   QTimeLine * p = hbqt_par_QTimeLine( 1 );
   if( p )
      ( p )->setDirection( ( QTimeLine::Direction ) hb_parni( 2 ) );
}

/* void setDuration ( int duration ) */
HB_FUNC( QT_QTIMELINE_SETDURATION )
{
   QTimeLine * p = hbqt_par_QTimeLine( 1 );
   if( p )
      ( p )->setDuration( hb_parni( 2 ) );
}

/* void setEndFrame ( int frame ) */
HB_FUNC( QT_QTIMELINE_SETENDFRAME )
{
   QTimeLine * p = hbqt_par_QTimeLine( 1 );
   if( p )
      ( p )->setEndFrame( hb_parni( 2 ) );
}

/* void setFrameRange ( int startFrame, int endFrame ) */
HB_FUNC( QT_QTIMELINE_SETFRAMERANGE )
{
   QTimeLine * p = hbqt_par_QTimeLine( 1 );
   if( p )
      ( p )->setFrameRange( hb_parni( 2 ), hb_parni( 3 ) );
}

/* void setLoopCount ( int count ) */
HB_FUNC( QT_QTIMELINE_SETLOOPCOUNT )
{
   QTimeLine * p = hbqt_par_QTimeLine( 1 );
   if( p )
      ( p )->setLoopCount( hb_parni( 2 ) );
}

/* void setStartFrame ( int frame ) */
HB_FUNC( QT_QTIMELINE_SETSTARTFRAME )
{
   QTimeLine * p = hbqt_par_QTimeLine( 1 );
   if( p )
      ( p )->setStartFrame( hb_parni( 2 ) );
}

/* void setUpdateInterval ( int interval ) */
HB_FUNC( QT_QTIMELINE_SETUPDATEINTERVAL )
{
   QTimeLine * p = hbqt_par_QTimeLine( 1 );
   if( p )
      ( p )->setUpdateInterval( hb_parni( 2 ) );
}

/* int startFrame () const */
HB_FUNC( QT_QTIMELINE_STARTFRAME )
{
   QTimeLine * p = hbqt_par_QTimeLine( 1 );
   if( p )
      hb_retni( ( p )->startFrame() );
}

/* State state () const */
HB_FUNC( QT_QTIMELINE_STATE )
{
   QTimeLine * p = hbqt_par_QTimeLine( 1 );
   if( p )
      hb_retni( ( QTimeLine::State ) ( p )->state() );
}

/* int updateInterval () const */
HB_FUNC( QT_QTIMELINE_UPDATEINTERVAL )
{
   QTimeLine * p = hbqt_par_QTimeLine( 1 );
   if( p )
      hb_retni( ( p )->updateInterval() );
}

/* virtual qreal valueForTime ( int msec ) const */
HB_FUNC( QT_QTIMELINE_VALUEFORTIME )
{
   QTimeLine * p = hbqt_par_QTimeLine( 1 );
   if( p )
      hb_retnd( ( p )->valueForTime( hb_parni( 2 ) ) );
}

/* void resume () */
HB_FUNC( QT_QTIMELINE_RESUME )
{
   QTimeLine * p = hbqt_par_QTimeLine( 1 );
   if( p )
      ( p )->resume();
}

/* void setCurrentTime ( int msec ) */
HB_FUNC( QT_QTIMELINE_SETCURRENTTIME )
{
   QTimeLine * p = hbqt_par_QTimeLine( 1 );
   if( p )
      ( p )->setCurrentTime( hb_parni( 2 ) );
}

/* void setPaused ( bool paused ) */
HB_FUNC( QT_QTIMELINE_SETPAUSED )
{
   QTimeLine * p = hbqt_par_QTimeLine( 1 );
   if( p )
      ( p )->setPaused( hb_parl( 2 ) );
}

/* void start () */
HB_FUNC( QT_QTIMELINE_START )
{
   QTimeLine * p = hbqt_par_QTimeLine( 1 );
   if( p )
      ( p )->start();
}

/* void stop () */
HB_FUNC( QT_QTIMELINE_STOP )
{
   QTimeLine * p = hbqt_par_QTimeLine( 1 );
   if( p )
      ( p )->stop();
}

/* void toggleDirection () */
HB_FUNC( QT_QTIMELINE_TOGGLEDIRECTION )
{
   QTimeLine * p = hbqt_par_QTimeLine( 1 );
   if( p )
      ( p )->toggleDirection();
}


#endif /* #if QT_VERSION >= 0x040500 */
