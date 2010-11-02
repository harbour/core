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
#include "hbqtgui.h"

#if QT_VERSION >= 0x040500

/*
 *  enum TickPosition { NoTicks, TicksBothSides, TicksAbove, TicksBelow, TicksLeft, TicksRight }
 */

/*
 *  Constructed[ 4/4 [ 100.00% ] ]
 *
 */

#include <QtCore/QPointer>

#include <QtGui/QSlider>


/*
 * QSlider ( QWidget * parent = 0 )
 * QSlider ( Qt::Orientation orientation, QWidget * parent = 0 )
 * ~QSlider ()
 */

typedef struct
{
   QPointer< QSlider > ph;
   bool bNew;
   PHBQT_GC_FUNC func;
   int type;
} HBQT_GC_T_QSlider;

HBQT_GC_FUNC( hbqt_gcRelease_QSlider )
{
   HBQT_GC_T_QSlider * p = ( HBQT_GC_T_QSlider * ) Cargo;

   if( p )
   {
      if( p->bNew && p->ph )
      {
         QSlider * ph = p->ph;
         const QMetaObject * m = ( ph )->metaObject();
         if( ( QString ) m->className() != ( QString ) "QObject" )
            delete ( p->ph );
      }
      p->ph = NULL;
   }
}

void * hbqt_gcAllocate_QSlider( void * pObj, bool bNew )
{
   HBQT_GC_T_QSlider * p = ( HBQT_GC_T_QSlider * ) hb_gcAllocate( sizeof( HBQT_GC_T_QSlider ), hbqt_gcFuncs() );

   new( & p->ph ) QPointer< QSlider >( ( QSlider * ) pObj );
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QSlider;
   p->type = HBQT_TYPE_QSlider;

   return p;
}

HB_FUNC( QT_QSLIDER )
{
   QSlider * pObj = NULL;

   if( hb_pcount() >= 1 && HB_ISNUM( 1 ) )
      pObj = new QSlider( ( Qt::Orientation ) hb_parni( 1 ), hbqt_par_QWidget( 2 ) ) ;
   else
      pObj = new QSlider( hbqt_par_QWidget( 1 ) ) ;

   hb_retptrGC( hbqt_gcAllocate_QSlider( ( void * ) pObj, true ) );
}

/* void setTickInterval ( int ti ) */
HB_FUNC( QT_QSLIDER_SETTICKINTERVAL )
{
   QSlider * p = hbqt_par_QSlider( 1 );
   if( p )
      ( p )->setTickInterval( hb_parni( 2 ) );
}

/* void setTickPosition ( TickPosition position ) */
HB_FUNC( QT_QSLIDER_SETTICKPOSITION )
{
   QSlider * p = hbqt_par_QSlider( 1 );
   if( p )
      ( p )->setTickPosition( ( QSlider::TickPosition ) hb_parni( 2 ) );
}

/* int tickInterval () const */
HB_FUNC( QT_QSLIDER_TICKINTERVAL )
{
   QSlider * p = hbqt_par_QSlider( 1 );
   if( p )
      hb_retni( ( p )->tickInterval() );
}

/* TickPosition tickPosition () const */
HB_FUNC( QT_QSLIDER_TICKPOSITION )
{
   QSlider * p = hbqt_par_QSlider( 1 );
   if( p )
      hb_retni( ( QSlider::TickPosition ) ( p )->tickPosition() );
}


#endif /* #if QT_VERSION >= 0x040500 */
