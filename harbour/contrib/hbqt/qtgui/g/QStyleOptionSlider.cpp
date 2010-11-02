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
 *  enum StyleOptionType { Type }
 *  enum StyleOptionVersion { Version }
 */

/*
 *  Constructed[ 12/12 [ 100.00% ] ]
 *
 */

#include <QtCore/QPointer>

#include <QtGui/QStyleOptionSlider>


/*
 * QStyleOptionSlider ()
 * QStyleOptionSlider ( const QStyleOptionSlider & other )
 */

typedef struct
{
   QStyleOptionSlider * ph;
   bool bNew;
   PHBQT_GC_FUNC func;
   int type;
} HBQT_GC_T_QStyleOptionSlider;

HBQT_GC_FUNC( hbqt_gcRelease_QStyleOptionSlider )
{
   HBQT_GC_T * p = ( HBQT_GC_T * ) Cargo;

   if( p && p->bNew )
   {
      if( p->ph )
      {
         delete ( ( QStyleOptionSlider * ) p->ph );
         p->ph = NULL;
      }
      else
         p->ph = NULL;
   }
   else
      p->ph = NULL;
}

void * hbqt_gcAllocate_QStyleOptionSlider( void * pObj, bool bNew )
{
   HBQT_GC_T * p = ( HBQT_GC_T * ) hb_gcAllocate( sizeof( HBQT_GC_T ), hbqt_gcFuncs() );

   p->ph = ( QStyleOptionSlider * ) pObj;
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QStyleOptionSlider;
   p->type = HBQT_TYPE_QStyleOptionSlider;

   return p;
}

HB_FUNC( QT_QSTYLEOPTIONSLIDER )
{
   QStyleOptionSlider * pObj = NULL;

   pObj = new QStyleOptionSlider() ;

   hb_retptrGC( hbqt_gcAllocate_QStyleOptionSlider( ( void * ) pObj, true ) );
}

/* bool dialWrapping */
HB_FUNC( QT_QSTYLEOPTIONSLIDER_DIALWRAPPING )
{
   QStyleOptionSlider * p = hbqt_par_QStyleOptionSlider( 1 );
   if( p )
      hb_retl( ( p )->dialWrapping );
}

/* int maximum */
HB_FUNC( QT_QSTYLEOPTIONSLIDER_MAXIMUM )
{
   QStyleOptionSlider * p = hbqt_par_QStyleOptionSlider( 1 );
   if( p )
      hb_retni( ( p )->maximum );
}

/* int minimum */
HB_FUNC( QT_QSTYLEOPTIONSLIDER_MINIMUM )
{
   QStyleOptionSlider * p = hbqt_par_QStyleOptionSlider( 1 );
   if( p )
      hb_retni( ( p )->minimum );
}

/* qreal notchTarget */
HB_FUNC( QT_QSTYLEOPTIONSLIDER_NOTCHTARGET )
{
   QStyleOptionSlider * p = hbqt_par_QStyleOptionSlider( 1 );
   if( p )
      hb_retnd( ( p )->notchTarget );
}

/* Qt::Orientation orientation */
HB_FUNC( QT_QSTYLEOPTIONSLIDER_ORIENTATION )
{
   QStyleOptionSlider * p = hbqt_par_QStyleOptionSlider( 1 );
   if( p )
      hb_retni( ( Qt::Orientation ) ( p )->orientation );
}

/* int pageStep */
HB_FUNC( QT_QSTYLEOPTIONSLIDER_PAGESTEP )
{
   QStyleOptionSlider * p = hbqt_par_QStyleOptionSlider( 1 );
   if( p )
      hb_retni( ( p )->pageStep );
}

/* int singleStep */
HB_FUNC( QT_QSTYLEOPTIONSLIDER_SINGLESTEP )
{
   QStyleOptionSlider * p = hbqt_par_QStyleOptionSlider( 1 );
   if( p )
      hb_retni( ( p )->singleStep );
}

/* int sliderPosition */
HB_FUNC( QT_QSTYLEOPTIONSLIDER_SLIDERPOSITION )
{
   QStyleOptionSlider * p = hbqt_par_QStyleOptionSlider( 1 );
   if( p )
      hb_retni( ( p )->sliderPosition );
}

/* int sliderValue */
HB_FUNC( QT_QSTYLEOPTIONSLIDER_SLIDERVALUE )
{
   QStyleOptionSlider * p = hbqt_par_QStyleOptionSlider( 1 );
   if( p )
      hb_retni( ( p )->sliderValue );
}

/* int tickInterval */
HB_FUNC( QT_QSTYLEOPTIONSLIDER_TICKINTERVAL )
{
   QStyleOptionSlider * p = hbqt_par_QStyleOptionSlider( 1 );
   if( p )
      hb_retni( ( p )->tickInterval );
}

/* QSlider::TickPosition tickPosition */
HB_FUNC( QT_QSTYLEOPTIONSLIDER_TICKPOSITION )
{
   QStyleOptionSlider * p = hbqt_par_QStyleOptionSlider( 1 );
   if( p )
      hb_retni( ( QSlider::TickPosition ) ( p )->tickPosition );
}

/* bool upsideDown */
HB_FUNC( QT_QSTYLEOPTIONSLIDER_UPSIDEDOWN )
{
   QStyleOptionSlider * p = hbqt_par_QStyleOptionSlider( 1 );
   if( p )
      hb_retl( ( p )->upsideDown );
}


#endif /* #if QT_VERSION >= 0x040500 */
