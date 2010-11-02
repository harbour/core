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
 *  enum SliderAction { SliderNoAction, SliderSingleStepAdd, SliderSingleStepSub, SliderPageStepAdd, ..., SliderMove }
 */

/*
 *  Constructed[ 24/24 [ 100.00% ] ]
 *
 */

#include <QtCore/QPointer>

#include <QtGui/QAbstractSlider>


/*
 * QAbstractSlider ( QWidget * parent = 0 )
 * ~QAbstractSlider ()
 */

typedef struct
{
   QPointer< QAbstractSlider > ph;
   bool bNew;
   PHBQT_GC_FUNC func;
   int type;
} HBQT_GC_T_QAbstractSlider;

HBQT_GC_FUNC( hbqt_gcRelease_QAbstractSlider )
{
   HB_SYMBOL_UNUSED( Cargo );
   HBQT_GC_T * p = ( HBQT_GC_T * ) Cargo;

   if( p && p->bNew )
      p->ph = NULL;
}

void * hbqt_gcAllocate_QAbstractSlider( void * pObj, bool bNew )
{
   HBQT_GC_T_QAbstractSlider * p = ( HBQT_GC_T_QAbstractSlider * ) hb_gcAllocate( sizeof( HBQT_GC_T_QAbstractSlider ), hbqt_gcFuncs() );

   new( & p->ph ) QPointer< QAbstractSlider >( ( QAbstractSlider * ) pObj );
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QAbstractSlider;
   p->type = HBQT_TYPE_QAbstractSlider;

   return p;
}

HB_FUNC( QT_QABSTRACTSLIDER )
{

}

/* bool hasTracking () const */
HB_FUNC( QT_QABSTRACTSLIDER_HASTRACKING )
{
   QAbstractSlider * p = hbqt_par_QAbstractSlider( 1 );
   if( p )
      hb_retl( ( p )->hasTracking() );
}

/* bool invertedAppearance () const */
HB_FUNC( QT_QABSTRACTSLIDER_INVERTEDAPPEARANCE )
{
   QAbstractSlider * p = hbqt_par_QAbstractSlider( 1 );
   if( p )
      hb_retl( ( p )->invertedAppearance() );
}

/* bool invertedControls () const */
HB_FUNC( QT_QABSTRACTSLIDER_INVERTEDCONTROLS )
{
   QAbstractSlider * p = hbqt_par_QAbstractSlider( 1 );
   if( p )
      hb_retl( ( p )->invertedControls() );
}

/* bool isSliderDown () const */
HB_FUNC( QT_QABSTRACTSLIDER_ISSLIDERDOWN )
{
   QAbstractSlider * p = hbqt_par_QAbstractSlider( 1 );
   if( p )
      hb_retl( ( p )->isSliderDown() );
}

/* int maximum () const */
HB_FUNC( QT_QABSTRACTSLIDER_MAXIMUM )
{
   QAbstractSlider * p = hbqt_par_QAbstractSlider( 1 );
   if( p )
      hb_retni( ( p )->maximum() );
}

/* int minimum () const */
HB_FUNC( QT_QABSTRACTSLIDER_MINIMUM )
{
   QAbstractSlider * p = hbqt_par_QAbstractSlider( 1 );
   if( p )
      hb_retni( ( p )->minimum() );
}

/* Qt::Orientation orientation () const */
HB_FUNC( QT_QABSTRACTSLIDER_ORIENTATION )
{
   QAbstractSlider * p = hbqt_par_QAbstractSlider( 1 );
   if( p )
      hb_retni( ( Qt::Orientation ) ( p )->orientation() );
}

/* int pageStep () const */
HB_FUNC( QT_QABSTRACTSLIDER_PAGESTEP )
{
   QAbstractSlider * p = hbqt_par_QAbstractSlider( 1 );
   if( p )
      hb_retni( ( p )->pageStep() );
}

/* void setInvertedAppearance ( bool ) */
HB_FUNC( QT_QABSTRACTSLIDER_SETINVERTEDAPPEARANCE )
{
   QAbstractSlider * p = hbqt_par_QAbstractSlider( 1 );
   if( p )
      ( p )->setInvertedAppearance( hb_parl( 2 ) );
}

/* void setInvertedControls ( bool ) */
HB_FUNC( QT_QABSTRACTSLIDER_SETINVERTEDCONTROLS )
{
   QAbstractSlider * p = hbqt_par_QAbstractSlider( 1 );
   if( p )
      ( p )->setInvertedControls( hb_parl( 2 ) );
}

/* void setMaximum ( int ) */
HB_FUNC( QT_QABSTRACTSLIDER_SETMAXIMUM )
{
   QAbstractSlider * p = hbqt_par_QAbstractSlider( 1 );
   if( p )
      ( p )->setMaximum( hb_parni( 2 ) );
}

/* void setMinimum ( int ) */
HB_FUNC( QT_QABSTRACTSLIDER_SETMINIMUM )
{
   QAbstractSlider * p = hbqt_par_QAbstractSlider( 1 );
   if( p )
      ( p )->setMinimum( hb_parni( 2 ) );
}

/* void setPageStep ( int ) */
HB_FUNC( QT_QABSTRACTSLIDER_SETPAGESTEP )
{
   QAbstractSlider * p = hbqt_par_QAbstractSlider( 1 );
   if( p )
      ( p )->setPageStep( hb_parni( 2 ) );
}

/* void setRange ( int min, int max ) */
HB_FUNC( QT_QABSTRACTSLIDER_SETRANGE )
{
   QAbstractSlider * p = hbqt_par_QAbstractSlider( 1 );
   if( p )
      ( p )->setRange( hb_parni( 2 ), hb_parni( 3 ) );
}

/* void setSingleStep ( int ) */
HB_FUNC( QT_QABSTRACTSLIDER_SETSINGLESTEP )
{
   QAbstractSlider * p = hbqt_par_QAbstractSlider( 1 );
   if( p )
      ( p )->setSingleStep( hb_parni( 2 ) );
}

/* void setSliderDown ( bool ) */
HB_FUNC( QT_QABSTRACTSLIDER_SETSLIDERDOWN )
{
   QAbstractSlider * p = hbqt_par_QAbstractSlider( 1 );
   if( p )
      ( p )->setSliderDown( hb_parl( 2 ) );
}

/* void setSliderPosition ( int ) */
HB_FUNC( QT_QABSTRACTSLIDER_SETSLIDERPOSITION )
{
   QAbstractSlider * p = hbqt_par_QAbstractSlider( 1 );
   if( p )
      ( p )->setSliderPosition( hb_parni( 2 ) );
}

/* void setTracking ( bool enable ) */
HB_FUNC( QT_QABSTRACTSLIDER_SETTRACKING )
{
   QAbstractSlider * p = hbqt_par_QAbstractSlider( 1 );
   if( p )
      ( p )->setTracking( hb_parl( 2 ) );
}

/* int singleStep () const */
HB_FUNC( QT_QABSTRACTSLIDER_SINGLESTEP )
{
   QAbstractSlider * p = hbqt_par_QAbstractSlider( 1 );
   if( p )
      hb_retni( ( p )->singleStep() );
}

/* int sliderPosition () const */
HB_FUNC( QT_QABSTRACTSLIDER_SLIDERPOSITION )
{
   QAbstractSlider * p = hbqt_par_QAbstractSlider( 1 );
   if( p )
      hb_retni( ( p )->sliderPosition() );
}

/* void triggerAction ( SliderAction action ) */
HB_FUNC( QT_QABSTRACTSLIDER_TRIGGERACTION )
{
   QAbstractSlider * p = hbqt_par_QAbstractSlider( 1 );
   if( p )
      ( p )->triggerAction( ( QAbstractSlider::SliderAction ) hb_parni( 2 ) );
}

/* int value () const */
HB_FUNC( QT_QABSTRACTSLIDER_VALUE )
{
   QAbstractSlider * p = hbqt_par_QAbstractSlider( 1 );
   if( p )
      hb_retni( ( p )->value() );
}

/* void setOrientation ( Qt::Orientation ) */
HB_FUNC( QT_QABSTRACTSLIDER_SETORIENTATION )
{
   QAbstractSlider * p = hbqt_par_QAbstractSlider( 1 );
   if( p )
      ( p )->setOrientation( ( Qt::Orientation ) hb_parni( 2 ) );
}

/* void setValue ( int ) */
HB_FUNC( QT_QABSTRACTSLIDER_SETVALUE )
{
   QAbstractSlider * p = hbqt_par_QAbstractSlider( 1 );
   if( p )
      ( p )->setValue( hb_parni( 2 ) );
}


#endif /* #if QT_VERSION >= 0x040500 */
