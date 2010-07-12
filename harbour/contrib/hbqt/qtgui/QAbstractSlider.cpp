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
 *  enum SliderAction { SliderNoAction, SliderSingleStepAdd, SliderSingleStepSub, SliderPageStepAdd, ..., SliderMove }
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
   QT_G_FUNC_PTR func;
   int type;
} QGC_POINTER_QAbstractSlider;

QT_G_FUNC( hbqt_gcRelease_QAbstractSlider )
{
   HB_SYMBOL_UNUSED( Cargo );
   QGC_POINTER * p = ( QGC_POINTER * ) Cargo;

   if( p && p->bNew )
   {
      p->ph = NULL;
   }
}

void * hbqt_gcAllocate_QAbstractSlider( void * pObj, bool bNew )
{
   QGC_POINTER_QAbstractSlider * p = ( QGC_POINTER_QAbstractSlider * ) hb_gcAllocate( sizeof( QGC_POINTER_QAbstractSlider ), hbqt_gcFuncs() );

   new( & p->ph ) QPointer< QAbstractSlider >( ( QAbstractSlider * ) pObj );
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QAbstractSlider;
   p->type = HBQT_TYPE_QAbstractSlider;

   if( bNew )
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p    _new_QAbstractSlider  under p->pq", pObj ) );
   }
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p NOT_new_QAbstractSlider", pObj ) );
   }
   return p;
}

HB_FUNC( QT_QABSTRACTSLIDER )
{

}

/*
 * bool hasTracking () const
 */
HB_FUNC( QT_QABSTRACTSLIDER_HASTRACKING )
{
   QAbstractSlider * p = hbqt_par_QAbstractSlider( 1 );
   if( p )
      hb_retl( ( p )->hasTracking() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QABSTRACTSLIDER_HASTRACKING FP=hb_retl( ( p )->hasTracking() ); p is NULL" ) );
   }
}

/*
 * bool invertedAppearance () const
 */
HB_FUNC( QT_QABSTRACTSLIDER_INVERTEDAPPEARANCE )
{
   QAbstractSlider * p = hbqt_par_QAbstractSlider( 1 );
   if( p )
      hb_retl( ( p )->invertedAppearance() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QABSTRACTSLIDER_INVERTEDAPPEARANCE FP=hb_retl( ( p )->invertedAppearance() ); p is NULL" ) );
   }
}

/*
 * bool invertedControls () const
 */
HB_FUNC( QT_QABSTRACTSLIDER_INVERTEDCONTROLS )
{
   QAbstractSlider * p = hbqt_par_QAbstractSlider( 1 );
   if( p )
      hb_retl( ( p )->invertedControls() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QABSTRACTSLIDER_INVERTEDCONTROLS FP=hb_retl( ( p )->invertedControls() ); p is NULL" ) );
   }
}

/*
 * bool isSliderDown () const
 */
HB_FUNC( QT_QABSTRACTSLIDER_ISSLIDERDOWN )
{
   QAbstractSlider * p = hbqt_par_QAbstractSlider( 1 );
   if( p )
      hb_retl( ( p )->isSliderDown() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QABSTRACTSLIDER_ISSLIDERDOWN FP=hb_retl( ( p )->isSliderDown() ); p is NULL" ) );
   }
}

/*
 * int maximum () const
 */
HB_FUNC( QT_QABSTRACTSLIDER_MAXIMUM )
{
   QAbstractSlider * p = hbqt_par_QAbstractSlider( 1 );
   if( p )
      hb_retni( ( p )->maximum() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QABSTRACTSLIDER_MAXIMUM FP=hb_retni( ( p )->maximum() ); p is NULL" ) );
   }
}

/*
 * int minimum () const
 */
HB_FUNC( QT_QABSTRACTSLIDER_MINIMUM )
{
   QAbstractSlider * p = hbqt_par_QAbstractSlider( 1 );
   if( p )
      hb_retni( ( p )->minimum() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QABSTRACTSLIDER_MINIMUM FP=hb_retni( ( p )->minimum() ); p is NULL" ) );
   }
}

/*
 * Qt::Orientation orientation () const
 */
HB_FUNC( QT_QABSTRACTSLIDER_ORIENTATION )
{
   QAbstractSlider * p = hbqt_par_QAbstractSlider( 1 );
   if( p )
      hb_retni( ( Qt::Orientation ) ( p )->orientation() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QABSTRACTSLIDER_ORIENTATION FP=hb_retni( ( Qt::Orientation ) ( p )->orientation() ); p is NULL" ) );
   }
}

/*
 * int pageStep () const
 */
HB_FUNC( QT_QABSTRACTSLIDER_PAGESTEP )
{
   QAbstractSlider * p = hbqt_par_QAbstractSlider( 1 );
   if( p )
      hb_retni( ( p )->pageStep() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QABSTRACTSLIDER_PAGESTEP FP=hb_retni( ( p )->pageStep() ); p is NULL" ) );
   }
}

/*
 * void setInvertedAppearance ( bool )
 */
HB_FUNC( QT_QABSTRACTSLIDER_SETINVERTEDAPPEARANCE )
{
   QAbstractSlider * p = hbqt_par_QAbstractSlider( 1 );
   if( p )
      ( p )->setInvertedAppearance( hb_parl( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QABSTRACTSLIDER_SETINVERTEDAPPEARANCE FP=( p )->setInvertedAppearance( hb_parl( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setInvertedControls ( bool )
 */
HB_FUNC( QT_QABSTRACTSLIDER_SETINVERTEDCONTROLS )
{
   QAbstractSlider * p = hbqt_par_QAbstractSlider( 1 );
   if( p )
      ( p )->setInvertedControls( hb_parl( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QABSTRACTSLIDER_SETINVERTEDCONTROLS FP=( p )->setInvertedControls( hb_parl( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setMaximum ( int )
 */
HB_FUNC( QT_QABSTRACTSLIDER_SETMAXIMUM )
{
   QAbstractSlider * p = hbqt_par_QAbstractSlider( 1 );
   if( p )
      ( p )->setMaximum( hb_parni( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QABSTRACTSLIDER_SETMAXIMUM FP=( p )->setMaximum( hb_parni( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setMinimum ( int )
 */
HB_FUNC( QT_QABSTRACTSLIDER_SETMINIMUM )
{
   QAbstractSlider * p = hbqt_par_QAbstractSlider( 1 );
   if( p )
      ( p )->setMinimum( hb_parni( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QABSTRACTSLIDER_SETMINIMUM FP=( p )->setMinimum( hb_parni( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setPageStep ( int )
 */
HB_FUNC( QT_QABSTRACTSLIDER_SETPAGESTEP )
{
   QAbstractSlider * p = hbqt_par_QAbstractSlider( 1 );
   if( p )
      ( p )->setPageStep( hb_parni( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QABSTRACTSLIDER_SETPAGESTEP FP=( p )->setPageStep( hb_parni( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setRange ( int min, int max )
 */
HB_FUNC( QT_QABSTRACTSLIDER_SETRANGE )
{
   QAbstractSlider * p = hbqt_par_QAbstractSlider( 1 );
   if( p )
      ( p )->setRange( hb_parni( 2 ), hb_parni( 3 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QABSTRACTSLIDER_SETRANGE FP=( p )->setRange( hb_parni( 2 ), hb_parni( 3 ) ); p is NULL" ) );
   }
}

/*
 * void setSingleStep ( int )
 */
HB_FUNC( QT_QABSTRACTSLIDER_SETSINGLESTEP )
{
   QAbstractSlider * p = hbqt_par_QAbstractSlider( 1 );
   if( p )
      ( p )->setSingleStep( hb_parni( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QABSTRACTSLIDER_SETSINGLESTEP FP=( p )->setSingleStep( hb_parni( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setSliderDown ( bool )
 */
HB_FUNC( QT_QABSTRACTSLIDER_SETSLIDERDOWN )
{
   QAbstractSlider * p = hbqt_par_QAbstractSlider( 1 );
   if( p )
      ( p )->setSliderDown( hb_parl( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QABSTRACTSLIDER_SETSLIDERDOWN FP=( p )->setSliderDown( hb_parl( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setSliderPosition ( int )
 */
HB_FUNC( QT_QABSTRACTSLIDER_SETSLIDERPOSITION )
{
   QAbstractSlider * p = hbqt_par_QAbstractSlider( 1 );
   if( p )
      ( p )->setSliderPosition( hb_parni( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QABSTRACTSLIDER_SETSLIDERPOSITION FP=( p )->setSliderPosition( hb_parni( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setTracking ( bool enable )
 */
HB_FUNC( QT_QABSTRACTSLIDER_SETTRACKING )
{
   QAbstractSlider * p = hbqt_par_QAbstractSlider( 1 );
   if( p )
      ( p )->setTracking( hb_parl( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QABSTRACTSLIDER_SETTRACKING FP=( p )->setTracking( hb_parl( 2 ) ); p is NULL" ) );
   }
}

/*
 * int singleStep () const
 */
HB_FUNC( QT_QABSTRACTSLIDER_SINGLESTEP )
{
   QAbstractSlider * p = hbqt_par_QAbstractSlider( 1 );
   if( p )
      hb_retni( ( p )->singleStep() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QABSTRACTSLIDER_SINGLESTEP FP=hb_retni( ( p )->singleStep() ); p is NULL" ) );
   }
}

/*
 * int sliderPosition () const
 */
HB_FUNC( QT_QABSTRACTSLIDER_SLIDERPOSITION )
{
   QAbstractSlider * p = hbqt_par_QAbstractSlider( 1 );
   if( p )
      hb_retni( ( p )->sliderPosition() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QABSTRACTSLIDER_SLIDERPOSITION FP=hb_retni( ( p )->sliderPosition() ); p is NULL" ) );
   }
}

/*
 * void triggerAction ( SliderAction action )
 */
HB_FUNC( QT_QABSTRACTSLIDER_TRIGGERACTION )
{
   QAbstractSlider * p = hbqt_par_QAbstractSlider( 1 );
   if( p )
      ( p )->triggerAction( ( QAbstractSlider::SliderAction ) hb_parni( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QABSTRACTSLIDER_TRIGGERACTION FP=( p )->triggerAction( ( QAbstractSlider::SliderAction ) hb_parni( 2 ) ); p is NULL" ) );
   }
}

/*
 * int value () const
 */
HB_FUNC( QT_QABSTRACTSLIDER_VALUE )
{
   QAbstractSlider * p = hbqt_par_QAbstractSlider( 1 );
   if( p )
      hb_retni( ( p )->value() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QABSTRACTSLIDER_VALUE FP=hb_retni( ( p )->value() ); p is NULL" ) );
   }
}

/*
 * void setOrientation ( Qt::Orientation )
 */
HB_FUNC( QT_QABSTRACTSLIDER_SETORIENTATION )
{
   QAbstractSlider * p = hbqt_par_QAbstractSlider( 1 );
   if( p )
      ( p )->setOrientation( ( Qt::Orientation ) hb_parni( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QABSTRACTSLIDER_SETORIENTATION FP=( p )->setOrientation( ( Qt::Orientation ) hb_parni( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setValue ( int )
 */
HB_FUNC( QT_QABSTRACTSLIDER_SETVALUE )
{
   QAbstractSlider * p = hbqt_par_QAbstractSlider( 1 );
   if( p )
      ( p )->setValue( hb_parni( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QABSTRACTSLIDER_SETVALUE FP=( p )->setValue( hb_parni( 2 ) ); p is NULL" ) );
   }
}


/*----------------------------------------------------------------------*/
#endif             /* #if QT_VERSION >= 0x040500 */
/*----------------------------------------------------------------------*/
