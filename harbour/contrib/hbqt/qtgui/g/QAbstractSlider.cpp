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
#include "hbqtgui.h"

/*----------------------------------------------------------------------*/
#if QT_VERSION >= 0x040500
/*----------------------------------------------------------------------*/

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
   {
      p->ph = NULL;
   }
}

void * hbqt_gcAllocate_QAbstractSlider( void * pObj, bool bNew )
{
   HBQT_GC_T_QAbstractSlider * p = ( HBQT_GC_T_QAbstractSlider * ) hb_gcAllocate( sizeof( HBQT_GC_T_QAbstractSlider ), hbqt_gcFuncs() );

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
   {
      hb_retl( ( p )->hasTracking() );
   }
}

/*
 * bool invertedAppearance () const
 */
HB_FUNC( QT_QABSTRACTSLIDER_INVERTEDAPPEARANCE )
{
   QAbstractSlider * p = hbqt_par_QAbstractSlider( 1 );
   if( p )
   {
      hb_retl( ( p )->invertedAppearance() );
   }
}

/*
 * bool invertedControls () const
 */
HB_FUNC( QT_QABSTRACTSLIDER_INVERTEDCONTROLS )
{
   QAbstractSlider * p = hbqt_par_QAbstractSlider( 1 );
   if( p )
   {
      hb_retl( ( p )->invertedControls() );
   }
}

/*
 * bool isSliderDown () const
 */
HB_FUNC( QT_QABSTRACTSLIDER_ISSLIDERDOWN )
{
   QAbstractSlider * p = hbqt_par_QAbstractSlider( 1 );
   if( p )
   {
      hb_retl( ( p )->isSliderDown() );
   }
}

/*
 * int maximum () const
 */
HB_FUNC( QT_QABSTRACTSLIDER_MAXIMUM )
{
   QAbstractSlider * p = hbqt_par_QAbstractSlider( 1 );
   if( p )
   {
      hb_retni( ( p )->maximum() );
   }
}

/*
 * int minimum () const
 */
HB_FUNC( QT_QABSTRACTSLIDER_MINIMUM )
{
   QAbstractSlider * p = hbqt_par_QAbstractSlider( 1 );
   if( p )
   {
      hb_retni( ( p )->minimum() );
   }
}

/*
 * Qt::Orientation orientation () const
 */
HB_FUNC( QT_QABSTRACTSLIDER_ORIENTATION )
{
   QAbstractSlider * p = hbqt_par_QAbstractSlider( 1 );
   if( p )
   {
      hb_retni( ( Qt::Orientation ) ( p )->orientation() );
   }
}

/*
 * int pageStep () const
 */
HB_FUNC( QT_QABSTRACTSLIDER_PAGESTEP )
{
   QAbstractSlider * p = hbqt_par_QAbstractSlider( 1 );
   if( p )
   {
      hb_retni( ( p )->pageStep() );
   }
}

/*
 * void setInvertedAppearance ( bool )
 */
HB_FUNC( QT_QABSTRACTSLIDER_SETINVERTEDAPPEARANCE )
{
   QAbstractSlider * p = hbqt_par_QAbstractSlider( 1 );
   if( p )
   {
      ( p )->setInvertedAppearance( hb_parl( 2 ) );
   }
}

/*
 * void setInvertedControls ( bool )
 */
HB_FUNC( QT_QABSTRACTSLIDER_SETINVERTEDCONTROLS )
{
   QAbstractSlider * p = hbqt_par_QAbstractSlider( 1 );
   if( p )
   {
      ( p )->setInvertedControls( hb_parl( 2 ) );
   }
}

/*
 * void setMaximum ( int )
 */
HB_FUNC( QT_QABSTRACTSLIDER_SETMAXIMUM )
{
   QAbstractSlider * p = hbqt_par_QAbstractSlider( 1 );
   if( p )
   {
      ( p )->setMaximum( hb_parni( 2 ) );
   }
}

/*
 * void setMinimum ( int )
 */
HB_FUNC( QT_QABSTRACTSLIDER_SETMINIMUM )
{
   QAbstractSlider * p = hbqt_par_QAbstractSlider( 1 );
   if( p )
   {
      ( p )->setMinimum( hb_parni( 2 ) );
   }
}

/*
 * void setPageStep ( int )
 */
HB_FUNC( QT_QABSTRACTSLIDER_SETPAGESTEP )
{
   QAbstractSlider * p = hbqt_par_QAbstractSlider( 1 );
   if( p )
   {
      ( p )->setPageStep( hb_parni( 2 ) );
   }
}

/*
 * void setRange ( int min, int max )
 */
HB_FUNC( QT_QABSTRACTSLIDER_SETRANGE )
{
   QAbstractSlider * p = hbqt_par_QAbstractSlider( 1 );
   if( p )
   {
      ( p )->setRange( hb_parni( 2 ), hb_parni( 3 ) );
   }
}

/*
 * void setSingleStep ( int )
 */
HB_FUNC( QT_QABSTRACTSLIDER_SETSINGLESTEP )
{
   QAbstractSlider * p = hbqt_par_QAbstractSlider( 1 );
   if( p )
   {
      ( p )->setSingleStep( hb_parni( 2 ) );
   }
}

/*
 * void setSliderDown ( bool )
 */
HB_FUNC( QT_QABSTRACTSLIDER_SETSLIDERDOWN )
{
   QAbstractSlider * p = hbqt_par_QAbstractSlider( 1 );
   if( p )
   {
      ( p )->setSliderDown( hb_parl( 2 ) );
   }
}

/*
 * void setSliderPosition ( int )
 */
HB_FUNC( QT_QABSTRACTSLIDER_SETSLIDERPOSITION )
{
   QAbstractSlider * p = hbqt_par_QAbstractSlider( 1 );
   if( p )
   {
      ( p )->setSliderPosition( hb_parni( 2 ) );
   }
}

/*
 * void setTracking ( bool enable )
 */
HB_FUNC( QT_QABSTRACTSLIDER_SETTRACKING )
{
   QAbstractSlider * p = hbqt_par_QAbstractSlider( 1 );
   if( p )
   {
      ( p )->setTracking( hb_parl( 2 ) );
   }
}

/*
 * int singleStep () const
 */
HB_FUNC( QT_QABSTRACTSLIDER_SINGLESTEP )
{
   QAbstractSlider * p = hbqt_par_QAbstractSlider( 1 );
   if( p )
   {
      hb_retni( ( p )->singleStep() );
   }
}

/*
 * int sliderPosition () const
 */
HB_FUNC( QT_QABSTRACTSLIDER_SLIDERPOSITION )
{
   QAbstractSlider * p = hbqt_par_QAbstractSlider( 1 );
   if( p )
   {
      hb_retni( ( p )->sliderPosition() );
   }
}

/*
 * void triggerAction ( SliderAction action )
 */
HB_FUNC( QT_QABSTRACTSLIDER_TRIGGERACTION )
{
   QAbstractSlider * p = hbqt_par_QAbstractSlider( 1 );
   if( p )
   {
      ( p )->triggerAction( ( QAbstractSlider::SliderAction ) hb_parni( 2 ) );
   }
}

/*
 * int value () const
 */
HB_FUNC( QT_QABSTRACTSLIDER_VALUE )
{
   QAbstractSlider * p = hbqt_par_QAbstractSlider( 1 );
   if( p )
   {
      hb_retni( ( p )->value() );
   }
}

/*
 * void setOrientation ( Qt::Orientation )
 */
HB_FUNC( QT_QABSTRACTSLIDER_SETORIENTATION )
{
   QAbstractSlider * p = hbqt_par_QAbstractSlider( 1 );
   if( p )
   {
      ( p )->setOrientation( ( Qt::Orientation ) hb_parni( 2 ) );
   }
}

/*
 * void setValue ( int )
 */
HB_FUNC( QT_QABSTRACTSLIDER_SETVALUE )
{
   QAbstractSlider * p = hbqt_par_QAbstractSlider( 1 );
   if( p )
   {
      ( p )->setValue( hb_parni( 2 ) );
   }
}


/*----------------------------------------------------------------------*/
#endif             /* #if QT_VERSION >= 0x040500 */
/*----------------------------------------------------------------------*/
