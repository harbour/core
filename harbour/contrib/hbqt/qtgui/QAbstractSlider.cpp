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
 * Copyright 2009 Pritpal Bedi <pritpal@vouchcac.com>
 *
 * Copyright 2009 Marcos Antonio Gambeta <marcosgambeta at gmail dot com>
 * www - http://www.harbour-project.org
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

#include "hbapi.h"
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

QT_G_FUNC( release_QAbstractSlider )
{
   HB_SYMBOL_UNUSED( Cargo );
}

HB_FUNC( QT_QABSTRACTSLIDER )
{
}
/*
 * bool hasTracking () const
 */
HB_FUNC( QT_QABSTRACTSLIDER_HASTRACKING )
{
   hb_retl( hbqt_par_QAbstractSlider( 1 )->hasTracking() );
}

/*
 * bool invertedAppearance () const
 */
HB_FUNC( QT_QABSTRACTSLIDER_INVERTEDAPPEARANCE )
{
   hb_retl( hbqt_par_QAbstractSlider( 1 )->invertedAppearance() );
}

/*
 * bool invertedControls () const
 */
HB_FUNC( QT_QABSTRACTSLIDER_INVERTEDCONTROLS )
{
   hb_retl( hbqt_par_QAbstractSlider( 1 )->invertedControls() );
}

/*
 * bool isSliderDown () const
 */
HB_FUNC( QT_QABSTRACTSLIDER_ISSLIDERDOWN )
{
   hb_retl( hbqt_par_QAbstractSlider( 1 )->isSliderDown() );
}

/*
 * int maximum () const
 */
HB_FUNC( QT_QABSTRACTSLIDER_MAXIMUM )
{
   hb_retni( hbqt_par_QAbstractSlider( 1 )->maximum() );
}

/*
 * int minimum () const
 */
HB_FUNC( QT_QABSTRACTSLIDER_MINIMUM )
{
   hb_retni( hbqt_par_QAbstractSlider( 1 )->minimum() );
}

/*
 * Qt::Orientation orientation () const
 */
HB_FUNC( QT_QABSTRACTSLIDER_ORIENTATION )
{
   hb_retni( ( Qt::Orientation ) hbqt_par_QAbstractSlider( 1 )->orientation() );
}

/*
 * int pageStep () const
 */
HB_FUNC( QT_QABSTRACTSLIDER_PAGESTEP )
{
   hb_retni( hbqt_par_QAbstractSlider( 1 )->pageStep() );
}

/*
 * void setInvertedAppearance ( bool )
 */
HB_FUNC( QT_QABSTRACTSLIDER_SETINVERTEDAPPEARANCE )
{
   hbqt_par_QAbstractSlider( 1 )->setInvertedAppearance( hb_parl( 2 ) );
}

/*
 * void setInvertedControls ( bool )
 */
HB_FUNC( QT_QABSTRACTSLIDER_SETINVERTEDCONTROLS )
{
   hbqt_par_QAbstractSlider( 1 )->setInvertedControls( hb_parl( 2 ) );
}

/*
 * void setMaximum ( int )
 */
HB_FUNC( QT_QABSTRACTSLIDER_SETMAXIMUM )
{
   hbqt_par_QAbstractSlider( 1 )->setMaximum( hb_parni( 2 ) );
}

/*
 * void setMinimum ( int )
 */
HB_FUNC( QT_QABSTRACTSLIDER_SETMINIMUM )
{
   hbqt_par_QAbstractSlider( 1 )->setMinimum( hb_parni( 2 ) );
}

/*
 * void setPageStep ( int )
 */
HB_FUNC( QT_QABSTRACTSLIDER_SETPAGESTEP )
{
   hbqt_par_QAbstractSlider( 1 )->setPageStep( hb_parni( 2 ) );
}

/*
 * void setRange ( int min, int max )
 */
HB_FUNC( QT_QABSTRACTSLIDER_SETRANGE )
{
   hbqt_par_QAbstractSlider( 1 )->setRange( hb_parni( 2 ), hb_parni( 3 ) );
}

/*
 * void setSingleStep ( int )
 */
HB_FUNC( QT_QABSTRACTSLIDER_SETSINGLESTEP )
{
   hbqt_par_QAbstractSlider( 1 )->setSingleStep( hb_parni( 2 ) );
}

/*
 * void setSliderDown ( bool )
 */
HB_FUNC( QT_QABSTRACTSLIDER_SETSLIDERDOWN )
{
   hbqt_par_QAbstractSlider( 1 )->setSliderDown( hb_parl( 2 ) );
}

/*
 * void setSliderPosition ( int )
 */
HB_FUNC( QT_QABSTRACTSLIDER_SETSLIDERPOSITION )
{
   hbqt_par_QAbstractSlider( 1 )->setSliderPosition( hb_parni( 2 ) );
}

/*
 * void setTracking ( bool enable )
 */
HB_FUNC( QT_QABSTRACTSLIDER_SETTRACKING )
{
   hbqt_par_QAbstractSlider( 1 )->setTracking( hb_parl( 2 ) );
}

/*
 * int singleStep () const
 */
HB_FUNC( QT_QABSTRACTSLIDER_SINGLESTEP )
{
   hb_retni( hbqt_par_QAbstractSlider( 1 )->singleStep() );
}

/*
 * int sliderPosition () const
 */
HB_FUNC( QT_QABSTRACTSLIDER_SLIDERPOSITION )
{
   hb_retni( hbqt_par_QAbstractSlider( 1 )->sliderPosition() );
}

/*
 * void triggerAction ( SliderAction action )
 */
HB_FUNC( QT_QABSTRACTSLIDER_TRIGGERACTION )
{
   hbqt_par_QAbstractSlider( 1 )->triggerAction( ( QAbstractSlider::SliderAction ) hb_parni( 2 ) );
}

/*
 * int value () const
 */
HB_FUNC( QT_QABSTRACTSLIDER_VALUE )
{
   hb_retni( hbqt_par_QAbstractSlider( 1 )->value() );
}

/*
 * void setOrientation ( Qt::Orientation )
 */
HB_FUNC( QT_QABSTRACTSLIDER_SETORIENTATION )
{
   hbqt_par_QAbstractSlider( 1 )->setOrientation( ( Qt::Orientation ) hb_parni( 2 ) );
}

/*
 * void setValue ( int )
 */
HB_FUNC( QT_QABSTRACTSLIDER_SETVALUE )
{
   hbqt_par_QAbstractSlider( 1 )->setValue( hb_parni( 2 ) );
}


/*----------------------------------------------------------------------*/
#endif             /* #if QT_VERSION >= 0x040500 */
/*----------------------------------------------------------------------*/
