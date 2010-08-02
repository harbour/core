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

#include "hbqtcore.h"
#include "hbqtgui.h"

/*----------------------------------------------------------------------*/
#if QT_VERSION >= 0x040500
/*----------------------------------------------------------------------*/

/*
 *  enum StyleOptionType { Type }
 *  enum StyleOptionVersion { Version }
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
         HB_TRACE( HB_TR_DEBUG, ( "ph=%p    _rel_QStyleOptionSlider   /.\\", p->ph ) );
         delete ( ( QStyleOptionSlider * ) p->ph );
         HB_TRACE( HB_TR_DEBUG, ( "ph=%p YES_rel_QStyleOptionSlider   \\./", p->ph ) );
         p->ph = NULL;
      }
      else
      {
         HB_TRACE( HB_TR_DEBUG, ( "ph=%p DEL_rel_QStyleOptionSlider    :     Object already deleted!", p->ph ) );
         p->ph = NULL;
      }
   }
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p PTR_rel_QStyleOptionSlider    :    Object not created with new=true", p->ph ) );
      p->ph = NULL;
   }
}

void * hbqt_gcAllocate_QStyleOptionSlider( void * pObj, bool bNew )
{
   HBQT_GC_T * p = ( HBQT_GC_T * ) hb_gcAllocate( sizeof( HBQT_GC_T ), hbqt_gcFuncs() );

   p->ph = ( QStyleOptionSlider * ) pObj;
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QStyleOptionSlider;
   p->type = HBQT_TYPE_QStyleOptionSlider;

   if( bNew )
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p    _new_QStyleOptionSlider", pObj ) );
   }
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p NOT_new_QStyleOptionSlider", pObj ) );
   }
   return p;
}

HB_FUNC( QT_QSTYLEOPTIONSLIDER )
{
   QStyleOptionSlider * pObj = NULL;

   pObj =  new QStyleOptionSlider() ;

   hb_retptrGC( hbqt_gcAllocate_QStyleOptionSlider( ( void * ) pObj, true ) );
}

/*
 * bool dialWrapping
 */
HB_FUNC( QT_QSTYLEOPTIONSLIDER_DIALWRAPPING )
{
   hb_retl( hbqt_par_QStyleOptionSlider( 1 )->dialWrapping );
}

/*
 * int maximum
 */
HB_FUNC( QT_QSTYLEOPTIONSLIDER_MAXIMUM )
{
   hb_retni( hbqt_par_QStyleOptionSlider( 1 )->maximum );
}

/*
 * int minimum
 */
HB_FUNC( QT_QSTYLEOPTIONSLIDER_MINIMUM )
{
   hb_retni( hbqt_par_QStyleOptionSlider( 1 )->minimum );
}

/*
 * qreal notchTarget
 */
HB_FUNC( QT_QSTYLEOPTIONSLIDER_NOTCHTARGET )
{
   hb_retnd( hbqt_par_QStyleOptionSlider( 1 )->notchTarget );
}

/*
 * Qt::Orientation orientation
 */
HB_FUNC( QT_QSTYLEOPTIONSLIDER_ORIENTATION )
{
   hb_retni( ( Qt::Orientation ) hbqt_par_QStyleOptionSlider( 1 )->orientation );
}

/*
 * int pageStep
 */
HB_FUNC( QT_QSTYLEOPTIONSLIDER_PAGESTEP )
{
   hb_retni( hbqt_par_QStyleOptionSlider( 1 )->pageStep );
}

/*
 * int singleStep
 */
HB_FUNC( QT_QSTYLEOPTIONSLIDER_SINGLESTEP )
{
   hb_retni( hbqt_par_QStyleOptionSlider( 1 )->singleStep );
}

/*
 * int sliderPosition
 */
HB_FUNC( QT_QSTYLEOPTIONSLIDER_SLIDERPOSITION )
{
   hb_retni( hbqt_par_QStyleOptionSlider( 1 )->sliderPosition );
}

/*
 * int sliderValue
 */
HB_FUNC( QT_QSTYLEOPTIONSLIDER_SLIDERVALUE )
{
   hb_retni( hbqt_par_QStyleOptionSlider( 1 )->sliderValue );
}

/*
 * int tickInterval
 */
HB_FUNC( QT_QSTYLEOPTIONSLIDER_TICKINTERVAL )
{
   hb_retni( hbqt_par_QStyleOptionSlider( 1 )->tickInterval );
}

/*
 * QSlider::TickPosition tickPosition
 */
HB_FUNC( QT_QSTYLEOPTIONSLIDER_TICKPOSITION )
{
   hb_retni( ( QSlider::TickPosition ) hbqt_par_QStyleOptionSlider( 1 )->tickPosition );
}

/*
 * bool upsideDown
 */
HB_FUNC( QT_QSTYLEOPTIONSLIDER_UPSIDEDOWN )
{
   hb_retl( hbqt_par_QStyleOptionSlider( 1 )->upsideDown );
}


/*----------------------------------------------------------------------*/
#endif             /* #if QT_VERSION >= 0x040500 */
/*----------------------------------------------------------------------*/
