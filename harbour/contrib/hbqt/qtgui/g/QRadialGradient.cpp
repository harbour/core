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
 *  enum CoordinateMode { LogicalMode, StretchToDeviceMode, ObjectBoundingMode }
 *  enum Spread { PadSpread, RepeatSpread, ReflectSpread }
 *  enum Type { LinearGradient, RadialGradient, ConicalGradient, NoGradient }
 */

/*
 *  Constructed[ 8/8 [ 100.00% ] ]
 *
 */

#include <QtCore/QPointer>

#include <QtGui/QRadialGradient>


/*
 * QRadialGradient ()
 * QRadialGradient ( const QPointF & center, qreal radius, const QPointF & focalPoint )
 * QRadialGradient ( qreal cx, qreal cy, qreal radius, qreal fx, qreal fy )
 * QRadialGradient ( const QPointF & center, qreal radius )
 * QRadialGradient ( qreal cx, qreal cy, qreal radius )
 */

typedef struct
{
   QRadialGradient * ph;
   bool bNew;
   PHBQT_GC_FUNC func;
   int type;
} HBQT_GC_T_QRadialGradient;

HBQT_GC_FUNC( hbqt_gcRelease_QRadialGradient )
{
   HBQT_GC_T * p = ( HBQT_GC_T * ) Cargo;

   if( p && p->bNew )
   {
      if( p->ph )
      {
         HB_TRACE( HB_TR_DEBUG, ( "ph=%p    _rel_QRadialGradient   /.\\", p->ph ) );
         delete ( ( QRadialGradient * ) p->ph );
         HB_TRACE( HB_TR_DEBUG, ( "ph=%p YES_rel_QRadialGradient   \\./", p->ph ) );
         p->ph = NULL;
      }
      else
      {
         HB_TRACE( HB_TR_DEBUG, ( "ph=%p DEL_rel_QRadialGradient    :     Object already deleted!", p->ph ) );
         p->ph = NULL;
      }
   }
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p PTR_rel_QRadialGradient    :    Object not created with new=true", p->ph ) );
      p->ph = NULL;
   }
}

void * hbqt_gcAllocate_QRadialGradient( void * pObj, bool bNew )
{
   HBQT_GC_T * p = ( HBQT_GC_T * ) hb_gcAllocate( sizeof( HBQT_GC_T ), hbqt_gcFuncs() );

   p->ph = ( QRadialGradient * ) pObj;
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QRadialGradient;
   p->type = HBQT_TYPE_QRadialGradient;

   if( bNew )
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p    _new_QRadialGradient", pObj ) );
   }
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p NOT_new_QRadialGradient", pObj ) );
   }
   return p;
}

HB_FUNC( QT_QRADIALGRADIENT )
{
   QRadialGradient * pObj = NULL;

   if( hb_pcount() == 1 && HB_ISPOINTER( 1 ) )
   {
      pObj =  new QRadialGradient( *hbqt_par_QRadialGradient( 1 ) ) ;
   }
   else if( hb_pcount() == 2 && HB_ISPOINTER( 1 ) && HB_ISNUM( 2 ) )
   {
      pObj =  new QRadialGradient( *hbqt_par_QPointF( 1 ), hb_parnd( 2 ) ) ;
   }
   else if( hb_pcount() == 3 && HB_ISPOINTER( 1 ) && HB_ISNUM( 2 ) && HB_ISPOINTER( 3 ) )
   {
      pObj =  new QRadialGradient( *hbqt_par_QPointF( 1 ), hb_parnd( 2 ), *hbqt_par_QPointF( 3 ) ) ;
   }
   else if( hb_pcount() == 5 && HB_ISNUM( 1 ) && HB_ISNUM( 2 ) && HB_ISNUM( 3 ) && HB_ISNUM( 4 ) && HB_ISNUM( 5 ) )
   {
      pObj =  new QRadialGradient( hb_parnd( 1 ), hb_parnd( 2 ), hb_parnd( 3 ), hb_parnd( 4 ), hb_parnd( 5 ) ) ;
   }
   else
   {
      pObj =  new QRadialGradient() ;
   }

   hb_retptrGC( hbqt_gcAllocate_QRadialGradient( ( void * ) pObj, true ) );
}

/*
 * QPointF center () const
 */
HB_FUNC( QT_QRADIALGRADIENT_CENTER )
{
   QRadialGradient * p = hbqt_par_QRadialGradient( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QPointF( new QPointF( ( p )->center() ), true ) );
   }
}

/*
 * QPointF focalPoint () const
 */
HB_FUNC( QT_QRADIALGRADIENT_FOCALPOINT )
{
   QRadialGradient * p = hbqt_par_QRadialGradient( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QPointF( new QPointF( ( p )->focalPoint() ), true ) );
   }
}

/*
 * qreal radius () const
 */
HB_FUNC( QT_QRADIALGRADIENT_RADIUS )
{
   QRadialGradient * p = hbqt_par_QRadialGradient( 1 );
   if( p )
   {
      hb_retnd( ( p )->radius() );
   }
}

/*
 * void setCenter ( const QPointF & center )
 */
HB_FUNC( QT_QRADIALGRADIENT_SETCENTER )
{
   QRadialGradient * p = hbqt_par_QRadialGradient( 1 );
   if( p )
   {
      ( p )->setCenter( *hbqt_par_QPointF( 2 ) );
   }
}

/*
 * void setCenter ( qreal x, qreal y )
 */
HB_FUNC( QT_QRADIALGRADIENT_SETCENTER_1 )
{
   QRadialGradient * p = hbqt_par_QRadialGradient( 1 );
   if( p )
   {
      ( p )->setCenter( hb_parnd( 2 ), hb_parnd( 3 ) );
   }
}

/*
 * void setFocalPoint ( const QPointF & focalPoint )
 */
HB_FUNC( QT_QRADIALGRADIENT_SETFOCALPOINT )
{
   QRadialGradient * p = hbqt_par_QRadialGradient( 1 );
   if( p )
   {
      ( p )->setFocalPoint( *hbqt_par_QPointF( 2 ) );
   }
}

/*
 * void setFocalPoint ( qreal x, qreal y )
 */
HB_FUNC( QT_QRADIALGRADIENT_SETFOCALPOINT_1 )
{
   QRadialGradient * p = hbqt_par_QRadialGradient( 1 );
   if( p )
   {
      ( p )->setFocalPoint( hb_parnd( 2 ), hb_parnd( 3 ) );
   }
}

/*
 * void setRadius ( qreal radius )
 */
HB_FUNC( QT_QRADIALGRADIENT_SETRADIUS )
{
   QRadialGradient * p = hbqt_par_QRadialGradient( 1 );
   if( p )
   {
      ( p )->setRadius( hb_parnd( 2 ) );
   }
}


/*----------------------------------------------------------------------*/
#endif             /* #if QT_VERSION >= 0x040500 */
/*----------------------------------------------------------------------*/
