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
 *  enum CoordinateMode { LogicalMode, StretchToDeviceMode, ObjectBoundingMode }
 *  enum Spread { PadSpread, RepeatSpread, ReflectSpread }
 *  enum Type { LinearGradient, RadialGradient, ConicalGradient, NoGradient }
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
   QT_G_FUNC_PTR func;
   int type;
} QGC_POINTER_QRadialGradient;

QT_G_FUNC( hbqt_gcRelease_QRadialGradient )
{
   QGC_POINTER * p = ( QGC_POINTER * ) Cargo;

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
   QGC_POINTER * p = ( QGC_POINTER * ) hb_gcAllocate( sizeof( QGC_POINTER ), hbqt_gcFuncs() );

   p->ph = ( QRadialGradient * ) pObj;
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QRadialGradient;
   p->type = QT_TYPE_QRadialGradient;

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
      hb_retptrGC( hbqt_gcAllocate_QPointF( new QPointF( ( p )->center() ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QRADIALGRADIENT_CENTER FP=hb_retptrGC( hbqt_gcAllocate_QPointF( new QPointF( ( p )->center() ), true ) ); p is NULL" ) );
   }
}

/*
 * QPointF focalPoint () const
 */
HB_FUNC( QT_QRADIALGRADIENT_FOCALPOINT )
{
   QRadialGradient * p = hbqt_par_QRadialGradient( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QPointF( new QPointF( ( p )->focalPoint() ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QRADIALGRADIENT_FOCALPOINT FP=hb_retptrGC( hbqt_gcAllocate_QPointF( new QPointF( ( p )->focalPoint() ), true ) ); p is NULL" ) );
   }
}

/*
 * qreal radius () const
 */
HB_FUNC( QT_QRADIALGRADIENT_RADIUS )
{
   QRadialGradient * p = hbqt_par_QRadialGradient( 1 );
   if( p )
      hb_retnd( ( p )->radius() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QRADIALGRADIENT_RADIUS FP=hb_retnd( ( p )->radius() ); p is NULL" ) );
   }
}

/*
 * void setCenter ( const QPointF & center )
 */
HB_FUNC( QT_QRADIALGRADIENT_SETCENTER )
{
   QRadialGradient * p = hbqt_par_QRadialGradient( 1 );
   if( p )
      ( p )->setCenter( *hbqt_par_QPointF( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QRADIALGRADIENT_SETCENTER FP=( p )->setCenter( *hbqt_par_QPointF( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setCenter ( qreal x, qreal y )
 */
HB_FUNC( QT_QRADIALGRADIENT_SETCENTER_1 )
{
   QRadialGradient * p = hbqt_par_QRadialGradient( 1 );
   if( p )
      ( p )->setCenter( hb_parnd( 2 ), hb_parnd( 3 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QRADIALGRADIENT_SETCENTER_1 FP=( p )->setCenter( hb_parnd( 2 ), hb_parnd( 3 ) ); p is NULL" ) );
   }
}

/*
 * void setFocalPoint ( const QPointF & focalPoint )
 */
HB_FUNC( QT_QRADIALGRADIENT_SETFOCALPOINT )
{
   QRadialGradient * p = hbqt_par_QRadialGradient( 1 );
   if( p )
      ( p )->setFocalPoint( *hbqt_par_QPointF( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QRADIALGRADIENT_SETFOCALPOINT FP=( p )->setFocalPoint( *hbqt_par_QPointF( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setFocalPoint ( qreal x, qreal y )
 */
HB_FUNC( QT_QRADIALGRADIENT_SETFOCALPOINT_1 )
{
   QRadialGradient * p = hbqt_par_QRadialGradient( 1 );
   if( p )
      ( p )->setFocalPoint( hb_parnd( 2 ), hb_parnd( 3 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QRADIALGRADIENT_SETFOCALPOINT_1 FP=( p )->setFocalPoint( hb_parnd( 2 ), hb_parnd( 3 ) ); p is NULL" ) );
   }
}

/*
 * void setRadius ( qreal radius )
 */
HB_FUNC( QT_QRADIALGRADIENT_SETRADIUS )
{
   QRadialGradient * p = hbqt_par_QRadialGradient( 1 );
   if( p )
      ( p )->setRadius( hb_parnd( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QRADIALGRADIENT_SETRADIUS FP=( p )->setRadius( hb_parnd( 2 ) ); p is NULL" ) );
   }
}


/*----------------------------------------------------------------------*/
#endif             /* #if QT_VERSION >= 0x040500 */
/*----------------------------------------------------------------------*/
