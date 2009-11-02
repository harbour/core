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

QT_G_FUNC( release_QRadialGradient )
{
#if defined(__debug__)
hb_snprintf( str, sizeof(str), "release_QRadialGradient             %i B %i KB", ( int ) hb_xquery( 1001 ), hb_getMemUsed() );  OutputDebugString( str );
#endif
   void * ph = ( void * ) Cargo;
   if( ph )
   {
      ( ( QRadialGradient * ) ph )->~QRadialGradient();
      ph = NULL;
   }
   else
   {
#if defined(__debug__)
hb_snprintf( str, sizeof(str), "! ph____QRadialGradient" );  OutputDebugString( str );
#endif
   }
}

HB_FUNC( QT_QRADIALGRADIENT )
{
   QGC_POINTER * p = ( QGC_POINTER * ) hb_gcAllocate( sizeof( QGC_POINTER ), gcFuncs() );
   void * pObj = NULL;
#if defined(__debug__)
hb_snprintf( str, sizeof(str), "   GC:  new QRadialGradient             %i B %i KB", ( int ) hb_xquery( 1001 ), hb_getMemUsed() );  OutputDebugString( str );
#endif

   if( hb_pcount() == 1 && HB_ISPOINTER( 1 ) )
   {
      pObj = ( QRadialGradient* ) new QRadialGradient( *hbqt_par_QRadialGradient( 1 ) ) ;
   }
   else if( hb_pcount() == 2 && HB_ISPOINTER( 1 ) && HB_ISNUM( 2 ) )
   {
      pObj = ( QRadialGradient* ) new QRadialGradient( *hbqt_par_QPointF( 1 ), hb_parnd( 2 ) ) ;
   }
   else if( hb_pcount() == 3 && HB_ISPOINTER( 1 ) && HB_ISNUM( 2 ) && HB_ISPOINTER( 3 ) )
   {
      pObj = ( QRadialGradient* ) new QRadialGradient( *hbqt_par_QPointF( 1 ), hb_parnd( 2 ), *hbqt_par_QPointF( 3 ) ) ;
   }
   else if( hb_pcount() == 5 && HB_ISNUM( 1 ) && HB_ISNUM( 2 ) && HB_ISNUM( 3 ) && HB_ISNUM( 4 ) && HB_ISNUM( 5 ) )
   {
      pObj = ( QRadialGradient* ) new QRadialGradient( hb_parnd( 1 ), hb_parnd( 2 ), hb_parnd( 3 ), hb_parnd( 4 ), hb_parnd( 5 ) ) ;
   }
   else
   {
      pObj = ( QRadialGradient* ) new QRadialGradient() ;
   }

#if defined(__debug__)
hb_snprintf( str, sizeof(str), "   GC:                                  %i B %i KB", ( int ) hb_xquery( 1001 ), hb_getMemUsed() );  OutputDebugString( str );
#endif
   p->ph = pObj;
   p->func = release_QRadialGradient;

   hb_retptrGC( p );
}
/*
 * QPointF center () const
 */
HB_FUNC( QT_QRADIALGRADIENT_CENTER )
{
   hb_retptrGC( hbqt_ptrTOgcpointer( new QPointF( hbqt_par_QRadialGradient( 1 )->center() ), release_QPointF ) );
}

/*
 * QPointF focalPoint () const
 */
HB_FUNC( QT_QRADIALGRADIENT_FOCALPOINT )
{
   hb_retptrGC( hbqt_ptrTOgcpointer( new QPointF( hbqt_par_QRadialGradient( 1 )->focalPoint() ), release_QPointF ) );
}

/*
 * qreal radius () const
 */
HB_FUNC( QT_QRADIALGRADIENT_RADIUS )
{
   hb_retnd( hbqt_par_QRadialGradient( 1 )->radius() );
}

/*
 * void setCenter ( const QPointF & center )
 */
HB_FUNC( QT_QRADIALGRADIENT_SETCENTER )
{
   hbqt_par_QRadialGradient( 1 )->setCenter( *hbqt_par_QPointF( 2 ) );
}

/*
 * void setCenter ( qreal x, qreal y )
 */
HB_FUNC( QT_QRADIALGRADIENT_SETCENTER_1 )
{
   hbqt_par_QRadialGradient( 1 )->setCenter( hb_parnd( 2 ), hb_parnd( 3 ) );
}

/*
 * void setFocalPoint ( const QPointF & focalPoint )
 */
HB_FUNC( QT_QRADIALGRADIENT_SETFOCALPOINT )
{
   hbqt_par_QRadialGradient( 1 )->setFocalPoint( *hbqt_par_QPointF( 2 ) );
}

/*
 * void setFocalPoint ( qreal x, qreal y )
 */
HB_FUNC( QT_QRADIALGRADIENT_SETFOCALPOINT_1 )
{
   hbqt_par_QRadialGradient( 1 )->setFocalPoint( hb_parnd( 2 ), hb_parnd( 3 ) );
}

/*
 * void setRadius ( qreal radius )
 */
HB_FUNC( QT_QRADIALGRADIENT_SETRADIUS )
{
   hbqt_par_QRadialGradient( 1 )->setRadius( hb_parnd( 2 ) );
}


/*----------------------------------------------------------------------*/
#endif             /* #if QT_VERSION >= 0x040500 */
/*----------------------------------------------------------------------*/
