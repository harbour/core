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
 *  enum IntersectType { NoIntersection, UnboundedIntersection, BoundedIntersection }
 */

#include <QtCore/QPointer>

#include <QtCore/QLineF>


/* QLineF ()
 * QLineF ( const QPointF & p1, const QPointF & p2 )
 * QLineF ( qreal x1, qreal y1, qreal x2, qreal y2 )
 * QLineF ( const QLine & line )
 */

QT_G_FUNC( release_QLineF )
{
   QGC_POINTER * p = ( QGC_POINTER * ) Cargo;

   HB_TRACE( HB_TR_DEBUG, ( "release_QLineF                       p=%p", p ) );
   HB_TRACE( HB_TR_DEBUG, ( "release_QLineF                      ph=%p", p->ph ) );

   if( p && p->ph )
   {
      delete ( ( QLineF * ) p->ph );
      p->ph = NULL;
      HB_TRACE( HB_TR_DEBUG, ( "YES release_QLineF                      Object deleted! %i B %i KB", ( int ) hb_xquery( 1001 ), hbqt_getmemused() ) );
   }
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "DEL release_QLineF                      Object Already deleted!" ) );
   }
}

void * gcAllocate_QLineF( void * pObj )
{
   QGC_POINTER * p = ( QGC_POINTER * ) hb_gcAllocate( sizeof( QGC_POINTER ), gcFuncs() );

   p->ph = pObj;
   p->func = release_QLineF;
   HB_TRACE( HB_TR_DEBUG, ( "          new_QLineF                      %i B %i KB", ( int ) hb_xquery( 1001 ), hbqt_getmemused() ) );
   return( p );
}

HB_FUNC( QT_QLINEF )
{
   void * pObj = NULL;

   pObj = new QLineF() ;

   hb_retptrGC( gcAllocate_QLineF( pObj ) );
}
/*
 * QPointF p1 () const
 */
HB_FUNC( QT_QLINEF_P1 )
{
   hb_retptrGC( gcAllocate_QPointF( new QPointF( hbqt_par_QLineF( 1 )->p1() ) ) );
}

/*
 * QPointF p2 () const
 */
HB_FUNC( QT_QLINEF_P2 )
{
   hb_retptrGC( gcAllocate_QPointF( new QPointF( hbqt_par_QLineF( 1 )->p2() ) ) );
}

/*
 * qreal x1 () const
 */
HB_FUNC( QT_QLINEF_X1 )
{
   hb_retnd( hbqt_par_QLineF( 1 )->x1() );
}

/*
 * qreal x2 () const
 */
HB_FUNC( QT_QLINEF_X2 )
{
   hb_retnd( hbqt_par_QLineF( 1 )->x2() );
}

/*
 * qreal y1 () const
 */
HB_FUNC( QT_QLINEF_Y1 )
{
   hb_retnd( hbqt_par_QLineF( 1 )->y1() );
}

/*
 * qreal y2 () const
 */
HB_FUNC( QT_QLINEF_Y2 )
{
   hb_retnd( hbqt_par_QLineF( 1 )->y2() );
}

/*
 * qreal angle () const
 */
HB_FUNC( QT_QLINEF_ANGLE )
{
   hb_retnd( hbqt_par_QLineF( 1 )->angle() );
}

/*
 * qreal angleTo ( const QLineF & line ) const
 */
HB_FUNC( QT_QLINEF_ANGLETO )
{
   hb_retnd( hbqt_par_QLineF( 1 )->angleTo( *hbqt_par_QLineF( 2 ) ) );
}

/*
 * qreal dx () const
 */
HB_FUNC( QT_QLINEF_DX )
{
   hb_retnd( hbqt_par_QLineF( 1 )->dx() );
}

/*
 * qreal dy () const
 */
HB_FUNC( QT_QLINEF_DY )
{
   hb_retnd( hbqt_par_QLineF( 1 )->dy() );
}

/*
 * IntersectType intersect ( const QLineF & line, QPointF * intersectionPoint ) const
 */
HB_FUNC( QT_QLINEF_INTERSECT )
{
   hb_retni( ( QLineF::IntersectType ) hbqt_par_QLineF( 1 )->intersect( *hbqt_par_QLineF( 2 ), hbqt_par_QPointF( 3 ) ) );
}

/*
 * bool isNull () const
 */
HB_FUNC( QT_QLINEF_ISNULL )
{
   hb_retl( hbqt_par_QLineF( 1 )->isNull() );
}

/*
 * qreal length () const
 */
HB_FUNC( QT_QLINEF_LENGTH )
{
   hb_retnd( hbqt_par_QLineF( 1 )->length() );
}

/*
 * QLineF normalVector () const
 */
HB_FUNC( QT_QLINEF_NORMALVECTOR )
{
   hb_retptrGC( gcAllocate_QLineF( new QLineF( hbqt_par_QLineF( 1 )->normalVector() ) ) );
}

/*
 * QPointF pointAt ( qreal t ) const
 */
HB_FUNC( QT_QLINEF_POINTAT )
{
   hb_retptrGC( gcAllocate_QPointF( new QPointF( hbqt_par_QLineF( 1 )->pointAt( hb_parnd( 2 ) ) ) ) );
}

/*
 * void setP1 ( const QPointF & p1 )
 */
HB_FUNC( QT_QLINEF_SETP1 )
{
   hbqt_par_QLineF( 1 )->setP1( *hbqt_par_QPointF( 2 ) );
}

/*
 * void setP2 ( const QPointF & p2 )
 */
HB_FUNC( QT_QLINEF_SETP2 )
{
   hbqt_par_QLineF( 1 )->setP2( *hbqt_par_QPointF( 2 ) );
}

/*
 * void setAngle ( qreal angle )
 */
HB_FUNC( QT_QLINEF_SETANGLE )
{
   hbqt_par_QLineF( 1 )->setAngle( hb_parnd( 2 ) );
}

/*
 * void setLength ( qreal length )
 */
HB_FUNC( QT_QLINEF_SETLENGTH )
{
   hbqt_par_QLineF( 1 )->setLength( hb_parnd( 2 ) );
}

/*
 * void setLine ( qreal x1, qreal y1, qreal x2, qreal y2 )
 */
HB_FUNC( QT_QLINEF_SETLINE )
{
   hbqt_par_QLineF( 1 )->setLine( hb_parnd( 2 ), hb_parnd( 3 ), hb_parnd( 4 ), hb_parnd( 5 ) );
}

/*
 * void setPoints ( const QPointF & p1, const QPointF & p2 )
 */
HB_FUNC( QT_QLINEF_SETPOINTS )
{
   hbqt_par_QLineF( 1 )->setPoints( *hbqt_par_QPointF( 2 ), *hbqt_par_QPointF( 3 ) );
}

/*
 * QLine toLine () const
 */
HB_FUNC( QT_QLINEF_TOLINE )
{
   hb_retptrGC( gcAllocate_QLine( new QLine( hbqt_par_QLineF( 1 )->toLine() ) ) );
}

/*
 * void translate ( const QPointF & offset )
 */
HB_FUNC( QT_QLINEF_TRANSLATE )
{
   hbqt_par_QLineF( 1 )->translate( *hbqt_par_QPointF( 2 ) );
}

/*
 * void translate ( qreal dx, qreal dy )
 */
HB_FUNC( QT_QLINEF_TRANSLATE_1 )
{
   hbqt_par_QLineF( 1 )->translate( hb_parnd( 2 ), hb_parnd( 3 ) );
}

/*
 * QLineF translated ( const QPointF & offset ) const
 */
HB_FUNC( QT_QLINEF_TRANSLATED )
{
   hb_retptrGC( gcAllocate_QLineF( new QLineF( hbqt_par_QLineF( 1 )->translated( *hbqt_par_QPointF( 2 ) ) ) ) );
}

/*
 * QLineF translated ( qreal dx, qreal dy ) const
 */
HB_FUNC( QT_QLINEF_TRANSLATED_1 )
{
   hb_retptrGC( gcAllocate_QLineF( new QLineF( hbqt_par_QLineF( 1 )->translated( hb_parnd( 2 ), hb_parnd( 3 ) ) ) ) );
}

/*
 * QLineF unitVector () const
 */
HB_FUNC( QT_QLINEF_UNITVECTOR )
{
   hb_retptrGC( gcAllocate_QLineF( new QLineF( hbqt_par_QLineF( 1 )->unitVector() ) ) );
}


/*----------------------------------------------------------------------*/
#endif             /* #if QT_VERSION >= 0x040500 */
/*----------------------------------------------------------------------*/
