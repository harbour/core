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

/*----------------------------------------------------------------------*/
#if QT_VERSION >= 0x040500
/*----------------------------------------------------------------------*/

/*
 *  enum IntersectType { NoIntersection, UnboundedIntersection, BoundedIntersection }
 */

/*
 *  Constructed[ 27/27 [ 100.00% ] ]
 *
 */

#include <QtCore/QPointer>

#include <QtCore/QLineF>


/* QLineF ()
 * QLineF ( const QPointF & p1, const QPointF & p2 )
 * QLineF ( qreal x1, qreal y1, qreal x2, qreal y2 )
 * QLineF ( const QLine & line )
 */

typedef struct
{
   QLineF * ph;
   bool bNew;
   PHBQT_GC_FUNC func;
   int type;
} HBQT_GC_T_QLineF;

HBQT_GC_FUNC( hbqt_gcRelease_QLineF )
{
   HBQT_GC_T * p = ( HBQT_GC_T * ) Cargo;

   if( p && p->bNew )
   {
      if( p->ph )
      {
         HB_TRACE( HB_TR_DEBUG, ( "ph=%p    _rel_QLineF   /.\\", p->ph ) );
         delete ( ( QLineF * ) p->ph );
         HB_TRACE( HB_TR_DEBUG, ( "ph=%p YES_rel_QLineF   \\./", p->ph ) );
         p->ph = NULL;
      }
      else
      {
         HB_TRACE( HB_TR_DEBUG, ( "ph=%p DEL_rel_QLineF    :     Object already deleted!", p->ph ) );
         p->ph = NULL;
      }
   }
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p PTR_rel_QLineF    :    Object not created with new=true", p->ph ) );
      p->ph = NULL;
   }
}

void * hbqt_gcAllocate_QLineF( void * pObj, bool bNew )
{
   HBQT_GC_T * p = ( HBQT_GC_T * ) hb_gcAllocate( sizeof( HBQT_GC_T ), hbqt_gcFuncs() );

   p->ph = ( QLineF * ) pObj;
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QLineF;
   p->type = HBQT_TYPE_QLineF;

   if( bNew )
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p    _new_QLineF", pObj ) );
   }
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p NOT_new_QLineF", pObj ) );
   }
   return p;
}

HB_FUNC( QT_QLINEF )
{
   QLineF * pObj = NULL;

   pObj = new QLineF() ;

   hb_retptrGC( hbqt_gcAllocate_QLineF( ( void * ) pObj, true ) );
}

/*
 * QPointF p1 () const
 */
HB_FUNC( QT_QLINEF_P1 )
{
   QLineF * p = hbqt_par_QLineF( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QPointF( new QPointF( ( p )->p1() ), true ) );
   }
}

/*
 * QPointF p2 () const
 */
HB_FUNC( QT_QLINEF_P2 )
{
   QLineF * p = hbqt_par_QLineF( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QPointF( new QPointF( ( p )->p2() ), true ) );
   }
}

/*
 * qreal x1 () const
 */
HB_FUNC( QT_QLINEF_X1 )
{
   QLineF * p = hbqt_par_QLineF( 1 );
   if( p )
   {
      hb_retnd( ( p )->x1() );
   }
}

/*
 * qreal x2 () const
 */
HB_FUNC( QT_QLINEF_X2 )
{
   QLineF * p = hbqt_par_QLineF( 1 );
   if( p )
   {
      hb_retnd( ( p )->x2() );
   }
}

/*
 * qreal y1 () const
 */
HB_FUNC( QT_QLINEF_Y1 )
{
   QLineF * p = hbqt_par_QLineF( 1 );
   if( p )
   {
      hb_retnd( ( p )->y1() );
   }
}

/*
 * qreal y2 () const
 */
HB_FUNC( QT_QLINEF_Y2 )
{
   QLineF * p = hbqt_par_QLineF( 1 );
   if( p )
   {
      hb_retnd( ( p )->y2() );
   }
}

/*
 * qreal angle () const
 */
HB_FUNC( QT_QLINEF_ANGLE )
{
   QLineF * p = hbqt_par_QLineF( 1 );
   if( p )
   {
      hb_retnd( ( p )->angle() );
   }
}

/*
 * qreal angleTo ( const QLineF & line ) const
 */
HB_FUNC( QT_QLINEF_ANGLETO )
{
   QLineF * p = hbqt_par_QLineF( 1 );
   if( p )
   {
      hb_retnd( ( p )->angleTo( *hbqt_par_QLineF( 2 ) ) );
   }
}

/*
 * qreal dx () const
 */
HB_FUNC( QT_QLINEF_DX )
{
   QLineF * p = hbqt_par_QLineF( 1 );
   if( p )
   {
      hb_retnd( ( p )->dx() );
   }
}

/*
 * qreal dy () const
 */
HB_FUNC( QT_QLINEF_DY )
{
   QLineF * p = hbqt_par_QLineF( 1 );
   if( p )
   {
      hb_retnd( ( p )->dy() );
   }
}

/*
 * IntersectType intersect ( const QLineF & line, QPointF * intersectionPoint ) const
 */
HB_FUNC( QT_QLINEF_INTERSECT )
{
   QLineF * p = hbqt_par_QLineF( 1 );
   if( p )
   {
      hb_retni( ( QLineF::IntersectType ) ( p )->intersect( *hbqt_par_QLineF( 2 ), hbqt_par_QPointF( 3 ) ) );
   }
}

/*
 * bool isNull () const
 */
HB_FUNC( QT_QLINEF_ISNULL )
{
   QLineF * p = hbqt_par_QLineF( 1 );
   if( p )
   {
      hb_retl( ( p )->isNull() );
   }
}

/*
 * qreal length () const
 */
HB_FUNC( QT_QLINEF_LENGTH )
{
   QLineF * p = hbqt_par_QLineF( 1 );
   if( p )
   {
      hb_retnd( ( p )->length() );
   }
}

/*
 * QLineF normalVector () const
 */
HB_FUNC( QT_QLINEF_NORMALVECTOR )
{
   QLineF * p = hbqt_par_QLineF( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QLineF( new QLineF( ( p )->normalVector() ), true ) );
   }
}

/*
 * QPointF pointAt ( qreal t ) const
 */
HB_FUNC( QT_QLINEF_POINTAT )
{
   QLineF * p = hbqt_par_QLineF( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QPointF( new QPointF( ( p )->pointAt( hb_parnd( 2 ) ) ), true ) );
   }
}

/*
 * void setP1 ( const QPointF & p1 )
 */
HB_FUNC( QT_QLINEF_SETP1 )
{
   QLineF * p = hbqt_par_QLineF( 1 );
   if( p )
   {
      ( p )->setP1( *hbqt_par_QPointF( 2 ) );
   }
}

/*
 * void setP2 ( const QPointF & p2 )
 */
HB_FUNC( QT_QLINEF_SETP2 )
{
   QLineF * p = hbqt_par_QLineF( 1 );
   if( p )
   {
      ( p )->setP2( *hbqt_par_QPointF( 2 ) );
   }
}

/*
 * void setAngle ( qreal angle )
 */
HB_FUNC( QT_QLINEF_SETANGLE )
{
   QLineF * p = hbqt_par_QLineF( 1 );
   if( p )
   {
      ( p )->setAngle( hb_parnd( 2 ) );
   }
}

/*
 * void setLength ( qreal length )
 */
HB_FUNC( QT_QLINEF_SETLENGTH )
{
   QLineF * p = hbqt_par_QLineF( 1 );
   if( p )
   {
      ( p )->setLength( hb_parnd( 2 ) );
   }
}

/*
 * void setLine ( qreal x1, qreal y1, qreal x2, qreal y2 )
 */
HB_FUNC( QT_QLINEF_SETLINE )
{
   QLineF * p = hbqt_par_QLineF( 1 );
   if( p )
   {
      ( p )->setLine( hb_parnd( 2 ), hb_parnd( 3 ), hb_parnd( 4 ), hb_parnd( 5 ) );
   }
}

/*
 * void setPoints ( const QPointF & p1, const QPointF & p2 )
 */
HB_FUNC( QT_QLINEF_SETPOINTS )
{
   QLineF * p = hbqt_par_QLineF( 1 );
   if( p )
   {
      ( p )->setPoints( *hbqt_par_QPointF( 2 ), *hbqt_par_QPointF( 3 ) );
   }
}

/*
 * QLine toLine () const
 */
HB_FUNC( QT_QLINEF_TOLINE )
{
   QLineF * p = hbqt_par_QLineF( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QLine( new QLine( ( p )->toLine() ), true ) );
   }
}

/*
 * void translate ( const QPointF & offset )
 */
HB_FUNC( QT_QLINEF_TRANSLATE )
{
   QLineF * p = hbqt_par_QLineF( 1 );
   if( p )
   {
      ( p )->translate( *hbqt_par_QPointF( 2 ) );
   }
}

/*
 * void translate ( qreal dx, qreal dy )
 */
HB_FUNC( QT_QLINEF_TRANSLATE_1 )
{
   QLineF * p = hbqt_par_QLineF( 1 );
   if( p )
   {
      ( p )->translate( hb_parnd( 2 ), hb_parnd( 3 ) );
   }
}

/*
 * QLineF translated ( const QPointF & offset ) const
 */
HB_FUNC( QT_QLINEF_TRANSLATED )
{
   QLineF * p = hbqt_par_QLineF( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QLineF( new QLineF( ( p )->translated( *hbqt_par_QPointF( 2 ) ) ), true ) );
   }
}

/*
 * QLineF translated ( qreal dx, qreal dy ) const
 */
HB_FUNC( QT_QLINEF_TRANSLATED_1 )
{
   QLineF * p = hbqt_par_QLineF( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QLineF( new QLineF( ( p )->translated( hb_parnd( 2 ), hb_parnd( 3 ) ) ), true ) );
   }
}

/*
 * QLineF unitVector () const
 */
HB_FUNC( QT_QLINEF_UNITVECTOR )
{
   QLineF * p = hbqt_par_QLineF( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QLineF( new QLineF( ( p )->unitVector() ), true ) );
   }
}


/*----------------------------------------------------------------------*/
#endif             /* #if QT_VERSION >= 0x040500 */
/*----------------------------------------------------------------------*/
