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
 *  Constructed[ 29/29 [ 100.00% ] ]
 *
 */

#include <QtCore/QPointer>

#include <QtGui/QMatrix>
#include <QtGui/QPainterPath>


/* QMatrix ()
 * QMatrix ( qreal m11, qreal m12, qreal m21, qreal m22, qreal dx, qreal dy )
 * QMatrix ( const QMatrix & matrix )
 */

typedef struct
{
   QMatrix * ph;
   bool bNew;
   PHBQT_GC_FUNC func;
   int type;
} HBQT_GC_T_QMatrix;

HBQT_GC_FUNC( hbqt_gcRelease_QMatrix )
{
   HBQT_GC_T * p = ( HBQT_GC_T * ) Cargo;

   if( p && p->bNew )
   {
      if( p->ph )
      {
         HB_TRACE( HB_TR_DEBUG, ( "ph=%p    _rel_QMatrix   /.\\", p->ph ) );
         delete ( ( QMatrix * ) p->ph );
         HB_TRACE( HB_TR_DEBUG, ( "ph=%p YES_rel_QMatrix   \\./", p->ph ) );
         p->ph = NULL;
      }
      else
      {
         HB_TRACE( HB_TR_DEBUG, ( "ph=%p DEL_rel_QMatrix    :     Object already deleted!", p->ph ) );
         p->ph = NULL;
      }
   }
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p PTR_rel_QMatrix    :    Object not created with new=true", p->ph ) );
      p->ph = NULL;
   }
}

void * hbqt_gcAllocate_QMatrix( void * pObj, bool bNew )
{
   HBQT_GC_T * p = ( HBQT_GC_T * ) hb_gcAllocate( sizeof( HBQT_GC_T ), hbqt_gcFuncs() );

   p->ph = ( QMatrix * ) pObj;
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QMatrix;
   p->type = HBQT_TYPE_QMatrix;

   if( bNew )
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p    _new_QMatrix", pObj ) );
   }
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p NOT_new_QMatrix", pObj ) );
   }
   return p;
}

HB_FUNC( QT_QMATRIX )
{
   QMatrix * pObj = NULL;

   pObj = new QMatrix() ;

   hb_retptrGC( hbqt_gcAllocate_QMatrix( ( void * ) pObj, true ) );
}

/*
 * qreal m11 () const
 */
HB_FUNC( QT_QMATRIX_M11 )
{
   QMatrix * p = hbqt_par_QMatrix( 1 );
   if( p )
   {
      hb_retnd( ( p )->m11() );
   }
}

/*
 * qreal m12 () const
 */
HB_FUNC( QT_QMATRIX_M12 )
{
   QMatrix * p = hbqt_par_QMatrix( 1 );
   if( p )
   {
      hb_retnd( ( p )->m12() );
   }
}

/*
 * qreal m21 () const
 */
HB_FUNC( QT_QMATRIX_M21 )
{
   QMatrix * p = hbqt_par_QMatrix( 1 );
   if( p )
   {
      hb_retnd( ( p )->m21() );
   }
}

/*
 * qreal m22 () const
 */
HB_FUNC( QT_QMATRIX_M22 )
{
   QMatrix * p = hbqt_par_QMatrix( 1 );
   if( p )
   {
      hb_retnd( ( p )->m22() );
   }
}

/*
 * qreal det () const
 */
HB_FUNC( QT_QMATRIX_DET )
{
   QMatrix * p = hbqt_par_QMatrix( 1 );
   if( p )
   {
      hb_retnd( ( p )->det() );
   }
}

/*
 * qreal dx () const
 */
HB_FUNC( QT_QMATRIX_DX )
{
   QMatrix * p = hbqt_par_QMatrix( 1 );
   if( p )
   {
      hb_retnd( ( p )->dx() );
   }
}

/*
 * qreal dy () const
 */
HB_FUNC( QT_QMATRIX_DY )
{
   QMatrix * p = hbqt_par_QMatrix( 1 );
   if( p )
   {
      hb_retnd( ( p )->dy() );
   }
}

/*
 * QMatrix inverted ( bool * invertible = 0 ) const
 */
HB_FUNC( QT_QMATRIX_INVERTED )
{
   QMatrix * p = hbqt_par_QMatrix( 1 );
   bool iInvertible = 0;

   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QMatrix( new QMatrix( ( p )->inverted( &iInvertible ) ), true ) );
   }

   hb_stornl( iInvertible, 2 );
}

/*
 * bool isIdentity () const
 */
HB_FUNC( QT_QMATRIX_ISIDENTITY )
{
   QMatrix * p = hbqt_par_QMatrix( 1 );
   if( p )
   {
      hb_retl( ( p )->isIdentity() );
   }
}

/*
 * bool isInvertible () const
 */
HB_FUNC( QT_QMATRIX_ISINVERTIBLE )
{
   QMatrix * p = hbqt_par_QMatrix( 1 );
   if( p )
   {
      hb_retl( ( p )->isInvertible() );
   }
}

/*
 * void map ( qreal x, qreal y, qreal * tx, qreal * ty ) const
 */
HB_FUNC( QT_QMATRIX_MAP )
{
   QMatrix * p = hbqt_par_QMatrix( 1 );
   qreal qrTx = 0;
   qreal qrTy = 0;

   if( p )
   {
      ( p )->map( hb_parnd( 2 ), hb_parnd( 3 ), &qrTx, &qrTy );
   }

   hb_stornd( qrTx, 4 );
   hb_stornd( qrTy, 5 );
}

/*
 * void map ( int x, int y, int * tx, int * ty ) const
 */
HB_FUNC( QT_QMATRIX_MAP_1 )
{
   QMatrix * p = hbqt_par_QMatrix( 1 );
   int iTx = 0;
   int iTy = 0;

   if( p )
   {
      ( p )->map( hb_parni( 2 ), hb_parni( 3 ), &iTx, &iTy );
   }

   hb_storni( iTx, 4 );
   hb_storni( iTy, 5 );
}

/*
 * QPointF map ( const QPointF & point ) const
 */
HB_FUNC( QT_QMATRIX_MAP_2 )
{
   QMatrix * p = hbqt_par_QMatrix( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QPointF( new QPointF( ( p )->map( *hbqt_par_QPointF( 2 ) ) ), true ) );
   }
}

/*
 * QPoint map ( const QPoint & point ) const
 */
HB_FUNC( QT_QMATRIX_MAP_3 )
{
   QMatrix * p = hbqt_par_QMatrix( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QPoint( new QPoint( ( p )->map( *hbqt_par_QPoint( 2 ) ) ), true ) );
   }
}

/*
 * QLineF map ( const QLineF & line ) const
 */
HB_FUNC( QT_QMATRIX_MAP_4 )
{
   QMatrix * p = hbqt_par_QMatrix( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QLineF( new QLineF( ( p )->map( *hbqt_par_QLineF( 2 ) ) ), true ) );
   }
}

/*
 * QLine map ( const QLine & line ) const
 */
HB_FUNC( QT_QMATRIX_MAP_5 )
{
   QMatrix * p = hbqt_par_QMatrix( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QLine( new QLine( ( p )->map( *hbqt_par_QLine( 2 ) ) ), true ) );
   }
}

/*
 * QPolygonF map ( const QPolygonF & polygon ) const
 */
HB_FUNC( QT_QMATRIX_MAP_6 )
{
   QMatrix * p = hbqt_par_QMatrix( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QPolygonF( new QPolygonF( ( p )->map( *hbqt_par_QPolygonF( 2 ) ) ), true ) );
   }
}

/*
 * QPolygon map ( const QPolygon & polygon ) const
 */
HB_FUNC( QT_QMATRIX_MAP_7 )
{
   QMatrix * p = hbqt_par_QMatrix( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QPolygon( new QPolygon( ( p )->map( *hbqt_par_QPolygon( 2 ) ) ), true ) );
   }
}

/*
 * QRegion map ( const QRegion & region ) const
 */
HB_FUNC( QT_QMATRIX_MAP_8 )
{
   QMatrix * p = hbqt_par_QMatrix( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QRegion( new QRegion( ( p )->map( *hbqt_par_QRegion( 2 ) ) ), true ) );
   }
}

/*
 * QPainterPath map ( const QPainterPath & path ) const
 */
HB_FUNC( QT_QMATRIX_MAP_9 )
{
   QMatrix * p = hbqt_par_QMatrix( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QPainterPath( new QPainterPath( ( p )->map( *hbqt_par_QPainterPath( 2 ) ) ), true ) );
   }
}

/*
 * QRectF mapRect ( const QRectF & rectangle ) const
 */
HB_FUNC( QT_QMATRIX_MAPRECT )
{
   QMatrix * p = hbqt_par_QMatrix( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QRectF( new QRectF( ( p )->mapRect( *hbqt_par_QRectF( 2 ) ) ), true ) );
   }
}

/*
 * QRect mapRect ( const QRect & rectangle ) const
 */
HB_FUNC( QT_QMATRIX_MAPRECT_1 )
{
   QMatrix * p = hbqt_par_QMatrix( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QRect( new QRect( ( p )->mapRect( *hbqt_par_QRect( 2 ) ) ), true ) );
   }
}

/*
 * QPolygon mapToPolygon ( const QRect & rectangle ) const
 */
HB_FUNC( QT_QMATRIX_MAPTOPOLYGON )
{
   QMatrix * p = hbqt_par_QMatrix( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QPolygon( new QPolygon( ( p )->mapToPolygon( *hbqt_par_QRect( 2 ) ) ), true ) );
   }
}

/*
 * void reset ()
 */
HB_FUNC( QT_QMATRIX_RESET )
{
   QMatrix * p = hbqt_par_QMatrix( 1 );
   if( p )
   {
      ( p )->reset();
   }
}

/*
 * QMatrix & rotate ( qreal degrees )
 */
HB_FUNC( QT_QMATRIX_ROTATE )
{
   QMatrix * p = hbqt_par_QMatrix( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QMatrix( new QMatrix( ( p )->rotate( hb_parnd( 2 ) ) ), true ) );
   }
}

/*
 * QMatrix & scale ( qreal sx, qreal sy )
 */
HB_FUNC( QT_QMATRIX_SCALE )
{
   QMatrix * p = hbqt_par_QMatrix( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QMatrix( new QMatrix( ( p )->scale( hb_parnd( 2 ), hb_parnd( 3 ) ) ), true ) );
   }
}

/*
 * void setMatrix ( qreal m11, qreal m12, qreal m21, qreal m22, qreal dx, qreal dy )
 */
HB_FUNC( QT_QMATRIX_SETMATRIX )
{
   QMatrix * p = hbqt_par_QMatrix( 1 );
   if( p )
   {
      ( p )->setMatrix( hb_parnd( 2 ), hb_parnd( 3 ), hb_parnd( 4 ), hb_parnd( 5 ), hb_parnd( 6 ), hb_parnd( 7 ) );
   }
}

/*
 * QMatrix & shear ( qreal sh, qreal sv )
 */
HB_FUNC( QT_QMATRIX_SHEAR )
{
   QMatrix * p = hbqt_par_QMatrix( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QMatrix( new QMatrix( ( p )->shear( hb_parnd( 2 ), hb_parnd( 3 ) ) ), true ) );
   }
}

/*
 * QMatrix & translate ( qreal dx, qreal dy )
 */
HB_FUNC( QT_QMATRIX_TRANSLATE )
{
   QMatrix * p = hbqt_par_QMatrix( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QMatrix( new QMatrix( ( p )->translate( hb_parnd( 2 ), hb_parnd( 3 ) ) ), true ) );
   }
}


/*----------------------------------------------------------------------*/
#endif             /* #if QT_VERSION >= 0x040500 */
/*----------------------------------------------------------------------*/
