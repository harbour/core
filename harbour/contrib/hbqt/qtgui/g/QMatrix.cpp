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
   QT_G_FUNC_PTR func;
   int type;
} QGC_POINTER_QMatrix;

QT_G_FUNC( hbqt_gcRelease_QMatrix )
{
   QGC_POINTER * p = ( QGC_POINTER * ) Cargo;

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
   QGC_POINTER * p = ( QGC_POINTER * ) hb_gcAllocate( sizeof( QGC_POINTER ), hbqt_gcFuncs() );

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
      hb_retnd( ( p )->m11() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QMATRIX_M11 FP=hb_retnd( ( p )->m11() ); p is NULL" ) );
   }
}

/*
 * qreal m12 () const
 */
HB_FUNC( QT_QMATRIX_M12 )
{
   QMatrix * p = hbqt_par_QMatrix( 1 );
   if( p )
      hb_retnd( ( p )->m12() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QMATRIX_M12 FP=hb_retnd( ( p )->m12() ); p is NULL" ) );
   }
}

/*
 * qreal m21 () const
 */
HB_FUNC( QT_QMATRIX_M21 )
{
   QMatrix * p = hbqt_par_QMatrix( 1 );
   if( p )
      hb_retnd( ( p )->m21() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QMATRIX_M21 FP=hb_retnd( ( p )->m21() ); p is NULL" ) );
   }
}

/*
 * qreal m22 () const
 */
HB_FUNC( QT_QMATRIX_M22 )
{
   QMatrix * p = hbqt_par_QMatrix( 1 );
   if( p )
      hb_retnd( ( p )->m22() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QMATRIX_M22 FP=hb_retnd( ( p )->m22() ); p is NULL" ) );
   }
}

/*
 * qreal det () const
 */
HB_FUNC( QT_QMATRIX_DET )
{
   QMatrix * p = hbqt_par_QMatrix( 1 );
   if( p )
      hb_retnd( ( p )->det() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QMATRIX_DET FP=hb_retnd( ( p )->det() ); p is NULL" ) );
   }
}

/*
 * qreal dx () const
 */
HB_FUNC( QT_QMATRIX_DX )
{
   QMatrix * p = hbqt_par_QMatrix( 1 );
   if( p )
      hb_retnd( ( p )->dx() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QMATRIX_DX FP=hb_retnd( ( p )->dx() ); p is NULL" ) );
   }
}

/*
 * qreal dy () const
 */
HB_FUNC( QT_QMATRIX_DY )
{
   QMatrix * p = hbqt_par_QMatrix( 1 );
   if( p )
      hb_retnd( ( p )->dy() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QMATRIX_DY FP=hb_retnd( ( p )->dy() ); p is NULL" ) );
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
      hb_retptrGC( hbqt_gcAllocate_QMatrix( new QMatrix( ( p )->inverted( &iInvertible ) ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QMATRIX_INVERTED FP=hb_retptrGC( hbqt_gcAllocate_QMatrix( new QMatrix( ( p )->inverted( &iInvertible ) ), true ) ); p is NULL" ) );
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
      hb_retl( ( p )->isIdentity() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QMATRIX_ISIDENTITY FP=hb_retl( ( p )->isIdentity() ); p is NULL" ) );
   }
}

/*
 * bool isInvertible () const
 */
HB_FUNC( QT_QMATRIX_ISINVERTIBLE )
{
   QMatrix * p = hbqt_par_QMatrix( 1 );
   if( p )
      hb_retl( ( p )->isInvertible() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QMATRIX_ISINVERTIBLE FP=hb_retl( ( p )->isInvertible() ); p is NULL" ) );
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
      ( p )->map( hb_parnd( 2 ), hb_parnd( 3 ), &qrTx, &qrTy );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QMATRIX_MAP FP=( p )->map( hb_parnd( 2 ), hb_parnd( 3 ), &qrTx, &qrTy ); p is NULL" ) );
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
      ( p )->map( hb_parni( 2 ), hb_parni( 3 ), &iTx, &iTy );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QMATRIX_MAP_1 FP=( p )->map( hb_parni( 2 ), hb_parni( 3 ), &iTx, &iTy ); p is NULL" ) );
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
      hb_retptrGC( hbqt_gcAllocate_QPointF( new QPointF( ( p )->map( *hbqt_par_QPointF( 2 ) ) ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QMATRIX_MAP_2 FP=hb_retptrGC( hbqt_gcAllocate_QPointF( new QPointF( ( p )->map( *hbqt_par_QPointF( 2 ) ) ), true ) ); p is NULL" ) );
   }
}

/*
 * QPoint map ( const QPoint & point ) const
 */
HB_FUNC( QT_QMATRIX_MAP_3 )
{
   QMatrix * p = hbqt_par_QMatrix( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QPoint( new QPoint( ( p )->map( *hbqt_par_QPoint( 2 ) ) ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QMATRIX_MAP_3 FP=hb_retptrGC( hbqt_gcAllocate_QPoint( new QPoint( ( p )->map( *hbqt_par_QPoint( 2 ) ) ), true ) ); p is NULL" ) );
   }
}

/*
 * QLineF map ( const QLineF & line ) const
 */
HB_FUNC( QT_QMATRIX_MAP_4 )
{
   QMatrix * p = hbqt_par_QMatrix( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QLineF( new QLineF( ( p )->map( *hbqt_par_QLineF( 2 ) ) ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QMATRIX_MAP_4 FP=hb_retptrGC( hbqt_gcAllocate_QLineF( new QLineF( ( p )->map( *hbqt_par_QLineF( 2 ) ) ), true ) ); p is NULL" ) );
   }
}

/*
 * QLine map ( const QLine & line ) const
 */
HB_FUNC( QT_QMATRIX_MAP_5 )
{
   QMatrix * p = hbqt_par_QMatrix( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QLine( new QLine( ( p )->map( *hbqt_par_QLine( 2 ) ) ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QMATRIX_MAP_5 FP=hb_retptrGC( hbqt_gcAllocate_QLine( new QLine( ( p )->map( *hbqt_par_QLine( 2 ) ) ), true ) ); p is NULL" ) );
   }
}

/*
 * QPolygonF map ( const QPolygonF & polygon ) const
 */
HB_FUNC( QT_QMATRIX_MAP_6 )
{
   QMatrix * p = hbqt_par_QMatrix( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QPolygonF( new QPolygonF( ( p )->map( *hbqt_par_QPolygonF( 2 ) ) ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QMATRIX_MAP_6 FP=hb_retptrGC( hbqt_gcAllocate_QPolygonF( new QPolygonF( ( p )->map( *hbqt_par_QPolygonF( 2 ) ) ), true ) ); p is NULL" ) );
   }
}

/*
 * QPolygon map ( const QPolygon & polygon ) const
 */
HB_FUNC( QT_QMATRIX_MAP_7 )
{
   QMatrix * p = hbqt_par_QMatrix( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QPolygon( new QPolygon( ( p )->map( *hbqt_par_QPolygon( 2 ) ) ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QMATRIX_MAP_7 FP=hb_retptrGC( hbqt_gcAllocate_QPolygon( new QPolygon( ( p )->map( *hbqt_par_QPolygon( 2 ) ) ), true ) ); p is NULL" ) );
   }
}

/*
 * QRegion map ( const QRegion & region ) const
 */
HB_FUNC( QT_QMATRIX_MAP_8 )
{
   QMatrix * p = hbqt_par_QMatrix( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QRegion( new QRegion( ( p )->map( *hbqt_par_QRegion( 2 ) ) ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QMATRIX_MAP_8 FP=hb_retptrGC( hbqt_gcAllocate_QRegion( new QRegion( ( p )->map( *hbqt_par_QRegion( 2 ) ) ), true ) ); p is NULL" ) );
   }
}

/*
 * QPainterPath map ( const QPainterPath & path ) const
 */
HB_FUNC( QT_QMATRIX_MAP_9 )
{
   QMatrix * p = hbqt_par_QMatrix( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QPainterPath( new QPainterPath( ( p )->map( *hbqt_par_QPainterPath( 2 ) ) ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QMATRIX_MAP_9 FP=hb_retptrGC( hbqt_gcAllocate_QPainterPath( new QPainterPath( ( p )->map( *hbqt_par_QPainterPath( 2 ) ) ), true ) ); p is NULL" ) );
   }
}

/*
 * QRectF mapRect ( const QRectF & rectangle ) const
 */
HB_FUNC( QT_QMATRIX_MAPRECT )
{
   QMatrix * p = hbqt_par_QMatrix( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QRectF( new QRectF( ( p )->mapRect( *hbqt_par_QRectF( 2 ) ) ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QMATRIX_MAPRECT FP=hb_retptrGC( hbqt_gcAllocate_QRectF( new QRectF( ( p )->mapRect( *hbqt_par_QRectF( 2 ) ) ), true ) ); p is NULL" ) );
   }
}

/*
 * QRect mapRect ( const QRect & rectangle ) const
 */
HB_FUNC( QT_QMATRIX_MAPRECT_1 )
{
   QMatrix * p = hbqt_par_QMatrix( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QRect( new QRect( ( p )->mapRect( *hbqt_par_QRect( 2 ) ) ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QMATRIX_MAPRECT_1 FP=hb_retptrGC( hbqt_gcAllocate_QRect( new QRect( ( p )->mapRect( *hbqt_par_QRect( 2 ) ) ), true ) ); p is NULL" ) );
   }
}

/*
 * QPolygon mapToPolygon ( const QRect & rectangle ) const
 */
HB_FUNC( QT_QMATRIX_MAPTOPOLYGON )
{
   QMatrix * p = hbqt_par_QMatrix( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QPolygon( new QPolygon( ( p )->mapToPolygon( *hbqt_par_QRect( 2 ) ) ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QMATRIX_MAPTOPOLYGON FP=hb_retptrGC( hbqt_gcAllocate_QPolygon( new QPolygon( ( p )->mapToPolygon( *hbqt_par_QRect( 2 ) ) ), true ) ); p is NULL" ) );
   }
}

/*
 * void reset ()
 */
HB_FUNC( QT_QMATRIX_RESET )
{
   QMatrix * p = hbqt_par_QMatrix( 1 );
   if( p )
      ( p )->reset();
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QMATRIX_RESET FP=( p )->reset(); p is NULL" ) );
   }
}

/*
 * QMatrix & rotate ( qreal degrees )
 */
HB_FUNC( QT_QMATRIX_ROTATE )
{
   QMatrix * p = hbqt_par_QMatrix( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QMatrix( new QMatrix( ( p )->rotate( hb_parnd( 2 ) ) ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QMATRIX_ROTATE FP=hb_retptrGC( hbqt_gcAllocate_QMatrix( new QMatrix( ( p )->rotate( hb_parnd( 2 ) ) ), true ) ); p is NULL" ) );
   }
}

/*
 * QMatrix & scale ( qreal sx, qreal sy )
 */
HB_FUNC( QT_QMATRIX_SCALE )
{
   QMatrix * p = hbqt_par_QMatrix( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QMatrix( new QMatrix( ( p )->scale( hb_parnd( 2 ), hb_parnd( 3 ) ) ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QMATRIX_SCALE FP=hb_retptrGC( hbqt_gcAllocate_QMatrix( new QMatrix( ( p )->scale( hb_parnd( 2 ), hb_parnd( 3 ) ) ), true ) ); p is NULL" ) );
   }
}

/*
 * void setMatrix ( qreal m11, qreal m12, qreal m21, qreal m22, qreal dx, qreal dy )
 */
HB_FUNC( QT_QMATRIX_SETMATRIX )
{
   QMatrix * p = hbqt_par_QMatrix( 1 );
   if( p )
      ( p )->setMatrix( hb_parnd( 2 ), hb_parnd( 3 ), hb_parnd( 4 ), hb_parnd( 5 ), hb_parnd( 6 ), hb_parnd( 7 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QMATRIX_SETMATRIX FP=( p )->setMatrix( hb_parnd( 2 ), hb_parnd( 3 ), hb_parnd( 4 ), hb_parnd( 5 ), hb_parnd( 6 ), hb_parnd( 7 ) ); p is NULL" ) );
   }
}

/*
 * QMatrix & shear ( qreal sh, qreal sv )
 */
HB_FUNC( QT_QMATRIX_SHEAR )
{
   QMatrix * p = hbqt_par_QMatrix( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QMatrix( new QMatrix( ( p )->shear( hb_parnd( 2 ), hb_parnd( 3 ) ) ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QMATRIX_SHEAR FP=hb_retptrGC( hbqt_gcAllocate_QMatrix( new QMatrix( ( p )->shear( hb_parnd( 2 ), hb_parnd( 3 ) ) ), true ) ); p is NULL" ) );
   }
}

/*
 * QMatrix & translate ( qreal dx, qreal dy )
 */
HB_FUNC( QT_QMATRIX_TRANSLATE )
{
   QMatrix * p = hbqt_par_QMatrix( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QMatrix( new QMatrix( ( p )->translate( hb_parnd( 2 ), hb_parnd( 3 ) ) ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QMATRIX_TRANSLATE FP=hb_retptrGC( hbqt_gcAllocate_QMatrix( new QMatrix( ( p )->translate( hb_parnd( 2 ), hb_parnd( 3 ) ) ), true ) ); p is NULL" ) );
   }
}


/*----------------------------------------------------------------------*/
#endif             /* #if QT_VERSION >= 0x040500 */
/*----------------------------------------------------------------------*/
