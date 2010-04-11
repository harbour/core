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

#include "../hbqt.h"

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
   void * ph;
   bool bNew;
   QT_G_FUNC_PTR func;
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

   p->ph = pObj;
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QMatrix;

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
   void * pObj = NULL;

   pObj = new QMatrix() ;

   hb_retptrGC( hbqt_gcAllocate_QMatrix( pObj, true ) );
}

/*
 * qreal m11 () const
 */
HB_FUNC( QT_QMATRIX_M11 )
{
   hb_retnd( hbqt_par_QMatrix( 1 )->m11() );
}

/*
 * qreal m12 () const
 */
HB_FUNC( QT_QMATRIX_M12 )
{
   hb_retnd( hbqt_par_QMatrix( 1 )->m12() );
}

/*
 * qreal m21 () const
 */
HB_FUNC( QT_QMATRIX_M21 )
{
   hb_retnd( hbqt_par_QMatrix( 1 )->m21() );
}

/*
 * qreal m22 () const
 */
HB_FUNC( QT_QMATRIX_M22 )
{
   hb_retnd( hbqt_par_QMatrix( 1 )->m22() );
}

/*
 * qreal det () const
 */
HB_FUNC( QT_QMATRIX_DET )
{
   hb_retnd( hbqt_par_QMatrix( 1 )->det() );
}

/*
 * qreal dx () const
 */
HB_FUNC( QT_QMATRIX_DX )
{
   hb_retnd( hbqt_par_QMatrix( 1 )->dx() );
}

/*
 * qreal dy () const
 */
HB_FUNC( QT_QMATRIX_DY )
{
   hb_retnd( hbqt_par_QMatrix( 1 )->dy() );
}

/*
 * QMatrix inverted ( bool * invertible = 0 ) const
 */
HB_FUNC( QT_QMATRIX_INVERTED )
{
   bool iInvertible = 0;

   hb_retptrGC( hbqt_gcAllocate_QMatrix( new QMatrix( hbqt_par_QMatrix( 1 )->inverted( &iInvertible ) ), true ) );

   hb_stornl( iInvertible, 2 );
}

/*
 * bool isIdentity () const
 */
HB_FUNC( QT_QMATRIX_ISIDENTITY )
{
   hb_retl( hbqt_par_QMatrix( 1 )->isIdentity() );
}

/*
 * bool isInvertible () const
 */
HB_FUNC( QT_QMATRIX_ISINVERTIBLE )
{
   hb_retl( hbqt_par_QMatrix( 1 )->isInvertible() );
}

/*
 * void map ( qreal x, qreal y, qreal * tx, qreal * ty ) const
 */
HB_FUNC( QT_QMATRIX_MAP )
{
   qreal qrTx = 0;
   qreal qrTy = 0;

   hbqt_par_QMatrix( 1 )->map( hb_parnd( 2 ), hb_parnd( 3 ), &qrTx, &qrTy );

   hb_stornd( qrTx, 4 );
   hb_stornd( qrTy, 5 );
}

/*
 * void map ( int x, int y, int * tx, int * ty ) const
 */
HB_FUNC( QT_QMATRIX_MAP_1 )
{
   int iTx = 0;
   int iTy = 0;

   hbqt_par_QMatrix( 1 )->map( hb_parni( 2 ), hb_parni( 3 ), &iTx, &iTy );

   hb_storni( iTx, 4 );
   hb_storni( iTy, 5 );
}

/*
 * QPointF map ( const QPointF & point ) const
 */
HB_FUNC( QT_QMATRIX_MAP_2 )
{
   hb_retptrGC( hbqt_gcAllocate_QPointF( new QPointF( hbqt_par_QMatrix( 1 )->map( *hbqt_par_QPointF( 2 ) ) ), true ) );
}

/*
 * QPoint map ( const QPoint & point ) const
 */
HB_FUNC( QT_QMATRIX_MAP_3 )
{
   hb_retptrGC( hbqt_gcAllocate_QPoint( new QPoint( hbqt_par_QMatrix( 1 )->map( *hbqt_par_QPoint( 2 ) ) ), true ) );
}

/*
 * QLineF map ( const QLineF & line ) const
 */
HB_FUNC( QT_QMATRIX_MAP_4 )
{
   hb_retptrGC( hbqt_gcAllocate_QLineF( new QLineF( hbqt_par_QMatrix( 1 )->map( *hbqt_par_QLineF( 2 ) ) ), true ) );
}

/*
 * QLine map ( const QLine & line ) const
 */
HB_FUNC( QT_QMATRIX_MAP_5 )
{
   hb_retptrGC( hbqt_gcAllocate_QLine( new QLine( hbqt_par_QMatrix( 1 )->map( *hbqt_par_QLine( 2 ) ) ), true ) );
}

/*
 * QPolygonF map ( const QPolygonF & polygon ) const
 */
HB_FUNC( QT_QMATRIX_MAP_6 )
{
   hb_retptrGC( hbqt_gcAllocate_QPolygonF( new QPolygonF( hbqt_par_QMatrix( 1 )->map( *hbqt_par_QPolygonF( 2 ) ) ), true ) );
}

/*
 * QPolygon map ( const QPolygon & polygon ) const
 */
HB_FUNC( QT_QMATRIX_MAP_7 )
{
   hb_retptrGC( hbqt_gcAllocate_QPolygon( new QPolygon( hbqt_par_QMatrix( 1 )->map( *hbqt_par_QPolygon( 2 ) ) ), true ) );
}

/*
 * QRegion map ( const QRegion & region ) const
 */
HB_FUNC( QT_QMATRIX_MAP_8 )
{
   hb_retptrGC( hbqt_gcAllocate_QRegion( new QRegion( hbqt_par_QMatrix( 1 )->map( *hbqt_par_QRegion( 2 ) ) ), true ) );
}

/*
 * QPainterPath map ( const QPainterPath & path ) const
 */
HB_FUNC( QT_QMATRIX_MAP_9 )
{
   hb_retptrGC( hbqt_gcAllocate_QPainterPath( new QPainterPath( hbqt_par_QMatrix( 1 )->map( *hbqt_par_QPainterPath( 2 ) ) ), true ) );
}

/*
 * QRectF mapRect ( const QRectF & rectangle ) const
 */
HB_FUNC( QT_QMATRIX_MAPRECT )
{
   hb_retptrGC( hbqt_gcAllocate_QRectF( new QRectF( hbqt_par_QMatrix( 1 )->mapRect( *hbqt_par_QRectF( 2 ) ) ), true ) );
}

/*
 * QRect mapRect ( const QRect & rectangle ) const
 */
HB_FUNC( QT_QMATRIX_MAPRECT_1 )
{
   hb_retptrGC( hbqt_gcAllocate_QRect( new QRect( hbqt_par_QMatrix( 1 )->mapRect( *hbqt_par_QRect( 2 ) ) ), true ) );
}

/*
 * QPolygon mapToPolygon ( const QRect & rectangle ) const
 */
HB_FUNC( QT_QMATRIX_MAPTOPOLYGON )
{
   hb_retptrGC( hbqt_gcAllocate_QPolygon( new QPolygon( hbqt_par_QMatrix( 1 )->mapToPolygon( *hbqt_par_QRect( 2 ) ) ), true ) );
}

/*
 * void reset ()
 */
HB_FUNC( QT_QMATRIX_RESET )
{
   hbqt_par_QMatrix( 1 )->reset();
}

/*
 * QMatrix & rotate ( qreal degrees )
 */
HB_FUNC( QT_QMATRIX_ROTATE )
{
   hb_retptrGC( hbqt_gcAllocate_QMatrix( new QMatrix( hbqt_par_QMatrix( 1 )->rotate( hb_parnd( 2 ) ) ), true ) );
}

/*
 * QMatrix & scale ( qreal sx, qreal sy )
 */
HB_FUNC( QT_QMATRIX_SCALE )
{
   hb_retptrGC( hbqt_gcAllocate_QMatrix( new QMatrix( hbqt_par_QMatrix( 1 )->scale( hb_parnd( 2 ), hb_parnd( 3 ) ) ), true ) );
}

/*
 * void setMatrix ( qreal m11, qreal m12, qreal m21, qreal m22, qreal dx, qreal dy )
 */
HB_FUNC( QT_QMATRIX_SETMATRIX )
{
   hbqt_par_QMatrix( 1 )->setMatrix( hb_parnd( 2 ), hb_parnd( 3 ), hb_parnd( 4 ), hb_parnd( 5 ), hb_parnd( 6 ), hb_parnd( 7 ) );
}

/*
 * QMatrix & shear ( qreal sh, qreal sv )
 */
HB_FUNC( QT_QMATRIX_SHEAR )
{
   hb_retptrGC( hbqt_gcAllocate_QMatrix( new QMatrix( hbqt_par_QMatrix( 1 )->shear( hb_parnd( 2 ), hb_parnd( 3 ) ) ), true ) );
}

/*
 * QMatrix & translate ( qreal dx, qreal dy )
 */
HB_FUNC( QT_QMATRIX_TRANSLATE )
{
   hb_retptrGC( hbqt_gcAllocate_QMatrix( new QMatrix( hbqt_par_QMatrix( 1 )->translate( hb_parnd( 2 ), hb_parnd( 3 ) ) ), true ) );
}


/*----------------------------------------------------------------------*/
#endif             /* #if QT_VERSION >= 0x040500 */
/*----------------------------------------------------------------------*/
