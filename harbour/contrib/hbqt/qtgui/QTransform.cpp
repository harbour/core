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
 *  enum TransformationType { TxNone, TxTranslate, TxScale, TxRotate, TxShear, TxProject }
 */

#include <QtCore/QPointer>

#include <QtGui/QTransform>


/* QTransform ()
 * QTransform ( qreal m11, qreal m12, qreal m13, qreal m21, qreal m22, qreal m23, qreal m31, qreal m32, qreal m33 = 1.0 )
 * QTransform ( qreal m11, qreal m12, qreal m21, qreal m22, qreal dx, qreal dy )
 * QTransform ( const QMatrix & matrix )
 */

QT_G_FUNC( release_QTransform )
{
#if defined(__debug__)
   hb_snprintf( str, sizeof(str), "release_QTransform" );  OutputDebugString( str );
#endif
   void * ph = ( void * ) Cargo;
   if( ph )
   {
      delete ( ( QTransform * ) ph );
      ph = NULL;
   }
}

HB_FUNC( QT_QTRANSFORM )
{
   QGC_POINTER * p = ( QGC_POINTER * ) hb_gcAlloc( sizeof( QGC_POINTER ), Q_release );
   void * pObj = NULL;

   pObj = new QTransform() ;

   p->ph = pObj;
   p->func = release_QTransform;

   hb_retptrGC( p );
}
/*
 * qreal m11 () const
 */
HB_FUNC( QT_QTRANSFORM_M11 )
{
   hb_retnd( hbqt_par_QTransform( 1 )->m11() );
}

/*
 * qreal m12 () const
 */
HB_FUNC( QT_QTRANSFORM_M12 )
{
   hb_retnd( hbqt_par_QTransform( 1 )->m12() );
}

/*
 * qreal m13 () const
 */
HB_FUNC( QT_QTRANSFORM_M13 )
{
   hb_retnd( hbqt_par_QTransform( 1 )->m13() );
}

/*
 * qreal m21 () const
 */
HB_FUNC( QT_QTRANSFORM_M21 )
{
   hb_retnd( hbqt_par_QTransform( 1 )->m21() );
}

/*
 * qreal m22 () const
 */
HB_FUNC( QT_QTRANSFORM_M22 )
{
   hb_retnd( hbqt_par_QTransform( 1 )->m22() );
}

/*
 * qreal m23 () const
 */
HB_FUNC( QT_QTRANSFORM_M23 )
{
   hb_retnd( hbqt_par_QTransform( 1 )->m23() );
}

/*
 * qreal m31 () const
 */
HB_FUNC( QT_QTRANSFORM_M31 )
{
   hb_retnd( hbqt_par_QTransform( 1 )->m31() );
}

/*
 * qreal m32 () const
 */
HB_FUNC( QT_QTRANSFORM_M32 )
{
   hb_retnd( hbqt_par_QTransform( 1 )->m32() );
}

/*
 * qreal m33 () const
 */
HB_FUNC( QT_QTRANSFORM_M33 )
{
   hb_retnd( hbqt_par_QTransform( 1 )->m33() );
}

/*
 * QTransform adjoint () const
 */
HB_FUNC( QT_QTRANSFORM_ADJOINT )
{
   hb_retptrGC( hbqt_ptrTOgcpointer( new QTransform( hbqt_par_QTransform( 1 )->adjoint() ), release_QTransform ) );
}

/*
 * qreal det () const
 */
HB_FUNC( QT_QTRANSFORM_DET )
{
   hb_retnd( hbqt_par_QTransform( 1 )->det() );
}

/*
 * qreal determinant () const
 */
HB_FUNC( QT_QTRANSFORM_DETERMINANT )
{
   hb_retnd( hbqt_par_QTransform( 1 )->determinant() );
}

/*
 * qreal dx () const
 */
HB_FUNC( QT_QTRANSFORM_DX )
{
   hb_retnd( hbqt_par_QTransform( 1 )->dx() );
}

/*
 * qreal dy () const
 */
HB_FUNC( QT_QTRANSFORM_DY )
{
   hb_retnd( hbqt_par_QTransform( 1 )->dy() );
}

/*
 * QTransform inverted ( bool * invertible = 0 ) const
 */
HB_FUNC( QT_QTRANSFORM_INVERTED )
{
   bool iInvertible = 0;

   hb_retptrGC( hbqt_ptrTOgcpointer( new QTransform( hbqt_par_QTransform( 1 )->inverted( &iInvertible ) ), release_QTransform ) );

   hb_stornl( iInvertible, 2 );
}

/*
 * bool isAffine () const
 */
HB_FUNC( QT_QTRANSFORM_ISAFFINE )
{
   hb_retl( hbqt_par_QTransform( 1 )->isAffine() );
}

/*
 * bool isIdentity () const
 */
HB_FUNC( QT_QTRANSFORM_ISIDENTITY )
{
   hb_retl( hbqt_par_QTransform( 1 )->isIdentity() );
}

/*
 * bool isInvertible () const
 */
HB_FUNC( QT_QTRANSFORM_ISINVERTIBLE )
{
   hb_retl( hbqt_par_QTransform( 1 )->isInvertible() );
}

/*
 * bool isRotating () const
 */
HB_FUNC( QT_QTRANSFORM_ISROTATING )
{
   hb_retl( hbqt_par_QTransform( 1 )->isRotating() );
}

/*
 * bool isScaling () const
 */
HB_FUNC( QT_QTRANSFORM_ISSCALING )
{
   hb_retl( hbqt_par_QTransform( 1 )->isScaling() );
}

/*
 * bool isTranslating () const
 */
HB_FUNC( QT_QTRANSFORM_ISTRANSLATING )
{
   hb_retl( hbqt_par_QTransform( 1 )->isTranslating() );
}

/*
 * void map ( qreal x, qreal y, qreal * tx, qreal * ty ) const
 */
HB_FUNC( QT_QTRANSFORM_MAP )
{
   qreal qrTx = 0;
   qreal qrTy = 0;

   hbqt_par_QTransform( 1 )->map( hb_parnd( 2 ), hb_parnd( 3 ), &qrTx, &qrTy );

   hb_stornd( qrTx, 4 );
   hb_stornd( qrTy, 5 );
}

/*
 * QPointF map ( const QPointF & p ) const
 */
HB_FUNC( QT_QTRANSFORM_MAP_1 )
{
   hb_retptrGC( hbqt_ptrTOgcpointer( new QPointF( hbqt_par_QTransform( 1 )->map( *hbqt_par_QPointF( 2 ) ) ), release_QPointF ) );
}

/*
 * QPoint map ( const QPoint & point ) const
 */
HB_FUNC( QT_QTRANSFORM_MAP_2 )
{
   hb_retptrGC( hbqt_ptrTOgcpointer( new QPoint( hbqt_par_QTransform( 1 )->map( *hbqt_par_QPoint( 2 ) ) ), release_QPoint ) );
}

/*
 * QLine map ( const QLine & l ) const
 */
HB_FUNC( QT_QTRANSFORM_MAP_3 )
{
   hb_retptrGC( hbqt_ptrTOgcpointer( new QLine( hbqt_par_QTransform( 1 )->map( *hbqt_par_QLine( 2 ) ) ), release_QLine ) );
}

/*
 * QLineF map ( const QLineF & line ) const
 */
HB_FUNC( QT_QTRANSFORM_MAP_4 )
{
   hb_retptrGC( hbqt_ptrTOgcpointer( new QLineF( hbqt_par_QTransform( 1 )->map( *hbqt_par_QLineF( 2 ) ) ), release_QLineF ) );
}

/*
 * QPolygonF map ( const QPolygonF & polygon ) const
 */
HB_FUNC( QT_QTRANSFORM_MAP_5 )
{
   hb_retptrGC( hbqt_ptrTOgcpointer( new QPolygonF( hbqt_par_QTransform( 1 )->map( *hbqt_par_QPolygonF( 2 ) ) ), release_QPolygonF ) );
}

/*
 * QPolygon map ( const QPolygon & polygon ) const
 */
HB_FUNC( QT_QTRANSFORM_MAP_6 )
{
   hb_retptrGC( hbqt_ptrTOgcpointer( new QPolygon( hbqt_par_QTransform( 1 )->map( *hbqt_par_QPolygon( 2 ) ) ), release_QPolygon ) );
}

/*
 * QRegion map ( const QRegion & region ) const
 */
HB_FUNC( QT_QTRANSFORM_MAP_7 )
{
   hb_retptrGC( hbqt_ptrTOgcpointer( new QRegion( hbqt_par_QTransform( 1 )->map( *hbqt_par_QRegion( 2 ) ) ), release_QRegion ) );
}

/*
 * QPainterPath map ( const QPainterPath & path ) const
 */
HB_FUNC( QT_QTRANSFORM_MAP_8 )
{
   hb_retptrGC( hbqt_ptrTOgcpointer( new QPainterPath( hbqt_par_QTransform( 1 )->map( *hbqt_par_QPainterPath( 2 ) ) ), release_QPainterPath ) );
}

/*
 * void map ( int x, int y, int * tx, int * ty ) const
 */
HB_FUNC( QT_QTRANSFORM_MAP_9 )
{
   int iTx = 0;
   int iTy = 0;

   hbqt_par_QTransform( 1 )->map( hb_parni( 2 ), hb_parni( 3 ), &iTx, &iTy );

   hb_storni( iTx, 4 );
   hb_storni( iTy, 5 );
}

/*
 * QRectF mapRect ( const QRectF & rectangle ) const
 */
HB_FUNC( QT_QTRANSFORM_MAPRECT )
{
   hb_retptrGC( hbqt_ptrTOgcpointer( new QRectF( hbqt_par_QTransform( 1 )->mapRect( *hbqt_par_QRectF( 2 ) ) ), release_QRectF ) );
}

/*
 * QRect mapRect ( const QRect & rectangle ) const
 */
HB_FUNC( QT_QTRANSFORM_MAPRECT_1 )
{
   hb_retptrGC( hbqt_ptrTOgcpointer( new QRect( hbqt_par_QTransform( 1 )->mapRect( *hbqt_par_QRect( 2 ) ) ), release_QRect ) );
}

/*
 * QPolygon mapToPolygon ( const QRect & rectangle ) const
 */
HB_FUNC( QT_QTRANSFORM_MAPTOPOLYGON )
{
   hb_retptrGC( hbqt_ptrTOgcpointer( new QPolygon( hbqt_par_QTransform( 1 )->mapToPolygon( *hbqt_par_QRect( 2 ) ) ), release_QPolygon ) );
}

/*
 * void reset ()
 */
HB_FUNC( QT_QTRANSFORM_RESET )
{
   hbqt_par_QTransform( 1 )->reset();
}

/*
 * QTransform & rotate ( qreal angle, Qt::Axis axis = Qt::ZAxis )
 */
HB_FUNC( QT_QTRANSFORM_ROTATE )
{
   hb_retptrGC( hbqt_ptrTOgcpointer( new QTransform( hbqt_par_QTransform( 1 )->rotate( hb_parnd( 2 ), ( HB_ISNUM( 3 ) ? ( Qt::Axis ) hb_parni( 3 ) : ( Qt::Axis ) Qt::ZAxis ) ) ), release_QTransform ) );
}

/*
 * QTransform & rotateRadians ( qreal angle, Qt::Axis axis = Qt::ZAxis )
 */
HB_FUNC( QT_QTRANSFORM_ROTATERADIANS )
{
   hb_retptrGC( hbqt_ptrTOgcpointer( new QTransform( hbqt_par_QTransform( 1 )->rotateRadians( hb_parnd( 2 ), ( HB_ISNUM( 3 ) ? ( Qt::Axis ) hb_parni( 3 ) : ( Qt::Axis ) Qt::ZAxis ) ) ), release_QTransform ) );
}

/*
 * QTransform & scale ( qreal sx, qreal sy )
 */
HB_FUNC( QT_QTRANSFORM_SCALE )
{
   hb_retptrGC( hbqt_ptrTOgcpointer( new QTransform( hbqt_par_QTransform( 1 )->scale( hb_parnd( 2 ), hb_parnd( 3 ) ) ), release_QTransform ) );
}

/*
 * void setMatrix ( qreal m11, qreal m12, qreal m13, qreal m21, qreal m22, qreal m23, qreal m31, qreal m32, qreal m33 )
 */
HB_FUNC( QT_QTRANSFORM_SETMATRIX )
{
   hbqt_par_QTransform( 1 )->setMatrix( hb_parnd( 2 ), hb_parnd( 3 ), hb_parnd( 4 ), hb_parnd( 5 ), hb_parnd( 6 ), hb_parnd( 7 ), hb_parnd( 8 ), hb_parnd( 9 ), hb_parnd( 10 ) );
}

/*
 * QTransform & shear ( qreal sh, qreal sv )
 */
HB_FUNC( QT_QTRANSFORM_SHEAR )
{
   hb_retptrGC( hbqt_ptrTOgcpointer( new QTransform( hbqt_par_QTransform( 1 )->shear( hb_parnd( 2 ), hb_parnd( 3 ) ) ), release_QTransform ) );
}

/*
 * const QMatrix & toAffine () const
 */
HB_FUNC( QT_QTRANSFORM_TOAFFINE )
{
   hb_retptrGC( hbqt_ptrTOgcpointer( new QMatrix( hbqt_par_QTransform( 1 )->toAffine() ), release_QMatrix ) );
}

/*
 * QTransform & translate ( qreal dx, qreal dy )
 */
HB_FUNC( QT_QTRANSFORM_TRANSLATE )
{
   hb_retptrGC( hbqt_ptrTOgcpointer( new QTransform( hbqt_par_QTransform( 1 )->translate( hb_parnd( 2 ), hb_parnd( 3 ) ) ), release_QTransform ) );
}

/*
 * QTransform transposed () const
 */
HB_FUNC( QT_QTRANSFORM_TRANSPOSED )
{
   hb_retptrGC( hbqt_ptrTOgcpointer( new QTransform( hbqt_par_QTransform( 1 )->transposed() ), release_QTransform ) );
}

/*
 * TransformationType type () const
 */
HB_FUNC( QT_QTRANSFORM_TYPE )
{
   hb_retni( ( QTransform::TransformationType ) hbqt_par_QTransform( 1 )->type() );
}

/*
 * QTransform fromScale ( qreal sx, qreal sy )
 */
HB_FUNC( QT_QTRANSFORM_FROMSCALE )
{
   hb_retptrGC( hbqt_ptrTOgcpointer( new QTransform( hbqt_par_QTransform( 1 )->fromScale( hb_parnd( 2 ), hb_parnd( 3 ) ) ), release_QTransform ) );
}

/*
 * QTransform fromTranslate ( qreal dx, qreal dy )
 */
HB_FUNC( QT_QTRANSFORM_FROMTRANSLATE )
{
   hb_retptrGC( hbqt_ptrTOgcpointer( new QTransform( hbqt_par_QTransform( 1 )->fromTranslate( hb_parnd( 2 ), hb_parnd( 3 ) ) ), release_QTransform ) );
}

/*
 * bool quadToQuad ( const QPolygonF & one, const QPolygonF & two, QTransform & trans )
 */
HB_FUNC( QT_QTRANSFORM_QUADTOQUAD )
{
   hb_retl( hbqt_par_QTransform( 1 )->quadToQuad( *hbqt_par_QPolygonF( 2 ), *hbqt_par_QPolygonF( 3 ), *hbqt_par_QTransform( 4 ) ) );
}

/*
 * bool quadToSquare ( const QPolygonF & quad, QTransform & trans )
 */
HB_FUNC( QT_QTRANSFORM_QUADTOSQUARE )
{
   hb_retl( hbqt_par_QTransform( 1 )->quadToSquare( *hbqt_par_QPolygonF( 2 ), *hbqt_par_QTransform( 3 ) ) );
}

/*
 * bool squareToQuad ( const QPolygonF & quad, QTransform & trans )
 */
HB_FUNC( QT_QTRANSFORM_SQUARETOQUAD )
{
   hb_retl( hbqt_par_QTransform( 1 )->squareToQuad( *hbqt_par_QPolygonF( 2 ), *hbqt_par_QTransform( 3 ) ) );
}


/*----------------------------------------------------------------------*/
#endif             /* #if QT_VERSION >= 0x040500 */
/*----------------------------------------------------------------------*/
