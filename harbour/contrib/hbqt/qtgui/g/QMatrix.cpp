/*
 * $Id$
 */

/* -------------------------------------------------------------------- */
/* WARNING: Automatically generated source file. DO NOT EDIT!           */
/*          Instead, edit corresponding .qth file,                      */
/*          or the generator tool itself, and run regenarate.           */
/* -------------------------------------------------------------------- */

/*
 * Harbour Project QT wrapper
 *
 * Copyright 2009-2010 Pritpal Bedi <bedipritpal@hotmail.com>
 * www - http://harbour-project.org
 *
 * For full copyright message and credits, see: CREDITS.txt
 *
 */

#include "hbqtcore.h"
#include "hbqtgui.h"

#if QT_VERSION >= 0x040500

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

   if( p )
   {
      if( p->bNew && p->ph )
         delete ( ( QMatrix * ) p->ph );
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

   return p;
}

HB_FUNC( QT_QMATRIX )
{
   QMatrix * pObj = NULL;

   pObj = new QMatrix() ;

   hb_retptrGC( hbqt_gcAllocate_QMatrix( ( void * ) pObj, true ) );
}

/* qreal m11 () const */
HB_FUNC( QT_QMATRIX_M11 )
{
   QMatrix * p = hbqt_par_QMatrix( 1 );
   if( p )
      hb_retnd( ( p )->m11() );
}

/* qreal m12 () const */
HB_FUNC( QT_QMATRIX_M12 )
{
   QMatrix * p = hbqt_par_QMatrix( 1 );
   if( p )
      hb_retnd( ( p )->m12() );
}

/* qreal m21 () const */
HB_FUNC( QT_QMATRIX_M21 )
{
   QMatrix * p = hbqt_par_QMatrix( 1 );
   if( p )
      hb_retnd( ( p )->m21() );
}

/* qreal m22 () const */
HB_FUNC( QT_QMATRIX_M22 )
{
   QMatrix * p = hbqt_par_QMatrix( 1 );
   if( p )
      hb_retnd( ( p )->m22() );
}

/* qreal det () const */
HB_FUNC( QT_QMATRIX_DET )
{
   QMatrix * p = hbqt_par_QMatrix( 1 );
   if( p )
      hb_retnd( ( p )->det() );
}

/* qreal dx () const */
HB_FUNC( QT_QMATRIX_DX )
{
   QMatrix * p = hbqt_par_QMatrix( 1 );
   if( p )
      hb_retnd( ( p )->dx() );
}

/* qreal dy () const */
HB_FUNC( QT_QMATRIX_DY )
{
   QMatrix * p = hbqt_par_QMatrix( 1 );
   if( p )
      hb_retnd( ( p )->dy() );
}

/* QMatrix inverted ( bool * invertible = 0 ) const */
HB_FUNC( QT_QMATRIX_INVERTED )
{
   QMatrix * p = hbqt_par_QMatrix( 1 );
   bool iInvertible = 0;

   if( p )
      hb_retptrGC( hbqt_gcAllocate_QMatrix( new QMatrix( ( p )->inverted( &iInvertible ) ), true ) );

   hb_stornl( iInvertible, 2 );
}

/* bool isIdentity () const */
HB_FUNC( QT_QMATRIX_ISIDENTITY )
{
   QMatrix * p = hbqt_par_QMatrix( 1 );
   if( p )
      hb_retl( ( p )->isIdentity() );
}

/* bool isInvertible () const */
HB_FUNC( QT_QMATRIX_ISINVERTIBLE )
{
   QMatrix * p = hbqt_par_QMatrix( 1 );
   if( p )
      hb_retl( ( p )->isInvertible() );
}

/* void map ( qreal x, qreal y, qreal * tx, qreal * ty ) const */
HB_FUNC( QT_QMATRIX_MAP )
{
   QMatrix * p = hbqt_par_QMatrix( 1 );
   qreal qrTx = 0;
   qreal qrTy = 0;

   if( p )
      ( p )->map( hb_parnd( 2 ), hb_parnd( 3 ), &qrTx, &qrTy );

   hb_stornd( qrTx, 4 );
   hb_stornd( qrTy, 5 );
}

/* void map ( int x, int y, int * tx, int * ty ) const */
HB_FUNC( QT_QMATRIX_MAP_1 )
{
   QMatrix * p = hbqt_par_QMatrix( 1 );
   int iTx = 0;
   int iTy = 0;

   if( p )
      ( p )->map( hb_parni( 2 ), hb_parni( 3 ), &iTx, &iTy );

   hb_storni( iTx, 4 );
   hb_storni( iTy, 5 );
}

/* QPointF map ( const QPointF & point ) const */
HB_FUNC( QT_QMATRIX_MAP_2 )
{
   QMatrix * p = hbqt_par_QMatrix( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QPointF( new QPointF( ( p )->map( *hbqt_par_QPointF( 2 ) ) ), true ) );
}

/* QPoint map ( const QPoint & point ) const */
HB_FUNC( QT_QMATRIX_MAP_3 )
{
   QMatrix * p = hbqt_par_QMatrix( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QPoint( new QPoint( ( p )->map( *hbqt_par_QPoint( 2 ) ) ), true ) );
}

/* QLineF map ( const QLineF & line ) const */
HB_FUNC( QT_QMATRIX_MAP_4 )
{
   QMatrix * p = hbqt_par_QMatrix( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QLineF( new QLineF( ( p )->map( *hbqt_par_QLineF( 2 ) ) ), true ) );
}

/* QLine map ( const QLine & line ) const */
HB_FUNC( QT_QMATRIX_MAP_5 )
{
   QMatrix * p = hbqt_par_QMatrix( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QLine( new QLine( ( p )->map( *hbqt_par_QLine( 2 ) ) ), true ) );
}

/* QPolygonF map ( const QPolygonF & polygon ) const */
HB_FUNC( QT_QMATRIX_MAP_6 )
{
   QMatrix * p = hbqt_par_QMatrix( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QPolygonF( new QPolygonF( ( p )->map( *hbqt_par_QPolygonF( 2 ) ) ), true ) );
}

/* QPolygon map ( const QPolygon & polygon ) const */
HB_FUNC( QT_QMATRIX_MAP_7 )
{
   QMatrix * p = hbqt_par_QMatrix( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QPolygon( new QPolygon( ( p )->map( *hbqt_par_QPolygon( 2 ) ) ), true ) );
}

/* QRegion map ( const QRegion & region ) const */
HB_FUNC( QT_QMATRIX_MAP_8 )
{
   QMatrix * p = hbqt_par_QMatrix( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QRegion( new QRegion( ( p )->map( *hbqt_par_QRegion( 2 ) ) ), true ) );
}

/* QPainterPath map ( const QPainterPath & path ) const */
HB_FUNC( QT_QMATRIX_MAP_9 )
{
   QMatrix * p = hbqt_par_QMatrix( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QPainterPath( new QPainterPath( ( p )->map( *hbqt_par_QPainterPath( 2 ) ) ), true ) );
}

/* QRectF mapRect ( const QRectF & rectangle ) const */
HB_FUNC( QT_QMATRIX_MAPRECT )
{
   QMatrix * p = hbqt_par_QMatrix( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QRectF( new QRectF( ( p )->mapRect( *hbqt_par_QRectF( 2 ) ) ), true ) );
}

/* QRect mapRect ( const QRect & rectangle ) const */
HB_FUNC( QT_QMATRIX_MAPRECT_1 )
{
   QMatrix * p = hbqt_par_QMatrix( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QRect( new QRect( ( p )->mapRect( *hbqt_par_QRect( 2 ) ) ), true ) );
}

/* QPolygon mapToPolygon ( const QRect & rectangle ) const */
HB_FUNC( QT_QMATRIX_MAPTOPOLYGON )
{
   QMatrix * p = hbqt_par_QMatrix( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QPolygon( new QPolygon( ( p )->mapToPolygon( *hbqt_par_QRect( 2 ) ) ), true ) );
}

/* void reset () */
HB_FUNC( QT_QMATRIX_RESET )
{
   QMatrix * p = hbqt_par_QMatrix( 1 );
   if( p )
      ( p )->reset();
}

/* QMatrix & rotate ( qreal degrees ) */
HB_FUNC( QT_QMATRIX_ROTATE )
{
   QMatrix * p = hbqt_par_QMatrix( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QMatrix( new QMatrix( ( p )->rotate( hb_parnd( 2 ) ) ), true ) );
}

/* QMatrix & scale ( qreal sx, qreal sy ) */
HB_FUNC( QT_QMATRIX_SCALE )
{
   QMatrix * p = hbqt_par_QMatrix( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QMatrix( new QMatrix( ( p )->scale( hb_parnd( 2 ), hb_parnd( 3 ) ) ), true ) );
}

/* void setMatrix ( qreal m11, qreal m12, qreal m21, qreal m22, qreal dx, qreal dy ) */
HB_FUNC( QT_QMATRIX_SETMATRIX )
{
   QMatrix * p = hbqt_par_QMatrix( 1 );
   if( p )
      ( p )->setMatrix( hb_parnd( 2 ), hb_parnd( 3 ), hb_parnd( 4 ), hb_parnd( 5 ), hb_parnd( 6 ), hb_parnd( 7 ) );
}

/* QMatrix & shear ( qreal sh, qreal sv ) */
HB_FUNC( QT_QMATRIX_SHEAR )
{
   QMatrix * p = hbqt_par_QMatrix( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QMatrix( new QMatrix( ( p )->shear( hb_parnd( 2 ), hb_parnd( 3 ) ) ), true ) );
}

/* QMatrix & translate ( qreal dx, qreal dy ) */
HB_FUNC( QT_QMATRIX_TRANSLATE )
{
   QMatrix * p = hbqt_par_QMatrix( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QMatrix( new QMatrix( ( p )->translate( hb_parnd( 2 ), hb_parnd( 3 ) ) ), true ) );
}


#endif /* #if QT_VERSION >= 0x040500 */
