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
 *  Constructed[ 9/9 [ 100.00% ] ]
 *
 */

#include <QtCore/QPointer>

#include <QtGui/QPolygonF>


/* QPolygonF ()
 * QPolygonF ( int size )
 * QPolygonF ( const QPolygonF & polygon )
 * QPolygonF ( const QVector<QPointF> & points )
 * QPolygonF ( const QRectF & rectangle )
 * QPolygonF ( const QPolygon & polygon )
 * ~QPolygonF ()
 */

typedef struct
{
   QPolygonF * ph;
   bool bNew;
   PHBQT_GC_FUNC func;
   int type;
} HBQT_GC_T_QPolygonF;

HBQT_GC_FUNC( hbqt_gcRelease_QPolygonF )
{
   HBQT_GC_T * p = ( HBQT_GC_T * ) Cargo;

   if( p )
   {
      if( p->bNew && p->ph )
         delete ( ( QPolygonF * ) p->ph );
      p->ph = NULL;
   }
}

void * hbqt_gcAllocate_QPolygonF( void * pObj, bool bNew )
{
   HBQT_GC_T * p = ( HBQT_GC_T * ) hb_gcAllocate( sizeof( HBQT_GC_T ), hbqt_gcFuncs() );

   p->ph = ( QPolygonF * ) pObj;
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QPolygonF;
   p->type = HBQT_TYPE_QPolygonF;

   return p;
}

HB_FUNC( QT_QPOLYGONF )
{
   QPolygonF * pObj = NULL;

   pObj = new QPolygonF() ;

   hb_retptrGC( hbqt_gcAllocate_QPolygonF( ( void * ) pObj, true ) );
}

/* QRectF boundingRect () const */
HB_FUNC( QT_QPOLYGONF_BOUNDINGRECT )
{
   QPolygonF * p = hbqt_par_QPolygonF( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QRectF( new QRectF( ( p )->boundingRect() ), true ) );
}

/* bool containsPoint ( const QPointF & point, Qt::FillRule fillRule ) const */
HB_FUNC( QT_QPOLYGONF_CONTAINSPOINT )
{
   QPolygonF * p = hbqt_par_QPolygonF( 1 );
   if( p )
      hb_retl( ( p )->containsPoint( *hbqt_par_QPointF( 2 ), ( Qt::FillRule ) hb_parni( 3 ) ) );
}

/* QPolygonF intersected ( const QPolygonF & r ) const */
HB_FUNC( QT_QPOLYGONF_INTERSECTED )
{
   QPolygonF * p = hbqt_par_QPolygonF( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QPolygonF( new QPolygonF( ( p )->intersected( *hbqt_par_QPolygonF( 2 ) ) ), true ) );
}

/* bool isClosed () const */
HB_FUNC( QT_QPOLYGONF_ISCLOSED )
{
   QPolygonF * p = hbqt_par_QPolygonF( 1 );
   if( p )
      hb_retl( ( p )->isClosed() );
}

/* QPolygonF subtracted ( const QPolygonF & r ) const */
HB_FUNC( QT_QPOLYGONF_SUBTRACTED )
{
   QPolygonF * p = hbqt_par_QPolygonF( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QPolygonF( new QPolygonF( ( p )->subtracted( *hbqt_par_QPolygonF( 2 ) ) ), true ) );
}

/* QPolygon toPolygon () const */
HB_FUNC( QT_QPOLYGONF_TOPOLYGON )
{
   QPolygonF * p = hbqt_par_QPolygonF( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QPolygon( new QPolygon( ( p )->toPolygon() ), true ) );
}

/* void translate ( const QPointF & offset ) */
HB_FUNC( QT_QPOLYGONF_TRANSLATE )
{
   QPolygonF * p = hbqt_par_QPolygonF( 1 );
   if( p )
      ( p )->translate( *hbqt_par_QPointF( 2 ) );
}

/* void translate ( qreal dx, qreal dy ) */
HB_FUNC( QT_QPOLYGONF_TRANSLATE_1 )
{
   QPolygonF * p = hbqt_par_QPolygonF( 1 );
   if( p )
      ( p )->translate( hb_parnd( 2 ), hb_parnd( 3 ) );
}

/* QPolygonF united ( const QPolygonF & r ) const */
HB_FUNC( QT_QPOLYGONF_UNITED )
{
   QPolygonF * p = hbqt_par_QPolygonF( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QPolygonF( new QPolygonF( ( p )->united( *hbqt_par_QPolygonF( 2 ) ) ), true ) );
}


#endif /* #if QT_VERSION >= 0x040500 */
