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
 *  Constructed[ 13/13 [ 100.00% ] ]
 *
 *
 *  *** Commented out protostypes ***
 *
 *  // void putPoints ( int index, int nPoints, int firstx, int firsty, ... )
 *  // void setPoints ( int nPoints, int firstx, int firsty, ... )
 */

#include <QtCore/QPointer>

#include <QtGui/QPolygon>


/* QPolygon ()
 * QPolygon ( int size )
 * QPolygon ( const QPolygon & polygon )
 * QPolygon ( const QVector<QPoint> & points )
 * QPolygon ( const QRect & rectangle, bool closed = false )
 * ~QPolygon ()
 */

typedef struct
{
   QPolygon * ph;
   bool bNew;
   PHBQT_GC_FUNC func;
   int type;
} HBQT_GC_T_QPolygon;

HBQT_GC_FUNC( hbqt_gcRelease_QPolygon )
{
   HBQT_GC_T * p = ( HBQT_GC_T * ) Cargo;

   if( p )
   {
      if( p->bNew && p->ph )
         delete ( ( QPolygon * ) p->ph );
      p->ph = NULL;
   }
}

void * hbqt_gcAllocate_QPolygon( void * pObj, bool bNew )
{
   HBQT_GC_T * p = ( HBQT_GC_T * ) hb_gcAllocate( sizeof( HBQT_GC_T ), hbqt_gcFuncs() );

   p->ph = ( QPolygon * ) pObj;
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QPolygon;
   p->type = HBQT_TYPE_QPolygon;

   return p;
}

HB_FUNC( QT_QPOLYGON )
{
   QPolygon * pObj = NULL;

   pObj = new QPolygon() ;

   hb_retptrGC( hbqt_gcAllocate_QPolygon( ( void * ) pObj, true ) );
}

/* QRect boundingRect () const */
HB_FUNC( QT_QPOLYGON_BOUNDINGRECT )
{
   QPolygon * p = hbqt_par_QPolygon( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QRect( new QRect( ( p )->boundingRect() ), true ) );
}

/* bool containsPoint ( const QPoint & point, Qt::FillRule fillRule ) const */
HB_FUNC( QT_QPOLYGON_CONTAINSPOINT )
{
   QPolygon * p = hbqt_par_QPolygon( 1 );
   if( p )
      hb_retl( ( p )->containsPoint( *hbqt_par_QPoint( 2 ), ( Qt::FillRule ) hb_parni( 3 ) ) );
}

/* QPolygon intersected ( const QPolygon & r ) const */
HB_FUNC( QT_QPOLYGON_INTERSECTED )
{
   QPolygon * p = hbqt_par_QPolygon( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QPolygon( new QPolygon( ( p )->intersected( *hbqt_par_QPolygon( 2 ) ) ), true ) );
}

/* void point ( int index, int * x, int * y ) const */
HB_FUNC( QT_QPOLYGON_POINT )
{
   QPolygon * p = hbqt_par_QPolygon( 1 );
   int iX = 0;
   int iY = 0;

   if( p )
      ( p )->point( hb_parni( 2 ), &iX, &iY );

   hb_storni( iX, 3 );
   hb_storni( iY, 4 );
}

/* QPoint point ( int index ) const */
HB_FUNC( QT_QPOLYGON_POINT_1 )
{
   QPolygon * p = hbqt_par_QPolygon( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QPoint( new QPoint( ( p )->point( hb_parni( 2 ) ) ), true ) );
}

/* void putPoints ( int index, int nPoints, const QPolygon & fromPolygon, int fromIndex = 0 ) */
HB_FUNC( QT_QPOLYGON_PUTPOINTS )
{
   QPolygon * p = hbqt_par_QPolygon( 1 );
   if( p )
      ( p )->putPoints( hb_parni( 2 ), hb_parni( 3 ), *hbqt_par_QPolygon( 4 ), hb_parni( 5 ) );
}

/* void setPoint ( int index, int x, int y ) */
HB_FUNC( QT_QPOLYGON_SETPOINT )
{
   QPolygon * p = hbqt_par_QPolygon( 1 );
   if( p )
      ( p )->setPoint( hb_parni( 2 ), hb_parni( 3 ), hb_parni( 4 ) );
}

/* void setPoint ( int index, const QPoint & point ) */
HB_FUNC( QT_QPOLYGON_SETPOINT_1 )
{
   QPolygon * p = hbqt_par_QPolygon( 1 );
   if( p )
      ( p )->setPoint( hb_parni( 2 ), *hbqt_par_QPoint( 3 ) );
}

/* void setPoints ( int nPoints, const int * points ) */
HB_FUNC( QT_QPOLYGON_SETPOINTS )
{
   QPolygon * p = hbqt_par_QPolygon( 1 );
   int iPoints = 0;

   if( p )
      ( p )->setPoints( hb_parni( 2 ), &iPoints );

   hb_storni( iPoints, 3 );
}

/* QPolygon subtracted ( const QPolygon & r ) const */
HB_FUNC( QT_QPOLYGON_SUBTRACTED )
{
   QPolygon * p = hbqt_par_QPolygon( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QPolygon( new QPolygon( ( p )->subtracted( *hbqt_par_QPolygon( 2 ) ) ), true ) );
}

/* void translate ( int dx, int dy ) */
HB_FUNC( QT_QPOLYGON_TRANSLATE )
{
   QPolygon * p = hbqt_par_QPolygon( 1 );
   if( p )
      ( p )->translate( hb_parni( 2 ), hb_parni( 3 ) );
}

/* void translate ( const QPoint & offset ) */
HB_FUNC( QT_QPOLYGON_TRANSLATE_1 )
{
   QPolygon * p = hbqt_par_QPolygon( 1 );
   if( p )
      ( p )->translate( *hbqt_par_QPoint( 2 ) );
}

/* QPolygon united ( const QPolygon & r ) const */
HB_FUNC( QT_QPOLYGON_UNITED )
{
   QPolygon * p = hbqt_par_QPolygon( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QPolygon( new QPolygon( ( p )->united( *hbqt_par_QPolygon( 2 ) ) ), true ) );
}


#endif /* #if QT_VERSION >= 0x040500 */
