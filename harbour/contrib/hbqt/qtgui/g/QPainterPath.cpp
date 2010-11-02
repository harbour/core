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
 *  enum ElementType { MoveToElement, LineToElement, CurveToElement, CurveToDataElement }
 */

/*
 *  Constructed[ 55/55 [ 100.00% ] ]
 *
 *
 *  *** Commented out protostypes ***
 *
 *  // const QPainterPath::Element & elementAt ( int index ) const
 */

#include <QtCore/QPointer>

#include <QtGui/QPainterPath>


/* QPainterPath ()
 * QPainterPath ( const QPointF & startPoint )
 * QPainterPath ( const QPainterPath & path )
 * ~QPainterPath ()
 */

typedef struct
{
   QPainterPath * ph;
   bool bNew;
   PHBQT_GC_FUNC func;
   int type;
} HBQT_GC_T_QPainterPath;

HBQT_GC_FUNC( hbqt_gcRelease_QPainterPath )
{
   HBQT_GC_T * p = ( HBQT_GC_T * ) Cargo;

   if( p && p->bNew )
   {
      if( p->ph )
      {
         delete ( ( QPainterPath * ) p->ph );
         p->ph = NULL;
      }
      else
         p->ph = NULL;
   }
   else
      p->ph = NULL;
}

void * hbqt_gcAllocate_QPainterPath( void * pObj, bool bNew )
{
   HBQT_GC_T * p = ( HBQT_GC_T * ) hb_gcAllocate( sizeof( HBQT_GC_T ), hbqt_gcFuncs() );

   p->ph = ( QPainterPath * ) pObj;
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QPainterPath;
   p->type = HBQT_TYPE_QPainterPath;

   return p;
}

HB_FUNC( QT_QPAINTERPATH )
{
   QPainterPath * pObj = NULL;

   pObj = new QPainterPath() ;

   hb_retptrGC( hbqt_gcAllocate_QPainterPath( ( void * ) pObj, true ) );
}

/* void addEllipse ( const QRectF & boundingRectangle ) */
HB_FUNC( QT_QPAINTERPATH_ADDELLIPSE )
{
   QPainterPath * p = hbqt_par_QPainterPath( 1 );
   if( p )
      ( p )->addEllipse( *hbqt_par_QRectF( 2 ) );
}

/* void addEllipse ( qreal x, qreal y, qreal width, qreal height ) */
HB_FUNC( QT_QPAINTERPATH_ADDELLIPSE_1 )
{
   QPainterPath * p = hbqt_par_QPainterPath( 1 );
   if( p )
      ( p )->addEllipse( hb_parnd( 2 ), hb_parnd( 3 ), hb_parnd( 4 ), hb_parnd( 5 ) );
}

/* void addEllipse ( const QPointF & center, qreal rx, qreal ry ) */
HB_FUNC( QT_QPAINTERPATH_ADDELLIPSE_2 )
{
   QPainterPath * p = hbqt_par_QPainterPath( 1 );
   if( p )
      ( p )->addEllipse( *hbqt_par_QPointF( 2 ), hb_parnd( 3 ), hb_parnd( 4 ) );
}

/* void addPath ( const QPainterPath & path ) */
HB_FUNC( QT_QPAINTERPATH_ADDPATH )
{
   QPainterPath * p = hbqt_par_QPainterPath( 1 );
   if( p )
      ( p )->addPath( *hbqt_par_QPainterPath( 2 ) );
}

/* void addPolygon ( const QPolygonF & polygon ) */
HB_FUNC( QT_QPAINTERPATH_ADDPOLYGON )
{
   QPainterPath * p = hbqt_par_QPainterPath( 1 );
   if( p )
      ( p )->addPolygon( *hbqt_par_QPolygonF( 2 ) );
}

/* void addRect ( const QRectF & rectangle ) */
HB_FUNC( QT_QPAINTERPATH_ADDRECT )
{
   QPainterPath * p = hbqt_par_QPainterPath( 1 );
   if( p )
      ( p )->addRect( *hbqt_par_QRectF( 2 ) );
}

/* void addRect ( qreal x, qreal y, qreal width, qreal height ) */
HB_FUNC( QT_QPAINTERPATH_ADDRECT_1 )
{
   QPainterPath * p = hbqt_par_QPainterPath( 1 );
   if( p )
      ( p )->addRect( hb_parnd( 2 ), hb_parnd( 3 ), hb_parnd( 4 ), hb_parnd( 5 ) );
}

/* void addRegion ( const QRegion & region ) */
HB_FUNC( QT_QPAINTERPATH_ADDREGION )
{
   QPainterPath * p = hbqt_par_QPainterPath( 1 );
   if( p )
      ( p )->addRegion( *hbqt_par_QRegion( 2 ) );
}

/* void addRoundedRect ( const QRectF & rect, qreal xRadius, qreal yRadius, Qt::SizeMode mode = Qt::AbsoluteSize ) */
HB_FUNC( QT_QPAINTERPATH_ADDROUNDEDRECT )
{
   QPainterPath * p = hbqt_par_QPainterPath( 1 );
   if( p )
      ( p )->addRoundedRect( *hbqt_par_QRectF( 2 ), hb_parnd( 3 ), hb_parnd( 4 ), ( HB_ISNUM( 5 ) ? ( Qt::SizeMode ) hb_parni( 5 ) : ( Qt::SizeMode ) Qt::AbsoluteSize ) );
}

/* void addRoundedRect ( qreal x, qreal y, qreal w, qreal h, qreal xRadius, qreal yRadius, Qt::SizeMode mode = Qt::AbsoluteSize ) */
HB_FUNC( QT_QPAINTERPATH_ADDROUNDEDRECT_1 )
{
   QPainterPath * p = hbqt_par_QPainterPath( 1 );
   if( p )
      ( p )->addRoundedRect( hb_parnd( 2 ), hb_parnd( 3 ), hb_parnd( 4 ), hb_parnd( 5 ), hb_parnd( 6 ), hb_parnd( 7 ), ( HB_ISNUM( 8 ) ? ( Qt::SizeMode ) hb_parni( 8 ) : ( Qt::SizeMode ) Qt::AbsoluteSize ) );
}

/* void addText ( const QPointF & point, const QFont & font, const QString & text ) */
HB_FUNC( QT_QPAINTERPATH_ADDTEXT )
{
   QPainterPath * p = hbqt_par_QPainterPath( 1 );
   if( p )
   {
      void * pText;
      ( p )->addText( *hbqt_par_QPointF( 2 ), *hbqt_par_QFont( 3 ), hb_parstr_utf8( 4, &pText, NULL ) );
      hb_strfree( pText );
   }
}

/* void addText ( qreal x, qreal y, const QFont & font, const QString & text ) */
HB_FUNC( QT_QPAINTERPATH_ADDTEXT_1 )
{
   QPainterPath * p = hbqt_par_QPainterPath( 1 );
   if( p )
   {
      void * pText;
      ( p )->addText( hb_parnd( 2 ), hb_parnd( 3 ), *hbqt_par_QFont( 4 ), hb_parstr_utf8( 5, &pText, NULL ) );
      hb_strfree( pText );
   }
}

/* qreal angleAtPercent ( qreal t ) const */
HB_FUNC( QT_QPAINTERPATH_ANGLEATPERCENT )
{
   QPainterPath * p = hbqt_par_QPainterPath( 1 );
   if( p )
      hb_retnd( ( p )->angleAtPercent( hb_parnd( 2 ) ) );
}

/* void arcMoveTo ( const QRectF & rectangle, qreal angle ) */
HB_FUNC( QT_QPAINTERPATH_ARCMOVETO )
{
   QPainterPath * p = hbqt_par_QPainterPath( 1 );
   if( p )
      ( p )->arcMoveTo( *hbqt_par_QRectF( 2 ), hb_parnd( 3 ) );
}

/* void arcMoveTo ( qreal x, qreal y, qreal width, qreal height, qreal angle ) */
HB_FUNC( QT_QPAINTERPATH_ARCMOVETO_1 )
{
   QPainterPath * p = hbqt_par_QPainterPath( 1 );
   if( p )
      ( p )->arcMoveTo( hb_parnd( 2 ), hb_parnd( 3 ), hb_parnd( 4 ), hb_parnd( 5 ), hb_parnd( 6 ) );
}

/* void arcTo ( const QRectF & rectangle, qreal startAngle, qreal sweepLength ) */
HB_FUNC( QT_QPAINTERPATH_ARCTO )
{
   QPainterPath * p = hbqt_par_QPainterPath( 1 );
   if( p )
      ( p )->arcTo( *hbqt_par_QRectF( 2 ), hb_parnd( 3 ), hb_parnd( 4 ) );
}

/* void arcTo ( qreal x, qreal y, qreal width, qreal height, qreal startAngle, qreal sweepLength ) */
HB_FUNC( QT_QPAINTERPATH_ARCTO_1 )
{
   QPainterPath * p = hbqt_par_QPainterPath( 1 );
   if( p )
      ( p )->arcTo( hb_parnd( 2 ), hb_parnd( 3 ), hb_parnd( 4 ), hb_parnd( 5 ), hb_parnd( 6 ), hb_parnd( 7 ) );
}

/* QRectF boundingRect () const */
HB_FUNC( QT_QPAINTERPATH_BOUNDINGRECT )
{
   QPainterPath * p = hbqt_par_QPainterPath( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QRectF( new QRectF( ( p )->boundingRect() ), true ) );
}

/* void closeSubpath () */
HB_FUNC( QT_QPAINTERPATH_CLOSESUBPATH )
{
   QPainterPath * p = hbqt_par_QPainterPath( 1 );
   if( p )
      ( p )->closeSubpath();
}

/* void connectPath ( const QPainterPath & path ) */
HB_FUNC( QT_QPAINTERPATH_CONNECTPATH )
{
   QPainterPath * p = hbqt_par_QPainterPath( 1 );
   if( p )
      ( p )->connectPath( *hbqt_par_QPainterPath( 2 ) );
}

/* bool contains ( const QPointF & point ) const */
HB_FUNC( QT_QPAINTERPATH_CONTAINS )
{
   QPainterPath * p = hbqt_par_QPainterPath( 1 );
   if( p )
      hb_retl( ( p )->contains( *hbqt_par_QPointF( 2 ) ) );
}

/* bool contains ( const QRectF & rectangle ) const */
HB_FUNC( QT_QPAINTERPATH_CONTAINS_1 )
{
   QPainterPath * p = hbqt_par_QPainterPath( 1 );
   if( p )
      hb_retl( ( p )->contains( *hbqt_par_QRectF( 2 ) ) );
}

/* bool contains ( const QPainterPath & p ) const */
HB_FUNC( QT_QPAINTERPATH_CONTAINS_2 )
{
   QPainterPath * p = hbqt_par_QPainterPath( 1 );
   if( p )
      hb_retl( ( p )->contains( *hbqt_par_QPainterPath( 2 ) ) );
}

/* QRectF controlPointRect () const */
HB_FUNC( QT_QPAINTERPATH_CONTROLPOINTRECT )
{
   QPainterPath * p = hbqt_par_QPainterPath( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QRectF( new QRectF( ( p )->controlPointRect() ), true ) );
}

/* void cubicTo ( const QPointF & c1, const QPointF & c2, const QPointF & endPoint ) */
HB_FUNC( QT_QPAINTERPATH_CUBICTO )
{
   QPainterPath * p = hbqt_par_QPainterPath( 1 );
   if( p )
      ( p )->cubicTo( *hbqt_par_QPointF( 2 ), *hbqt_par_QPointF( 3 ), *hbqt_par_QPointF( 4 ) );
}

/* void cubicTo ( qreal c1X, qreal c1Y, qreal c2X, qreal c2Y, qreal endPointX, qreal endPointY ) */
HB_FUNC( QT_QPAINTERPATH_CUBICTO_1 )
{
   QPainterPath * p = hbqt_par_QPainterPath( 1 );
   if( p )
      ( p )->cubicTo( hb_parnd( 2 ), hb_parnd( 3 ), hb_parnd( 4 ), hb_parnd( 5 ), hb_parnd( 6 ), hb_parnd( 7 ) );
}

/* QPointF currentPosition () const */
HB_FUNC( QT_QPAINTERPATH_CURRENTPOSITION )
{
   QPainterPath * p = hbqt_par_QPainterPath( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QPointF( new QPointF( ( p )->currentPosition() ), true ) );
}

/* int elementCount () const */
HB_FUNC( QT_QPAINTERPATH_ELEMENTCOUNT )
{
   QPainterPath * p = hbqt_par_QPainterPath( 1 );
   if( p )
      hb_retni( ( p )->elementCount() );
}

/* Qt::FillRule fillRule () const */
HB_FUNC( QT_QPAINTERPATH_FILLRULE )
{
   QPainterPath * p = hbqt_par_QPainterPath( 1 );
   if( p )
      hb_retni( ( Qt::FillRule ) ( p )->fillRule() );
}

/* QPainterPath intersected ( const QPainterPath & p ) const */
HB_FUNC( QT_QPAINTERPATH_INTERSECTED )
{
   QPainterPath * p = hbqt_par_QPainterPath( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QPainterPath( new QPainterPath( ( p )->intersected( *hbqt_par_QPainterPath( 2 ) ) ), true ) );
}

/* bool intersects ( const QRectF & rectangle ) const */
HB_FUNC( QT_QPAINTERPATH_INTERSECTS )
{
   QPainterPath * p = hbqt_par_QPainterPath( 1 );
   if( p )
      hb_retl( ( p )->intersects( *hbqt_par_QRectF( 2 ) ) );
}

/* bool intersects ( const QPainterPath & p ) const */
HB_FUNC( QT_QPAINTERPATH_INTERSECTS_1 )
{
   QPainterPath * p = hbqt_par_QPainterPath( 1 );
   if( p )
      hb_retl( ( p )->intersects( *hbqt_par_QPainterPath( 2 ) ) );
}

/* bool isEmpty () const */
HB_FUNC( QT_QPAINTERPATH_ISEMPTY )
{
   QPainterPath * p = hbqt_par_QPainterPath( 1 );
   if( p )
      hb_retl( ( p )->isEmpty() );
}

/* qreal length () const */
HB_FUNC( QT_QPAINTERPATH_LENGTH )
{
   QPainterPath * p = hbqt_par_QPainterPath( 1 );
   if( p )
      hb_retnd( ( p )->length() );
}

/* void lineTo ( const QPointF & endPoint ) */
HB_FUNC( QT_QPAINTERPATH_LINETO )
{
   QPainterPath * p = hbqt_par_QPainterPath( 1 );
   if( p )
      ( p )->lineTo( *hbqt_par_QPointF( 2 ) );
}

/* void lineTo ( qreal x, qreal y ) */
HB_FUNC( QT_QPAINTERPATH_LINETO_1 )
{
   QPainterPath * p = hbqt_par_QPainterPath( 1 );
   if( p )
      ( p )->lineTo( hb_parnd( 2 ), hb_parnd( 3 ) );
}

/* void moveTo ( const QPointF & point ) */
HB_FUNC( QT_QPAINTERPATH_MOVETO )
{
   QPainterPath * p = hbqt_par_QPainterPath( 1 );
   if( p )
      ( p )->moveTo( *hbqt_par_QPointF( 2 ) );
}

/* void moveTo ( qreal x, qreal y ) */
HB_FUNC( QT_QPAINTERPATH_MOVETO_1 )
{
   QPainterPath * p = hbqt_par_QPainterPath( 1 );
   if( p )
      ( p )->moveTo( hb_parnd( 2 ), hb_parnd( 3 ) );
}

/* qreal percentAtLength ( qreal len ) const */
HB_FUNC( QT_QPAINTERPATH_PERCENTATLENGTH )
{
   QPainterPath * p = hbqt_par_QPainterPath( 1 );
   if( p )
      hb_retnd( ( p )->percentAtLength( hb_parnd( 2 ) ) );
}

/* QPointF pointAtPercent ( qreal t ) const */
HB_FUNC( QT_QPAINTERPATH_POINTATPERCENT )
{
   QPainterPath * p = hbqt_par_QPainterPath( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QPointF( new QPointF( ( p )->pointAtPercent( hb_parnd( 2 ) ) ), true ) );
}

/* void quadTo ( const QPointF & c, const QPointF & endPoint ) */
HB_FUNC( QT_QPAINTERPATH_QUADTO )
{
   QPainterPath * p = hbqt_par_QPainterPath( 1 );
   if( p )
      ( p )->quadTo( *hbqt_par_QPointF( 2 ), *hbqt_par_QPointF( 3 ) );
}

/* void quadTo ( qreal cx, qreal cy, qreal endPointX, qreal endPointY ) */
HB_FUNC( QT_QPAINTERPATH_QUADTO_1 )
{
   QPainterPath * p = hbqt_par_QPainterPath( 1 );
   if( p )
      ( p )->quadTo( hb_parnd( 2 ), hb_parnd( 3 ), hb_parnd( 4 ), hb_parnd( 5 ) );
}

/* void setElementPositionAt ( int index, qreal x, qreal y ) */
HB_FUNC( QT_QPAINTERPATH_SETELEMENTPOSITIONAT )
{
   QPainterPath * p = hbqt_par_QPainterPath( 1 );
   if( p )
      ( p )->setElementPositionAt( hb_parni( 2 ), hb_parnd( 3 ), hb_parnd( 4 ) );
}

/* void setFillRule ( Qt::FillRule fillRule ) */
HB_FUNC( QT_QPAINTERPATH_SETFILLRULE )
{
   QPainterPath * p = hbqt_par_QPainterPath( 1 );
   if( p )
      ( p )->setFillRule( ( Qt::FillRule ) hb_parni( 2 ) );
}

/* QPainterPath simplified () const */
HB_FUNC( QT_QPAINTERPATH_SIMPLIFIED )
{
   QPainterPath * p = hbqt_par_QPainterPath( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QPainterPath( new QPainterPath( ( p )->simplified() ), true ) );
}

/* qreal slopeAtPercent ( qreal t ) const */
HB_FUNC( QT_QPAINTERPATH_SLOPEATPERCENT )
{
   QPainterPath * p = hbqt_par_QPainterPath( 1 );
   if( p )
      hb_retnd( ( p )->slopeAtPercent( hb_parnd( 2 ) ) );
}

/* QPainterPath subtracted ( const QPainterPath & p ) const */
HB_FUNC( QT_QPAINTERPATH_SUBTRACTED )
{
   QPainterPath * p = hbqt_par_QPainterPath( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QPainterPath( new QPainterPath( ( p )->subtracted( *hbqt_par_QPainterPath( 2 ) ) ), true ) );
}

/* QPolygonF toFillPolygon ( const QTransform & matrix ) const */
HB_FUNC( QT_QPAINTERPATH_TOFILLPOLYGON )
{
   QPainterPath * p = hbqt_par_QPainterPath( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QPolygonF( new QPolygonF( ( p )->toFillPolygon( *hbqt_par_QTransform( 2 ) ) ), true ) );
}

/* QPolygonF toFillPolygon ( const QMatrix & matrix = QMatrix() ) const */
HB_FUNC( QT_QPAINTERPATH_TOFILLPOLYGON_1 )
{
   QPainterPath * p = hbqt_par_QPainterPath( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QPolygonF( new QPolygonF( ( p )->toFillPolygon( ( HB_ISOBJECT( 2 ) ? *hbqt_par_QMatrix( 2 ) : QMatrix() ) ) ), true ) );
}

/* QList<QPolygonF> toFillPolygons ( const QTransform & matrix ) const */
HB_FUNC( QT_QPAINTERPATH_TOFILLPOLYGONS )
{
   QPainterPath * p = hbqt_par_QPainterPath( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QList( new QList<QPolygonF>( ( p )->toFillPolygons( *hbqt_par_QTransform( 2 ) ) ), true ) );
}

/* QList<QPolygonF> toFillPolygons ( const QMatrix & matrix = QMatrix() ) const */
HB_FUNC( QT_QPAINTERPATH_TOFILLPOLYGONS_1 )
{
   QPainterPath * p = hbqt_par_QPainterPath( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QList( new QList<QPolygonF>( ( p )->toFillPolygons( ( HB_ISOBJECT( 2 ) ? *hbqt_par_QMatrix( 2 ) : QMatrix() ) ) ), true ) );
}

/* QPainterPath toReversed () const */
HB_FUNC( QT_QPAINTERPATH_TOREVERSED )
{
   QPainterPath * p = hbqt_par_QPainterPath( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QPainterPath( new QPainterPath( ( p )->toReversed() ), true ) );
}

/* QList<QPolygonF> toSubpathPolygons ( const QTransform & matrix ) const */
HB_FUNC( QT_QPAINTERPATH_TOSUBPATHPOLYGONS )
{
   QPainterPath * p = hbqt_par_QPainterPath( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QList( new QList<QPolygonF>( ( p )->toSubpathPolygons( *hbqt_par_QTransform( 2 ) ) ), true ) );
}

/* QList<QPolygonF> toSubpathPolygons ( const QMatrix & matrix = QMatrix() ) const */
HB_FUNC( QT_QPAINTERPATH_TOSUBPATHPOLYGONS_1 )
{
   QPainterPath * p = hbqt_par_QPainterPath( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QList( new QList<QPolygonF>( ( p )->toSubpathPolygons( ( HB_ISOBJECT( 2 ) ? *hbqt_par_QMatrix( 2 ) : QMatrix() ) ) ), true ) );
}

/* QPainterPath united ( const QPainterPath & p ) const */
HB_FUNC( QT_QPAINTERPATH_UNITED )
{
   QPainterPath * p = hbqt_par_QPainterPath( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QPainterPath( new QPainterPath( ( p )->united( *hbqt_par_QPainterPath( 2 ) ) ), true ) );
}


#endif /* #if QT_VERSION >= 0x040500 */
