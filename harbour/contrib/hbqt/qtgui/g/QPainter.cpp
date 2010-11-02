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
 *  enum CompositionMode { CompositionMode_SourceOver, CompositionMode_DestinationOver, CompositionMode_Clear, CompositionMode_Source, ..., RasterOp_SourceAndNotDestination }
 *  enum RenderHint { Antialiasing, TextAntialiasing, SmoothPixmapTransform, HighQualityAntialiasing, NonCosmeticDefaultPen }
 *  enum RenderHints
 */

/*
 *  Constructed[ 179/185 [ 96.76% ] ]
 *
 *  *** Unconvered Prototypes ***
 *
 *  void drawLines ( const QVector<QPointF> & pointPairs )
 *  void drawLines ( const QVector<QPoint> & pointPairs )
 *  void drawLines ( const QVector<QLineF> & lines )
 *  void drawLines ( const QVector<QLine> & lines )
 *  void drawRects ( const QVector<QRectF> & rectangles )
 *  void drawRects ( const QVector<QRect> & rectangles )
 *
 *  *** Commented out protostypes ***
 *
 *  //void fillRect ( int x, int y, int width, int height, Qt::BrushStyle style )
 *  //void fillRect ( const QRect  & rectangle, Qt::BrushStyle style )
 *  //void fillRect ( const QRectF & rectangle, Qt::BrushStyle style )
 */

#include <QtCore/QPointer>

#include <QtGui/QPainter>

/*
 * QPainter ()
 * QPainter ( QPaintDevice * device )
 * ~QPainter ()
 */

typedef struct
{
   QPainter * ph;
   bool bNew;
   PHBQT_GC_FUNC func;
   int type;
} HBQT_GC_T_QPainter;

HBQT_GC_FUNC( hbqt_gcRelease_QPainter )
{
   HBQT_GC_T * p = ( HBQT_GC_T * ) Cargo;

   if( p )
   {
      if( p->bNew && p->ph )
         delete ( ( QPainter * ) p->ph );
      p->ph = NULL;
   }
}

void * hbqt_gcAllocate_QPainter( void * pObj, bool bNew )
{
   HBQT_GC_T * p = ( HBQT_GC_T * ) hb_gcAllocate( sizeof( HBQT_GC_T ), hbqt_gcFuncs() );

   p->ph = ( QPainter * ) pObj;
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QPainter;
   p->type = HBQT_TYPE_QPainter;

   return p;
}

HB_FUNC( QT_QPAINTER )
{
   QPainter * pObj = NULL;

   if( hb_pcount() >= 1 && HB_ISPOINTER( 1 ) )
   {
      pObj = new QPainter( hbqt_par_QPaintDevice( 1 ) ) ;
   }
   else
   {
      pObj = new QPainter() ;
   }

   hb_retptrGC( hbqt_gcAllocate_QPainter( ( void * ) pObj, true ) );
}

/* const QBrush & background () const */
HB_FUNC( QT_QPAINTER_BACKGROUND )
{
   QPainter * p = hbqt_par_QPainter( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QBrush( new QBrush( ( p )->background() ), true ) );
}

/* Qt::BGMode backgroundMode () const */
HB_FUNC( QT_QPAINTER_BACKGROUNDMODE )
{
   QPainter * p = hbqt_par_QPainter( 1 );
   if( p )
      hb_retni( ( Qt::BGMode ) ( p )->backgroundMode() );
}

/* bool begin ( QPaintDevice * device ) */
HB_FUNC( QT_QPAINTER_BEGIN )
{
   QPainter * p = hbqt_par_QPainter( 1 );
   if( p )
      hb_retl( ( p )->begin( hbqt_par_QPaintDevice( 2 ) ) );
}

/* QRectF boundingRect ( const QRectF & rectangle, int flags, const QString & text ) */
HB_FUNC( QT_QPAINTER_BOUNDINGRECT )
{
   QPainter * p = hbqt_par_QPainter( 1 );
   if( p )
   {
      void * pText;
      hb_retptrGC( hbqt_gcAllocate_QRectF( new QRectF( ( p )->boundingRect( *hbqt_par_QRectF( 2 ), hb_parni( 3 ), hb_parstr_utf8( 4, &pText, NULL ) ) ), true ) );
      hb_strfree( pText );
   }
}

/* QRect boundingRect ( const QRect & rectangle, int flags, const QString & text ) */
HB_FUNC( QT_QPAINTER_BOUNDINGRECT_1 )
{
   QPainter * p = hbqt_par_QPainter( 1 );
   if( p )
   {
      void * pText;
      hb_retptrGC( hbqt_gcAllocate_QRect( new QRect( ( p )->boundingRect( *hbqt_par_QRect( 2 ), hb_parni( 3 ), hb_parstr_utf8( 4, &pText, NULL ) ) ), true ) );
      hb_strfree( pText );
   }
}

/* QRect boundingRect ( int x, int y, int w, int h, int flags, const QString & text ) */
HB_FUNC( QT_QPAINTER_BOUNDINGRECT_2 )
{
   QPainter * p = hbqt_par_QPainter( 1 );
   if( p )
   {
      void * pText;
      hb_retptrGC( hbqt_gcAllocate_QRect( new QRect( ( p )->boundingRect( hb_parni( 2 ), hb_parni( 3 ), hb_parni( 4 ), hb_parni( 5 ), hb_parni( 6 ), hb_parstr_utf8( 7, &pText, NULL ) ) ), true ) );
      hb_strfree( pText );
   }
}

/* QRectF boundingRect ( const QRectF & rectangle, const QString & text, const QTextOption & option = QTextOption() ) */
HB_FUNC( QT_QPAINTER_BOUNDINGRECT_3 )
{
   QPainter * p = hbqt_par_QPainter( 1 );
   if( p )
   {
      void * pText;
      hb_retptrGC( hbqt_gcAllocate_QRectF( new QRectF( ( p )->boundingRect( *hbqt_par_QRectF( 2 ), hb_parstr_utf8( 3, &pText, NULL ), ( HB_ISOBJECT( 4 ) ? *hbqt_par_QTextOption( 4 ) : QTextOption() ) ) ), true ) );
      hb_strfree( pText );
   }
}

/* const QBrush & brush () const */
HB_FUNC( QT_QPAINTER_BRUSH )
{
   QPainter * p = hbqt_par_QPainter( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QBrush( new QBrush( ( p )->brush() ), true ) );
}

/* QPoint brushOrigin () const */
HB_FUNC( QT_QPAINTER_BRUSHORIGIN )
{
   QPainter * p = hbqt_par_QPainter( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QPoint( new QPoint( ( p )->brushOrigin() ), true ) );
}

/* QPainterPath clipPath () const */
HB_FUNC( QT_QPAINTER_CLIPPATH )
{
   QPainter * p = hbqt_par_QPainter( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QPainterPath( new QPainterPath( ( p )->clipPath() ), true ) );
}

/* QRegion clipRegion () const */
HB_FUNC( QT_QPAINTER_CLIPREGION )
{
   QPainter * p = hbqt_par_QPainter( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QRegion( new QRegion( ( p )->clipRegion() ), true ) );
}

/* QMatrix combinedMatrix () const */
HB_FUNC( QT_QPAINTER_COMBINEDMATRIX )
{
   QPainter * p = hbqt_par_QPainter( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QMatrix( new QMatrix( ( p )->combinedMatrix() ), true ) );
}

/* QTransform combinedTransform () const */
HB_FUNC( QT_QPAINTER_COMBINEDTRANSFORM )
{
   QPainter * p = hbqt_par_QPainter( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QTransform( new QTransform( ( p )->combinedTransform() ), true ) );
}

/* CompositionMode compositionMode () const */
HB_FUNC( QT_QPAINTER_COMPOSITIONMODE )
{
   QPainter * p = hbqt_par_QPainter( 1 );
   if( p )
      hb_retni( ( QPainter::CompositionMode ) ( p )->compositionMode() );
}

/* QPaintDevice * device () const */
HB_FUNC( QT_QPAINTER_DEVICE )
{
   QPainter * p = hbqt_par_QPainter( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QPaintDevice( ( p )->device(), false ) );
}

/* const QMatrix & deviceMatrix () const */
HB_FUNC( QT_QPAINTER_DEVICEMATRIX )
{
   QPainter * p = hbqt_par_QPainter( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QMatrix( new QMatrix( ( p )->deviceMatrix() ), true ) );
}

/* const QTransform & deviceTransform () const */
HB_FUNC( QT_QPAINTER_DEVICETRANSFORM )
{
   QPainter * p = hbqt_par_QPainter( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QTransform( new QTransform( ( p )->deviceTransform() ), true ) );
}

/* void drawArc ( const QRectF & rectangle, int startAngle, int spanAngle ) */
HB_FUNC( QT_QPAINTER_DRAWARC )
{
   QPainter * p = hbqt_par_QPainter( 1 );
   if( p )
      ( p )->drawArc( *hbqt_par_QRectF( 2 ), hb_parni( 3 ), hb_parni( 4 ) );
}

/* void drawArc ( const QRect & rectangle, int startAngle, int spanAngle ) */
HB_FUNC( QT_QPAINTER_DRAWARC_1 )
{
   QPainter * p = hbqt_par_QPainter( 1 );
   if( p )
      ( p )->drawArc( *hbqt_par_QRect( 2 ), hb_parni( 3 ), hb_parni( 4 ) );
}

/* void drawArc ( int x, int y, int width, int height, int startAngle, int spanAngle ) */
HB_FUNC( QT_QPAINTER_DRAWARC_2 )
{
   QPainter * p = hbqt_par_QPainter( 1 );
   if( p )
      ( p )->drawArc( hb_parni( 2 ), hb_parni( 3 ), hb_parni( 4 ), hb_parni( 5 ), hb_parni( 6 ), hb_parni( 7 ) );
}

/* void drawChord ( const QRectF & rectangle, int startAngle, int spanAngle ) */
HB_FUNC( QT_QPAINTER_DRAWCHORD )
{
   QPainter * p = hbqt_par_QPainter( 1 );
   if( p )
      ( p )->drawChord( *hbqt_par_QRectF( 2 ), hb_parni( 3 ), hb_parni( 4 ) );
}

/* void drawChord ( const QRect & rectangle, int startAngle, int spanAngle ) */
HB_FUNC( QT_QPAINTER_DRAWCHORD_1 )
{
   QPainter * p = hbqt_par_QPainter( 1 );
   if( p )
      ( p )->drawChord( *hbqt_par_QRect( 2 ), hb_parni( 3 ), hb_parni( 4 ) );
}

/* void drawChord ( int x, int y, int width, int height, int startAngle, int spanAngle ) */
HB_FUNC( QT_QPAINTER_DRAWCHORD_2 )
{
   QPainter * p = hbqt_par_QPainter( 1 );
   if( p )
      ( p )->drawChord( hb_parni( 2 ), hb_parni( 3 ), hb_parni( 4 ), hb_parni( 5 ), hb_parni( 6 ), hb_parni( 7 ) );
}

/* void drawConvexPolygon ( const QPointF * points, int pointCount ) */
HB_FUNC( QT_QPAINTER_DRAWCONVEXPOLYGON )
{
   QPainter * p = hbqt_par_QPainter( 1 );
   if( p )
      ( p )->drawConvexPolygon( hbqt_par_QPointF( 2 ), hb_parni( 3 ) );
}

/* void drawConvexPolygon ( const QPoint * points, int pointCount ) */
HB_FUNC( QT_QPAINTER_DRAWCONVEXPOLYGON_1 )
{
   QPainter * p = hbqt_par_QPainter( 1 );
   if( p )
      ( p )->drawConvexPolygon( hbqt_par_QPoint( 2 ), hb_parni( 3 ) );
}

/* void drawConvexPolygon ( const QPolygonF & polygon ) */
HB_FUNC( QT_QPAINTER_DRAWCONVEXPOLYGON_2 )
{
   QPainter * p = hbqt_par_QPainter( 1 );
   if( p )
      ( p )->drawConvexPolygon( *hbqt_par_QPolygonF( 2 ) );
}

/* void drawConvexPolygon ( const QPolygon & polygon ) */
HB_FUNC( QT_QPAINTER_DRAWCONVEXPOLYGON_3 )
{
   QPainter * p = hbqt_par_QPainter( 1 );
   if( p )
      ( p )->drawConvexPolygon( *hbqt_par_QPolygon( 2 ) );
}

/* void drawEllipse ( const QRectF & rectangle ) */
HB_FUNC( QT_QPAINTER_DRAWELLIPSE )
{
   QPainter * p = hbqt_par_QPainter( 1 );
   if( p )
      ( p )->drawEllipse( *hbqt_par_QRectF( 2 ) );
}

/* void drawEllipse ( const QRect & rectangle ) */
HB_FUNC( QT_QPAINTER_DRAWELLIPSE_1 )
{
   QPainter * p = hbqt_par_QPainter( 1 );
   if( p )
      ( p )->drawEllipse( *hbqt_par_QRect( 2 ) );
}

/* void drawEllipse ( int x, int y, int width, int height ) */
HB_FUNC( QT_QPAINTER_DRAWELLIPSE_2 )
{
   QPainter * p = hbqt_par_QPainter( 1 );
   if( p )
      ( p )->drawEllipse( hb_parni( 2 ), hb_parni( 3 ), hb_parni( 4 ), hb_parni( 5 ) );
}

/* void drawEllipse ( const QPointF & center, qreal rx, qreal ry ) */
HB_FUNC( QT_QPAINTER_DRAWELLIPSE_3 )
{
   QPainter * p = hbqt_par_QPainter( 1 );
   if( p )
      ( p )->drawEllipse( *hbqt_par_QPointF( 2 ), hb_parnd( 3 ), hb_parnd( 4 ) );
}

/* void drawEllipse ( const QPoint & center, int rx, int ry ) */
HB_FUNC( QT_QPAINTER_DRAWELLIPSE_4 )
{
   QPainter * p = hbqt_par_QPainter( 1 );
   if( p )
      ( p )->drawEllipse( *hbqt_par_QPoint( 2 ), hb_parni( 3 ), hb_parni( 4 ) );
}

/* void drawImage ( const QRectF & target, const QImage & image, const QRectF & source, Qt::ImageConversionFlags flags = Qt::AutoColor ) */
HB_FUNC( QT_QPAINTER_DRAWIMAGE )
{
   QPainter * p = hbqt_par_QPainter( 1 );
   if( p )
      ( p )->drawImage( *hbqt_par_QRectF( 2 ), *hbqt_par_QImage( 3 ), *hbqt_par_QRectF( 4 ), ( HB_ISNUM( 5 ) ? ( Qt::ImageConversionFlags ) hb_parni( 5 ) : ( Qt::ImageConversionFlags ) Qt::AutoColor ) );
}

/* void drawImage ( const QRect & target, const QImage & image, const QRect & source, Qt::ImageConversionFlags flags = Qt::AutoColor ) */
HB_FUNC( QT_QPAINTER_DRAWIMAGE_1 )
{
   QPainter * p = hbqt_par_QPainter( 1 );
   if( p )
      ( p )->drawImage( *hbqt_par_QRect( 2 ), *hbqt_par_QImage( 3 ), *hbqt_par_QRect( 4 ), ( HB_ISNUM( 5 ) ? ( Qt::ImageConversionFlags ) hb_parni( 5 ) : ( Qt::ImageConversionFlags ) Qt::AutoColor ) );
}

/* void drawImage ( const QPointF & point, const QImage & image ) */
HB_FUNC( QT_QPAINTER_DRAWIMAGE_2 )
{
   QPainter * p = hbqt_par_QPainter( 1 );
   if( p )
      ( p )->drawImage( *hbqt_par_QPointF( 2 ), *hbqt_par_QImage( 3 ) );
}

/* void drawImage ( const QPoint & point, const QImage & image ) */
HB_FUNC( QT_QPAINTER_DRAWIMAGE_3 )
{
   QPainter * p = hbqt_par_QPainter( 1 );
   if( p )
      ( p )->drawImage( *hbqt_par_QPoint( 2 ), *hbqt_par_QImage( 3 ) );
}

/* void drawImage ( const QPointF & point, const QImage & image, const QRectF & source, Qt::ImageConversionFlags flags = Qt::AutoColor ) */
HB_FUNC( QT_QPAINTER_DRAWIMAGE_4 )
{
   QPainter * p = hbqt_par_QPainter( 1 );
   if( p )
      ( p )->drawImage( *hbqt_par_QPointF( 2 ), *hbqt_par_QImage( 3 ), *hbqt_par_QRectF( 4 ), ( HB_ISNUM( 5 ) ? ( Qt::ImageConversionFlags ) hb_parni( 5 ) : ( Qt::ImageConversionFlags ) Qt::AutoColor ) );
}

/* void drawImage ( const QPoint & point, const QImage & image, const QRect & source, Qt::ImageConversionFlags flags = Qt::AutoColor ) */
HB_FUNC( QT_QPAINTER_DRAWIMAGE_5 )
{
   QPainter * p = hbqt_par_QPainter( 1 );
   if( p )
      ( p )->drawImage( *hbqt_par_QPoint( 2 ), *hbqt_par_QImage( 3 ), *hbqt_par_QRect( 4 ), ( HB_ISNUM( 5 ) ? ( Qt::ImageConversionFlags ) hb_parni( 5 ) : ( Qt::ImageConversionFlags ) Qt::AutoColor ) );
}

/* void drawImage ( const QRectF & rectangle, const QImage & image ) */
HB_FUNC( QT_QPAINTER_DRAWIMAGE_6 )
{
   QPainter * p = hbqt_par_QPainter( 1 );
   if( p )
      ( p )->drawImage( *hbqt_par_QRectF( 2 ), *hbqt_par_QImage( 3 ) );
}

/* void drawImage ( const QRect & rectangle, const QImage & image ) */
HB_FUNC( QT_QPAINTER_DRAWIMAGE_7 )
{
   QPainter * p = hbqt_par_QPainter( 1 );
   if( p )
      ( p )->drawImage( *hbqt_par_QRect( 2 ), *hbqt_par_QImage( 3 ) );
}

/* void drawImage ( int x, int y, const QImage & image, int sx = 0, int sy = 0, int sw = -1, int sh = -1, Qt::ImageConversionFlags flags = Qt::AutoColor ) */
HB_FUNC( QT_QPAINTER_DRAWIMAGE_8 )
{
   QPainter * p = hbqt_par_QPainter( 1 );
   if( p )
      ( p )->drawImage( hb_parni( 2 ), hb_parni( 3 ), *hbqt_par_QImage( 4 ), hb_parni( 5 ), hb_parni( 6 ), hb_parnidef( 7, -1 ), hb_parnidef( 8, -1 ), ( HB_ISNUM( 9 ) ? ( Qt::ImageConversionFlags ) hb_parni( 9 ) : ( Qt::ImageConversionFlags ) Qt::AutoColor ) );
}

/* void drawLine ( const QLineF & line ) */
HB_FUNC( QT_QPAINTER_DRAWLINE )
{
   QPainter * p = hbqt_par_QPainter( 1 );
   if( p )
      ( p )->drawLine( *hbqt_par_QLineF( 2 ) );
}

/* void drawLine ( const QLine & line ) */
HB_FUNC( QT_QPAINTER_DRAWLINE_1 )
{
   QPainter * p = hbqt_par_QPainter( 1 );
   if( p )
      ( p )->drawLine( *hbqt_par_QLine( 2 ) );
}

/* void drawLine ( const QPoint & p1, const QPoint & p2 ) */
HB_FUNC( QT_QPAINTER_DRAWLINE_2 )
{
   QPainter * p = hbqt_par_QPainter( 1 );
   if( p )
      ( p )->drawLine( *hbqt_par_QPoint( 2 ), *hbqt_par_QPoint( 3 ) );
}

/* void drawLine ( const QPointF & p1, const QPointF & p2 ) */
HB_FUNC( QT_QPAINTER_DRAWLINE_3 )
{
   QPainter * p = hbqt_par_QPainter( 1 );
   if( p )
      ( p )->drawLine( *hbqt_par_QPointF( 2 ), *hbqt_par_QPointF( 3 ) );
}

/* void drawLine ( int x1, int y1, int x2, int y2 ) */
HB_FUNC( QT_QPAINTER_DRAWLINE_4 )
{
   QPainter * p = hbqt_par_QPainter( 1 );
   if( p )
      ( p )->drawLine( hb_parni( 2 ), hb_parni( 3 ), hb_parni( 4 ), hb_parni( 5 ) );
}

/* void drawLines ( const QLineF * lines, int lineCount ) */
HB_FUNC( QT_QPAINTER_DRAWLINES )
{
   QPainter * p = hbqt_par_QPainter( 1 );
   if( p )
      ( p )->drawLines( hbqt_par_QLineF( 2 ), hb_parni( 3 ) );
}

/* void drawLines ( const QLine * lines, int lineCount ) */
HB_FUNC( QT_QPAINTER_DRAWLINES_1 )
{
   QPainter * p = hbqt_par_QPainter( 1 );
   if( p )
      ( p )->drawLines( hbqt_par_QLine( 2 ), hb_parni( 3 ) );
}

/* void drawLines ( const QPointF * pointPairs, int lineCount ) */
HB_FUNC( QT_QPAINTER_DRAWLINES_2 )
{
   QPainter * p = hbqt_par_QPainter( 1 );
   if( p )
      ( p )->drawLines( hbqt_par_QPointF( 2 ), hb_parni( 3 ) );
}

/* void drawLines ( const QPoint * pointPairs, int lineCount ) */
HB_FUNC( QT_QPAINTER_DRAWLINES_3 )
{
   QPainter * p = hbqt_par_QPainter( 1 );
   if( p )
      ( p )->drawLines( hbqt_par_QPoint( 2 ), hb_parni( 3 ) );
}

/* void drawPath ( const QPainterPath & path ) */
HB_FUNC( QT_QPAINTER_DRAWPATH )
{
   QPainter * p = hbqt_par_QPainter( 1 );
   if( p )
      ( p )->drawPath( *hbqt_par_QPainterPath( 2 ) );
}

/* void drawPicture ( const QPointF & point, const QPicture & picture ) */
HB_FUNC( QT_QPAINTER_DRAWPICTURE )
{
   QPainter * p = hbqt_par_QPainter( 1 );
   if( p )
      ( p )->drawPicture( *hbqt_par_QPointF( 2 ), *hbqt_par_QPicture( 3 ) );
}

/* void drawPicture ( const QPoint & point, const QPicture & picture ) */
HB_FUNC( QT_QPAINTER_DRAWPICTURE_1 )
{
   QPainter * p = hbqt_par_QPainter( 1 );
   if( p )
      ( p )->drawPicture( *hbqt_par_QPoint( 2 ), *hbqt_par_QPicture( 3 ) );
}

/* void drawPicture ( int x, int y, const QPicture & picture ) */
HB_FUNC( QT_QPAINTER_DRAWPICTURE_2 )
{
   QPainter * p = hbqt_par_QPainter( 1 );
   if( p )
      ( p )->drawPicture( hb_parni( 2 ), hb_parni( 3 ), *hbqt_par_QPicture( 4 ) );
}

/* void drawPie ( const QRectF & rectangle, int startAngle, int spanAngle ) */
HB_FUNC( QT_QPAINTER_DRAWPIE )
{
   QPainter * p = hbqt_par_QPainter( 1 );
   if( p )
      ( p )->drawPie( *hbqt_par_QRectF( 2 ), hb_parni( 3 ), hb_parni( 4 ) );
}

/* void drawPie ( const QRect & rectangle, int startAngle, int spanAngle ) */
HB_FUNC( QT_QPAINTER_DRAWPIE_1 )
{
   QPainter * p = hbqt_par_QPainter( 1 );
   if( p )
      ( p )->drawPie( *hbqt_par_QRect( 2 ), hb_parni( 3 ), hb_parni( 4 ) );
}

/* void drawPie ( int x, int y, int width, int height, int startAngle, int spanAngle ) */
HB_FUNC( QT_QPAINTER_DRAWPIE_2 )
{
   QPainter * p = hbqt_par_QPainter( 1 );
   if( p )
      ( p )->drawPie( hb_parni( 2 ), hb_parni( 3 ), hb_parni( 4 ), hb_parni( 5 ), hb_parni( 6 ), hb_parni( 7 ) );
}

/* void drawPixmap ( const QRectF & target, const QPixmap & pixmap, const QRectF & source ) */
HB_FUNC( QT_QPAINTER_DRAWPIXMAP )
{
   QPainter * p = hbqt_par_QPainter( 1 );
   if( p )
      ( p )->drawPixmap( *hbqt_par_QRectF( 2 ), *hbqt_par_QPixmap( 3 ), *hbqt_par_QRectF( 4 ) );
}

/* void drawPixmap ( const QRect & target, const QPixmap & pixmap, const QRect & source ) */
HB_FUNC( QT_QPAINTER_DRAWPIXMAP_1 )
{
   QPainter * p = hbqt_par_QPainter( 1 );
   if( p )
      ( p )->drawPixmap( *hbqt_par_QRect( 2 ), *hbqt_par_QPixmap( 3 ), *hbqt_par_QRect( 4 ) );
}

/* void drawPixmap ( const QPointF & point, const QPixmap & pixmap, const QRectF & source ) */
HB_FUNC( QT_QPAINTER_DRAWPIXMAP_2 )
{
   QPainter * p = hbqt_par_QPainter( 1 );
   if( p )
      ( p )->drawPixmap( *hbqt_par_QPointF( 2 ), *hbqt_par_QPixmap( 3 ), *hbqt_par_QRectF( 4 ) );
}

/* void drawPixmap ( const QPoint & point, const QPixmap & pixmap, const QRect & source ) */
HB_FUNC( QT_QPAINTER_DRAWPIXMAP_3 )
{
   QPainter * p = hbqt_par_QPainter( 1 );
   if( p )
      ( p )->drawPixmap( *hbqt_par_QPoint( 2 ), *hbqt_par_QPixmap( 3 ), *hbqt_par_QRect( 4 ) );
}

/* void drawPixmap ( const QPointF & point, const QPixmap & pixmap ) */
HB_FUNC( QT_QPAINTER_DRAWPIXMAP_4 )
{
   QPainter * p = hbqt_par_QPainter( 1 );
   if( p )
      ( p )->drawPixmap( *hbqt_par_QPointF( 2 ), *hbqt_par_QPixmap( 3 ) );
}

/* void drawPixmap ( const QPoint & point, const QPixmap & pixmap ) */
HB_FUNC( QT_QPAINTER_DRAWPIXMAP_5 )
{
   QPainter * p = hbqt_par_QPainter( 1 );
   if( p )
      ( p )->drawPixmap( *hbqt_par_QPoint( 2 ), *hbqt_par_QPixmap( 3 ) );
}

/* void drawPixmap ( int x, int y, const QPixmap & pixmap ) */
HB_FUNC( QT_QPAINTER_DRAWPIXMAP_6 )
{
   QPainter * p = hbqt_par_QPainter( 1 );
   if( p )
      ( p )->drawPixmap( hb_parni( 2 ), hb_parni( 3 ), *hbqt_par_QPixmap( 4 ) );
}

/* void drawPixmap ( const QRect & rectangle, const QPixmap & pixmap ) */
HB_FUNC( QT_QPAINTER_DRAWPIXMAP_7 )
{
   QPainter * p = hbqt_par_QPainter( 1 );
   if( p )
      ( p )->drawPixmap( *hbqt_par_QRect( 2 ), *hbqt_par_QPixmap( 3 ) );
}

/* void drawPixmap ( int x, int y, int width, int height, const QPixmap & pixmap ) */
HB_FUNC( QT_QPAINTER_DRAWPIXMAP_8 )
{
   QPainter * p = hbqt_par_QPainter( 1 );
   if( p )
      ( p )->drawPixmap( hb_parni( 2 ), hb_parni( 3 ), hb_parni( 4 ), hb_parni( 5 ), *hbqt_par_QPixmap( 6 ) );
}

/* void drawPixmap ( int x, int y, int w, int h, const QPixmap & pixmap, int sx, int sy, int sw, int sh ) */
HB_FUNC( QT_QPAINTER_DRAWPIXMAP_9 )
{
   QPainter * p = hbqt_par_QPainter( 1 );
   if( p )
      ( p )->drawPixmap( hb_parni( 2 ), hb_parni( 3 ), hb_parni( 4 ), hb_parni( 5 ), *hbqt_par_QPixmap( 6 ), hb_parni( 7 ), hb_parni( 8 ), hb_parni( 9 ), hb_parni( 10 ) );
}

/* void drawPixmap ( int x, int y, const QPixmap & pixmap, int sx, int sy, int sw, int sh ) */
HB_FUNC( QT_QPAINTER_DRAWPIXMAP_10 )
{
   QPainter * p = hbqt_par_QPainter( 1 );
   if( p )
      ( p )->drawPixmap( hb_parni( 2 ), hb_parni( 3 ), *hbqt_par_QPixmap( 4 ), hb_parni( 5 ), hb_parni( 6 ), hb_parni( 7 ), hb_parni( 8 ) );
}

/* void drawPoint ( const QPointF & position ) */
HB_FUNC( QT_QPAINTER_DRAWPOINT )
{
   QPainter * p = hbqt_par_QPainter( 1 );
   if( p )
      ( p )->drawPoint( *hbqt_par_QPointF( 2 ) );
}

/* void drawPoint ( const QPoint & position ) */
HB_FUNC( QT_QPAINTER_DRAWPOINT_1 )
{
   QPainter * p = hbqt_par_QPainter( 1 );
   if( p )
      ( p )->drawPoint( *hbqt_par_QPoint( 2 ) );
}

/* void drawPoint ( int x, int y ) */
HB_FUNC( QT_QPAINTER_DRAWPOINT_2 )
{
   QPainter * p = hbqt_par_QPainter( 1 );
   if( p )
      ( p )->drawPoint( hb_parni( 2 ), hb_parni( 3 ) );
}

/* void drawPoints ( const QPointF * points, int pointCount ) */
HB_FUNC( QT_QPAINTER_DRAWPOINTS )
{
   QPainter * p = hbqt_par_QPainter( 1 );
   if( p )
      ( p )->drawPoints( hbqt_par_QPointF( 2 ), hb_parni( 3 ) );
}

/* void drawPoints ( const QPoint * points, int pointCount ) */
HB_FUNC( QT_QPAINTER_DRAWPOINTS_1 )
{
   QPainter * p = hbqt_par_QPainter( 1 );
   if( p )
      ( p )->drawPoints( hbqt_par_QPoint( 2 ), hb_parni( 3 ) );
}

/* void drawPoints ( const QPolygonF & points ) */
HB_FUNC( QT_QPAINTER_DRAWPOINTS_2 )
{
   QPainter * p = hbqt_par_QPainter( 1 );
   if( p )
      ( p )->drawPoints( *hbqt_par_QPolygonF( 2 ) );
}

/* void drawPoints ( const QPolygon & points ) */
HB_FUNC( QT_QPAINTER_DRAWPOINTS_3 )
{
   QPainter * p = hbqt_par_QPainter( 1 );
   if( p )
      ( p )->drawPoints( *hbqt_par_QPolygon( 2 ) );
}

/* void drawPolygon ( const QPointF * points, int pointCount, Qt::FillRule fillRule = Qt::OddEvenFill ) */
HB_FUNC( QT_QPAINTER_DRAWPOLYGON )
{
   QPainter * p = hbqt_par_QPainter( 1 );
   if( p )
      ( p )->drawPolygon( hbqt_par_QPointF( 2 ), hb_parni( 3 ), ( HB_ISNUM( 4 ) ? ( Qt::FillRule ) hb_parni( 4 ) : ( Qt::FillRule ) Qt::OddEvenFill ) );
}

/* void drawPolygon ( const QPoint * points, int pointCount, Qt::FillRule fillRule = Qt::OddEvenFill ) */
HB_FUNC( QT_QPAINTER_DRAWPOLYGON_1 )
{
   QPainter * p = hbqt_par_QPainter( 1 );
   if( p )
      ( p )->drawPolygon( hbqt_par_QPoint( 2 ), hb_parni( 3 ), ( HB_ISNUM( 4 ) ? ( Qt::FillRule ) hb_parni( 4 ) : ( Qt::FillRule ) Qt::OddEvenFill ) );
}

/* void drawPolygon ( const QPolygonF & points, Qt::FillRule fillRule = Qt::OddEvenFill ) */
HB_FUNC( QT_QPAINTER_DRAWPOLYGON_2 )
{
   QPainter * p = hbqt_par_QPainter( 1 );
   if( p )
      ( p )->drawPolygon( *hbqt_par_QPolygonF( 2 ), ( HB_ISNUM( 3 ) ? ( Qt::FillRule ) hb_parni( 3 ) : ( Qt::FillRule ) Qt::OddEvenFill ) );
}

/* void drawPolygon ( const QPolygon & points, Qt::FillRule fillRule = Qt::OddEvenFill ) */
HB_FUNC( QT_QPAINTER_DRAWPOLYGON_3 )
{
   QPainter * p = hbqt_par_QPainter( 1 );
   if( p )
      ( p )->drawPolygon( *hbqt_par_QPolygon( 2 ), ( HB_ISNUM( 3 ) ? ( Qt::FillRule ) hb_parni( 3 ) : ( Qt::FillRule ) Qt::OddEvenFill ) );
}

/* void drawPolyline ( const QPointF * points, int pointCount ) */
HB_FUNC( QT_QPAINTER_DRAWPOLYLINE )
{
   QPainter * p = hbqt_par_QPainter( 1 );
   if( p )
      ( p )->drawPolyline( hbqt_par_QPointF( 2 ), hb_parni( 3 ) );
}

/* void drawPolyline ( const QPoint * points, int pointCount ) */
HB_FUNC( QT_QPAINTER_DRAWPOLYLINE_1 )
{
   QPainter * p = hbqt_par_QPainter( 1 );
   if( p )
      ( p )->drawPolyline( hbqt_par_QPoint( 2 ), hb_parni( 3 ) );
}

/* void drawPolyline ( const QPolygonF & points ) */
HB_FUNC( QT_QPAINTER_DRAWPOLYLINE_2 )
{
   QPainter * p = hbqt_par_QPainter( 1 );
   if( p )
      ( p )->drawPolyline( *hbqt_par_QPolygonF( 2 ) );
}

/* void drawPolyline ( const QPolygon & points ) */
HB_FUNC( QT_QPAINTER_DRAWPOLYLINE_3 )
{
   QPainter * p = hbqt_par_QPainter( 1 );
   if( p )
      ( p )->drawPolyline( *hbqt_par_QPolygon( 2 ) );
}

/* void drawRect ( const QRectF & rectangle ) */
HB_FUNC( QT_QPAINTER_DRAWRECT )
{
   QPainter * p = hbqt_par_QPainter( 1 );
   if( p )
      ( p )->drawRect( *hbqt_par_QRectF( 2 ) );
}

/* void drawRect ( const QRect & rectangle ) */
HB_FUNC( QT_QPAINTER_DRAWRECT_1 )
{
   QPainter * p = hbqt_par_QPainter( 1 );
   if( p )
      ( p )->drawRect( *hbqt_par_QRect( 2 ) );
}

/* void drawRect ( int x, int y, int width, int height ) */
HB_FUNC( QT_QPAINTER_DRAWRECT_2 )
{
   QPainter * p = hbqt_par_QPainter( 1 );
   if( p )
      ( p )->drawRect( hb_parni( 2 ), hb_parni( 3 ), hb_parni( 4 ), hb_parni( 5 ) );
}

/* void drawRects ( const QRectF * rectangles, int rectCount ) */
HB_FUNC( QT_QPAINTER_DRAWRECTS )
{
   QPainter * p = hbqt_par_QPainter( 1 );
   if( p )
      ( p )->drawRects( hbqt_par_QRectF( 2 ), hb_parni( 3 ) );
}

/* void drawRects ( const QRect * rectangles, int rectCount ) */
HB_FUNC( QT_QPAINTER_DRAWRECTS_1 )
{
   QPainter * p = hbqt_par_QPainter( 1 );
   if( p )
      ( p )->drawRects( hbqt_par_QRect( 2 ), hb_parni( 3 ) );
}

/* void drawRoundedRect ( const QRectF & rect, qreal xRadius, qreal yRadius, Qt::SizeMode mode = Qt::AbsoluteSize ) */
HB_FUNC( QT_QPAINTER_DRAWROUNDEDRECT )
{
   QPainter * p = hbqt_par_QPainter( 1 );
   if( p )
      ( p )->drawRoundedRect( *hbqt_par_QRectF( 2 ), hb_parnd( 3 ), hb_parnd( 4 ), ( HB_ISNUM( 5 ) ? ( Qt::SizeMode ) hb_parni( 5 ) : ( Qt::SizeMode ) Qt::AbsoluteSize ) );
}

/* void drawRoundedRect ( const QRect & rect, qreal xRadius, qreal yRadius, Qt::SizeMode mode = Qt::AbsoluteSize ) */
HB_FUNC( QT_QPAINTER_DRAWROUNDEDRECT_1 )
{
   QPainter * p = hbqt_par_QPainter( 1 );
   if( p )
      ( p )->drawRoundedRect( *hbqt_par_QRect( 2 ), hb_parnd( 3 ), hb_parnd( 4 ), ( HB_ISNUM( 5 ) ? ( Qt::SizeMode ) hb_parni( 5 ) : ( Qt::SizeMode ) Qt::AbsoluteSize ) );
}

/* void drawRoundedRect ( int x, int y, int w, int h, qreal xRadius, qreal yRadius, Qt::SizeMode mode = Qt::AbsoluteSize ) */
HB_FUNC( QT_QPAINTER_DRAWROUNDEDRECT_2 )
{
   QPainter * p = hbqt_par_QPainter( 1 );
   if( p )
      ( p )->drawRoundedRect( hb_parni( 2 ), hb_parni( 3 ), hb_parni( 4 ), hb_parni( 5 ), hb_parnd( 6 ), hb_parnd( 7 ), ( HB_ISNUM( 8 ) ? ( Qt::SizeMode ) hb_parni( 8 ) : ( Qt::SizeMode ) Qt::AbsoluteSize ) );
}

/* void drawText ( const QPointF & position, const QString & text ) */
HB_FUNC( QT_QPAINTER_DRAWTEXT )
{
   QPainter * p = hbqt_par_QPainter( 1 );
   if( p )
   {
      void * pText;
      ( p )->drawText( *hbqt_par_QPointF( 2 ), hb_parstr_utf8( 3, &pText, NULL ) );
      hb_strfree( pText );
   }
}

/* void drawText ( const QPoint & position, const QString & text ) */
HB_FUNC( QT_QPAINTER_DRAWTEXT_1 )
{
   QPainter * p = hbqt_par_QPainter( 1 );
   if( p )
   {
      void * pText;
      ( p )->drawText( *hbqt_par_QPoint( 2 ), hb_parstr_utf8( 3, &pText, NULL ) );
      hb_strfree( pText );
   }
}

/* void drawText ( const QRectF & rectangle, int flags, const QString & text, QRectF * boundingRect = 0 ) */
HB_FUNC( QT_QPAINTER_DRAWTEXT_2 )
{
   QPainter * p = hbqt_par_QPainter( 1 );
   if( p )
   {
      void * pText;
      ( p )->drawText( *hbqt_par_QRectF( 2 ), hb_parni( 3 ), hb_parstr_utf8( 4, &pText, NULL ), hbqt_par_QRectF( 5 ) );
      hb_strfree( pText );
   }
}

/* void drawText ( const QRect & rectangle, int flags, const QString & text, QRect * boundingRect = 0 ) */
HB_FUNC( QT_QPAINTER_DRAWTEXT_3 )
{
   QPainter * p = hbqt_par_QPainter( 1 );
   if( p )
   {
      void * pText;
      ( p )->drawText( *hbqt_par_QRect( 2 ), hb_parni( 3 ), hb_parstr_utf8( 4, &pText, NULL ), hbqt_par_QRect( 5 ) );
      hb_strfree( pText );
   }
}

/* void drawText ( int x, int y, const QString & text ) */
HB_FUNC( QT_QPAINTER_DRAWTEXT_4 )
{
   QPainter * p = hbqt_par_QPainter( 1 );
   if( p )
   {
      void * pText;
      ( p )->drawText( hb_parni( 2 ), hb_parni( 3 ), hb_parstr_utf8( 4, &pText, NULL ) );
      hb_strfree( pText );
   }
}

/* void drawText ( int x, int y, int width, int height, int flags, const QString & text, QRect * boundingRect = 0 ) */
HB_FUNC( QT_QPAINTER_DRAWTEXT_5 )
{
   QPainter * p = hbqt_par_QPainter( 1 );
   if( p )
   {
      void * pText;
      ( p )->drawText( hb_parni( 2 ), hb_parni( 3 ), hb_parni( 4 ), hb_parni( 5 ), hb_parni( 6 ), hb_parstr_utf8( 7, &pText, NULL ), hbqt_par_QRect( 8 ) );
      hb_strfree( pText );
   }
}

/* void drawText ( const QRectF & rectangle, const QString & text, const QTextOption & option = QTextOption() ) */
HB_FUNC( QT_QPAINTER_DRAWTEXT_6 )
{
   QPainter * p = hbqt_par_QPainter( 1 );
   if( p )
   {
      void * pText;
      ( p )->drawText( *hbqt_par_QRectF( 2 ), hb_parstr_utf8( 3, &pText, NULL ), ( HB_ISOBJECT( 4 ) ? *hbqt_par_QTextOption( 4 ) : QTextOption() ) );
      hb_strfree( pText );
   }
}

/* void drawTiledPixmap ( const QRectF & rectangle, const QPixmap & pixmap, const QPointF & position = QPointF() ) */
HB_FUNC( QT_QPAINTER_DRAWTILEDPIXMAP )
{
   QPainter * p = hbqt_par_QPainter( 1 );
   if( p )
      ( p )->drawTiledPixmap( *hbqt_par_QRectF( 2 ), *hbqt_par_QPixmap( 3 ), ( HB_ISOBJECT( 4 ) ? *hbqt_par_QPointF( 4 ) : QPointF() ) );
}

/* void drawTiledPixmap ( const QRect & rectangle, const QPixmap & pixmap, const QPoint & position = QPoint() ) */
HB_FUNC( QT_QPAINTER_DRAWTILEDPIXMAP_1 )
{
   QPainter * p = hbqt_par_QPainter( 1 );
   if( p )
      ( p )->drawTiledPixmap( *hbqt_par_QRect( 2 ), *hbqt_par_QPixmap( 3 ), ( HB_ISOBJECT( 4 ) ? *hbqt_par_QPoint( 4 ) : QPoint() ) );
}

/* void drawTiledPixmap ( int x, int y, int width, int height, const QPixmap & pixmap, int sx = 0, int sy = 0 ) */
HB_FUNC( QT_QPAINTER_DRAWTILEDPIXMAP_2 )
{
   QPainter * p = hbqt_par_QPainter( 1 );
   if( p )
      ( p )->drawTiledPixmap( hb_parni( 2 ), hb_parni( 3 ), hb_parni( 4 ), hb_parni( 5 ), *hbqt_par_QPixmap( 6 ), hb_parni( 7 ), hb_parni( 8 ) );
}

/* bool end () */
HB_FUNC( QT_QPAINTER_END )
{
   QPainter * p = hbqt_par_QPainter( 1 );
   if( p )
      hb_retl( ( p )->end() );
}

/* void eraseRect ( const QRectF & rectangle ) */
HB_FUNC( QT_QPAINTER_ERASERECT )
{
   QPainter * p = hbqt_par_QPainter( 1 );
   if( p )
      ( p )->eraseRect( *hbqt_par_QRectF( 2 ) );
}

/* void eraseRect ( const QRect & rectangle ) */
HB_FUNC( QT_QPAINTER_ERASERECT_1 )
{
   QPainter * p = hbqt_par_QPainter( 1 );
   if( p )
      ( p )->eraseRect( *hbqt_par_QRect( 2 ) );
}

/* void eraseRect ( int x, int y, int width, int height ) */
HB_FUNC( QT_QPAINTER_ERASERECT_2 )
{
   QPainter * p = hbqt_par_QPainter( 1 );
   if( p )
      ( p )->eraseRect( hb_parni( 2 ), hb_parni( 3 ), hb_parni( 4 ), hb_parni( 5 ) );
}

/* void fillPath ( const QPainterPath & path, const QBrush & brush ) */
HB_FUNC( QT_QPAINTER_FILLPATH )
{
   QPainter * p = hbqt_par_QPainter( 1 );
   if( p )
      ( p )->fillPath( *hbqt_par_QPainterPath( 2 ), *hbqt_par_QBrush( 3 ) );
}

/* void fillRect ( const QRectF & rectangle, const QBrush & brush ) */
HB_FUNC( QT_QPAINTER_FILLRECT )
{
   QPainter * p = hbqt_par_QPainter( 1 );
   if( p )
      ( p )->fillRect( *hbqt_par_QRectF( 2 ), *hbqt_par_QBrush( 3 ) );
}

/* void fillRect ( const QRect  & rectangle, const QBrush & brush ) */
HB_FUNC( QT_QPAINTER_FILLRECT_1 )
{
   QPainter * p = hbqt_par_QPainter( 1 );
   if( p )
      ( p )->fillRect( *hbqt_par_QRect( 2 ), *hbqt_par_QBrush( 3 ) );
}

/* void fillRect ( const QRect  & rectangle, const QColor & color ) */
HB_FUNC( QT_QPAINTER_FILLRECT_2 )
{
   QPainter * p = hbqt_par_QPainter( 1 );
   if( p )
      ( p )->fillRect( *hbqt_par_QRect( 2 ), *hbqt_par_QColor( 3 ) );
}

/* void fillRect ( const QRectF & rectangle, const QColor & color ) */
HB_FUNC( QT_QPAINTER_FILLRECT_3 )
{
   QPainter * p = hbqt_par_QPainter( 1 );
   if( p )
      ( p )->fillRect( *hbqt_par_QRectF( 2 ), *hbqt_par_QColor( 3 ) );
}

/* void fillRect ( int x, int y, int width, int height, const QBrush & brush ) */
HB_FUNC( QT_QPAINTER_FILLRECT_4 )
{
   QPainter * p = hbqt_par_QPainter( 1 );
   if( p )
      ( p )->fillRect( hb_parni( 2 ), hb_parni( 3 ), hb_parni( 4 ), hb_parni( 5 ), *hbqt_par_QBrush( 6 ) );
}

/* void fillRect ( int x, int y, int width, int height, const QColor & color ) */
HB_FUNC( QT_QPAINTER_FILLRECT_5 )
{
   QPainter * p = hbqt_par_QPainter( 1 );
   if( p )
      ( p )->fillRect( hb_parni( 2 ), hb_parni( 3 ), hb_parni( 4 ), hb_parni( 5 ), *hbqt_par_QColor( 6 ) );
}

/* void fillRect ( int x, int y, int width, int height, Qt::GlobalColor color ) */
HB_FUNC( QT_QPAINTER_FILLRECT_6 )
{
   QPainter * p = hbqt_par_QPainter( 1 );
   if( p )
      ( p )->fillRect( hb_parni( 2 ), hb_parni( 3 ), hb_parni( 4 ), hb_parni( 5 ), ( Qt::GlobalColor ) hb_parni( 6 ) );
}

/* void fillRect ( const QRect  & rectangle, Qt::GlobalColor color ) */
HB_FUNC( QT_QPAINTER_FILLRECT_7 )
{
   QPainter * p = hbqt_par_QPainter( 1 );
   if( p )
      ( p )->fillRect( *hbqt_par_QRect( 2 ), ( Qt::GlobalColor ) hb_parni( 3 ) );
}

/* void fillRect ( const QRectF & rectangle, Qt::GlobalColor color ) */
HB_FUNC( QT_QPAINTER_FILLRECT_8 )
{
   QPainter * p = hbqt_par_QPainter( 1 );
   if( p )
      ( p )->fillRect( *hbqt_par_QRectF( 2 ), ( Qt::GlobalColor ) hb_parni( 3 ) );
}

/* const QFont & font () const */
HB_FUNC( QT_QPAINTER_FONT )
{
   QPainter * p = hbqt_par_QPainter( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QFont( new QFont( ( p )->font() ), true ) );
}

/* QFontInfo fontInfo () const */
HB_FUNC( QT_QPAINTER_FONTINFO )
{
   QPainter * p = hbqt_par_QPainter( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QFontInfo( new QFontInfo( ( p )->fontInfo() ), true ) );
}

/* QFontMetrics fontMetrics () const */
HB_FUNC( QT_QPAINTER_FONTMETRICS )
{
   QPainter * p = hbqt_par_QPainter( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QFontMetrics( new QFontMetrics( ( p )->fontMetrics() ), true ) );
}

/* bool hasClipping () const */
HB_FUNC( QT_QPAINTER_HASCLIPPING )
{
   QPainter * p = hbqt_par_QPainter( 1 );
   if( p )
      hb_retl( ( p )->hasClipping() );
}

/* void initFrom ( const QWidget * widget ) */
HB_FUNC( QT_QPAINTER_INITFROM )
{
   QPainter * p = hbqt_par_QPainter( 1 );
   if( p )
      ( p )->initFrom( hbqt_par_QWidget( 2 ) );
}

/* bool isActive () const */
HB_FUNC( QT_QPAINTER_ISACTIVE )
{
   QPainter * p = hbqt_par_QPainter( 1 );
   if( p )
      hb_retl( ( p )->isActive() );
}

/* Qt::LayoutDirection layoutDirection () const */
HB_FUNC( QT_QPAINTER_LAYOUTDIRECTION )
{
   QPainter * p = hbqt_par_QPainter( 1 );
   if( p )
      hb_retni( ( Qt::LayoutDirection ) ( p )->layoutDirection() );
}

/* qreal opacity () const */
HB_FUNC( QT_QPAINTER_OPACITY )
{
   QPainter * p = hbqt_par_QPainter( 1 );
   if( p )
      hb_retnd( ( p )->opacity() );
}

/* QPaintEngine * paintEngine () const */
HB_FUNC( QT_QPAINTER_PAINTENGINE )
{
   QPainter * p = hbqt_par_QPainter( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QPaintEngine( ( p )->paintEngine(), false ) );
}

/* const QPen & pen () const */
HB_FUNC( QT_QPAINTER_PEN )
{
   QPainter * p = hbqt_par_QPainter( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QPen( new QPen( ( p )->pen() ), true ) );
}

/* RenderHints renderHints () const */
HB_FUNC( QT_QPAINTER_RENDERHINTS )
{
   QPainter * p = hbqt_par_QPainter( 1 );
   if( p )
      hb_retni( ( QPainter::RenderHints ) ( p )->renderHints() );
}

/* void resetMatrix () */
HB_FUNC( QT_QPAINTER_RESETMATRIX )
{
   QPainter * p = hbqt_par_QPainter( 1 );
   if( p )
      ( p )->resetMatrix();
}

/* void resetTransform () */
HB_FUNC( QT_QPAINTER_RESETTRANSFORM )
{
   QPainter * p = hbqt_par_QPainter( 1 );
   if( p )
      ( p )->resetTransform();
}

/* void restore () */
HB_FUNC( QT_QPAINTER_RESTORE )
{
   QPainter * p = hbqt_par_QPainter( 1 );
   if( p )
      ( p )->restore();
}

/* void rotate ( qreal angle ) */
HB_FUNC( QT_QPAINTER_ROTATE )
{
   QPainter * p = hbqt_par_QPainter( 1 );
   if( p )
      ( p )->rotate( hb_parnd( 2 ) );
}

/* void save () */
HB_FUNC( QT_QPAINTER_SAVE )
{
   QPainter * p = hbqt_par_QPainter( 1 );
   if( p )
      ( p )->save();
}

/* void scale ( qreal sx, qreal sy ) */
HB_FUNC( QT_QPAINTER_SCALE )
{
   QPainter * p = hbqt_par_QPainter( 1 );
   if( p )
      ( p )->scale( hb_parnd( 2 ), hb_parnd( 3 ) );
}

/* void setBackground ( const QBrush & brush ) */
HB_FUNC( QT_QPAINTER_SETBACKGROUND )
{
   QPainter * p = hbqt_par_QPainter( 1 );
   if( p )
      ( p )->setBackground( *hbqt_par_QBrush( 2 ) );
}

/* void setBackgroundMode ( Qt::BGMode mode ) */
HB_FUNC( QT_QPAINTER_SETBACKGROUNDMODE )
{
   QPainter * p = hbqt_par_QPainter( 1 );
   if( p )
      ( p )->setBackgroundMode( ( Qt::BGMode ) hb_parni( 2 ) );
}

/* void setBrush ( const QBrush & brush ) */
HB_FUNC( QT_QPAINTER_SETBRUSH )
{
   QPainter * p = hbqt_par_QPainter( 1 );
   if( p )
      ( p )->setBrush( *hbqt_par_QBrush( 2 ) );
}

/* void setBrush ( Qt::BrushStyle style ) */
HB_FUNC( QT_QPAINTER_SETBRUSH_1 )
{
   QPainter * p = hbqt_par_QPainter( 1 );
   if( p )
      ( p )->setBrush( ( Qt::BrushStyle ) hb_parni( 2 ) );
}

/* void setBrushOrigin ( const QPointF & position ) */
HB_FUNC( QT_QPAINTER_SETBRUSHORIGIN )
{
   QPainter * p = hbqt_par_QPainter( 1 );
   if( p )
      ( p )->setBrushOrigin( *hbqt_par_QPointF( 2 ) );
}

/* void setBrushOrigin ( const QPoint & position ) */
HB_FUNC( QT_QPAINTER_SETBRUSHORIGIN_1 )
{
   QPainter * p = hbqt_par_QPainter( 1 );
   if( p )
      ( p )->setBrushOrigin( *hbqt_par_QPoint( 2 ) );
}

/* void setBrushOrigin ( int x, int y ) */
HB_FUNC( QT_QPAINTER_SETBRUSHORIGIN_2 )
{
   QPainter * p = hbqt_par_QPainter( 1 );
   if( p )
      ( p )->setBrushOrigin( hb_parni( 2 ), hb_parni( 3 ) );
}

/* void setClipPath ( const QPainterPath & path, Qt::ClipOperation operation = Qt::ReplaceClip ) */
HB_FUNC( QT_QPAINTER_SETCLIPPATH )
{
   QPainter * p = hbqt_par_QPainter( 1 );
   if( p )
      ( p )->setClipPath( *hbqt_par_QPainterPath( 2 ), ( HB_ISNUM( 3 ) ? ( Qt::ClipOperation ) hb_parni( 3 ) : ( Qt::ClipOperation ) Qt::ReplaceClip ) );
}

/* void setClipRect ( const QRectF & rectangle, Qt::ClipOperation operation = Qt::ReplaceClip ) */
HB_FUNC( QT_QPAINTER_SETCLIPRECT )
{
   QPainter * p = hbqt_par_QPainter( 1 );
   if( p )
      ( p )->setClipRect( *hbqt_par_QRectF( 2 ), ( HB_ISNUM( 3 ) ? ( Qt::ClipOperation ) hb_parni( 3 ) : ( Qt::ClipOperation ) Qt::ReplaceClip ) );
}

/* void setClipRect ( int x, int y, int width, int height, Qt::ClipOperation operation = Qt::ReplaceClip ) */
HB_FUNC( QT_QPAINTER_SETCLIPRECT_1 )
{
   QPainter * p = hbqt_par_QPainter( 1 );
   if( p )
      ( p )->setClipRect( hb_parni( 2 ), hb_parni( 3 ), hb_parni( 4 ), hb_parni( 5 ), ( HB_ISNUM( 6 ) ? ( Qt::ClipOperation ) hb_parni( 6 ) : ( Qt::ClipOperation ) Qt::ReplaceClip ) );
}

/* void setClipRect ( const QRect & rectangle, Qt::ClipOperation operation = Qt::ReplaceClip ) */
HB_FUNC( QT_QPAINTER_SETCLIPRECT_2 )
{
   QPainter * p = hbqt_par_QPainter( 1 );
   if( p )
      ( p )->setClipRect( *hbqt_par_QRect( 2 ), ( HB_ISNUM( 3 ) ? ( Qt::ClipOperation ) hb_parni( 3 ) : ( Qt::ClipOperation ) Qt::ReplaceClip ) );
}

/* void setClipRegion ( const QRegion & region, Qt::ClipOperation operation = Qt::ReplaceClip ) */
HB_FUNC( QT_QPAINTER_SETCLIPREGION )
{
   QPainter * p = hbqt_par_QPainter( 1 );
   if( p )
      ( p )->setClipRegion( *hbqt_par_QRegion( 2 ), ( HB_ISNUM( 3 ) ? ( Qt::ClipOperation ) hb_parni( 3 ) : ( Qt::ClipOperation ) Qt::ReplaceClip ) );
}

/* void setClipping ( bool enable ) */
HB_FUNC( QT_QPAINTER_SETCLIPPING )
{
   QPainter * p = hbqt_par_QPainter( 1 );
   if( p )
      ( p )->setClipping( hb_parl( 2 ) );
}

/* void setCompositionMode ( CompositionMode mode ) */
HB_FUNC( QT_QPAINTER_SETCOMPOSITIONMODE )
{
   QPainter * p = hbqt_par_QPainter( 1 );
   if( p )
      ( p )->setCompositionMode( ( QPainter::CompositionMode ) hb_parni( 2 ) );
}

/* void setFont ( const QFont & font ) */
HB_FUNC( QT_QPAINTER_SETFONT )
{
   QPainter * p = hbqt_par_QPainter( 1 );
   if( p )
      ( p )->setFont( *hbqt_par_QFont( 2 ) );
}

/* void setLayoutDirection ( Qt::LayoutDirection direction ) */
HB_FUNC( QT_QPAINTER_SETLAYOUTDIRECTION )
{
   QPainter * p = hbqt_par_QPainter( 1 );
   if( p )
      ( p )->setLayoutDirection( ( Qt::LayoutDirection ) hb_parni( 2 ) );
}

/* void setOpacity ( qreal opacity ) */
HB_FUNC( QT_QPAINTER_SETOPACITY )
{
   QPainter * p = hbqt_par_QPainter( 1 );
   if( p )
      ( p )->setOpacity( hb_parnd( 2 ) );
}

/* void setPen ( const QPen & pen ) */
HB_FUNC( QT_QPAINTER_SETPEN )
{
   QPainter * p = hbqt_par_QPainter( 1 );
   if( p )
      ( p )->setPen( *hbqt_par_QPen( 2 ) );
}

/* void setPen ( const QColor & color ) */
HB_FUNC( QT_QPAINTER_SETPEN_1 )
{
   QPainter * p = hbqt_par_QPainter( 1 );
   if( p )
      ( p )->setPen( *hbqt_par_QColor( 2 ) );
}

/* void setPen ( Qt::PenStyle style ) */
HB_FUNC( QT_QPAINTER_SETPEN_2 )
{
   QPainter * p = hbqt_par_QPainter( 1 );
   if( p )
      ( p )->setPen( ( Qt::PenStyle ) hb_parni( 2 ) );
}

/* void setRenderHint ( RenderHint hint, bool on = true ) */
HB_FUNC( QT_QPAINTER_SETRENDERHINT )
{
   QPainter * p = hbqt_par_QPainter( 1 );
   if( p )
      ( p )->setRenderHint( ( QPainter::RenderHint ) hb_parni( 2 ), hb_parl( 3 ) );
}

/* void setRenderHints ( RenderHints hints, bool on = true ) */
HB_FUNC( QT_QPAINTER_SETRENDERHINTS )
{
   QPainter * p = hbqt_par_QPainter( 1 );
   if( p )
      ( p )->setRenderHints( ( QPainter::RenderHints ) hb_parni( 2 ), hb_parl( 3 ) );
}

/* void setTransform ( const QTransform & transform, bool combine = false ) */
HB_FUNC( QT_QPAINTER_SETTRANSFORM )
{
   QPainter * p = hbqt_par_QPainter( 1 );
   if( p )
      ( p )->setTransform( *hbqt_par_QTransform( 2 ), hb_parl( 3 ) );
}

/* void setViewTransformEnabled ( bool enable ) */
HB_FUNC( QT_QPAINTER_SETVIEWTRANSFORMENABLED )
{
   QPainter * p = hbqt_par_QPainter( 1 );
   if( p )
      ( p )->setViewTransformEnabled( hb_parl( 2 ) );
}

/* void setViewport ( const QRect & rectangle ) */
HB_FUNC( QT_QPAINTER_SETVIEWPORT )
{
   QPainter * p = hbqt_par_QPainter( 1 );
   if( p )
      ( p )->setViewport( *hbqt_par_QRect( 2 ) );
}

/* void setViewport ( int x, int y, int width, int height ) */
HB_FUNC( QT_QPAINTER_SETVIEWPORT_1 )
{
   QPainter * p = hbqt_par_QPainter( 1 );
   if( p )
      ( p )->setViewport( hb_parni( 2 ), hb_parni( 3 ), hb_parni( 4 ), hb_parni( 5 ) );
}

/* void setWindow ( const QRect & rectangle ) */
HB_FUNC( QT_QPAINTER_SETWINDOW )
{
   QPainter * p = hbqt_par_QPainter( 1 );
   if( p )
      ( p )->setWindow( *hbqt_par_QRect( 2 ) );
}

/* void setWindow ( int x, int y, int width, int height ) */
HB_FUNC( QT_QPAINTER_SETWINDOW_1 )
{
   QPainter * p = hbqt_par_QPainter( 1 );
   if( p )
      ( p )->setWindow( hb_parni( 2 ), hb_parni( 3 ), hb_parni( 4 ), hb_parni( 5 ) );
}

/* void setWorldMatrix ( const QMatrix & matrix, bool combine = false ) */
HB_FUNC( QT_QPAINTER_SETWORLDMATRIX )
{
   QPainter * p = hbqt_par_QPainter( 1 );
   if( p )
      ( p )->setWorldMatrix( *hbqt_par_QMatrix( 2 ), hb_parl( 3 ) );
}

/* void setWorldMatrixEnabled ( bool enable ) */
HB_FUNC( QT_QPAINTER_SETWORLDMATRIXENABLED )
{
   QPainter * p = hbqt_par_QPainter( 1 );
   if( p )
      ( p )->setWorldMatrixEnabled( hb_parl( 2 ) );
}

/* void setWorldTransform ( const QTransform & matrix, bool combine = false ) */
HB_FUNC( QT_QPAINTER_SETWORLDTRANSFORM )
{
   QPainter * p = hbqt_par_QPainter( 1 );
   if( p )
      ( p )->setWorldTransform( *hbqt_par_QTransform( 2 ), hb_parl( 3 ) );
}

/* void shear ( qreal sh, qreal sv ) */
HB_FUNC( QT_QPAINTER_SHEAR )
{
   QPainter * p = hbqt_par_QPainter( 1 );
   if( p )
      ( p )->shear( hb_parnd( 2 ), hb_parnd( 3 ) );
}

/* void strokePath ( const QPainterPath & path, const QPen & pen ) */
HB_FUNC( QT_QPAINTER_STROKEPATH )
{
   QPainter * p = hbqt_par_QPainter( 1 );
   if( p )
      ( p )->strokePath( *hbqt_par_QPainterPath( 2 ), *hbqt_par_QPen( 3 ) );
}

/* bool testRenderHint ( RenderHint hint ) const */
HB_FUNC( QT_QPAINTER_TESTRENDERHINT )
{
   QPainter * p = hbqt_par_QPainter( 1 );
   if( p )
      hb_retl( ( p )->testRenderHint( ( QPainter::RenderHint ) hb_parni( 2 ) ) );
}

/* const QTransform & transform () const */
HB_FUNC( QT_QPAINTER_TRANSFORM )
{
   QPainter * p = hbqt_par_QPainter( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QTransform( new QTransform( ( p )->transform() ), true ) );
}

/* void translate ( const QPointF & offset ) */
HB_FUNC( QT_QPAINTER_TRANSLATE )
{
   QPainter * p = hbqt_par_QPainter( 1 );
   if( p )
      ( p )->translate( *hbqt_par_QPointF( 2 ) );
}

/* void translate ( const QPoint & offset ) */
HB_FUNC( QT_QPAINTER_TRANSLATE_1 )
{
   QPainter * p = hbqt_par_QPainter( 1 );
   if( p )
      ( p )->translate( *hbqt_par_QPoint( 2 ) );
}

/* void translate ( qreal dx, qreal dy ) */
HB_FUNC( QT_QPAINTER_TRANSLATE_2 )
{
   QPainter * p = hbqt_par_QPainter( 1 );
   if( p )
      ( p )->translate( hb_parnd( 2 ), hb_parnd( 3 ) );
}

/* bool viewTransformEnabled () const */
HB_FUNC( QT_QPAINTER_VIEWTRANSFORMENABLED )
{
   QPainter * p = hbqt_par_QPainter( 1 );
   if( p )
      hb_retl( ( p )->viewTransformEnabled() );
}

/* QRect viewport () const */
HB_FUNC( QT_QPAINTER_VIEWPORT )
{
   QPainter * p = hbqt_par_QPainter( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QRect( new QRect( ( p )->viewport() ), true ) );
}

/* QRect window () const */
HB_FUNC( QT_QPAINTER_WINDOW )
{
   QPainter * p = hbqt_par_QPainter( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QRect( new QRect( ( p )->window() ), true ) );
}

/* const QMatrix & worldMatrix () const */
HB_FUNC( QT_QPAINTER_WORLDMATRIX )
{
   QPainter * p = hbqt_par_QPainter( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QMatrix( new QMatrix( ( p )->worldMatrix() ), true ) );
}

/* bool worldMatrixEnabled () const */
HB_FUNC( QT_QPAINTER_WORLDMATRIXENABLED )
{
   QPainter * p = hbqt_par_QPainter( 1 );
   if( p )
      hb_retl( ( p )->worldMatrixEnabled() );
}

/* const QTransform & worldTransform () const */
HB_FUNC( QT_QPAINTER_WORLDTRANSFORM )
{
   QPainter * p = hbqt_par_QPainter( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QTransform( new QTransform( ( p )->worldTransform() ), true ) );
}

/* QPaintDevice * redirected ( const QPaintDevice * device, QPoint * offset = 0 ) */
HB_FUNC( QT_QPAINTER_REDIRECTED )
{
   QPainter * p = hbqt_par_QPainter( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QPaintDevice( ( p )->redirected( hbqt_par_QPaintDevice( 2 ), hbqt_par_QPoint( 3 ) ), false ) );
}

/* void restoreRedirected ( const QPaintDevice * device ) */
HB_FUNC( QT_QPAINTER_RESTOREREDIRECTED )
{
   QPainter * p = hbqt_par_QPainter( 1 );
   if( p )
      ( p )->restoreRedirected( hbqt_par_QPaintDevice( 2 ) );
}

/* void setRedirected ( const QPaintDevice * device, QPaintDevice * replacement, const QPoint & offset = QPoint() ) */
HB_FUNC( QT_QPAINTER_SETREDIRECTED )
{
   QPainter * p = hbqt_par_QPainter( 1 );
   if( p )
      ( p )->setRedirected( hbqt_par_QPaintDevice( 2 ), hbqt_par_QPaintDevice( 3 ), ( HB_ISOBJECT( 4 ) ? *hbqt_par_QPoint( 4 ) : QPoint() ) );
}


#endif /* #if QT_VERSION >= 0x040500 */
