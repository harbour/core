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
 *  enum CompositionMode { CompositionMode_SourceOver, CompositionMode_DestinationOver, CompositionMode_Clear, CompositionMode_Source, ..., RasterOp_SourceAndNotDestination }
 *  enum RenderHint { Antialiasing, TextAntialiasing, SmoothPixmapTransform, HighQualityAntialiasing, NonCosmeticDefaultPen }
 *  enum RenderHints
 */

/*
 *  Constructed[ 182/188 [ 96.81% ] ]
 *
 *  *** Unconvered Prototypes ***
 *  -----------------------------
 *
 *  void drawLines ( const QVector<QPointF> & pointPairs )
 *  void drawLines ( const QVector<QPoint> & pointPairs )
 *  void drawLines ( const QVector<QLineF> & lines )
 *  void drawLines ( const QVector<QLine> & lines )
 *  void drawRects ( const QVector<QRectF> & rectangles )
 *  void drawRects ( const QVector<QRect> & rectangles )
 */

#include <QtCore/QPointer>

#include <QtGui/QPainter>

/*
 * QPainter ()
 * QPainter ( QPaintDevice * device )
 * ~QPainter ()
 */

QT_G_FUNC( release_QPainter )
{
   QGC_POINTER * p = ( QGC_POINTER * ) Cargo;

   HB_TRACE( HB_TR_DEBUG, ( "release_QPainter                     p=%p", p ) );
   HB_TRACE( HB_TR_DEBUG, ( "release_QPainter                    ph=%p", p->ph ) );

   if( p && p->ph )
   {
      delete ( ( QPainter * ) p->ph );
      p->ph = NULL;
      HB_TRACE( HB_TR_DEBUG, ( "release_QPainter                    Object deleted!" ) );
      #if defined( __HB_DEBUG__ )
         hbqt_debug( "  YES release_QPainter                    %i B %i KB", ( int ) hb_xquery( 1001 ), hbqt_getmemused() );
      #endif
   }
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "release_QPainter                    Object Allready deleted!" ) );
      #if defined( __HB_DEBUG__ )
         hbqt_debug( "  DEL release_QPainter" );
      #endif
   }
}

void * gcAllocate_QPainter( void * pObj )
{
   QGC_POINTER * p = ( QGC_POINTER * ) hb_gcAllocate( sizeof( QGC_POINTER ), gcFuncs() );

   p->ph = pObj;
   p->func = release_QPainter;
   #if defined( __HB_DEBUG__ )
      hbqt_debug( "          new_QPainter                    %i B %i KB", ( int ) hb_xquery( 1001 ), hbqt_getmemused() );
   #endif
   return( p );
}

HB_FUNC( QT_QPAINTER )
{
   void * pObj = NULL;

   if( hb_pcount() >= 1 && HB_ISPOINTER( 1 ) )
   {
      pObj = new QPainter( hbqt_par_QPaintDevice( 1 ) ) ;
   }
   else
   {
      pObj = new QPainter() ;
   }

   hb_retptrGC( gcAllocate_QPainter( pObj ) );
}
/*
 * const QBrush & background () const
 */
HB_FUNC( QT_QPAINTER_BACKGROUND )
{
   hb_retptrGC( gcAllocate_QBrush( new QBrush( hbqt_par_QPainter( 1 )->background() ) ) );
}

/*
 * Qt::BGMode backgroundMode () const
 */
HB_FUNC( QT_QPAINTER_BACKGROUNDMODE )
{
   hb_retni( ( Qt::BGMode ) hbqt_par_QPainter( 1 )->backgroundMode() );
}

/*
 * bool begin ( QPaintDevice * device )
 */
HB_FUNC( QT_QPAINTER_BEGIN )
{
   hb_retl( hbqt_par_QPainter( 1 )->begin( hbqt_par_QPaintDevice( 2 ) ) );
}

/*
 * QRectF boundingRect ( const QRectF & rectangle, int flags, const QString & text )
 */
HB_FUNC( QT_QPAINTER_BOUNDINGRECT )
{
   hb_retptrGC( gcAllocate_QRectF( new QRectF( hbqt_par_QPainter( 1 )->boundingRect( *hbqt_par_QRectF( 2 ), hb_parni( 3 ), hbqt_par_QString( 4 ) ) ) ) );
}

/*
 * QRect boundingRect ( const QRect & rectangle, int flags, const QString & text )
 */
HB_FUNC( QT_QPAINTER_BOUNDINGRECT_1 )
{
   hb_retptrGC( gcAllocate_QRect( new QRect( hbqt_par_QPainter( 1 )->boundingRect( *hbqt_par_QRect( 2 ), hb_parni( 3 ), hbqt_par_QString( 4 ) ) ) ) );
}

/*
 * QRect boundingRect ( int x, int y, int w, int h, int flags, const QString & text )
 */
HB_FUNC( QT_QPAINTER_BOUNDINGRECT_2 )
{
   hb_retptrGC( gcAllocate_QRect( new QRect( hbqt_par_QPainter( 1 )->boundingRect( hb_parni( 2 ), hb_parni( 3 ), hb_parni( 4 ), hb_parni( 5 ), hb_parni( 6 ), hbqt_par_QString( 7 ) ) ) ) );
}

/*
 * QRectF boundingRect ( const QRectF & rectangle, const QString & text, const QTextOption & option = QTextOption() )
 */
HB_FUNC( QT_QPAINTER_BOUNDINGRECT_3 )
{
   hb_retptrGC( gcAllocate_QRectF( new QRectF( hbqt_par_QPainter( 1 )->boundingRect( *hbqt_par_QRectF( 2 ), hbqt_par_QString( 3 ), ( HB_ISPOINTER( 4 ) ? *hbqt_par_QTextOption( 4 ) : QTextOption() ) ) ) ) );
}

/*
 * const QBrush & brush () const
 */
HB_FUNC( QT_QPAINTER_BRUSH )
{
   hb_retptrGC( gcAllocate_QBrush( new QBrush( hbqt_par_QPainter( 1 )->brush() ) ) );
}

/*
 * QPoint brushOrigin () const
 */
HB_FUNC( QT_QPAINTER_BRUSHORIGIN )
{
   hb_retptrGC( gcAllocate_QPoint( new QPoint( hbqt_par_QPainter( 1 )->brushOrigin() ) ) );
}

/*
 * QPainterPath clipPath () const
 */
HB_FUNC( QT_QPAINTER_CLIPPATH )
{
   hb_retptrGC( gcAllocate_QPainterPath( new QPainterPath( hbqt_par_QPainter( 1 )->clipPath() ) ) );
}

/*
 * QRegion clipRegion () const
 */
HB_FUNC( QT_QPAINTER_CLIPREGION )
{
   hb_retptrGC( gcAllocate_QRegion( new QRegion( hbqt_par_QPainter( 1 )->clipRegion() ) ) );
}

/*
 * QMatrix combinedMatrix () const
 */
HB_FUNC( QT_QPAINTER_COMBINEDMATRIX )
{
   hb_retptrGC( gcAllocate_QMatrix( new QMatrix( hbqt_par_QPainter( 1 )->combinedMatrix() ) ) );
}

/*
 * QTransform combinedTransform () const
 */
HB_FUNC( QT_QPAINTER_COMBINEDTRANSFORM )
{
   hb_retptrGC( gcAllocate_QTransform( new QTransform( hbqt_par_QPainter( 1 )->combinedTransform() ) ) );
}

/*
 * CompositionMode compositionMode () const
 */
HB_FUNC( QT_QPAINTER_COMPOSITIONMODE )
{
   hb_retni( ( QPainter::CompositionMode ) hbqt_par_QPainter( 1 )->compositionMode() );
}

/*
 * QPaintDevice * device () const
 */
HB_FUNC( QT_QPAINTER_DEVICE )
{
   hb_retptr( ( QPaintDevice* ) hbqt_par_QPainter( 1 )->device() );
}

/*
 * const QMatrix & deviceMatrix () const
 */
HB_FUNC( QT_QPAINTER_DEVICEMATRIX )
{
   hb_retptrGC( gcAllocate_QMatrix( new QMatrix( hbqt_par_QPainter( 1 )->deviceMatrix() ) ) );
}

/*
 * const QTransform & deviceTransform () const
 */
HB_FUNC( QT_QPAINTER_DEVICETRANSFORM )
{
   hb_retptrGC( gcAllocate_QTransform( new QTransform( hbqt_par_QPainter( 1 )->deviceTransform() ) ) );
}

/*
 * void drawArc ( const QRectF & rectangle, int startAngle, int spanAngle )
 */
HB_FUNC( QT_QPAINTER_DRAWARC )
{
   hbqt_par_QPainter( 1 )->drawArc( *hbqt_par_QRectF( 2 ), hb_parni( 3 ), hb_parni( 4 ) );
}

/*
 * void drawArc ( const QRect & rectangle, int startAngle, int spanAngle )
 */
HB_FUNC( QT_QPAINTER_DRAWARC_1 )
{
   hbqt_par_QPainter( 1 )->drawArc( *hbqt_par_QRect( 2 ), hb_parni( 3 ), hb_parni( 4 ) );
}

/*
 * void drawArc ( int x, int y, int width, int height, int startAngle, int spanAngle )
 */
HB_FUNC( QT_QPAINTER_DRAWARC_2 )
{
   hbqt_par_QPainter( 1 )->drawArc( hb_parni( 2 ), hb_parni( 3 ), hb_parni( 4 ), hb_parni( 5 ), hb_parni( 6 ), hb_parni( 7 ) );
}

/*
 * void drawChord ( const QRectF & rectangle, int startAngle, int spanAngle )
 */
HB_FUNC( QT_QPAINTER_DRAWCHORD )
{
   hbqt_par_QPainter( 1 )->drawChord( *hbqt_par_QRectF( 2 ), hb_parni( 3 ), hb_parni( 4 ) );
}

/*
 * void drawChord ( const QRect & rectangle, int startAngle, int spanAngle )
 */
HB_FUNC( QT_QPAINTER_DRAWCHORD_1 )
{
   hbqt_par_QPainter( 1 )->drawChord( *hbqt_par_QRect( 2 ), hb_parni( 3 ), hb_parni( 4 ) );
}

/*
 * void drawChord ( int x, int y, int width, int height, int startAngle, int spanAngle )
 */
HB_FUNC( QT_QPAINTER_DRAWCHORD_2 )
{
   hbqt_par_QPainter( 1 )->drawChord( hb_parni( 2 ), hb_parni( 3 ), hb_parni( 4 ), hb_parni( 5 ), hb_parni( 6 ), hb_parni( 7 ) );
}

/*
 * void drawConvexPolygon ( const QPointF * points, int pointCount )
 */
HB_FUNC( QT_QPAINTER_DRAWCONVEXPOLYGON )
{
   hbqt_par_QPainter( 1 )->drawConvexPolygon( hbqt_par_QPointF( 2 ), hb_parni( 3 ) );
}

/*
 * void drawConvexPolygon ( const QPoint * points, int pointCount )
 */
HB_FUNC( QT_QPAINTER_DRAWCONVEXPOLYGON_1 )
{
   hbqt_par_QPainter( 1 )->drawConvexPolygon( hbqt_par_QPoint( 2 ), hb_parni( 3 ) );
}

/*
 * void drawConvexPolygon ( const QPolygonF & polygon )
 */
HB_FUNC( QT_QPAINTER_DRAWCONVEXPOLYGON_2 )
{
   hbqt_par_QPainter( 1 )->drawConvexPolygon( *hbqt_par_QPolygonF( 2 ) );
}

/*
 * void drawConvexPolygon ( const QPolygon & polygon )
 */
HB_FUNC( QT_QPAINTER_DRAWCONVEXPOLYGON_3 )
{
   hbqt_par_QPainter( 1 )->drawConvexPolygon( *hbqt_par_QPolygon( 2 ) );
}

/*
 * void drawEllipse ( const QRectF & rectangle )
 */
HB_FUNC( QT_QPAINTER_DRAWELLIPSE )
{
   hbqt_par_QPainter( 1 )->drawEllipse( *hbqt_par_QRectF( 2 ) );
}

/*
 * void drawEllipse ( const QRect & rectangle )
 */
HB_FUNC( QT_QPAINTER_DRAWELLIPSE_1 )
{
   hbqt_par_QPainter( 1 )->drawEllipse( *hbqt_par_QRect( 2 ) );
}

/*
 * void drawEllipse ( int x, int y, int width, int height )
 */
HB_FUNC( QT_QPAINTER_DRAWELLIPSE_2 )
{
   hbqt_par_QPainter( 1 )->drawEllipse( hb_parni( 2 ), hb_parni( 3 ), hb_parni( 4 ), hb_parni( 5 ) );
}

/*
 * void drawEllipse ( const QPointF & center, qreal rx, qreal ry )
 */
HB_FUNC( QT_QPAINTER_DRAWELLIPSE_3 )
{
   hbqt_par_QPainter( 1 )->drawEllipse( *hbqt_par_QPointF( 2 ), hb_parnd( 3 ), hb_parnd( 4 ) );
}

/*
 * void drawEllipse ( const QPoint & center, int rx, int ry )
 */
HB_FUNC( QT_QPAINTER_DRAWELLIPSE_4 )
{
   hbqt_par_QPainter( 1 )->drawEllipse( *hbqt_par_QPoint( 2 ), hb_parni( 3 ), hb_parni( 4 ) );
}

/*
 * void drawImage ( const QRectF & target, const QImage & image, const QRectF & source, Qt::ImageConversionFlags flags = Qt::AutoColor )
 */
HB_FUNC( QT_QPAINTER_DRAWIMAGE )
{
   hbqt_par_QPainter( 1 )->drawImage( *hbqt_par_QRectF( 2 ), *hbqt_par_QImage( 3 ), *hbqt_par_QRectF( 4 ), ( HB_ISNUM( 5 ) ? ( Qt::ImageConversionFlags ) hb_parni( 5 ) : ( Qt::ImageConversionFlags ) Qt::AutoColor ) );
}

/*
 * void drawImage ( const QRect & target, const QImage & image, const QRect & source, Qt::ImageConversionFlags flags = Qt::AutoColor )
 */
HB_FUNC( QT_QPAINTER_DRAWIMAGE_1 )
{
   hbqt_par_QPainter( 1 )->drawImage( *hbqt_par_QRect( 2 ), *hbqt_par_QImage( 3 ), *hbqt_par_QRect( 4 ), ( HB_ISNUM( 5 ) ? ( Qt::ImageConversionFlags ) hb_parni( 5 ) : ( Qt::ImageConversionFlags ) Qt::AutoColor ) );
}

/*
 * void drawImage ( const QPointF & point, const QImage & image )
 */
HB_FUNC( QT_QPAINTER_DRAWIMAGE_2 )
{
   hbqt_par_QPainter( 1 )->drawImage( *hbqt_par_QPointF( 2 ), *hbqt_par_QImage( 3 ) );
}

/*
 * void drawImage ( const QPoint & point, const QImage & image )
 */
HB_FUNC( QT_QPAINTER_DRAWIMAGE_3 )
{
   hbqt_par_QPainter( 1 )->drawImage( *hbqt_par_QPoint( 2 ), *hbqt_par_QImage( 3 ) );
}

/*
 * void drawImage ( const QPointF & point, const QImage & image, const QRectF & source, Qt::ImageConversionFlags flags = Qt::AutoColor )
 */
HB_FUNC( QT_QPAINTER_DRAWIMAGE_4 )
{
   hbqt_par_QPainter( 1 )->drawImage( *hbqt_par_QPointF( 2 ), *hbqt_par_QImage( 3 ), *hbqt_par_QRectF( 4 ), ( HB_ISNUM( 5 ) ? ( Qt::ImageConversionFlags ) hb_parni( 5 ) : ( Qt::ImageConversionFlags ) Qt::AutoColor ) );
}

/*
 * void drawImage ( const QPoint & point, const QImage & image, const QRect & source, Qt::ImageConversionFlags flags = Qt::AutoColor )
 */
HB_FUNC( QT_QPAINTER_DRAWIMAGE_5 )
{
   hbqt_par_QPainter( 1 )->drawImage( *hbqt_par_QPoint( 2 ), *hbqt_par_QImage( 3 ), *hbqt_par_QRect( 4 ), ( HB_ISNUM( 5 ) ? ( Qt::ImageConversionFlags ) hb_parni( 5 ) : ( Qt::ImageConversionFlags ) Qt::AutoColor ) );
}

/*
 * void drawImage ( const QRectF & rectangle, const QImage & image )
 */
HB_FUNC( QT_QPAINTER_DRAWIMAGE_6 )
{
   hbqt_par_QPainter( 1 )->drawImage( *hbqt_par_QRectF( 2 ), *hbqt_par_QImage( 3 ) );
}

/*
 * void drawImage ( const QRect & rectangle, const QImage & image )
 */
HB_FUNC( QT_QPAINTER_DRAWIMAGE_7 )
{
   hbqt_par_QPainter( 1 )->drawImage( *hbqt_par_QRect( 2 ), *hbqt_par_QImage( 3 ) );
}

/*
 * void drawImage ( int x, int y, const QImage & image, int sx = 0, int sy = 0, int sw = -1, int sh = -1, Qt::ImageConversionFlags flags = Qt::AutoColor )
 */
HB_FUNC( QT_QPAINTER_DRAWIMAGE_8 )
{
   hbqt_par_QPainter( 1 )->drawImage( hb_parni( 2 ), hb_parni( 3 ), *hbqt_par_QImage( 4 ), hb_parni( 5 ), hb_parni( 6 ), ( HB_ISNUM( 7 ) ? hb_parni( 7 ) : -1 ), ( HB_ISNUM( 8 ) ? hb_parni( 8 ) : -1 ), ( HB_ISNUM( 9 ) ? ( Qt::ImageConversionFlags ) hb_parni( 9 ) : ( Qt::ImageConversionFlags ) Qt::AutoColor ) );
}

/*
 * void drawLine ( const QLineF & line )
 */
HB_FUNC( QT_QPAINTER_DRAWLINE )
{
   hbqt_par_QPainter( 1 )->drawLine( *hbqt_par_QLineF( 2 ) );
}

/*
 * void drawLine ( const QLine & line )
 */
HB_FUNC( QT_QPAINTER_DRAWLINE_1 )
{
   hbqt_par_QPainter( 1 )->drawLine( *hbqt_par_QLine( 2 ) );
}

/*
 * void drawLine ( const QPoint & p1, const QPoint & p2 )
 */
HB_FUNC( QT_QPAINTER_DRAWLINE_2 )
{
   hbqt_par_QPainter( 1 )->drawLine( *hbqt_par_QPoint( 2 ), *hbqt_par_QPoint( 3 ) );
}

/*
 * void drawLine ( const QPointF & p1, const QPointF & p2 )
 */
HB_FUNC( QT_QPAINTER_DRAWLINE_3 )
{
   hbqt_par_QPainter( 1 )->drawLine( *hbqt_par_QPointF( 2 ), *hbqt_par_QPointF( 3 ) );
}

/*
 * void drawLine ( int x1, int y1, int x2, int y2 )
 */
HB_FUNC( QT_QPAINTER_DRAWLINE_4 )
{
   hbqt_par_QPainter( 1 )->drawLine( hb_parni( 2 ), hb_parni( 3 ), hb_parni( 4 ), hb_parni( 5 ) );
}

/*
 * void drawLines ( const QLineF * lines, int lineCount )
 */
HB_FUNC( QT_QPAINTER_DRAWLINES )
{
   hbqt_par_QPainter( 1 )->drawLines( hbqt_par_QLineF( 2 ), hb_parni( 3 ) );
}

/*
 * void drawLines ( const QLine * lines, int lineCount )
 */
HB_FUNC( QT_QPAINTER_DRAWLINES_1 )
{
   hbqt_par_QPainter( 1 )->drawLines( hbqt_par_QLine( 2 ), hb_parni( 3 ) );
}

/*
 * void drawLines ( const QPointF * pointPairs, int lineCount )
 */
HB_FUNC( QT_QPAINTER_DRAWLINES_2 )
{
   hbqt_par_QPainter( 1 )->drawLines( hbqt_par_QPointF( 2 ), hb_parni( 3 ) );
}

/*
 * void drawLines ( const QPoint * pointPairs, int lineCount )
 */
HB_FUNC( QT_QPAINTER_DRAWLINES_3 )
{
   hbqt_par_QPainter( 1 )->drawLines( hbqt_par_QPoint( 2 ), hb_parni( 3 ) );
}

/*
 * void drawPath ( const QPainterPath & path )
 */
HB_FUNC( QT_QPAINTER_DRAWPATH )
{
   hbqt_par_QPainter( 1 )->drawPath( *hbqt_par_QPainterPath( 2 ) );
}

/*
 * void drawPicture ( const QPointF & point, const QPicture & picture )
 */
HB_FUNC( QT_QPAINTER_DRAWPICTURE )
{
   hbqt_par_QPainter( 1 )->drawPicture( *hbqt_par_QPointF( 2 ), *hbqt_par_QPicture( 3 ) );
}

/*
 * void drawPicture ( const QPoint & point, const QPicture & picture )
 */
HB_FUNC( QT_QPAINTER_DRAWPICTURE_1 )
{
   hbqt_par_QPainter( 1 )->drawPicture( *hbqt_par_QPoint( 2 ), *hbqt_par_QPicture( 3 ) );
}

/*
 * void drawPicture ( int x, int y, const QPicture & picture )
 */
HB_FUNC( QT_QPAINTER_DRAWPICTURE_2 )
{
   hbqt_par_QPainter( 1 )->drawPicture( hb_parni( 2 ), hb_parni( 3 ), *hbqt_par_QPicture( 4 ) );
}

/*
 * void drawPie ( const QRectF & rectangle, int startAngle, int spanAngle )
 */
HB_FUNC( QT_QPAINTER_DRAWPIE )
{
   hbqt_par_QPainter( 1 )->drawPie( *hbqt_par_QRectF( 2 ), hb_parni( 3 ), hb_parni( 4 ) );
}

/*
 * void drawPie ( const QRect & rectangle, int startAngle, int spanAngle )
 */
HB_FUNC( QT_QPAINTER_DRAWPIE_1 )
{
   hbqt_par_QPainter( 1 )->drawPie( *hbqt_par_QRect( 2 ), hb_parni( 3 ), hb_parni( 4 ) );
}

/*
 * void drawPie ( int x, int y, int width, int height, int startAngle, int spanAngle )
 */
HB_FUNC( QT_QPAINTER_DRAWPIE_2 )
{
   hbqt_par_QPainter( 1 )->drawPie( hb_parni( 2 ), hb_parni( 3 ), hb_parni( 4 ), hb_parni( 5 ), hb_parni( 6 ), hb_parni( 7 ) );
}

/*
 * void drawPixmap ( const QRectF & target, const QPixmap & pixmap, const QRectF & source )
 */
HB_FUNC( QT_QPAINTER_DRAWPIXMAP )
{
   hbqt_par_QPainter( 1 )->drawPixmap( *hbqt_par_QRectF( 2 ), *hbqt_par_QPixmap( 3 ), *hbqt_par_QRectF( 4 ) );
}

/*
 * void drawPixmap ( const QRect & target, const QPixmap & pixmap, const QRect & source )
 */
HB_FUNC( QT_QPAINTER_DRAWPIXMAP_1 )
{
   hbqt_par_QPainter( 1 )->drawPixmap( *hbqt_par_QRect( 2 ), *hbqt_par_QPixmap( 3 ), *hbqt_par_QRect( 4 ) );
}

/*
 * void drawPixmap ( const QPointF & point, const QPixmap & pixmap, const QRectF & source )
 */
HB_FUNC( QT_QPAINTER_DRAWPIXMAP_2 )
{
   hbqt_par_QPainter( 1 )->drawPixmap( *hbqt_par_QPointF( 2 ), *hbqt_par_QPixmap( 3 ), *hbqt_par_QRectF( 4 ) );
}

/*
 * void drawPixmap ( const QPoint & point, const QPixmap & pixmap, const QRect & source )
 */
HB_FUNC( QT_QPAINTER_DRAWPIXMAP_3 )
{
   hbqt_par_QPainter( 1 )->drawPixmap( *hbqt_par_QPoint( 2 ), *hbqt_par_QPixmap( 3 ), *hbqt_par_QRect( 4 ) );
}

/*
 * void drawPixmap ( const QPointF & point, const QPixmap & pixmap )
 */
HB_FUNC( QT_QPAINTER_DRAWPIXMAP_4 )
{
   hbqt_par_QPainter( 1 )->drawPixmap( *hbqt_par_QPointF( 2 ), *hbqt_par_QPixmap( 3 ) );
}

/*
 * void drawPixmap ( const QPoint & point, const QPixmap & pixmap )
 */
HB_FUNC( QT_QPAINTER_DRAWPIXMAP_5 )
{
   hbqt_par_QPainter( 1 )->drawPixmap( *hbqt_par_QPoint( 2 ), *hbqt_par_QPixmap( 3 ) );
}

/*
 * void drawPixmap ( int x, int y, const QPixmap & pixmap )
 */
HB_FUNC( QT_QPAINTER_DRAWPIXMAP_6 )
{
   hbqt_par_QPainter( 1 )->drawPixmap( hb_parni( 2 ), hb_parni( 3 ), *hbqt_par_QPixmap( 4 ) );
}

/*
 * void drawPixmap ( const QRect & rectangle, const QPixmap & pixmap )
 */
HB_FUNC( QT_QPAINTER_DRAWPIXMAP_7 )
{
   hbqt_par_QPainter( 1 )->drawPixmap( *hbqt_par_QRect( 2 ), *hbqt_par_QPixmap( 3 ) );
}

/*
 * void drawPixmap ( int x, int y, int width, int height, const QPixmap & pixmap )
 */
HB_FUNC( QT_QPAINTER_DRAWPIXMAP_8 )
{
   hbqt_par_QPainter( 1 )->drawPixmap( hb_parni( 2 ), hb_parni( 3 ), hb_parni( 4 ), hb_parni( 5 ), *hbqt_par_QPixmap( 6 ) );
}

/*
 * void drawPixmap ( int x, int y, int w, int h, const QPixmap & pixmap, int sx, int sy, int sw, int sh )
 */
HB_FUNC( QT_QPAINTER_DRAWPIXMAP_9 )
{
   hbqt_par_QPainter( 1 )->drawPixmap( hb_parni( 2 ), hb_parni( 3 ), hb_parni( 4 ), hb_parni( 5 ), *hbqt_par_QPixmap( 6 ), hb_parni( 7 ), hb_parni( 8 ), hb_parni( 9 ), hb_parni( 10 ) );
}

/*
 * void drawPixmap ( int x, int y, const QPixmap & pixmap, int sx, int sy, int sw, int sh )
 */
HB_FUNC( QT_QPAINTER_DRAWPIXMAP_10 )
{
   hbqt_par_QPainter( 1 )->drawPixmap( hb_parni( 2 ), hb_parni( 3 ), *hbqt_par_QPixmap( 4 ), hb_parni( 5 ), hb_parni( 6 ), hb_parni( 7 ), hb_parni( 8 ) );
}

/*
 * void drawPoint ( const QPointF & position )
 */
HB_FUNC( QT_QPAINTER_DRAWPOINT )
{
   hbqt_par_QPainter( 1 )->drawPoint( *hbqt_par_QPointF( 2 ) );
}

/*
 * void drawPoint ( const QPoint & position )
 */
HB_FUNC( QT_QPAINTER_DRAWPOINT_1 )
{
   hbqt_par_QPainter( 1 )->drawPoint( *hbqt_par_QPoint( 2 ) );
}

/*
 * void drawPoint ( int x, int y )
 */
HB_FUNC( QT_QPAINTER_DRAWPOINT_2 )
{
   hbqt_par_QPainter( 1 )->drawPoint( hb_parni( 2 ), hb_parni( 3 ) );
}

/*
 * void drawPoints ( const QPointF * points, int pointCount )
 */
HB_FUNC( QT_QPAINTER_DRAWPOINTS )
{
   hbqt_par_QPainter( 1 )->drawPoints( hbqt_par_QPointF( 2 ), hb_parni( 3 ) );
}

/*
 * void drawPoints ( const QPoint * points, int pointCount )
 */
HB_FUNC( QT_QPAINTER_DRAWPOINTS_1 )
{
   hbqt_par_QPainter( 1 )->drawPoints( hbqt_par_QPoint( 2 ), hb_parni( 3 ) );
}

/*
 * void drawPoints ( const QPolygonF & points )
 */
HB_FUNC( QT_QPAINTER_DRAWPOINTS_2 )
{
   hbqt_par_QPainter( 1 )->drawPoints( *hbqt_par_QPolygonF( 2 ) );
}

/*
 * void drawPoints ( const QPolygon & points )
 */
HB_FUNC( QT_QPAINTER_DRAWPOINTS_3 )
{
   hbqt_par_QPainter( 1 )->drawPoints( *hbqt_par_QPolygon( 2 ) );
}

/*
 * void drawPolygon ( const QPointF * points, int pointCount, Qt::FillRule fillRule = Qt::OddEvenFill )
 */
HB_FUNC( QT_QPAINTER_DRAWPOLYGON )
{
   hbqt_par_QPainter( 1 )->drawPolygon( hbqt_par_QPointF( 2 ), hb_parni( 3 ), ( HB_ISNUM( 4 ) ? ( Qt::FillRule ) hb_parni( 4 ) : ( Qt::FillRule ) Qt::OddEvenFill ) );
}

/*
 * void drawPolygon ( const QPoint * points, int pointCount, Qt::FillRule fillRule = Qt::OddEvenFill )
 */
HB_FUNC( QT_QPAINTER_DRAWPOLYGON_1 )
{
   hbqt_par_QPainter( 1 )->drawPolygon( hbqt_par_QPoint( 2 ), hb_parni( 3 ), ( HB_ISNUM( 4 ) ? ( Qt::FillRule ) hb_parni( 4 ) : ( Qt::FillRule ) Qt::OddEvenFill ) );
}

/*
 * void drawPolygon ( const QPolygonF & points, Qt::FillRule fillRule = Qt::OddEvenFill )
 */
HB_FUNC( QT_QPAINTER_DRAWPOLYGON_2 )
{
   hbqt_par_QPainter( 1 )->drawPolygon( *hbqt_par_QPolygonF( 2 ), ( HB_ISNUM( 3 ) ? ( Qt::FillRule ) hb_parni( 3 ) : ( Qt::FillRule ) Qt::OddEvenFill ) );
}

/*
 * void drawPolygon ( const QPolygon & points, Qt::FillRule fillRule = Qt::OddEvenFill )
 */
HB_FUNC( QT_QPAINTER_DRAWPOLYGON_3 )
{
   hbqt_par_QPainter( 1 )->drawPolygon( *hbqt_par_QPolygon( 2 ), ( HB_ISNUM( 3 ) ? ( Qt::FillRule ) hb_parni( 3 ) : ( Qt::FillRule ) Qt::OddEvenFill ) );
}

/*
 * void drawPolyline ( const QPointF * points, int pointCount )
 */
HB_FUNC( QT_QPAINTER_DRAWPOLYLINE )
{
   hbqt_par_QPainter( 1 )->drawPolyline( hbqt_par_QPointF( 2 ), hb_parni( 3 ) );
}

/*
 * void drawPolyline ( const QPoint * points, int pointCount )
 */
HB_FUNC( QT_QPAINTER_DRAWPOLYLINE_1 )
{
   hbqt_par_QPainter( 1 )->drawPolyline( hbqt_par_QPoint( 2 ), hb_parni( 3 ) );
}

/*
 * void drawPolyline ( const QPolygonF & points )
 */
HB_FUNC( QT_QPAINTER_DRAWPOLYLINE_2 )
{
   hbqt_par_QPainter( 1 )->drawPolyline( *hbqt_par_QPolygonF( 2 ) );
}

/*
 * void drawPolyline ( const QPolygon & points )
 */
HB_FUNC( QT_QPAINTER_DRAWPOLYLINE_3 )
{
   hbqt_par_QPainter( 1 )->drawPolyline( *hbqt_par_QPolygon( 2 ) );
}

/*
 * void drawRect ( const QRectF & rectangle )
 */
HB_FUNC( QT_QPAINTER_DRAWRECT )
{
   hbqt_par_QPainter( 1 )->drawRect( *hbqt_par_QRectF( 2 ) );
}

/*
 * void drawRect ( const QRect & rectangle )
 */
HB_FUNC( QT_QPAINTER_DRAWRECT_1 )
{
   hbqt_par_QPainter( 1 )->drawRect( *hbqt_par_QRect( 2 ) );
}

/*
 * void drawRect ( int x, int y, int width, int height )
 */
HB_FUNC( QT_QPAINTER_DRAWRECT_2 )
{
   hbqt_par_QPainter( 1 )->drawRect( hb_parni( 2 ), hb_parni( 3 ), hb_parni( 4 ), hb_parni( 5 ) );
}

/*
 * void drawRects ( const QRectF * rectangles, int rectCount )
 */
HB_FUNC( QT_QPAINTER_DRAWRECTS )
{
   hbqt_par_QPainter( 1 )->drawRects( hbqt_par_QRectF( 2 ), hb_parni( 3 ) );
}

/*
 * void drawRects ( const QRect * rectangles, int rectCount )
 */
HB_FUNC( QT_QPAINTER_DRAWRECTS_1 )
{
   hbqt_par_QPainter( 1 )->drawRects( hbqt_par_QRect( 2 ), hb_parni( 3 ) );
}

/*
 * void drawRoundedRect ( const QRectF & rect, qreal xRadius, qreal yRadius, Qt::SizeMode mode = Qt::AbsoluteSize )
 */
HB_FUNC( QT_QPAINTER_DRAWROUNDEDRECT )
{
   hbqt_par_QPainter( 1 )->drawRoundedRect( *hbqt_par_QRectF( 2 ), hb_parnd( 3 ), hb_parnd( 4 ), ( HB_ISNUM( 5 ) ? ( Qt::SizeMode ) hb_parni( 5 ) : ( Qt::SizeMode ) Qt::AbsoluteSize ) );
}

/*
 * void drawRoundedRect ( const QRect & rect, qreal xRadius, qreal yRadius, Qt::SizeMode mode = Qt::AbsoluteSize )
 */
HB_FUNC( QT_QPAINTER_DRAWROUNDEDRECT_1 )
{
   hbqt_par_QPainter( 1 )->drawRoundedRect( *hbqt_par_QRect( 2 ), hb_parnd( 3 ), hb_parnd( 4 ), ( HB_ISNUM( 5 ) ? ( Qt::SizeMode ) hb_parni( 5 ) : ( Qt::SizeMode ) Qt::AbsoluteSize ) );
}

/*
 * void drawRoundedRect ( int x, int y, int w, int h, qreal xRadius, qreal yRadius, Qt::SizeMode mode = Qt::AbsoluteSize )
 */
HB_FUNC( QT_QPAINTER_DRAWROUNDEDRECT_2 )
{
   hbqt_par_QPainter( 1 )->drawRoundedRect( hb_parni( 2 ), hb_parni( 3 ), hb_parni( 4 ), hb_parni( 5 ), hb_parnd( 6 ), hb_parnd( 7 ), ( HB_ISNUM( 8 ) ? ( Qt::SizeMode ) hb_parni( 8 ) : ( Qt::SizeMode ) Qt::AbsoluteSize ) );
}

/*
 * void drawText ( const QPointF & position, const QString & text )
 */
HB_FUNC( QT_QPAINTER_DRAWTEXT )
{
   hbqt_par_QPainter( 1 )->drawText( *hbqt_par_QPointF( 2 ), hbqt_par_QString( 3 ) );
}

/*
 * void drawText ( const QPoint & position, const QString & text )
 */
HB_FUNC( QT_QPAINTER_DRAWTEXT_1 )
{
   hbqt_par_QPainter( 1 )->drawText( *hbqt_par_QPoint( 2 ), hbqt_par_QString( 3 ) );
}

/*
 * void drawText ( const QRectF & rectangle, int flags, const QString & text, QRectF * boundingRect = 0 )
 */
HB_FUNC( QT_QPAINTER_DRAWTEXT_2 )
{
   hbqt_par_QPainter( 1 )->drawText( *hbqt_par_QRectF( 2 ), hb_parni( 3 ), hbqt_par_QString( 4 ), hbqt_par_QRectF( 5 ) );
}

/*
 * void drawText ( const QRect & rectangle, int flags, const QString & text, QRect * boundingRect = 0 )
 */
HB_FUNC( QT_QPAINTER_DRAWTEXT_3 )
{
   hbqt_par_QPainter( 1 )->drawText( *hbqt_par_QRect( 2 ), hb_parni( 3 ), hbqt_par_QString( 4 ), hbqt_par_QRect( 5 ) );
}

/*
 * void drawText ( int x, int y, const QString & text )
 */
HB_FUNC( QT_QPAINTER_DRAWTEXT_4 )
{
   hbqt_par_QPainter( 1 )->drawText( hb_parni( 2 ), hb_parni( 3 ), hbqt_par_QString( 4 ) );
}

/*
 * void drawText ( int x, int y, int width, int height, int flags, const QString & text, QRect * boundingRect = 0 )
 */
HB_FUNC( QT_QPAINTER_DRAWTEXT_5 )
{
   hbqt_par_QPainter( 1 )->drawText( hb_parni( 2 ), hb_parni( 3 ), hb_parni( 4 ), hb_parni( 5 ), hb_parni( 6 ), hbqt_par_QString( 7 ), hbqt_par_QRect( 8 ) );
}

/*
 * void drawText ( const QRectF & rectangle, const QString & text, const QTextOption & option = QTextOption() )
 */
HB_FUNC( QT_QPAINTER_DRAWTEXT_6 )
{
   hbqt_par_QPainter( 1 )->drawText( *hbqt_par_QRectF( 2 ), hbqt_par_QString( 3 ), ( HB_ISPOINTER( 4 ) ? *hbqt_par_QTextOption( 4 ) : QTextOption() ) );
}

/*
 * void drawTiledPixmap ( const QRectF & rectangle, const QPixmap & pixmap, const QPointF & position = QPointF() )
 */
HB_FUNC( QT_QPAINTER_DRAWTILEDPIXMAP )
{
   hbqt_par_QPainter( 1 )->drawTiledPixmap( *hbqt_par_QRectF( 2 ), *hbqt_par_QPixmap( 3 ), ( HB_ISPOINTER( 4 ) ? *hbqt_par_QPointF( 4 ) : QPointF() ) );
}

/*
 * void drawTiledPixmap ( const QRect & rectangle, const QPixmap & pixmap, const QPoint & position = QPoint() )
 */
HB_FUNC( QT_QPAINTER_DRAWTILEDPIXMAP_1 )
{
   hbqt_par_QPainter( 1 )->drawTiledPixmap( *hbqt_par_QRect( 2 ), *hbqt_par_QPixmap( 3 ), ( HB_ISPOINTER( 4 ) ? *hbqt_par_QPoint( 4 ) : QPoint() ) );
}

/*
 * void drawTiledPixmap ( int x, int y, int width, int height, const QPixmap & pixmap, int sx = 0, int sy = 0 )
 */
HB_FUNC( QT_QPAINTER_DRAWTILEDPIXMAP_2 )
{
   hbqt_par_QPainter( 1 )->drawTiledPixmap( hb_parni( 2 ), hb_parni( 3 ), hb_parni( 4 ), hb_parni( 5 ), *hbqt_par_QPixmap( 6 ), hb_parni( 7 ), hb_parni( 8 ) );
}

/*
 * bool end ()
 */
HB_FUNC( QT_QPAINTER_END )
{
   hb_retl( hbqt_par_QPainter( 1 )->end() );
}

/*
 * void eraseRect ( const QRectF & rectangle )
 */
HB_FUNC( QT_QPAINTER_ERASERECT )
{
   hbqt_par_QPainter( 1 )->eraseRect( *hbqt_par_QRectF( 2 ) );
}

/*
 * void eraseRect ( const QRect & rectangle )
 */
HB_FUNC( QT_QPAINTER_ERASERECT_1 )
{
   hbqt_par_QPainter( 1 )->eraseRect( *hbqt_par_QRect( 2 ) );
}

/*
 * void eraseRect ( int x, int y, int width, int height )
 */
HB_FUNC( QT_QPAINTER_ERASERECT_2 )
{
   hbqt_par_QPainter( 1 )->eraseRect( hb_parni( 2 ), hb_parni( 3 ), hb_parni( 4 ), hb_parni( 5 ) );
}

/*
 * void fillPath ( const QPainterPath & path, const QBrush & brush )
 */
HB_FUNC( QT_QPAINTER_FILLPATH )
{
   hbqt_par_QPainter( 1 )->fillPath( *hbqt_par_QPainterPath( 2 ), *hbqt_par_QBrush( 3 ) );
}

/*
 * void fillRect ( const QRectF & rectangle, const QBrush & brush )
 */
HB_FUNC( QT_QPAINTER_FILLRECT )
{
   hbqt_par_QPainter( 1 )->fillRect( *hbqt_par_QRectF( 2 ), *hbqt_par_QBrush( 3 ) );
}

/*
 * void fillRect ( int x, int y, int width, int height, Qt::BrushStyle style )
 */
HB_FUNC( QT_QPAINTER_FILLRECT_1 )
{
   hbqt_par_QPainter( 1 )->fillRect( hb_parni( 2 ), hb_parni( 3 ), hb_parni( 4 ), hb_parni( 5 ), ( Qt::BrushStyle ) hb_parni( 6 ) );
}

/*
 * void fillRect ( const QRect & rectangle, Qt::BrushStyle style )
 */
HB_FUNC( QT_QPAINTER_FILLRECT_2 )
{
   hbqt_par_QPainter( 1 )->fillRect( *hbqt_par_QRect( 2 ), ( Qt::BrushStyle ) hb_parni( 3 ) );
}

/*
 * void fillRect ( const QRectF & rectangle, Qt::BrushStyle style )
 */
HB_FUNC( QT_QPAINTER_FILLRECT_3 )
{
   hbqt_par_QPainter( 1 )->fillRect( *hbqt_par_QRectF( 2 ), ( Qt::BrushStyle ) hb_parni( 3 ) );
}

/*
 * void fillRect ( const QRect & rectangle, const QBrush & brush )
 */
HB_FUNC( QT_QPAINTER_FILLRECT_4 )
{
   hbqt_par_QPainter( 1 )->fillRect( *hbqt_par_QRect( 2 ), *hbqt_par_QBrush( 3 ) );
}

/*
 * void fillRect ( const QRect & rectangle, const QColor & color )
 */
HB_FUNC( QT_QPAINTER_FILLRECT_5 )
{
   hbqt_par_QPainter( 1 )->fillRect( *hbqt_par_QRect( 2 ), *hbqt_par_QColor( 3 ) );
}

/*
 * void fillRect ( const QRectF & rectangle, const QColor & color )
 */
HB_FUNC( QT_QPAINTER_FILLRECT_6 )
{
   hbqt_par_QPainter( 1 )->fillRect( *hbqt_par_QRectF( 2 ), *hbqt_par_QColor( 3 ) );
}

/*
 * void fillRect ( int x, int y, int width, int height, const QBrush & brush )
 */
HB_FUNC( QT_QPAINTER_FILLRECT_7 )
{
   hbqt_par_QPainter( 1 )->fillRect( hb_parni( 2 ), hb_parni( 3 ), hb_parni( 4 ), hb_parni( 5 ), *hbqt_par_QBrush( 6 ) );
}

/*
 * void fillRect ( int x, int y, int width, int height, const QColor & color )
 */
HB_FUNC( QT_QPAINTER_FILLRECT_8 )
{
   hbqt_par_QPainter( 1 )->fillRect( hb_parni( 2 ), hb_parni( 3 ), hb_parni( 4 ), hb_parni( 5 ), *hbqt_par_QColor( 6 ) );
}

/*
 * void fillRect ( int x, int y, int width, int height, Qt::GlobalColor color )
 */
HB_FUNC( QT_QPAINTER_FILLRECT_9 )
{
   hbqt_par_QPainter( 1 )->fillRect( hb_parni( 2 ), hb_parni( 3 ), hb_parni( 4 ), hb_parni( 5 ), ( Qt::GlobalColor ) hb_parni( 6 ) );
}

/*
 * void fillRect ( const QRect & rectangle, Qt::GlobalColor color )
 */
HB_FUNC( QT_QPAINTER_FILLRECT_10 )
{
   hbqt_par_QPainter( 1 )->fillRect( *hbqt_par_QRect( 2 ), ( Qt::GlobalColor ) hb_parni( 3 ) );
}

/*
 * void fillRect ( const QRectF & rectangle, Qt::GlobalColor color )
 */
HB_FUNC( QT_QPAINTER_FILLRECT_11 )
{
   hbqt_par_QPainter( 1 )->fillRect( *hbqt_par_QRectF( 2 ), ( Qt::GlobalColor ) hb_parni( 3 ) );
}

/*
 * const QFont & font () const
 */
HB_FUNC( QT_QPAINTER_FONT )
{
   hb_retptrGC( gcAllocate_QFont( new QFont( hbqt_par_QPainter( 1 )->font() ) ) );
}

/*
 * QFontInfo fontInfo () const
 */
HB_FUNC( QT_QPAINTER_FONTINFO )
{
   hb_retptrGC( gcAllocate_QFontInfo( new QFontInfo( hbqt_par_QPainter( 1 )->fontInfo() ) ) );
}

/*
 * QFontMetrics fontMetrics () const
 */
HB_FUNC( QT_QPAINTER_FONTMETRICS )
{
   hb_retptrGC( gcAllocate_QFontMetrics( new QFontMetrics( hbqt_par_QPainter( 1 )->fontMetrics() ) ) );
}

/*
 * bool hasClipping () const
 */
HB_FUNC( QT_QPAINTER_HASCLIPPING )
{
   hb_retl( hbqt_par_QPainter( 1 )->hasClipping() );
}

/*
 * void initFrom ( const QWidget * widget )
 */
HB_FUNC( QT_QPAINTER_INITFROM )
{
   hbqt_par_QPainter( 1 )->initFrom( hbqt_par_QWidget( 2 ) );
}

/*
 * bool isActive () const
 */
HB_FUNC( QT_QPAINTER_ISACTIVE )
{
   hb_retl( hbqt_par_QPainter( 1 )->isActive() );
}

/*
 * Qt::LayoutDirection layoutDirection () const
 */
HB_FUNC( QT_QPAINTER_LAYOUTDIRECTION )
{
   hb_retni( ( Qt::LayoutDirection ) hbqt_par_QPainter( 1 )->layoutDirection() );
}

/*
 * qreal opacity () const
 */
HB_FUNC( QT_QPAINTER_OPACITY )
{
   hb_retnd( hbqt_par_QPainter( 1 )->opacity() );
}

/*
 * QPaintEngine * paintEngine () const
 */
HB_FUNC( QT_QPAINTER_PAINTENGINE )
{
   hb_retptr( ( QPaintEngine* ) hbqt_par_QPainter( 1 )->paintEngine() );
}

/*
 * const QPen & pen () const
 */
HB_FUNC( QT_QPAINTER_PEN )
{
   hb_retptrGC( gcAllocate_QPen( new QPen( hbqt_par_QPainter( 1 )->pen() ) ) );
}

/*
 * RenderHints renderHints () const
 */
HB_FUNC( QT_QPAINTER_RENDERHINTS )
{
   hb_retni( ( QPainter::RenderHints ) hbqt_par_QPainter( 1 )->renderHints() );
}

/*
 * void resetMatrix ()
 */
HB_FUNC( QT_QPAINTER_RESETMATRIX )
{
   hbqt_par_QPainter( 1 )->resetMatrix();
}

/*
 * void resetTransform ()
 */
HB_FUNC( QT_QPAINTER_RESETTRANSFORM )
{
   hbqt_par_QPainter( 1 )->resetTransform();
}

/*
 * void restore ()
 */
HB_FUNC( QT_QPAINTER_RESTORE )
{
   hbqt_par_QPainter( 1 )->restore();
}

/*
 * void rotate ( qreal angle )
 */
HB_FUNC( QT_QPAINTER_ROTATE )
{
   hbqt_par_QPainter( 1 )->rotate( hb_parnd( 2 ) );
}

/*
 * void save ()
 */
HB_FUNC( QT_QPAINTER_SAVE )
{
   hbqt_par_QPainter( 1 )->save();
}

/*
 * void scale ( qreal sx, qreal sy )
 */
HB_FUNC( QT_QPAINTER_SCALE )
{
   hbqt_par_QPainter( 1 )->scale( hb_parnd( 2 ), hb_parnd( 3 ) );
}

/*
 * void setBackground ( const QBrush & brush )
 */
HB_FUNC( QT_QPAINTER_SETBACKGROUND )
{
   hbqt_par_QPainter( 1 )->setBackground( *hbqt_par_QBrush( 2 ) );
}

/*
 * void setBackgroundMode ( Qt::BGMode mode )
 */
HB_FUNC( QT_QPAINTER_SETBACKGROUNDMODE )
{
   hbqt_par_QPainter( 1 )->setBackgroundMode( ( Qt::BGMode ) hb_parni( 2 ) );
}

/*
 * void setBrush ( const QBrush & brush )
 */
HB_FUNC( QT_QPAINTER_SETBRUSH )
{
   hbqt_par_QPainter( 1 )->setBrush( *hbqt_par_QBrush( 2 ) );
}

/*
 * void setBrush ( Qt::BrushStyle style )
 */
HB_FUNC( QT_QPAINTER_SETBRUSH_1 )
{
   hbqt_par_QPainter( 1 )->setBrush( ( Qt::BrushStyle ) hb_parni( 2 ) );
}

/*
 * void setBrushOrigin ( const QPointF & position )
 */
HB_FUNC( QT_QPAINTER_SETBRUSHORIGIN )
{
   hbqt_par_QPainter( 1 )->setBrushOrigin( *hbqt_par_QPointF( 2 ) );
}

/*
 * void setBrushOrigin ( const QPoint & position )
 */
HB_FUNC( QT_QPAINTER_SETBRUSHORIGIN_1 )
{
   hbqt_par_QPainter( 1 )->setBrushOrigin( *hbqt_par_QPoint( 2 ) );
}

/*
 * void setBrushOrigin ( int x, int y )
 */
HB_FUNC( QT_QPAINTER_SETBRUSHORIGIN_2 )
{
   hbqt_par_QPainter( 1 )->setBrushOrigin( hb_parni( 2 ), hb_parni( 3 ) );
}

/*
 * void setClipPath ( const QPainterPath & path, Qt::ClipOperation operation = Qt::ReplaceClip )
 */
HB_FUNC( QT_QPAINTER_SETCLIPPATH )
{
   hbqt_par_QPainter( 1 )->setClipPath( *hbqt_par_QPainterPath( 2 ), ( HB_ISNUM( 3 ) ? ( Qt::ClipOperation ) hb_parni( 3 ) : ( Qt::ClipOperation ) Qt::ReplaceClip ) );
}

/*
 * void setClipRect ( const QRectF & rectangle, Qt::ClipOperation operation = Qt::ReplaceClip )
 */
HB_FUNC( QT_QPAINTER_SETCLIPRECT )
{
   hbqt_par_QPainter( 1 )->setClipRect( *hbqt_par_QRectF( 2 ), ( HB_ISNUM( 3 ) ? ( Qt::ClipOperation ) hb_parni( 3 ) : ( Qt::ClipOperation ) Qt::ReplaceClip ) );
}

/*
 * void setClipRect ( int x, int y, int width, int height, Qt::ClipOperation operation = Qt::ReplaceClip )
 */
HB_FUNC( QT_QPAINTER_SETCLIPRECT_1 )
{
   hbqt_par_QPainter( 1 )->setClipRect( hb_parni( 2 ), hb_parni( 3 ), hb_parni( 4 ), hb_parni( 5 ), ( HB_ISNUM( 6 ) ? ( Qt::ClipOperation ) hb_parni( 6 ) : ( Qt::ClipOperation ) Qt::ReplaceClip ) );
}

/*
 * void setClipRect ( const QRect & rectangle, Qt::ClipOperation operation = Qt::ReplaceClip )
 */
HB_FUNC( QT_QPAINTER_SETCLIPRECT_2 )
{
   hbqt_par_QPainter( 1 )->setClipRect( *hbqt_par_QRect( 2 ), ( HB_ISNUM( 3 ) ? ( Qt::ClipOperation ) hb_parni( 3 ) : ( Qt::ClipOperation ) Qt::ReplaceClip ) );
}

/*
 * void setClipRegion ( const QRegion & region, Qt::ClipOperation operation = Qt::ReplaceClip )
 */
HB_FUNC( QT_QPAINTER_SETCLIPREGION )
{
   hbqt_par_QPainter( 1 )->setClipRegion( *hbqt_par_QRegion( 2 ), ( HB_ISNUM( 3 ) ? ( Qt::ClipOperation ) hb_parni( 3 ) : ( Qt::ClipOperation ) Qt::ReplaceClip ) );
}

/*
 * void setClipping ( bool enable )
 */
HB_FUNC( QT_QPAINTER_SETCLIPPING )
{
   hbqt_par_QPainter( 1 )->setClipping( hb_parl( 2 ) );
}

/*
 * void setCompositionMode ( CompositionMode mode )
 */
HB_FUNC( QT_QPAINTER_SETCOMPOSITIONMODE )
{
   hbqt_par_QPainter( 1 )->setCompositionMode( ( QPainter::CompositionMode ) hb_parni( 2 ) );
}

/*
 * void setFont ( const QFont & font )
 */
HB_FUNC( QT_QPAINTER_SETFONT )
{
   hbqt_par_QPainter( 1 )->setFont( *hbqt_par_QFont( 2 ) );
}

/*
 * void setLayoutDirection ( Qt::LayoutDirection direction )
 */
HB_FUNC( QT_QPAINTER_SETLAYOUTDIRECTION )
{
   hbqt_par_QPainter( 1 )->setLayoutDirection( ( Qt::LayoutDirection ) hb_parni( 2 ) );
}

/*
 * void setOpacity ( qreal opacity )
 */
HB_FUNC( QT_QPAINTER_SETOPACITY )
{
   hbqt_par_QPainter( 1 )->setOpacity( hb_parnd( 2 ) );
}

/*
 * void setPen ( const QPen & pen )
 */
HB_FUNC( QT_QPAINTER_SETPEN )
{
   hbqt_par_QPainter( 1 )->setPen( *hbqt_par_QPen( 2 ) );
}

/*
 * void setPen ( const QColor & color )
 */
HB_FUNC( QT_QPAINTER_SETPEN_1 )
{
   hbqt_par_QPainter( 1 )->setPen( *hbqt_par_QColor( 2 ) );
}

/*
 * void setPen ( Qt::PenStyle style )
 */
HB_FUNC( QT_QPAINTER_SETPEN_2 )
{
   hbqt_par_QPainter( 1 )->setPen( ( Qt::PenStyle ) hb_parni( 2 ) );
}

/*
 * void setRenderHint ( RenderHint hint, bool on = true )
 */
HB_FUNC( QT_QPAINTER_SETRENDERHINT )
{
   hbqt_par_QPainter( 1 )->setRenderHint( ( QPainter::RenderHint ) hb_parni( 2 ), hb_parl( 3 ) );
}

/*
 * void setRenderHints ( RenderHints hints, bool on = true )
 */
HB_FUNC( QT_QPAINTER_SETRENDERHINTS )
{
   hbqt_par_QPainter( 1 )->setRenderHints( ( QPainter::RenderHints ) hb_parni( 2 ), hb_parl( 3 ) );
}

/*
 * void setTransform ( const QTransform & transform, bool combine = false )
 */
HB_FUNC( QT_QPAINTER_SETTRANSFORM )
{
   hbqt_par_QPainter( 1 )->setTransform( *hbqt_par_QTransform( 2 ), hb_parl( 3 ) );
}

/*
 * void setViewTransformEnabled ( bool enable )
 */
HB_FUNC( QT_QPAINTER_SETVIEWTRANSFORMENABLED )
{
   hbqt_par_QPainter( 1 )->setViewTransformEnabled( hb_parl( 2 ) );
}

/*
 * void setViewport ( const QRect & rectangle )
 */
HB_FUNC( QT_QPAINTER_SETVIEWPORT )
{
   hbqt_par_QPainter( 1 )->setViewport( *hbqt_par_QRect( 2 ) );
}

/*
 * void setViewport ( int x, int y, int width, int height )
 */
HB_FUNC( QT_QPAINTER_SETVIEWPORT_1 )
{
   hbqt_par_QPainter( 1 )->setViewport( hb_parni( 2 ), hb_parni( 3 ), hb_parni( 4 ), hb_parni( 5 ) );
}

/*
 * void setWindow ( const QRect & rectangle )
 */
HB_FUNC( QT_QPAINTER_SETWINDOW )
{
   hbqt_par_QPainter( 1 )->setWindow( *hbqt_par_QRect( 2 ) );
}

/*
 * void setWindow ( int x, int y, int width, int height )
 */
HB_FUNC( QT_QPAINTER_SETWINDOW_1 )
{
   hbqt_par_QPainter( 1 )->setWindow( hb_parni( 2 ), hb_parni( 3 ), hb_parni( 4 ), hb_parni( 5 ) );
}

/*
 * void setWorldMatrix ( const QMatrix & matrix, bool combine = false )
 */
HB_FUNC( QT_QPAINTER_SETWORLDMATRIX )
{
   hbqt_par_QPainter( 1 )->setWorldMatrix( *hbqt_par_QMatrix( 2 ), hb_parl( 3 ) );
}

/*
 * void setWorldMatrixEnabled ( bool enable )
 */
HB_FUNC( QT_QPAINTER_SETWORLDMATRIXENABLED )
{
   hbqt_par_QPainter( 1 )->setWorldMatrixEnabled( hb_parl( 2 ) );
}

/*
 * void setWorldTransform ( const QTransform & matrix, bool combine = false )
 */
HB_FUNC( QT_QPAINTER_SETWORLDTRANSFORM )
{
   hbqt_par_QPainter( 1 )->setWorldTransform( *hbqt_par_QTransform( 2 ), hb_parl( 3 ) );
}

/*
 * void shear ( qreal sh, qreal sv )
 */
HB_FUNC( QT_QPAINTER_SHEAR )
{
   hbqt_par_QPainter( 1 )->shear( hb_parnd( 2 ), hb_parnd( 3 ) );
}

/*
 * void strokePath ( const QPainterPath & path, const QPen & pen )
 */
HB_FUNC( QT_QPAINTER_STROKEPATH )
{
   hbqt_par_QPainter( 1 )->strokePath( *hbqt_par_QPainterPath( 2 ), *hbqt_par_QPen( 3 ) );
}

/*
 * bool testRenderHint ( RenderHint hint ) const
 */
HB_FUNC( QT_QPAINTER_TESTRENDERHINT )
{
   hb_retl( hbqt_par_QPainter( 1 )->testRenderHint( ( QPainter::RenderHint ) hb_parni( 2 ) ) );
}

/*
 * const QTransform & transform () const
 */
HB_FUNC( QT_QPAINTER_TRANSFORM )
{
   hb_retptrGC( gcAllocate_QTransform( new QTransform( hbqt_par_QPainter( 1 )->transform() ) ) );
}

/*
 * void translate ( const QPointF & offset )
 */
HB_FUNC( QT_QPAINTER_TRANSLATE )
{
   hbqt_par_QPainter( 1 )->translate( *hbqt_par_QPointF( 2 ) );
}

/*
 * void translate ( const QPoint & offset )
 */
HB_FUNC( QT_QPAINTER_TRANSLATE_1 )
{
   hbqt_par_QPainter( 1 )->translate( *hbqt_par_QPoint( 2 ) );
}

/*
 * void translate ( qreal dx, qreal dy )
 */
HB_FUNC( QT_QPAINTER_TRANSLATE_2 )
{
   hbqt_par_QPainter( 1 )->translate( hb_parnd( 2 ), hb_parnd( 3 ) );
}

/*
 * bool viewTransformEnabled () const
 */
HB_FUNC( QT_QPAINTER_VIEWTRANSFORMENABLED )
{
   hb_retl( hbqt_par_QPainter( 1 )->viewTransformEnabled() );
}

/*
 * QRect viewport () const
 */
HB_FUNC( QT_QPAINTER_VIEWPORT )
{
   hb_retptrGC( gcAllocate_QRect( new QRect( hbqt_par_QPainter( 1 )->viewport() ) ) );
}

/*
 * QRect window () const
 */
HB_FUNC( QT_QPAINTER_WINDOW )
{
   hb_retptrGC( gcAllocate_QRect( new QRect( hbqt_par_QPainter( 1 )->window() ) ) );
}

/*
 * const QMatrix & worldMatrix () const
 */
HB_FUNC( QT_QPAINTER_WORLDMATRIX )
{
   hb_retptrGC( gcAllocate_QMatrix( new QMatrix( hbqt_par_QPainter( 1 )->worldMatrix() ) ) );
}

/*
 * bool worldMatrixEnabled () const
 */
HB_FUNC( QT_QPAINTER_WORLDMATRIXENABLED )
{
   hb_retl( hbqt_par_QPainter( 1 )->worldMatrixEnabled() );
}

/*
 * const QTransform & worldTransform () const
 */
HB_FUNC( QT_QPAINTER_WORLDTRANSFORM )
{
   hb_retptrGC( gcAllocate_QTransform( new QTransform( hbqt_par_QPainter( 1 )->worldTransform() ) ) );
}

/*
 * QPaintDevice * redirected ( const QPaintDevice * device, QPoint * offset = 0 )
 */
HB_FUNC( QT_QPAINTER_REDIRECTED )
{
   hb_retptr( ( QPaintDevice* ) hbqt_par_QPainter( 1 )->redirected( hbqt_par_QPaintDevice( 2 ), hbqt_par_QPoint( 3 ) ) );
}

/*
 * void restoreRedirected ( const QPaintDevice * device )
 */
HB_FUNC( QT_QPAINTER_RESTOREREDIRECTED )
{
   hbqt_par_QPainter( 1 )->restoreRedirected( hbqt_par_QPaintDevice( 2 ) );
}

/*
 * void setRedirected ( const QPaintDevice * device, QPaintDevice * replacement, const QPoint & offset = QPoint() )
 */
HB_FUNC( QT_QPAINTER_SETREDIRECTED )
{
   hbqt_par_QPainter( 1 )->setRedirected( hbqt_par_QPaintDevice( 2 ), hbqt_par_QPaintDevice( 3 ), ( HB_ISPOINTER( 4 ) ? *hbqt_par_QPoint( 4 ) : QPoint() ) );
}


/*----------------------------------------------------------------------*/
#endif             /* #if QT_VERSION >= 0x040500 */
/*----------------------------------------------------------------------*/
