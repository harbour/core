/*
 * $Id$
 */
   
/* 
 * Harbour Project source code:
 * QT wrapper main header
 * 
 * Copyright 2009 Marcos Antonio Gambeta <marcosgambeta at gmail dot com>
 * Copyright 2009 Pritpal Bedi <pritpal@vouchcac.com>
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
#include "hbqt.h"

/*----------------------------------------------------------------------*/
#if QT_VERSION >= 0x040500
/*----------------------------------------------------------------------*/


/*
 *  Constructed[ 79/180 [ 43.89% ] ]
 *  
 *  *** Unconvered Prototypes ***
 *  -----------------------------
 *  
 *  const QBrush & background () const
 *  QRectF boundingRect ( const QRectF & rectangle, int flags, const QString & text )
 *  QRectF boundingRect ( const QRectF & rectangle, const QString & text, const QTextOption & option = QTextOption() )
 *  const QBrush & brush () const
 *  QPainterPath clipPath () const
 *  QRegion clipRegion () const
 *  QMatrix combinedMatrix () const
 *  QTransform combinedTransform () const
 *  const QMatrix & deviceMatrix () const
 *  const QTransform & deviceTransform () const
 *  void drawArc ( const QRectF & rectangle, int startAngle, int spanAngle )
 *  void drawChord ( const QRectF & rectangle, int startAngle, int spanAngle )
 *  void drawConvexPolygon ( const QPoint * points, int pointCount )
 *  void drawConvexPolygon ( const QPolygonF & polygon )
 *  void drawConvexPolygon ( const QPolygon & polygon )
 *  void drawEllipse ( const QRectF & rectangle )
 *  void drawEllipse ( const QPointF & center, qreal rx, qreal ry )
 *  void drawImage ( const QPointF & point, const QImage & image )
 *  void drawImage ( const QPoint & point, const QImage & image )
 *  void drawImage ( const QRectF & rectangle, const QImage & image )
 *  void drawImage ( const QRect & rectangle, const QImage & image )
 *  void drawLine ( const QLineF & line )
 *  void drawLine ( const QLine & line )
 *  void drawLine ( const QPointF & p1, const QPointF & p2 )
 *  void drawLines ( const QPoint * pointPairs, int lineCount )
 *  void drawLines ( const QVector<QPointF> & pointPairs )
 *  void drawLines ( const QVector<QPoint> & pointPairs )
 *  void drawLines ( const QVector<QLineF> & lines )
 *  void drawLines ( const QVector<QLine> & lines )
 *  void drawPath ( const QPainterPath & path )
 *  void drawPicture ( const QPointF & point, const QPicture & picture )
 *  void drawPicture ( const QPoint & point, const QPicture & picture )
 *  void drawPicture ( int x, int y, const QPicture & picture )
 *  void drawPie ( const QRectF & rectangle, int startAngle, int spanAngle )
 *  void drawPixmap ( const QRectF & target, const QPixmap & pixmap, const QRectF & source )
 *  void drawPixmap ( const QRect & target, const QPixmap & pixmap, const QRect & source )
 *  void drawPixmap ( const QPointF & point, const QPixmap & pixmap, const QRectF & source )
 *  void drawPixmap ( const QPoint & point, const QPixmap & pixmap, const QRect & source )
 *  void drawPixmap ( const QPointF & point, const QPixmap & pixmap )
 *  void drawPixmap ( const QPoint & point, const QPixmap & pixmap )
 *  void drawPixmap ( int x, int y, const QPixmap & pixmap )
 *  void drawPixmap ( const QRect & rectangle, const QPixmap & pixmap )
 *  void drawPixmap ( int x, int y, int width, int height, const QPixmap & pixmap )
 *  void drawPixmap ( int x, int y, int w, int h, const QPixmap & pixmap, int sx, int sy, int sw, int sh )
 *  void drawPixmap ( int x, int y, const QPixmap & pixmap, int sx, int sy, int sw, int sh )
 *  void drawPoint ( const QPointF & position )
 *  void drawPoints ( const QPoint * points, int pointCount )
 *  void drawPoints ( const QPolygonF & points )
 *  void drawPoints ( const QPolygon & points )
 *  void drawPolygon ( const QPoint * points, int pointCount, Qt::FillRule fillRule = Qt::OddEvenFill )
 *  void drawPolygon ( const QPolygonF & points, Qt::FillRule fillRule = Qt::OddEvenFill )
 *  void drawPolygon ( const QPolygon & points, Qt::FillRule fillRule = Qt::OddEvenFill )
 *  void drawPolyline ( const QPoint * points, int pointCount )
 *  void drawPolyline ( const QPolygonF & points )
 *  void drawPolyline ( const QPolygon & points )
 *  void drawRect ( const QRectF & rectangle )
 *  void drawRects ( const QRect * rectangles, int rectCount )
 *  void drawRects ( const QVector<QRectF> & rectangles )
 *  void drawRects ( const QVector<QRect> & rectangles )
 *  void drawRoundedRect ( const QRectF & rect, qreal xRadius, qreal yRadius, Qt::SizeMode mode = Qt::AbsoluteSize )
 *  void drawText ( const QPointF & position, const QString & text )
 *  void drawText ( const QRectF & rectangle, int flags, const QString & text, QRectF * boundingRect = 0 )
 *  void drawText ( const QRect & rectangle, int flags, const QString & text, QRect * boundingRect = 0 )
 *  void drawText ( int x, int y, int width, int height, int flags, const QString & text, QRect * boundingRect = 0 )
 *  void drawText ( const QRectF & rectangle, const QString & text, const QTextOption & option = QTextOption() )
 *  void drawTiledPixmap ( const QRectF & rectangle, const QPixmap & pixmap, const QPointF & position = QPointF() )
 *  void drawTiledPixmap ( const QRect & rectangle, const QPixmap & pixmap, const QPoint & position = QPoint() )
 *  void drawTiledPixmap ( int x, int y, int width, int height, const QPixmap & pixmap, int sx = 0, int sy = 0 )
 *  void eraseRect ( const QRectF & rectangle )
 *  void fillPath ( const QPainterPath & path, const QBrush & brush )
 *  void fillRect ( const QRectF & rectangle, const QBrush & brush )
 *  void fillRect ( const QRectF & rectangle, Qt::BrushStyle style )
 *  void fillRect ( const QRect & rectangle, const QBrush & brush )
 *  void fillRect ( const QRect & rectangle, const QColor & color )
 *  void fillRect ( const QRectF & rectangle, const QColor & color )
 *  void fillRect ( int x, int y, int width, int height, const QBrush & brush )
 *  void fillRect ( int x, int y, int width, int height, const QColor & color )
 *  void fillRect ( const QRectF & rectangle, Qt::GlobalColor color )
 *  const QFont & font () const
 *  QFontInfo fontInfo () const
 *  QFontMetrics fontMetrics () const
 *  const QPen & pen () const
 *  RenderHints renderHints () const
 *  void setBackground ( const QBrush & brush )
 *  void setBrush ( const QBrush & brush )
 *  void setBrushOrigin ( const QPointF & position )
 *  void setClipPath ( const QPainterPath & path, Qt::ClipOperation operation = Qt::ReplaceClip )
 *  void setClipRect ( const QRectF & rectangle, Qt::ClipOperation operation = Qt::ReplaceClip )
 *  void setClipRegion ( const QRegion & region, Qt::ClipOperation operation = Qt::ReplaceClip )
 *  void setFont ( const QFont & font )
 *  void setPen ( const QPen & pen )
 *  void setPen ( const QColor & color )
 *  void setRenderHints ( RenderHints hints, bool on = true )
 *  void setTransform ( const QTransform & transform, bool combine = false )
 *  void setWorldMatrix ( const QMatrix & matrix, bool combine = false )
 *  void setWorldTransform ( const QTransform & matrix, bool combine = false )
 *  void strokePath ( const QPainterPath & path, const QPen & pen )
 *  const QTransform & transform () const
 *  void translate ( const QPointF & offset )
 *  const QMatrix & worldMatrix () const
 *  const QTransform & worldTransform () const
 */ 


#include <QtGui/QPainter>

/*
 * QPainter ()
 * QPainter ( QPaintDevice * device )
 * ~QPainter ()
 */
HB_FUNC( QT_QPAINTER )
{
  if( hb_pcount() >= 1 && HB_ISPOINTER( 1 ) )
  {
     hb_retptr( ( QPainter* ) new QPainter( hbqt_par_QPaintDevice( 1 ) ) );
  }
  else
  {
     hb_retptr( ( QPainter* ) new QPainter() );
  } 
}

/*
 * Qt::BGMode backgroundMode () const
 */
HB_FUNC( QT_QPAINTER_BACKGROUNDMODE )
{
   hb_retni( hbqt_par_QPainter( 1 )->backgroundMode(  ) );
}

/*
 * bool begin ( QPaintDevice * device )
 */
HB_FUNC( QT_QPAINTER_BEGIN )
{
   hb_retl( hbqt_par_QPainter( 1 )->begin( hbqt_par_QPaintDevice( 2 ) ) );
}

/*
 * QRect boundingRect ( const QRect & rectangle, int flags, const QString & text )
 */
HB_FUNC( QT_QPAINTER_BOUNDINGRECT )
{
   hbqt_ret_QRect( hbqt_par_QPainter( 1 )->boundingRect( hbqt_const_QRect( 2 ), hb_parni( 3 ), hbqt_par_QString( 4 ) ) );
}

/*
 * QRect boundingRect ( int x, int y, int w, int h, int flags, const QString & text )
 */
HB_FUNC( QT_QPAINTER_BOUNDINGRECT_1 )
{
   hbqt_ret_QRect( hbqt_par_QPainter( 1 )->boundingRect( hb_parni( 2 ), hb_parni( 3 ), hb_parni( 4 ), hb_parni( 5 ), hb_parni( 6 ), hbqt_par_QString( 7 ) ) );
}

/*
 * QPoint brushOrigin () const
 */
HB_FUNC( QT_QPAINTER_BRUSHORIGIN )
{
   hbqt_ret_QPoint( hbqt_par_QPainter( 1 )->brushOrigin(  ) );
}

/*
 * CompositionMode compositionMode () const
 */
HB_FUNC( QT_QPAINTER_COMPOSITIONMODE )
{
   hb_retni( hbqt_par_QPainter( 1 )->compositionMode(  ) );
}

/*
 * QPaintDevice * device () const
 */
HB_FUNC( QT_QPAINTER_DEVICE )
{
   hb_retptr( ( QPaintDevice* ) hbqt_par_QPainter( 1 )->device(  ) );
}

/*
 * void drawArc ( const QRect & rectangle, int startAngle, int spanAngle )
 */
HB_FUNC( QT_QPAINTER_DRAWARC )
{
   hbqt_par_QPainter( 1 )->drawArc( hbqt_const_QRect( 2 ), hb_parni( 3 ), hb_parni( 4 ) );
}

/*
 * void drawArc ( int x, int y, int width, int height, int startAngle, int spanAngle )
 */
HB_FUNC( QT_QPAINTER_DRAWARC_1 )
{
   hbqt_par_QPainter( 1 )->drawArc( hb_parni( 2 ), hb_parni( 3 ), hb_parni( 4 ), hb_parni( 5 ), hb_parni( 6 ), hb_parni( 7 ) );
}

/*
 * void drawChord ( const QRect & rectangle, int startAngle, int spanAngle )
 */
HB_FUNC( QT_QPAINTER_DRAWCHORD )
{
   hbqt_par_QPainter( 1 )->drawChord( hbqt_const_QRect( 2 ), hb_parni( 3 ), hb_parni( 4 ) );
}

/*
 * void drawChord ( int x, int y, int width, int height, int startAngle, int spanAngle )
 */
HB_FUNC( QT_QPAINTER_DRAWCHORD_1 )
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
 * void drawEllipse ( const QRect & rectangle )
 */
HB_FUNC( QT_QPAINTER_DRAWELLIPSE )
{
   hbqt_par_QPainter( 1 )->drawEllipse( hbqt_const_QRect( 2 ) );
}

/*
 * void drawEllipse ( int x, int y, int width, int height )
 */
HB_FUNC( QT_QPAINTER_DRAWELLIPSE_1 )
{
   hbqt_par_QPainter( 1 )->drawEllipse( hb_parni( 2 ), hb_parni( 3 ), hb_parni( 4 ), hb_parni( 5 ) );
}

/*
 * void drawEllipse ( const QPoint & center, int rx, int ry )
 */
HB_FUNC( QT_QPAINTER_DRAWELLIPSE_2 )
{
   hbqt_par_QPainter( 1 )->drawEllipse( hbqt_const_QPoint( 2 ), hb_parni( 3 ), hb_parni( 4 ) );
}

/*
 * void drawLine ( const QPoint & p1, const QPoint & p2 )
 */
HB_FUNC( QT_QPAINTER_DRAWLINE )
{
   hbqt_par_QPainter( 1 )->drawLine( hbqt_const_QPoint( 2 ), hbqt_const_QPoint( 3 ) );
}

/*
 * void drawLine ( int x1, int y1, int x2, int y2 )
 */
HB_FUNC( QT_QPAINTER_DRAWLINE_1 )
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
 * void drawPie ( const QRect & rectangle, int startAngle, int spanAngle )
 */
HB_FUNC( QT_QPAINTER_DRAWPIE )
{
   hbqt_par_QPainter( 1 )->drawPie( hbqt_const_QRect( 2 ), hb_parni( 3 ), hb_parni( 4 ) );
}

/*
 * void drawPie ( int x, int y, int width, int height, int startAngle, int spanAngle )
 */
HB_FUNC( QT_QPAINTER_DRAWPIE_1 )
{
   hbqt_par_QPainter( 1 )->drawPie( hb_parni( 2 ), hb_parni( 3 ), hb_parni( 4 ), hb_parni( 5 ), hb_parni( 6 ), hb_parni( 7 ) );
}

/*
 * void drawPoint ( const QPoint & position )
 */
HB_FUNC( QT_QPAINTER_DRAWPOINT )
{
   hbqt_par_QPainter( 1 )->drawPoint( hbqt_const_QPoint( 2 ) );
}

/*
 * void drawPoint ( int x, int y )
 */
HB_FUNC( QT_QPAINTER_DRAWPOINT_1 )
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
 * void drawPolygon ( const QPointF * points, int pointCount, Qt::FillRule fillRule = Qt::OddEvenFill )
 */
HB_FUNC( QT_QPAINTER_DRAWPOLYGON )
{
   hbqt_par_QPainter( 1 )->drawPolygon( hbqt_par_QPointF( 2 ), hb_parni( 3 ), ( Qt::FillRule ) hb_parni( 4 ) );
}

/*
 * void drawPolyline ( const QPointF * points, int pointCount )
 */
HB_FUNC( QT_QPAINTER_DRAWPOLYLINE )
{
   hbqt_par_QPainter( 1 )->drawPolyline( hbqt_par_QPointF( 2 ), hb_parni( 3 ) );
}

/*
 * void drawRect ( const QRect & rectangle )
 */
HB_FUNC( QT_QPAINTER_DRAWRECT )
{
   hbqt_par_QPainter( 1 )->drawRect( hbqt_const_QRect( 2 ) );
}

/*
 * void drawRect ( int x, int y, int width, int height )
 */
HB_FUNC( QT_QPAINTER_DRAWRECT_1 )
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
 * void drawRoundedRect ( const QRect & rect, qreal xRadius, qreal yRadius, Qt::SizeMode mode = Qt::AbsoluteSize )
 */
HB_FUNC( QT_QPAINTER_DRAWROUNDEDRECT )
{
   hbqt_par_QPainter( 1 )->drawRoundedRect( hbqt_const_QRect( 2 ), hb_parnd( 3 ), hb_parnd( 4 ), ( Qt::SizeMode ) hb_parni( 5 ) );
}

/*
 * void drawRoundedRect ( int x, int y, int w, int h, qreal xRadius, qreal yRadius, Qt::SizeMode mode = Qt::AbsoluteSize )
 */
HB_FUNC( QT_QPAINTER_DRAWROUNDEDRECT_1 )
{
   hbqt_par_QPainter( 1 )->drawRoundedRect( hb_parni( 2 ), hb_parni( 3 ), hb_parni( 4 ), hb_parni( 5 ), hb_parnd( 6 ), hb_parnd( 7 ), ( Qt::SizeMode ) hb_parni( 8 ) );
}

/*
 * void drawText ( const QPoint & position, const QString & text )
 */
HB_FUNC( QT_QPAINTER_DRAWTEXT )
{
   hbqt_par_QPainter( 1 )->drawText( hbqt_const_QPoint( 2 ), hbqt_par_QString( 3 ) );
}

/*
 * void drawText ( int x, int y, const QString & text )
 */
HB_FUNC( QT_QPAINTER_DRAWTEXT_1 )
{
   hbqt_par_QPainter( 1 )->drawText( hb_parni( 2 ), hb_parni( 3 ), hbqt_par_QString( 4 ) );
}

/*
 * bool end ()
 */
HB_FUNC( QT_QPAINTER_END )
{
   hb_retl( hbqt_par_QPainter( 1 )->end(  ) );
}

/*
 * void eraseRect ( const QRect & rectangle )
 */
HB_FUNC( QT_QPAINTER_ERASERECT )
{
   hbqt_par_QPainter( 1 )->eraseRect( hbqt_const_QRect( 2 ) );
}

/*
 * void eraseRect ( int x, int y, int width, int height )
 */
HB_FUNC( QT_QPAINTER_ERASERECT_1 )
{
   hbqt_par_QPainter( 1 )->eraseRect( hb_parni( 2 ), hb_parni( 3 ), hb_parni( 4 ), hb_parni( 5 ) );
}

/*
 * void fillRect ( int x, int y, int width, int height, Qt::BrushStyle style )
 */
HB_FUNC( QT_QPAINTER_FILLRECT )
{
   hbqt_par_QPainter( 1 )->fillRect( hb_parni( 2 ), hb_parni( 3 ), hb_parni( 4 ), hb_parni( 5 ), ( Qt::BrushStyle ) hb_parni( 6 ) );
}

/*
 * void fillRect ( const QRect & rectangle, Qt::BrushStyle style )
 */
HB_FUNC( QT_QPAINTER_FILLRECT_1 )
{
   hbqt_par_QPainter( 1 )->fillRect( hbqt_const_QRect( 2 ), ( Qt::BrushStyle ) hb_parni( 3 ) );
}

/*
 * void fillRect ( int x, int y, int width, int height, Qt::GlobalColor color )
 */
HB_FUNC( QT_QPAINTER_FILLRECT_2 )
{
   hbqt_par_QPainter( 1 )->fillRect( hb_parni( 2 ), hb_parni( 3 ), hb_parni( 4 ), hb_parni( 5 ), ( Qt::GlobalColor ) hb_parni( 6 ) );
}

/*
 * void fillRect ( const QRect & rectangle, Qt::GlobalColor color )
 */
HB_FUNC( QT_QPAINTER_FILLRECT_3 )
{
   hbqt_par_QPainter( 1 )->fillRect( hbqt_const_QRect( 2 ), ( Qt::GlobalColor ) hb_parni( 3 ) );
}

/*
 * bool hasClipping () const
 */
HB_FUNC( QT_QPAINTER_HASCLIPPING )
{
   hb_retl( hbqt_par_QPainter( 1 )->hasClipping(  ) );
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
   hb_retl( hbqt_par_QPainter( 1 )->isActive(  ) );
}

/*
 * Qt::LayoutDirection layoutDirection () const
 */
HB_FUNC( QT_QPAINTER_LAYOUTDIRECTION )
{
   hb_retni( hbqt_par_QPainter( 1 )->layoutDirection(  ) );
}

/*
 * qreal opacity () const
 */
HB_FUNC( QT_QPAINTER_OPACITY )
{
   hb_retnd( hbqt_par_QPainter( 1 )->opacity(  ) );
}

/*
 * QPaintEngine * paintEngine () const
 */
HB_FUNC( QT_QPAINTER_PAINTENGINE )
{
   hb_retptr( ( QPaintEngine* ) hbqt_par_QPainter( 1 )->paintEngine(  ) );
}

/*
 * void resetMatrix ()
 */
HB_FUNC( QT_QPAINTER_RESETMATRIX )
{
   hbqt_par_QPainter( 1 )->resetMatrix(  );
}

/*
 * void resetTransform ()
 */
HB_FUNC( QT_QPAINTER_RESETTRANSFORM )
{
   hbqt_par_QPainter( 1 )->resetTransform(  );
}

/*
 * void restore ()
 */
HB_FUNC( QT_QPAINTER_RESTORE )
{
   hbqt_par_QPainter( 1 )->restore(  );
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
   hbqt_par_QPainter( 1 )->save(  );
}

/*
 * void scale ( qreal sx, qreal sy )
 */
HB_FUNC( QT_QPAINTER_SCALE )
{
   hbqt_par_QPainter( 1 )->scale( hb_parnd( 2 ), hb_parnd( 3 ) );
}

/*
 * void setBackgroundMode ( Qt::BGMode mode )
 */
HB_FUNC( QT_QPAINTER_SETBACKGROUNDMODE )
{
   hbqt_par_QPainter( 1 )->setBackgroundMode( ( Qt::BGMode ) hb_parni( 2 ) );
}

/*
 * void setBrush ( Qt::BrushStyle style )
 */
HB_FUNC( QT_QPAINTER_SETBRUSH )
{
   hbqt_par_QPainter( 1 )->setBrush( ( Qt::BrushStyle ) hb_parni( 2 ) );
}

/*
 * void setBrushOrigin ( const QPoint & position )
 */
HB_FUNC( QT_QPAINTER_SETBRUSHORIGIN )
{
   hbqt_par_QPainter( 1 )->setBrushOrigin( hbqt_const_QPoint( 2 ) );
}

/*
 * void setBrushOrigin ( int x, int y )
 */
HB_FUNC( QT_QPAINTER_SETBRUSHORIGIN_1 )
{
   hbqt_par_QPainter( 1 )->setBrushOrigin( hb_parni( 2 ), hb_parni( 3 ) );
}

/*
 * void setClipRect ( int x, int y, int width, int height, Qt::ClipOperation operation = Qt::ReplaceClip )
 */
HB_FUNC( QT_QPAINTER_SETCLIPRECT )
{
   hbqt_par_QPainter( 1 )->setClipRect( hb_parni( 2 ), hb_parni( 3 ), hb_parni( 4 ), hb_parni( 5 ), ( Qt::ClipOperation ) hb_parni( 6 ) );
}

/*
 * void setClipRect ( const QRect & rectangle, Qt::ClipOperation operation = Qt::ReplaceClip )
 */
HB_FUNC( QT_QPAINTER_SETCLIPRECT_1 )
{
   hbqt_par_QPainter( 1 )->setClipRect( hbqt_const_QRect( 2 ), ( Qt::ClipOperation ) hb_parni( 3 ) );
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
 * void setPen ( Qt::PenStyle style )
 */
HB_FUNC( QT_QPAINTER_SETPEN )
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
   hbqt_par_QPainter( 1 )->setViewport( hbqt_const_QRect( 2 ) );
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
   hbqt_par_QPainter( 1 )->setWindow( hbqt_const_QRect( 2 ) );
}

/*
 * void setWindow ( int x, int y, int width, int height )
 */
HB_FUNC( QT_QPAINTER_SETWINDOW_1 )
{
   hbqt_par_QPainter( 1 )->setWindow( hb_parni( 2 ), hb_parni( 3 ), hb_parni( 4 ), hb_parni( 5 ) );
}

/*
 * void setWorldMatrixEnabled ( bool enable )
 */
HB_FUNC( QT_QPAINTER_SETWORLDMATRIXENABLED )
{
   hbqt_par_QPainter( 1 )->setWorldMatrixEnabled( hb_parl( 2 ) );
}

/*
 * void shear ( qreal sh, qreal sv )
 */
HB_FUNC( QT_QPAINTER_SHEAR )
{
   hbqt_par_QPainter( 1 )->shear( hb_parnd( 2 ), hb_parnd( 3 ) );
}

/*
 * bool testRenderHint ( RenderHint hint ) const
 */
HB_FUNC( QT_QPAINTER_TESTRENDERHINT )
{
   hb_retl( hbqt_par_QPainter( 1 )->testRenderHint( ( QPainter::RenderHint ) hb_parni( 2 ) ) );
}

/*
 * void translate ( const QPoint & offset )
 */
HB_FUNC( QT_QPAINTER_TRANSLATE )
{
   hbqt_par_QPainter( 1 )->translate( hbqt_const_QPoint( 2 ) );
}

/*
 * void translate ( qreal dx, qreal dy )
 */
HB_FUNC( QT_QPAINTER_TRANSLATE_1 )
{
   hbqt_par_QPainter( 1 )->translate( hb_parnd( 2 ), hb_parnd( 3 ) );
}

/*
 * bool viewTransformEnabled () const
 */
HB_FUNC( QT_QPAINTER_VIEWTRANSFORMENABLED )
{
   hb_retl( hbqt_par_QPainter( 1 )->viewTransformEnabled(  ) );
}

/*
 * QRect viewport () const
 */
HB_FUNC( QT_QPAINTER_VIEWPORT )
{
   hbqt_ret_QRect( hbqt_par_QPainter( 1 )->viewport(  ) );
}

/*
 * QRect window () const
 */
HB_FUNC( QT_QPAINTER_WINDOW )
{
   hbqt_ret_QRect( hbqt_par_QPainter( 1 )->window(  ) );
}

/*
 * bool worldMatrixEnabled () const
 */
HB_FUNC( QT_QPAINTER_WORLDMATRIXENABLED )
{
   hb_retl( hbqt_par_QPainter( 1 )->worldMatrixEnabled(  ) );
}


/*----------------------------------------------------------------------*/
#endif             /* #if QT_VERSION >= 0x040500 */
/*----------------------------------------------------------------------*/

