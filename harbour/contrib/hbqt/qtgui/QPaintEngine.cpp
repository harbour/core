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

/*
 *  enum DirtyFlag { DirtyPen, DirtyBrush, DirtyBrushOrigin, DirtyFont, ..., AllDirty }
 *  flags DirtyFlags
 *  enum PaintEngineFeature { AlphaBlend, Antialiasing, BlendModes, BrushStroke, ..., AllFeatures }
 *  flags PaintEngineFeatures
 *  enum PolygonDrawMode { OddEvenMode, WindingMode, ConvexMode, PolylineMode }
 *  enum Type { X11, Windows, MacPrinter, CoreGraphics, ..., MaxUser }
 */

#include <QtCore/QPointer>

#include <QtGui/QPaintEngine>


/*
 * QPaintEngine ( PaintEngineFeatures caps = 0 )
 * virtual ~QPaintEngine ()
 */

typedef struct
{
   void * ph;
   bool bNew;
   QT_G_FUNC_PTR func;
} QGC_POINTER_QPaintEngine;

QT_G_FUNC( hbqt_gcRelease_QPaintEngine )
{
   HB_SYMBOL_UNUSED( Cargo );
}

void * hbqt_gcAllocate_QPaintEngine( void * pObj, bool bNew )
{
   QGC_POINTER * p = ( QGC_POINTER * ) hb_gcAllocate( sizeof( QGC_POINTER ), hbqt_gcFuncs() );

   p->ph = pObj;
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QPaintEngine;

   if( bNew )
   {
      HB_TRACE( HB_TR_DEBUG, ( "   _new_QPaintEngine               ph=%p %i B %i KB", pObj, ( int ) hb_xquery( 1001 ), hbqt_getmemused() ) );
   }
   return p;
}

HB_FUNC( QT_QPAINTENGINE )
{
}

/*
 * virtual bool begin ( QPaintDevice * pdev ) = 0
 */
HB_FUNC( QT_QPAINTENGINE_BEGIN )
{
   hb_retl( hbqt_par_QPaintEngine( 1 )->begin( hbqt_par_QPaintDevice( 2 ) ) );
}

/*
 * virtual void drawEllipse ( const QRectF & rect )
 */
HB_FUNC( QT_QPAINTENGINE_DRAWELLIPSE )
{
   hbqt_par_QPaintEngine( 1 )->drawEllipse( *hbqt_par_QRectF( 2 ) );
}

/*
 * virtual void drawEllipse ( const QRect & rect )
 */
HB_FUNC( QT_QPAINTENGINE_DRAWELLIPSE_1 )
{
   hbqt_par_QPaintEngine( 1 )->drawEllipse( *hbqt_par_QRect( 2 ) );
}

/*
 * virtual void drawImage ( const QRectF & rectangle, const QImage & image, const QRectF & sr, Qt::ImageConversionFlags flags = Qt::AutoColor )
 */
HB_FUNC( QT_QPAINTENGINE_DRAWIMAGE )
{
   hbqt_par_QPaintEngine( 1 )->drawImage( *hbqt_par_QRectF( 2 ), *hbqt_par_QImage( 3 ), *hbqt_par_QRectF( 4 ), ( HB_ISNUM( 5 ) ? ( Qt::ImageConversionFlags ) hb_parni( 5 ) : ( Qt::ImageConversionFlags ) Qt::AutoColor ) );
}

/*
 * virtual void drawLines ( const QLineF * lines, int lineCount )
 */
HB_FUNC( QT_QPAINTENGINE_DRAWLINES )
{
   hbqt_par_QPaintEngine( 1 )->drawLines( hbqt_par_QLineF( 2 ), hb_parni( 3 ) );
}

/*
 * virtual void drawLines ( const QLine * lines, int lineCount )
 */
HB_FUNC( QT_QPAINTENGINE_DRAWLINES_1 )
{
   hbqt_par_QPaintEngine( 1 )->drawLines( hbqt_par_QLine( 2 ), hb_parni( 3 ) );
}

/*
 * virtual void drawPath ( const QPainterPath & path )
 */
HB_FUNC( QT_QPAINTENGINE_DRAWPATH )
{
   hbqt_par_QPaintEngine( 1 )->drawPath( *hbqt_par_QPainterPath( 2 ) );
}

/*
 * virtual void drawPixmap ( const QRectF & r, const QPixmap & pm, const QRectF & sr ) = 0
 */
HB_FUNC( QT_QPAINTENGINE_DRAWPIXMAP )
{
   hbqt_par_QPaintEngine( 1 )->drawPixmap( *hbqt_par_QRectF( 2 ), *hbqt_par_QPixmap( 3 ), *hbqt_par_QRectF( 4 ) );
}

/*
 * virtual void drawPoints ( const QPointF * points, int pointCount )
 */
HB_FUNC( QT_QPAINTENGINE_DRAWPOINTS )
{
   hbqt_par_QPaintEngine( 1 )->drawPoints( hbqt_par_QPointF( 2 ), hb_parni( 3 ) );
}

/*
 * virtual void drawPoints ( const QPoint * points, int pointCount )
 */
HB_FUNC( QT_QPAINTENGINE_DRAWPOINTS_1 )
{
   hbqt_par_QPaintEngine( 1 )->drawPoints( hbqt_par_QPoint( 2 ), hb_parni( 3 ) );
}

/*
 * virtual void drawPolygon ( const QPointF * points, int pointCount, PolygonDrawMode mode )
 */
HB_FUNC( QT_QPAINTENGINE_DRAWPOLYGON )
{
   hbqt_par_QPaintEngine( 1 )->drawPolygon( hbqt_par_QPointF( 2 ), hb_parni( 3 ), ( QPaintEngine::PolygonDrawMode ) hb_parni( 4 ) );
}

/*
 * virtual void drawPolygon ( const QPoint * points, int pointCount, PolygonDrawMode mode )
 */
HB_FUNC( QT_QPAINTENGINE_DRAWPOLYGON_1 )
{
   hbqt_par_QPaintEngine( 1 )->drawPolygon( hbqt_par_QPoint( 2 ), hb_parni( 3 ), ( QPaintEngine::PolygonDrawMode ) hb_parni( 4 ) );
}

/*
 * virtual void drawRects ( const QRectF * rects, int rectCount )
 */
HB_FUNC( QT_QPAINTENGINE_DRAWRECTS )
{
   hbqt_par_QPaintEngine( 1 )->drawRects( hbqt_par_QRectF( 2 ), hb_parni( 3 ) );
}

/*
 * virtual void drawRects ( const QRect * rects, int rectCount )
 */
HB_FUNC( QT_QPAINTENGINE_DRAWRECTS_1 )
{
   hbqt_par_QPaintEngine( 1 )->drawRects( hbqt_par_QRect( 2 ), hb_parni( 3 ) );
}

/*
 * virtual void drawTextItem ( const QPointF & p, const QTextItem & textItem )
 */
HB_FUNC( QT_QPAINTENGINE_DRAWTEXTITEM )
{
   hbqt_par_QPaintEngine( 1 )->drawTextItem( *hbqt_par_QPointF( 2 ), *hbqt_par_QTextItem( 3 ) );
}

/*
 * virtual void drawTiledPixmap ( const QRectF & rect, const QPixmap & pixmap, const QPointF & p )
 */
HB_FUNC( QT_QPAINTENGINE_DRAWTILEDPIXMAP )
{
   hbqt_par_QPaintEngine( 1 )->drawTiledPixmap( *hbqt_par_QRectF( 2 ), *hbqt_par_QPixmap( 3 ), *hbqt_par_QPointF( 4 ) );
}

/*
 * virtual bool end () = 0
 */
HB_FUNC( QT_QPAINTENGINE_END )
{
   hb_retl( hbqt_par_QPaintEngine( 1 )->end() );
}

/*
 * bool hasFeature ( PaintEngineFeatures feature ) const
 */
HB_FUNC( QT_QPAINTENGINE_HASFEATURE )
{
   hb_retl( hbqt_par_QPaintEngine( 1 )->hasFeature( ( QPaintEngine::PaintEngineFeatures ) hb_parni( 2 ) ) );
}

/*
 * bool isActive () const
 */
HB_FUNC( QT_QPAINTENGINE_ISACTIVE )
{
   hb_retl( hbqt_par_QPaintEngine( 1 )->isActive() );
}

/*
 * QPaintDevice * paintDevice () const
 */
HB_FUNC( QT_QPAINTENGINE_PAINTDEVICE )
{
   hb_retptrGC( hbqt_gcAllocate_QPaintDevice( hbqt_par_QPaintEngine( 1 )->paintDevice(), false ) );
}

/*
 * QPainter * painter () const
 */
HB_FUNC( QT_QPAINTENGINE_PAINTER )
{
   hb_retptrGC( hbqt_gcAllocate_QPainter( hbqt_par_QPaintEngine( 1 )->painter(), false ) );
}

/*
 * void setActive ( bool state )
 */
HB_FUNC( QT_QPAINTENGINE_SETACTIVE )
{
   hbqt_par_QPaintEngine( 1 )->setActive( hb_parl( 2 ) );
}

/*
 * virtual Type type () const = 0
 */
HB_FUNC( QT_QPAINTENGINE_TYPE )
{
   hb_retni( ( QPaintEngine::Type ) hbqt_par_QPaintEngine( 1 )->type() );
}

/*
 * virtual void updateState ( const QPaintEngineState & state ) = 0
 */
HB_FUNC( QT_QPAINTENGINE_UPDATESTATE )
{
   hbqt_par_QPaintEngine( 1 )->updateState( *hbqt_par_QPaintEngineState( 2 ) );
}


/*----------------------------------------------------------------------*/
#endif             /* #if QT_VERSION >= 0x040500 */
/*----------------------------------------------------------------------*/
