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
 * www - http://harbour-project.org
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

#include "hbqt.h"
#include "hbqtgui_garbage.h"
#include "hbqtcore_garbage.h"

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

typedef struct
{
   QPainter * ph;
   bool bNew;
   QT_G_FUNC_PTR func;
   int type;
} QGC_POINTER_QPainter;

QT_G_FUNC( hbqt_gcRelease_QPainter )
{
   QGC_POINTER * p = ( QGC_POINTER * ) Cargo;

   if( p && p->bNew )
   {
      if( p->ph )
      {
         HB_TRACE( HB_TR_DEBUG, ( "ph=%p    _rel_QPainter   /.\\", p->ph ) );
         delete ( ( QPainter * ) p->ph );
         HB_TRACE( HB_TR_DEBUG, ( "ph=%p YES_rel_QPainter   \\./", p->ph ) );
         p->ph = NULL;
      }
      else
      {
         HB_TRACE( HB_TR_DEBUG, ( "ph=%p DEL_rel_QPainter    :     Object already deleted!", p->ph ) );
         p->ph = NULL;
      }
   }
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p PTR_rel_QPainter    :    Object not created with new=true", p->ph ) );
      p->ph = NULL;
   }
}

void * hbqt_gcAllocate_QPainter( void * pObj, bool bNew )
{
   QGC_POINTER * p = ( QGC_POINTER * ) hb_gcAllocate( sizeof( QGC_POINTER ), hbqt_gcFuncs() );

   p->ph = ( QPainter * ) pObj;
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QPainter;
   p->type = HBQT_TYPE_QPainter;

   if( bNew )
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p    _new_QPainter", pObj ) );
   }
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p NOT_new_QPainter", pObj ) );
   }
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

/*
 * const QBrush & background () const
 */
HB_FUNC( QT_QPAINTER_BACKGROUND )
{
   QPainter * p = hbqt_par_QPainter( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QBrush( new QBrush( ( p )->background() ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QPAINTER_BACKGROUND FP=hb_retptrGC( hbqt_gcAllocate_QBrush( new QBrush( ( p )->background() ), true ) ); p is NULL" ) );
   }
}

/*
 * Qt::BGMode backgroundMode () const
 */
HB_FUNC( QT_QPAINTER_BACKGROUNDMODE )
{
   QPainter * p = hbqt_par_QPainter( 1 );
   if( p )
      hb_retni( ( Qt::BGMode ) ( p )->backgroundMode() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QPAINTER_BACKGROUNDMODE FP=hb_retni( ( Qt::BGMode ) ( p )->backgroundMode() ); p is NULL" ) );
   }
}

/*
 * bool begin ( QPaintDevice * device )
 */
HB_FUNC( QT_QPAINTER_BEGIN )
{
   QPainter * p = hbqt_par_QPainter( 1 );
   if( p )
      hb_retl( ( p )->begin( hbqt_par_QPaintDevice( 2 ) ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QPAINTER_BEGIN FP=hb_retl( ( p )->begin( hbqt_par_QPaintDevice( 2 ) ) ); p is NULL" ) );
   }
}

/*
 * QRectF boundingRect ( const QRectF & rectangle, int flags, const QString & text )
 */
HB_FUNC( QT_QPAINTER_BOUNDINGRECT )
{
   QPainter * p = hbqt_par_QPainter( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QRectF( new QRectF( ( p )->boundingRect( *hbqt_par_QRectF( 2 ), hb_parni( 3 ), hbqt_par_QString( 4 ) ) ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QPAINTER_BOUNDINGRECT FP=hb_retptrGC( hbqt_gcAllocate_QRectF( new QRectF( ( p )->boundingRect( *hbqt_par_QRectF( 2 ), hb_parni( 3 ), hbqt_par_QString( 4 ) ) ), true ) ); p is NULL" ) );
   }
}

/*
 * QRect boundingRect ( const QRect & rectangle, int flags, const QString & text )
 */
HB_FUNC( QT_QPAINTER_BOUNDINGRECT_1 )
{
   QPainter * p = hbqt_par_QPainter( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QRect( new QRect( ( p )->boundingRect( *hbqt_par_QRect( 2 ), hb_parni( 3 ), hbqt_par_QString( 4 ) ) ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QPAINTER_BOUNDINGRECT_1 FP=hb_retptrGC( hbqt_gcAllocate_QRect( new QRect( ( p )->boundingRect( *hbqt_par_QRect( 2 ), hb_parni( 3 ), hbqt_par_QString( 4 ) ) ), true ) ); p is NULL" ) );
   }
}

/*
 * QRect boundingRect ( int x, int y, int w, int h, int flags, const QString & text )
 */
HB_FUNC( QT_QPAINTER_BOUNDINGRECT_2 )
{
   QPainter * p = hbqt_par_QPainter( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QRect( new QRect( ( p )->boundingRect( hb_parni( 2 ), hb_parni( 3 ), hb_parni( 4 ), hb_parni( 5 ), hb_parni( 6 ), hbqt_par_QString( 7 ) ) ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QPAINTER_BOUNDINGRECT_2 FP=hb_retptrGC( hbqt_gcAllocate_QRect( new QRect( ( p )->boundingRect( hb_parni( 2 ), hb_parni( 3 ), hb_parni( 4 ), hb_parni( 5 ), hb_parni( 6 ), hbqt_par_QString( 7 ) ) ), true ) ); p is NULL" ) );
   }
}

/*
 * QRectF boundingRect ( const QRectF & rectangle, const QString & text, const QTextOption & option = QTextOption() )
 */
HB_FUNC( QT_QPAINTER_BOUNDINGRECT_3 )
{
   QPainter * p = hbqt_par_QPainter( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QRectF( new QRectF( ( p )->boundingRect( *hbqt_par_QRectF( 2 ), hbqt_par_QString( 3 ), ( HB_ISPOINTER( 4 ) ? *hbqt_par_QTextOption( 4 ) : QTextOption() ) ) ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QPAINTER_BOUNDINGRECT_3 FP=hb_retptrGC( hbqt_gcAllocate_QRectF( new QRectF( ( p )->boundingRect( *hbqt_par_QRectF( 2 ), hbqt_par_QString( 3 ), ( HB_ISPOINTER( 4 ) ? *hbqt_par_QTextOption( 4 ) : QTextOption() ) ) ), true ) ); p is NULL" ) );
   }
}

/*
 * const QBrush & brush () const
 */
HB_FUNC( QT_QPAINTER_BRUSH )
{
   QPainter * p = hbqt_par_QPainter( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QBrush( new QBrush( ( p )->brush() ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QPAINTER_BRUSH FP=hb_retptrGC( hbqt_gcAllocate_QBrush( new QBrush( ( p )->brush() ), true ) ); p is NULL" ) );
   }
}

/*
 * QPoint brushOrigin () const
 */
HB_FUNC( QT_QPAINTER_BRUSHORIGIN )
{
   QPainter * p = hbqt_par_QPainter( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QPoint( new QPoint( ( p )->brushOrigin() ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QPAINTER_BRUSHORIGIN FP=hb_retptrGC( hbqt_gcAllocate_QPoint( new QPoint( ( p )->brushOrigin() ), true ) ); p is NULL" ) );
   }
}

/*
 * QPainterPath clipPath () const
 */
HB_FUNC( QT_QPAINTER_CLIPPATH )
{
   QPainter * p = hbqt_par_QPainter( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QPainterPath( new QPainterPath( ( p )->clipPath() ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QPAINTER_CLIPPATH FP=hb_retptrGC( hbqt_gcAllocate_QPainterPath( new QPainterPath( ( p )->clipPath() ), true ) ); p is NULL" ) );
   }
}

/*
 * QRegion clipRegion () const
 */
HB_FUNC( QT_QPAINTER_CLIPREGION )
{
   QPainter * p = hbqt_par_QPainter( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QRegion( new QRegion( ( p )->clipRegion() ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QPAINTER_CLIPREGION FP=hb_retptrGC( hbqt_gcAllocate_QRegion( new QRegion( ( p )->clipRegion() ), true ) ); p is NULL" ) );
   }
}

/*
 * QMatrix combinedMatrix () const
 */
HB_FUNC( QT_QPAINTER_COMBINEDMATRIX )
{
   QPainter * p = hbqt_par_QPainter( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QMatrix( new QMatrix( ( p )->combinedMatrix() ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QPAINTER_COMBINEDMATRIX FP=hb_retptrGC( hbqt_gcAllocate_QMatrix( new QMatrix( ( p )->combinedMatrix() ), true ) ); p is NULL" ) );
   }
}

/*
 * QTransform combinedTransform () const
 */
HB_FUNC( QT_QPAINTER_COMBINEDTRANSFORM )
{
   QPainter * p = hbqt_par_QPainter( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QTransform( new QTransform( ( p )->combinedTransform() ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QPAINTER_COMBINEDTRANSFORM FP=hb_retptrGC( hbqt_gcAllocate_QTransform( new QTransform( ( p )->combinedTransform() ), true ) ); p is NULL" ) );
   }
}

/*
 * CompositionMode compositionMode () const
 */
HB_FUNC( QT_QPAINTER_COMPOSITIONMODE )
{
   QPainter * p = hbqt_par_QPainter( 1 );
   if( p )
      hb_retni( ( QPainter::CompositionMode ) ( p )->compositionMode() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QPAINTER_COMPOSITIONMODE FP=hb_retni( ( QPainter::CompositionMode ) ( p )->compositionMode() ); p is NULL" ) );
   }
}

/*
 * QPaintDevice * device () const
 */
HB_FUNC( QT_QPAINTER_DEVICE )
{
   QPainter * p = hbqt_par_QPainter( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QPaintDevice( ( p )->device(), false ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QPAINTER_DEVICE FP=hb_retptrGC( hbqt_gcAllocate_QPaintDevice( ( p )->device(), false ) ); p is NULL" ) );
   }
}

/*
 * const QMatrix & deviceMatrix () const
 */
HB_FUNC( QT_QPAINTER_DEVICEMATRIX )
{
   QPainter * p = hbqt_par_QPainter( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QMatrix( new QMatrix( ( p )->deviceMatrix() ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QPAINTER_DEVICEMATRIX FP=hb_retptrGC( hbqt_gcAllocate_QMatrix( new QMatrix( ( p )->deviceMatrix() ), true ) ); p is NULL" ) );
   }
}

/*
 * const QTransform & deviceTransform () const
 */
HB_FUNC( QT_QPAINTER_DEVICETRANSFORM )
{
   QPainter * p = hbqt_par_QPainter( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QTransform( new QTransform( ( p )->deviceTransform() ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QPAINTER_DEVICETRANSFORM FP=hb_retptrGC( hbqt_gcAllocate_QTransform( new QTransform( ( p )->deviceTransform() ), true ) ); p is NULL" ) );
   }
}

/*
 * void drawArc ( const QRectF & rectangle, int startAngle, int spanAngle )
 */
HB_FUNC( QT_QPAINTER_DRAWARC )
{
   QPainter * p = hbqt_par_QPainter( 1 );
   if( p )
      ( p )->drawArc( *hbqt_par_QRectF( 2 ), hb_parni( 3 ), hb_parni( 4 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QPAINTER_DRAWARC FP=( p )->drawArc( *hbqt_par_QRectF( 2 ), hb_parni( 3 ), hb_parni( 4 ) ); p is NULL" ) );
   }
}

/*
 * void drawArc ( const QRect & rectangle, int startAngle, int spanAngle )
 */
HB_FUNC( QT_QPAINTER_DRAWARC_1 )
{
   QPainter * p = hbqt_par_QPainter( 1 );
   if( p )
      ( p )->drawArc( *hbqt_par_QRect( 2 ), hb_parni( 3 ), hb_parni( 4 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QPAINTER_DRAWARC_1 FP=( p )->drawArc( *hbqt_par_QRect( 2 ), hb_parni( 3 ), hb_parni( 4 ) ); p is NULL" ) );
   }
}

/*
 * void drawArc ( int x, int y, int width, int height, int startAngle, int spanAngle )
 */
HB_FUNC( QT_QPAINTER_DRAWARC_2 )
{
   QPainter * p = hbqt_par_QPainter( 1 );
   if( p )
      ( p )->drawArc( hb_parni( 2 ), hb_parni( 3 ), hb_parni( 4 ), hb_parni( 5 ), hb_parni( 6 ), hb_parni( 7 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QPAINTER_DRAWARC_2 FP=( p )->drawArc( hb_parni( 2 ), hb_parni( 3 ), hb_parni( 4 ), hb_parni( 5 ), hb_parni( 6 ), hb_parni( 7 ) ); p is NULL" ) );
   }
}

/*
 * void drawChord ( const QRectF & rectangle, int startAngle, int spanAngle )
 */
HB_FUNC( QT_QPAINTER_DRAWCHORD )
{
   QPainter * p = hbqt_par_QPainter( 1 );
   if( p )
      ( p )->drawChord( *hbqt_par_QRectF( 2 ), hb_parni( 3 ), hb_parni( 4 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QPAINTER_DRAWCHORD FP=( p )->drawChord( *hbqt_par_QRectF( 2 ), hb_parni( 3 ), hb_parni( 4 ) ); p is NULL" ) );
   }
}

/*
 * void drawChord ( const QRect & rectangle, int startAngle, int spanAngle )
 */
HB_FUNC( QT_QPAINTER_DRAWCHORD_1 )
{
   QPainter * p = hbqt_par_QPainter( 1 );
   if( p )
      ( p )->drawChord( *hbqt_par_QRect( 2 ), hb_parni( 3 ), hb_parni( 4 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QPAINTER_DRAWCHORD_1 FP=( p )->drawChord( *hbqt_par_QRect( 2 ), hb_parni( 3 ), hb_parni( 4 ) ); p is NULL" ) );
   }
}

/*
 * void drawChord ( int x, int y, int width, int height, int startAngle, int spanAngle )
 */
HB_FUNC( QT_QPAINTER_DRAWCHORD_2 )
{
   QPainter * p = hbqt_par_QPainter( 1 );
   if( p )
      ( p )->drawChord( hb_parni( 2 ), hb_parni( 3 ), hb_parni( 4 ), hb_parni( 5 ), hb_parni( 6 ), hb_parni( 7 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QPAINTER_DRAWCHORD_2 FP=( p )->drawChord( hb_parni( 2 ), hb_parni( 3 ), hb_parni( 4 ), hb_parni( 5 ), hb_parni( 6 ), hb_parni( 7 ) ); p is NULL" ) );
   }
}

/*
 * void drawConvexPolygon ( const QPointF * points, int pointCount )
 */
HB_FUNC( QT_QPAINTER_DRAWCONVEXPOLYGON )
{
   QPainter * p = hbqt_par_QPainter( 1 );
   if( p )
      ( p )->drawConvexPolygon( hbqt_par_QPointF( 2 ), hb_parni( 3 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QPAINTER_DRAWCONVEXPOLYGON FP=( p )->drawConvexPolygon( hbqt_par_QPointF( 2 ), hb_parni( 3 ) ); p is NULL" ) );
   }
}

/*
 * void drawConvexPolygon ( const QPoint * points, int pointCount )
 */
HB_FUNC( QT_QPAINTER_DRAWCONVEXPOLYGON_1 )
{
   QPainter * p = hbqt_par_QPainter( 1 );
   if( p )
      ( p )->drawConvexPolygon( hbqt_par_QPoint( 2 ), hb_parni( 3 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QPAINTER_DRAWCONVEXPOLYGON_1 FP=( p )->drawConvexPolygon( hbqt_par_QPoint( 2 ), hb_parni( 3 ) ); p is NULL" ) );
   }
}

/*
 * void drawConvexPolygon ( const QPolygonF & polygon )
 */
HB_FUNC( QT_QPAINTER_DRAWCONVEXPOLYGON_2 )
{
   QPainter * p = hbqt_par_QPainter( 1 );
   if( p )
      ( p )->drawConvexPolygon( *hbqt_par_QPolygonF( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QPAINTER_DRAWCONVEXPOLYGON_2 FP=( p )->drawConvexPolygon( *hbqt_par_QPolygonF( 2 ) ); p is NULL" ) );
   }
}

/*
 * void drawConvexPolygon ( const QPolygon & polygon )
 */
HB_FUNC( QT_QPAINTER_DRAWCONVEXPOLYGON_3 )
{
   QPainter * p = hbqt_par_QPainter( 1 );
   if( p )
      ( p )->drawConvexPolygon( *hbqt_par_QPolygon( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QPAINTER_DRAWCONVEXPOLYGON_3 FP=( p )->drawConvexPolygon( *hbqt_par_QPolygon( 2 ) ); p is NULL" ) );
   }
}

/*
 * void drawEllipse ( const QRectF & rectangle )
 */
HB_FUNC( QT_QPAINTER_DRAWELLIPSE )
{
   QPainter * p = hbqt_par_QPainter( 1 );
   if( p )
      ( p )->drawEllipse( *hbqt_par_QRectF( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QPAINTER_DRAWELLIPSE FP=( p )->drawEllipse( *hbqt_par_QRectF( 2 ) ); p is NULL" ) );
   }
}

/*
 * void drawEllipse ( const QRect & rectangle )
 */
HB_FUNC( QT_QPAINTER_DRAWELLIPSE_1 )
{
   QPainter * p = hbqt_par_QPainter( 1 );
   if( p )
      ( p )->drawEllipse( *hbqt_par_QRect( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QPAINTER_DRAWELLIPSE_1 FP=( p )->drawEllipse( *hbqt_par_QRect( 2 ) ); p is NULL" ) );
   }
}

/*
 * void drawEllipse ( int x, int y, int width, int height )
 */
HB_FUNC( QT_QPAINTER_DRAWELLIPSE_2 )
{
   QPainter * p = hbqt_par_QPainter( 1 );
   if( p )
      ( p )->drawEllipse( hb_parni( 2 ), hb_parni( 3 ), hb_parni( 4 ), hb_parni( 5 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QPAINTER_DRAWELLIPSE_2 FP=( p )->drawEllipse( hb_parni( 2 ), hb_parni( 3 ), hb_parni( 4 ), hb_parni( 5 ) ); p is NULL" ) );
   }
}

/*
 * void drawEllipse ( const QPointF & center, qreal rx, qreal ry )
 */
HB_FUNC( QT_QPAINTER_DRAWELLIPSE_3 )
{
   QPainter * p = hbqt_par_QPainter( 1 );
   if( p )
      ( p )->drawEllipse( *hbqt_par_QPointF( 2 ), hb_parnd( 3 ), hb_parnd( 4 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QPAINTER_DRAWELLIPSE_3 FP=( p )->drawEllipse( *hbqt_par_QPointF( 2 ), hb_parnd( 3 ), hb_parnd( 4 ) ); p is NULL" ) );
   }
}

/*
 * void drawEllipse ( const QPoint & center, int rx, int ry )
 */
HB_FUNC( QT_QPAINTER_DRAWELLIPSE_4 )
{
   QPainter * p = hbqt_par_QPainter( 1 );
   if( p )
      ( p )->drawEllipse( *hbqt_par_QPoint( 2 ), hb_parni( 3 ), hb_parni( 4 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QPAINTER_DRAWELLIPSE_4 FP=( p )->drawEllipse( *hbqt_par_QPoint( 2 ), hb_parni( 3 ), hb_parni( 4 ) ); p is NULL" ) );
   }
}

/*
 * void drawImage ( const QRectF & target, const QImage & image, const QRectF & source, Qt::ImageConversionFlags flags = Qt::AutoColor )
 */
HB_FUNC( QT_QPAINTER_DRAWIMAGE )
{
   QPainter * p = hbqt_par_QPainter( 1 );
   if( p )
      ( p )->drawImage( *hbqt_par_QRectF( 2 ), *hbqt_par_QImage( 3 ), *hbqt_par_QRectF( 4 ), ( HB_ISNUM( 5 ) ? ( Qt::ImageConversionFlags ) hb_parni( 5 ) : ( Qt::ImageConversionFlags ) Qt::AutoColor ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QPAINTER_DRAWIMAGE FP=( p )->drawImage( *hbqt_par_QRectF( 2 ), *hbqt_par_QImage( 3 ), *hbqt_par_QRectF( 4 ), ( HB_ISNUM( 5 ) ? ( Qt::ImageConversionFlags ) hb_parni( 5 ) : ( Qt::ImageConversionFlags ) Qt::AutoColor ) ); p is NULL" ) );
   }
}

/*
 * void drawImage ( const QRect & target, const QImage & image, const QRect & source, Qt::ImageConversionFlags flags = Qt::AutoColor )
 */
HB_FUNC( QT_QPAINTER_DRAWIMAGE_1 )
{
   QPainter * p = hbqt_par_QPainter( 1 );
   if( p )
      ( p )->drawImage( *hbqt_par_QRect( 2 ), *hbqt_par_QImage( 3 ), *hbqt_par_QRect( 4 ), ( HB_ISNUM( 5 ) ? ( Qt::ImageConversionFlags ) hb_parni( 5 ) : ( Qt::ImageConversionFlags ) Qt::AutoColor ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QPAINTER_DRAWIMAGE_1 FP=( p )->drawImage( *hbqt_par_QRect( 2 ), *hbqt_par_QImage( 3 ), *hbqt_par_QRect( 4 ), ( HB_ISNUM( 5 ) ? ( Qt::ImageConversionFlags ) hb_parni( 5 ) : ( Qt::ImageConversionFlags ) Qt::AutoColor ) ); p is NULL" ) );
   }
}

/*
 * void drawImage ( const QPointF & point, const QImage & image )
 */
HB_FUNC( QT_QPAINTER_DRAWIMAGE_2 )
{
   QPainter * p = hbqt_par_QPainter( 1 );
   if( p )
      ( p )->drawImage( *hbqt_par_QPointF( 2 ), *hbqt_par_QImage( 3 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QPAINTER_DRAWIMAGE_2 FP=( p )->drawImage( *hbqt_par_QPointF( 2 ), *hbqt_par_QImage( 3 ) ); p is NULL" ) );
   }
}

/*
 * void drawImage ( const QPoint & point, const QImage & image )
 */
HB_FUNC( QT_QPAINTER_DRAWIMAGE_3 )
{
   QPainter * p = hbqt_par_QPainter( 1 );
   if( p )
      ( p )->drawImage( *hbqt_par_QPoint( 2 ), *hbqt_par_QImage( 3 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QPAINTER_DRAWIMAGE_3 FP=( p )->drawImage( *hbqt_par_QPoint( 2 ), *hbqt_par_QImage( 3 ) ); p is NULL" ) );
   }
}

/*
 * void drawImage ( const QPointF & point, const QImage & image, const QRectF & source, Qt::ImageConversionFlags flags = Qt::AutoColor )
 */
HB_FUNC( QT_QPAINTER_DRAWIMAGE_4 )
{
   QPainter * p = hbqt_par_QPainter( 1 );
   if( p )
      ( p )->drawImage( *hbqt_par_QPointF( 2 ), *hbqt_par_QImage( 3 ), *hbqt_par_QRectF( 4 ), ( HB_ISNUM( 5 ) ? ( Qt::ImageConversionFlags ) hb_parni( 5 ) : ( Qt::ImageConversionFlags ) Qt::AutoColor ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QPAINTER_DRAWIMAGE_4 FP=( p )->drawImage( *hbqt_par_QPointF( 2 ), *hbqt_par_QImage( 3 ), *hbqt_par_QRectF( 4 ), ( HB_ISNUM( 5 ) ? ( Qt::ImageConversionFlags ) hb_parni( 5 ) : ( Qt::ImageConversionFlags ) Qt::AutoColor ) ); p is NULL" ) );
   }
}

/*
 * void drawImage ( const QPoint & point, const QImage & image, const QRect & source, Qt::ImageConversionFlags flags = Qt::AutoColor )
 */
HB_FUNC( QT_QPAINTER_DRAWIMAGE_5 )
{
   QPainter * p = hbqt_par_QPainter( 1 );
   if( p )
      ( p )->drawImage( *hbqt_par_QPoint( 2 ), *hbqt_par_QImage( 3 ), *hbqt_par_QRect( 4 ), ( HB_ISNUM( 5 ) ? ( Qt::ImageConversionFlags ) hb_parni( 5 ) : ( Qt::ImageConversionFlags ) Qt::AutoColor ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QPAINTER_DRAWIMAGE_5 FP=( p )->drawImage( *hbqt_par_QPoint( 2 ), *hbqt_par_QImage( 3 ), *hbqt_par_QRect( 4 ), ( HB_ISNUM( 5 ) ? ( Qt::ImageConversionFlags ) hb_parni( 5 ) : ( Qt::ImageConversionFlags ) Qt::AutoColor ) ); p is NULL" ) );
   }
}

/*
 * void drawImage ( const QRectF & rectangle, const QImage & image )
 */
HB_FUNC( QT_QPAINTER_DRAWIMAGE_6 )
{
   QPainter * p = hbqt_par_QPainter( 1 );
   if( p )
      ( p )->drawImage( *hbqt_par_QRectF( 2 ), *hbqt_par_QImage( 3 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QPAINTER_DRAWIMAGE_6 FP=( p )->drawImage( *hbqt_par_QRectF( 2 ), *hbqt_par_QImage( 3 ) ); p is NULL" ) );
   }
}

/*
 * void drawImage ( const QRect & rectangle, const QImage & image )
 */
HB_FUNC( QT_QPAINTER_DRAWIMAGE_7 )
{
   QPainter * p = hbqt_par_QPainter( 1 );
   if( p )
      ( p )->drawImage( *hbqt_par_QRect( 2 ), *hbqt_par_QImage( 3 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QPAINTER_DRAWIMAGE_7 FP=( p )->drawImage( *hbqt_par_QRect( 2 ), *hbqt_par_QImage( 3 ) ); p is NULL" ) );
   }
}

/*
 * void drawImage ( int x, int y, const QImage & image, int sx = 0, int sy = 0, int sw = -1, int sh = -1, Qt::ImageConversionFlags flags = Qt::AutoColor )
 */
HB_FUNC( QT_QPAINTER_DRAWIMAGE_8 )
{
   QPainter * p = hbqt_par_QPainter( 1 );
   if( p )
      ( p )->drawImage( hb_parni( 2 ), hb_parni( 3 ), *hbqt_par_QImage( 4 ), hb_parni( 5 ), hb_parni( 6 ), hb_parnidef( 7, -1 ), hb_parnidef( 8, -1 ), ( HB_ISNUM( 9 ) ? ( Qt::ImageConversionFlags ) hb_parni( 9 ) : ( Qt::ImageConversionFlags ) Qt::AutoColor ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QPAINTER_DRAWIMAGE_8 FP=( p )->drawImage( hb_parni( 2 ), hb_parni( 3 ), *hbqt_par_QImage( 4 ), hb_parni( 5 ), hb_parni( 6 ), hb_parnidef( 7, -1 ), hb_parnidef( 8, -1 ), ( HB_ISNUM( 9 ) ? ( Qt::ImageConversionFlags ) hb_parni( 9 ) : ( Qt::ImageConversionFlags ) Qt::AutoColor ) ); p is NULL" ) );
   }
}

/*
 * void drawLine ( const QLineF & line )
 */
HB_FUNC( QT_QPAINTER_DRAWLINE )
{
   QPainter * p = hbqt_par_QPainter( 1 );
   if( p )
      ( p )->drawLine( *hbqt_par_QLineF( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QPAINTER_DRAWLINE FP=( p )->drawLine( *hbqt_par_QLineF( 2 ) ); p is NULL" ) );
   }
}

/*
 * void drawLine ( const QLine & line )
 */
HB_FUNC( QT_QPAINTER_DRAWLINE_1 )
{
   QPainter * p = hbqt_par_QPainter( 1 );
   if( p )
      ( p )->drawLine( *hbqt_par_QLine( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QPAINTER_DRAWLINE_1 FP=( p )->drawLine( *hbqt_par_QLine( 2 ) ); p is NULL" ) );
   }
}

/*
 * void drawLine ( const QPoint & p1, const QPoint & p2 )
 */
HB_FUNC( QT_QPAINTER_DRAWLINE_2 )
{
   QPainter * p = hbqt_par_QPainter( 1 );
   if( p )
      ( p )->drawLine( *hbqt_par_QPoint( 2 ), *hbqt_par_QPoint( 3 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QPAINTER_DRAWLINE_2 FP=( p )->drawLine( *hbqt_par_QPoint( 2 ), *hbqt_par_QPoint( 3 ) ); p is NULL" ) );
   }
}

/*
 * void drawLine ( const QPointF & p1, const QPointF & p2 )
 */
HB_FUNC( QT_QPAINTER_DRAWLINE_3 )
{
   QPainter * p = hbqt_par_QPainter( 1 );
   if( p )
      ( p )->drawLine( *hbqt_par_QPointF( 2 ), *hbqt_par_QPointF( 3 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QPAINTER_DRAWLINE_3 FP=( p )->drawLine( *hbqt_par_QPointF( 2 ), *hbqt_par_QPointF( 3 ) ); p is NULL" ) );
   }
}

/*
 * void drawLine ( int x1, int y1, int x2, int y2 )
 */
HB_FUNC( QT_QPAINTER_DRAWLINE_4 )
{
   QPainter * p = hbqt_par_QPainter( 1 );
   if( p )
      ( p )->drawLine( hb_parni( 2 ), hb_parni( 3 ), hb_parni( 4 ), hb_parni( 5 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QPAINTER_DRAWLINE_4 FP=( p )->drawLine( hb_parni( 2 ), hb_parni( 3 ), hb_parni( 4 ), hb_parni( 5 ) ); p is NULL" ) );
   }
}

/*
 * void drawLines ( const QLineF * lines, int lineCount )
 */
HB_FUNC( QT_QPAINTER_DRAWLINES )
{
   QPainter * p = hbqt_par_QPainter( 1 );
   if( p )
      ( p )->drawLines( hbqt_par_QLineF( 2 ), hb_parni( 3 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QPAINTER_DRAWLINES FP=( p )->drawLines( hbqt_par_QLineF( 2 ), hb_parni( 3 ) ); p is NULL" ) );
   }
}

/*
 * void drawLines ( const QLine * lines, int lineCount )
 */
HB_FUNC( QT_QPAINTER_DRAWLINES_1 )
{
   QPainter * p = hbqt_par_QPainter( 1 );
   if( p )
      ( p )->drawLines( hbqt_par_QLine( 2 ), hb_parni( 3 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QPAINTER_DRAWLINES_1 FP=( p )->drawLines( hbqt_par_QLine( 2 ), hb_parni( 3 ) ); p is NULL" ) );
   }
}

/*
 * void drawLines ( const QPointF * pointPairs, int lineCount )
 */
HB_FUNC( QT_QPAINTER_DRAWLINES_2 )
{
   QPainter * p = hbqt_par_QPainter( 1 );
   if( p )
      ( p )->drawLines( hbqt_par_QPointF( 2 ), hb_parni( 3 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QPAINTER_DRAWLINES_2 FP=( p )->drawLines( hbqt_par_QPointF( 2 ), hb_parni( 3 ) ); p is NULL" ) );
   }
}

/*
 * void drawLines ( const QPoint * pointPairs, int lineCount )
 */
HB_FUNC( QT_QPAINTER_DRAWLINES_3 )
{
   QPainter * p = hbqt_par_QPainter( 1 );
   if( p )
      ( p )->drawLines( hbqt_par_QPoint( 2 ), hb_parni( 3 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QPAINTER_DRAWLINES_3 FP=( p )->drawLines( hbqt_par_QPoint( 2 ), hb_parni( 3 ) ); p is NULL" ) );
   }
}

/*
 * void drawPath ( const QPainterPath & path )
 */
HB_FUNC( QT_QPAINTER_DRAWPATH )
{
   QPainter * p = hbqt_par_QPainter( 1 );
   if( p )
      ( p )->drawPath( *hbqt_par_QPainterPath( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QPAINTER_DRAWPATH FP=( p )->drawPath( *hbqt_par_QPainterPath( 2 ) ); p is NULL" ) );
   }
}

/*
 * void drawPicture ( const QPointF & point, const QPicture & picture )
 */
HB_FUNC( QT_QPAINTER_DRAWPICTURE )
{
   QPainter * p = hbqt_par_QPainter( 1 );
   if( p )
      ( p )->drawPicture( *hbqt_par_QPointF( 2 ), *hbqt_par_QPicture( 3 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QPAINTER_DRAWPICTURE FP=( p )->drawPicture( *hbqt_par_QPointF( 2 ), *hbqt_par_QPicture( 3 ) ); p is NULL" ) );
   }
}

/*
 * void drawPicture ( const QPoint & point, const QPicture & picture )
 */
HB_FUNC( QT_QPAINTER_DRAWPICTURE_1 )
{
   QPainter * p = hbqt_par_QPainter( 1 );
   if( p )
      ( p )->drawPicture( *hbqt_par_QPoint( 2 ), *hbqt_par_QPicture( 3 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QPAINTER_DRAWPICTURE_1 FP=( p )->drawPicture( *hbqt_par_QPoint( 2 ), *hbqt_par_QPicture( 3 ) ); p is NULL" ) );
   }
}

/*
 * void drawPicture ( int x, int y, const QPicture & picture )
 */
HB_FUNC( QT_QPAINTER_DRAWPICTURE_2 )
{
   QPainter * p = hbqt_par_QPainter( 1 );
   if( p )
      ( p )->drawPicture( hb_parni( 2 ), hb_parni( 3 ), *hbqt_par_QPicture( 4 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QPAINTER_DRAWPICTURE_2 FP=( p )->drawPicture( hb_parni( 2 ), hb_parni( 3 ), *hbqt_par_QPicture( 4 ) ); p is NULL" ) );
   }
}

/*
 * void drawPie ( const QRectF & rectangle, int startAngle, int spanAngle )
 */
HB_FUNC( QT_QPAINTER_DRAWPIE )
{
   QPainter * p = hbqt_par_QPainter( 1 );
   if( p )
      ( p )->drawPie( *hbqt_par_QRectF( 2 ), hb_parni( 3 ), hb_parni( 4 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QPAINTER_DRAWPIE FP=( p )->drawPie( *hbqt_par_QRectF( 2 ), hb_parni( 3 ), hb_parni( 4 ) ); p is NULL" ) );
   }
}

/*
 * void drawPie ( const QRect & rectangle, int startAngle, int spanAngle )
 */
HB_FUNC( QT_QPAINTER_DRAWPIE_1 )
{
   QPainter * p = hbqt_par_QPainter( 1 );
   if( p )
      ( p )->drawPie( *hbqt_par_QRect( 2 ), hb_parni( 3 ), hb_parni( 4 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QPAINTER_DRAWPIE_1 FP=( p )->drawPie( *hbqt_par_QRect( 2 ), hb_parni( 3 ), hb_parni( 4 ) ); p is NULL" ) );
   }
}

/*
 * void drawPie ( int x, int y, int width, int height, int startAngle, int spanAngle )
 */
HB_FUNC( QT_QPAINTER_DRAWPIE_2 )
{
   QPainter * p = hbqt_par_QPainter( 1 );
   if( p )
      ( p )->drawPie( hb_parni( 2 ), hb_parni( 3 ), hb_parni( 4 ), hb_parni( 5 ), hb_parni( 6 ), hb_parni( 7 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QPAINTER_DRAWPIE_2 FP=( p )->drawPie( hb_parni( 2 ), hb_parni( 3 ), hb_parni( 4 ), hb_parni( 5 ), hb_parni( 6 ), hb_parni( 7 ) ); p is NULL" ) );
   }
}

/*
 * void drawPixmap ( const QRectF & target, const QPixmap & pixmap, const QRectF & source )
 */
HB_FUNC( QT_QPAINTER_DRAWPIXMAP )
{
   QPainter * p = hbqt_par_QPainter( 1 );
   if( p )
      ( p )->drawPixmap( *hbqt_par_QRectF( 2 ), *hbqt_par_QPixmap( 3 ), *hbqt_par_QRectF( 4 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QPAINTER_DRAWPIXMAP FP=( p )->drawPixmap( *hbqt_par_QRectF( 2 ), *hbqt_par_QPixmap( 3 ), *hbqt_par_QRectF( 4 ) ); p is NULL" ) );
   }
}

/*
 * void drawPixmap ( const QRect & target, const QPixmap & pixmap, const QRect & source )
 */
HB_FUNC( QT_QPAINTER_DRAWPIXMAP_1 )
{
   QPainter * p = hbqt_par_QPainter( 1 );
   if( p )
      ( p )->drawPixmap( *hbqt_par_QRect( 2 ), *hbqt_par_QPixmap( 3 ), *hbqt_par_QRect( 4 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QPAINTER_DRAWPIXMAP_1 FP=( p )->drawPixmap( *hbqt_par_QRect( 2 ), *hbqt_par_QPixmap( 3 ), *hbqt_par_QRect( 4 ) ); p is NULL" ) );
   }
}

/*
 * void drawPixmap ( const QPointF & point, const QPixmap & pixmap, const QRectF & source )
 */
HB_FUNC( QT_QPAINTER_DRAWPIXMAP_2 )
{
   QPainter * p = hbqt_par_QPainter( 1 );
   if( p )
      ( p )->drawPixmap( *hbqt_par_QPointF( 2 ), *hbqt_par_QPixmap( 3 ), *hbqt_par_QRectF( 4 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QPAINTER_DRAWPIXMAP_2 FP=( p )->drawPixmap( *hbqt_par_QPointF( 2 ), *hbqt_par_QPixmap( 3 ), *hbqt_par_QRectF( 4 ) ); p is NULL" ) );
   }
}

/*
 * void drawPixmap ( const QPoint & point, const QPixmap & pixmap, const QRect & source )
 */
HB_FUNC( QT_QPAINTER_DRAWPIXMAP_3 )
{
   QPainter * p = hbqt_par_QPainter( 1 );
   if( p )
      ( p )->drawPixmap( *hbqt_par_QPoint( 2 ), *hbqt_par_QPixmap( 3 ), *hbqt_par_QRect( 4 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QPAINTER_DRAWPIXMAP_3 FP=( p )->drawPixmap( *hbqt_par_QPoint( 2 ), *hbqt_par_QPixmap( 3 ), *hbqt_par_QRect( 4 ) ); p is NULL" ) );
   }
}

/*
 * void drawPixmap ( const QPointF & point, const QPixmap & pixmap )
 */
HB_FUNC( QT_QPAINTER_DRAWPIXMAP_4 )
{
   QPainter * p = hbqt_par_QPainter( 1 );
   if( p )
      ( p )->drawPixmap( *hbqt_par_QPointF( 2 ), *hbqt_par_QPixmap( 3 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QPAINTER_DRAWPIXMAP_4 FP=( p )->drawPixmap( *hbqt_par_QPointF( 2 ), *hbqt_par_QPixmap( 3 ) ); p is NULL" ) );
   }
}

/*
 * void drawPixmap ( const QPoint & point, const QPixmap & pixmap )
 */
HB_FUNC( QT_QPAINTER_DRAWPIXMAP_5 )
{
   QPainter * p = hbqt_par_QPainter( 1 );
   if( p )
      ( p )->drawPixmap( *hbqt_par_QPoint( 2 ), *hbqt_par_QPixmap( 3 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QPAINTER_DRAWPIXMAP_5 FP=( p )->drawPixmap( *hbqt_par_QPoint( 2 ), *hbqt_par_QPixmap( 3 ) ); p is NULL" ) );
   }
}

/*
 * void drawPixmap ( int x, int y, const QPixmap & pixmap )
 */
HB_FUNC( QT_QPAINTER_DRAWPIXMAP_6 )
{
   QPainter * p = hbqt_par_QPainter( 1 );
   if( p )
      ( p )->drawPixmap( hb_parni( 2 ), hb_parni( 3 ), *hbqt_par_QPixmap( 4 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QPAINTER_DRAWPIXMAP_6 FP=( p )->drawPixmap( hb_parni( 2 ), hb_parni( 3 ), *hbqt_par_QPixmap( 4 ) ); p is NULL" ) );
   }
}

/*
 * void drawPixmap ( const QRect & rectangle, const QPixmap & pixmap )
 */
HB_FUNC( QT_QPAINTER_DRAWPIXMAP_7 )
{
   QPainter * p = hbqt_par_QPainter( 1 );
   if( p )
      ( p )->drawPixmap( *hbqt_par_QRect( 2 ), *hbqt_par_QPixmap( 3 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QPAINTER_DRAWPIXMAP_7 FP=( p )->drawPixmap( *hbqt_par_QRect( 2 ), *hbqt_par_QPixmap( 3 ) ); p is NULL" ) );
   }
}

/*
 * void drawPixmap ( int x, int y, int width, int height, const QPixmap & pixmap )
 */
HB_FUNC( QT_QPAINTER_DRAWPIXMAP_8 )
{
   QPainter * p = hbqt_par_QPainter( 1 );
   if( p )
      ( p )->drawPixmap( hb_parni( 2 ), hb_parni( 3 ), hb_parni( 4 ), hb_parni( 5 ), *hbqt_par_QPixmap( 6 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QPAINTER_DRAWPIXMAP_8 FP=( p )->drawPixmap( hb_parni( 2 ), hb_parni( 3 ), hb_parni( 4 ), hb_parni( 5 ), *hbqt_par_QPixmap( 6 ) ); p is NULL" ) );
   }
}

/*
 * void drawPixmap ( int x, int y, int w, int h, const QPixmap & pixmap, int sx, int sy, int sw, int sh )
 */
HB_FUNC( QT_QPAINTER_DRAWPIXMAP_9 )
{
   QPainter * p = hbqt_par_QPainter( 1 );
   if( p )
      ( p )->drawPixmap( hb_parni( 2 ), hb_parni( 3 ), hb_parni( 4 ), hb_parni( 5 ), *hbqt_par_QPixmap( 6 ), hb_parni( 7 ), hb_parni( 8 ), hb_parni( 9 ), hb_parni( 10 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QPAINTER_DRAWPIXMAP_9 FP=( p )->drawPixmap( hb_parni( 2 ), hb_parni( 3 ), hb_parni( 4 ), hb_parni( 5 ), *hbqt_par_QPixmap( 6 ), hb_parni( 7 ), hb_parni( 8 ), hb_parni( 9 ), hb_parni( 10 ) ); p is NULL" ) );
   }
}

/*
 * void drawPixmap ( int x, int y, const QPixmap & pixmap, int sx, int sy, int sw, int sh )
 */
HB_FUNC( QT_QPAINTER_DRAWPIXMAP_10 )
{
   QPainter * p = hbqt_par_QPainter( 1 );
   if( p )
      ( p )->drawPixmap( hb_parni( 2 ), hb_parni( 3 ), *hbqt_par_QPixmap( 4 ), hb_parni( 5 ), hb_parni( 6 ), hb_parni( 7 ), hb_parni( 8 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QPAINTER_DRAWPIXMAP_10 FP=( p )->drawPixmap( hb_parni( 2 ), hb_parni( 3 ), *hbqt_par_QPixmap( 4 ), hb_parni( 5 ), hb_parni( 6 ), hb_parni( 7 ), hb_parni( 8 ) ); p is NULL" ) );
   }
}

/*
 * void drawPoint ( const QPointF & position )
 */
HB_FUNC( QT_QPAINTER_DRAWPOINT )
{
   QPainter * p = hbqt_par_QPainter( 1 );
   if( p )
      ( p )->drawPoint( *hbqt_par_QPointF( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QPAINTER_DRAWPOINT FP=( p )->drawPoint( *hbqt_par_QPointF( 2 ) ); p is NULL" ) );
   }
}

/*
 * void drawPoint ( const QPoint & position )
 */
HB_FUNC( QT_QPAINTER_DRAWPOINT_1 )
{
   QPainter * p = hbqt_par_QPainter( 1 );
   if( p )
      ( p )->drawPoint( *hbqt_par_QPoint( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QPAINTER_DRAWPOINT_1 FP=( p )->drawPoint( *hbqt_par_QPoint( 2 ) ); p is NULL" ) );
   }
}

/*
 * void drawPoint ( int x, int y )
 */
HB_FUNC( QT_QPAINTER_DRAWPOINT_2 )
{
   QPainter * p = hbqt_par_QPainter( 1 );
   if( p )
      ( p )->drawPoint( hb_parni( 2 ), hb_parni( 3 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QPAINTER_DRAWPOINT_2 FP=( p )->drawPoint( hb_parni( 2 ), hb_parni( 3 ) ); p is NULL" ) );
   }
}

/*
 * void drawPoints ( const QPointF * points, int pointCount )
 */
HB_FUNC( QT_QPAINTER_DRAWPOINTS )
{
   QPainter * p = hbqt_par_QPainter( 1 );
   if( p )
      ( p )->drawPoints( hbqt_par_QPointF( 2 ), hb_parni( 3 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QPAINTER_DRAWPOINTS FP=( p )->drawPoints( hbqt_par_QPointF( 2 ), hb_parni( 3 ) ); p is NULL" ) );
   }
}

/*
 * void drawPoints ( const QPoint * points, int pointCount )
 */
HB_FUNC( QT_QPAINTER_DRAWPOINTS_1 )
{
   QPainter * p = hbqt_par_QPainter( 1 );
   if( p )
      ( p )->drawPoints( hbqt_par_QPoint( 2 ), hb_parni( 3 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QPAINTER_DRAWPOINTS_1 FP=( p )->drawPoints( hbqt_par_QPoint( 2 ), hb_parni( 3 ) ); p is NULL" ) );
   }
}

/*
 * void drawPoints ( const QPolygonF & points )
 */
HB_FUNC( QT_QPAINTER_DRAWPOINTS_2 )
{
   QPainter * p = hbqt_par_QPainter( 1 );
   if( p )
      ( p )->drawPoints( *hbqt_par_QPolygonF( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QPAINTER_DRAWPOINTS_2 FP=( p )->drawPoints( *hbqt_par_QPolygonF( 2 ) ); p is NULL" ) );
   }
}

/*
 * void drawPoints ( const QPolygon & points )
 */
HB_FUNC( QT_QPAINTER_DRAWPOINTS_3 )
{
   QPainter * p = hbqt_par_QPainter( 1 );
   if( p )
      ( p )->drawPoints( *hbqt_par_QPolygon( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QPAINTER_DRAWPOINTS_3 FP=( p )->drawPoints( *hbqt_par_QPolygon( 2 ) ); p is NULL" ) );
   }
}

/*
 * void drawPolygon ( const QPointF * points, int pointCount, Qt::FillRule fillRule = Qt::OddEvenFill )
 */
HB_FUNC( QT_QPAINTER_DRAWPOLYGON )
{
   QPainter * p = hbqt_par_QPainter( 1 );
   if( p )
      ( p )->drawPolygon( hbqt_par_QPointF( 2 ), hb_parni( 3 ), ( HB_ISNUM( 4 ) ? ( Qt::FillRule ) hb_parni( 4 ) : ( Qt::FillRule ) Qt::OddEvenFill ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QPAINTER_DRAWPOLYGON FP=( p )->drawPolygon( hbqt_par_QPointF( 2 ), hb_parni( 3 ), ( HB_ISNUM( 4 ) ? ( Qt::FillRule ) hb_parni( 4 ) : ( Qt::FillRule ) Qt::OddEvenFill ) ); p is NULL" ) );
   }
}

/*
 * void drawPolygon ( const QPoint * points, int pointCount, Qt::FillRule fillRule = Qt::OddEvenFill )
 */
HB_FUNC( QT_QPAINTER_DRAWPOLYGON_1 )
{
   QPainter * p = hbqt_par_QPainter( 1 );
   if( p )
      ( p )->drawPolygon( hbqt_par_QPoint( 2 ), hb_parni( 3 ), ( HB_ISNUM( 4 ) ? ( Qt::FillRule ) hb_parni( 4 ) : ( Qt::FillRule ) Qt::OddEvenFill ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QPAINTER_DRAWPOLYGON_1 FP=( p )->drawPolygon( hbqt_par_QPoint( 2 ), hb_parni( 3 ), ( HB_ISNUM( 4 ) ? ( Qt::FillRule ) hb_parni( 4 ) : ( Qt::FillRule ) Qt::OddEvenFill ) ); p is NULL" ) );
   }
}

/*
 * void drawPolygon ( const QPolygonF & points, Qt::FillRule fillRule = Qt::OddEvenFill )
 */
HB_FUNC( QT_QPAINTER_DRAWPOLYGON_2 )
{
   QPainter * p = hbqt_par_QPainter( 1 );
   if( p )
      ( p )->drawPolygon( *hbqt_par_QPolygonF( 2 ), ( HB_ISNUM( 3 ) ? ( Qt::FillRule ) hb_parni( 3 ) : ( Qt::FillRule ) Qt::OddEvenFill ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QPAINTER_DRAWPOLYGON_2 FP=( p )->drawPolygon( *hbqt_par_QPolygonF( 2 ), ( HB_ISNUM( 3 ) ? ( Qt::FillRule ) hb_parni( 3 ) : ( Qt::FillRule ) Qt::OddEvenFill ) ); p is NULL" ) );
   }
}

/*
 * void drawPolygon ( const QPolygon & points, Qt::FillRule fillRule = Qt::OddEvenFill )
 */
HB_FUNC( QT_QPAINTER_DRAWPOLYGON_3 )
{
   QPainter * p = hbqt_par_QPainter( 1 );
   if( p )
      ( p )->drawPolygon( *hbqt_par_QPolygon( 2 ), ( HB_ISNUM( 3 ) ? ( Qt::FillRule ) hb_parni( 3 ) : ( Qt::FillRule ) Qt::OddEvenFill ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QPAINTER_DRAWPOLYGON_3 FP=( p )->drawPolygon( *hbqt_par_QPolygon( 2 ), ( HB_ISNUM( 3 ) ? ( Qt::FillRule ) hb_parni( 3 ) : ( Qt::FillRule ) Qt::OddEvenFill ) ); p is NULL" ) );
   }
}

/*
 * void drawPolyline ( const QPointF * points, int pointCount )
 */
HB_FUNC( QT_QPAINTER_DRAWPOLYLINE )
{
   QPainter * p = hbqt_par_QPainter( 1 );
   if( p )
      ( p )->drawPolyline( hbqt_par_QPointF( 2 ), hb_parni( 3 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QPAINTER_DRAWPOLYLINE FP=( p )->drawPolyline( hbqt_par_QPointF( 2 ), hb_parni( 3 ) ); p is NULL" ) );
   }
}

/*
 * void drawPolyline ( const QPoint * points, int pointCount )
 */
HB_FUNC( QT_QPAINTER_DRAWPOLYLINE_1 )
{
   QPainter * p = hbqt_par_QPainter( 1 );
   if( p )
      ( p )->drawPolyline( hbqt_par_QPoint( 2 ), hb_parni( 3 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QPAINTER_DRAWPOLYLINE_1 FP=( p )->drawPolyline( hbqt_par_QPoint( 2 ), hb_parni( 3 ) ); p is NULL" ) );
   }
}

/*
 * void drawPolyline ( const QPolygonF & points )
 */
HB_FUNC( QT_QPAINTER_DRAWPOLYLINE_2 )
{
   QPainter * p = hbqt_par_QPainter( 1 );
   if( p )
      ( p )->drawPolyline( *hbqt_par_QPolygonF( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QPAINTER_DRAWPOLYLINE_2 FP=( p )->drawPolyline( *hbqt_par_QPolygonF( 2 ) ); p is NULL" ) );
   }
}

/*
 * void drawPolyline ( const QPolygon & points )
 */
HB_FUNC( QT_QPAINTER_DRAWPOLYLINE_3 )
{
   QPainter * p = hbqt_par_QPainter( 1 );
   if( p )
      ( p )->drawPolyline( *hbqt_par_QPolygon( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QPAINTER_DRAWPOLYLINE_3 FP=( p )->drawPolyline( *hbqt_par_QPolygon( 2 ) ); p is NULL" ) );
   }
}

/*
 * void drawRect ( const QRectF & rectangle )
 */
HB_FUNC( QT_QPAINTER_DRAWRECT )
{
   QPainter * p = hbqt_par_QPainter( 1 );
   if( p )
      ( p )->drawRect( *hbqt_par_QRectF( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QPAINTER_DRAWRECT FP=( p )->drawRect( *hbqt_par_QRectF( 2 ) ); p is NULL" ) );
   }
}

/*
 * void drawRect ( const QRect & rectangle )
 */
HB_FUNC( QT_QPAINTER_DRAWRECT_1 )
{
   QPainter * p = hbqt_par_QPainter( 1 );
   if( p )
      ( p )->drawRect( *hbqt_par_QRect( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QPAINTER_DRAWRECT_1 FP=( p )->drawRect( *hbqt_par_QRect( 2 ) ); p is NULL" ) );
   }
}

/*
 * void drawRect ( int x, int y, int width, int height )
 */
HB_FUNC( QT_QPAINTER_DRAWRECT_2 )
{
   QPainter * p = hbqt_par_QPainter( 1 );
   if( p )
      ( p )->drawRect( hb_parni( 2 ), hb_parni( 3 ), hb_parni( 4 ), hb_parni( 5 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QPAINTER_DRAWRECT_2 FP=( p )->drawRect( hb_parni( 2 ), hb_parni( 3 ), hb_parni( 4 ), hb_parni( 5 ) ); p is NULL" ) );
   }
}

/*
 * void drawRects ( const QRectF * rectangles, int rectCount )
 */
HB_FUNC( QT_QPAINTER_DRAWRECTS )
{
   QPainter * p = hbqt_par_QPainter( 1 );
   if( p )
      ( p )->drawRects( hbqt_par_QRectF( 2 ), hb_parni( 3 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QPAINTER_DRAWRECTS FP=( p )->drawRects( hbqt_par_QRectF( 2 ), hb_parni( 3 ) ); p is NULL" ) );
   }
}

/*
 * void drawRects ( const QRect * rectangles, int rectCount )
 */
HB_FUNC( QT_QPAINTER_DRAWRECTS_1 )
{
   QPainter * p = hbqt_par_QPainter( 1 );
   if( p )
      ( p )->drawRects( hbqt_par_QRect( 2 ), hb_parni( 3 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QPAINTER_DRAWRECTS_1 FP=( p )->drawRects( hbqt_par_QRect( 2 ), hb_parni( 3 ) ); p is NULL" ) );
   }
}

/*
 * void drawRoundedRect ( const QRectF & rect, qreal xRadius, qreal yRadius, Qt::SizeMode mode = Qt::AbsoluteSize )
 */
HB_FUNC( QT_QPAINTER_DRAWROUNDEDRECT )
{
   QPainter * p = hbqt_par_QPainter( 1 );
   if( p )
      ( p )->drawRoundedRect( *hbqt_par_QRectF( 2 ), hb_parnd( 3 ), hb_parnd( 4 ), ( HB_ISNUM( 5 ) ? ( Qt::SizeMode ) hb_parni( 5 ) : ( Qt::SizeMode ) Qt::AbsoluteSize ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QPAINTER_DRAWROUNDEDRECT FP=( p )->drawRoundedRect( *hbqt_par_QRectF( 2 ), hb_parnd( 3 ), hb_parnd( 4 ), ( HB_ISNUM( 5 ) ? ( Qt::SizeMode ) hb_parni( 5 ) : ( Qt::SizeMode ) Qt::AbsoluteSize ) ); p is NULL" ) );
   }
}

/*
 * void drawRoundedRect ( const QRect & rect, qreal xRadius, qreal yRadius, Qt::SizeMode mode = Qt::AbsoluteSize )
 */
HB_FUNC( QT_QPAINTER_DRAWROUNDEDRECT_1 )
{
   QPainter * p = hbqt_par_QPainter( 1 );
   if( p )
      ( p )->drawRoundedRect( *hbqt_par_QRect( 2 ), hb_parnd( 3 ), hb_parnd( 4 ), ( HB_ISNUM( 5 ) ? ( Qt::SizeMode ) hb_parni( 5 ) : ( Qt::SizeMode ) Qt::AbsoluteSize ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QPAINTER_DRAWROUNDEDRECT_1 FP=( p )->drawRoundedRect( *hbqt_par_QRect( 2 ), hb_parnd( 3 ), hb_parnd( 4 ), ( HB_ISNUM( 5 ) ? ( Qt::SizeMode ) hb_parni( 5 ) : ( Qt::SizeMode ) Qt::AbsoluteSize ) ); p is NULL" ) );
   }
}

/*
 * void drawRoundedRect ( int x, int y, int w, int h, qreal xRadius, qreal yRadius, Qt::SizeMode mode = Qt::AbsoluteSize )
 */
HB_FUNC( QT_QPAINTER_DRAWROUNDEDRECT_2 )
{
   QPainter * p = hbqt_par_QPainter( 1 );
   if( p )
      ( p )->drawRoundedRect( hb_parni( 2 ), hb_parni( 3 ), hb_parni( 4 ), hb_parni( 5 ), hb_parnd( 6 ), hb_parnd( 7 ), ( HB_ISNUM( 8 ) ? ( Qt::SizeMode ) hb_parni( 8 ) : ( Qt::SizeMode ) Qt::AbsoluteSize ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QPAINTER_DRAWROUNDEDRECT_2 FP=( p )->drawRoundedRect( hb_parni( 2 ), hb_parni( 3 ), hb_parni( 4 ), hb_parni( 5 ), hb_parnd( 6 ), hb_parnd( 7 ), ( HB_ISNUM( 8 ) ? ( Qt::SizeMode ) hb_parni( 8 ) : ( Qt::SizeMode ) Qt::AbsoluteSize ) ); p is NULL" ) );
   }
}

/*
 * void drawText ( const QPointF & position, const QString & text )
 */
HB_FUNC( QT_QPAINTER_DRAWTEXT )
{
   QPainter * p = hbqt_par_QPainter( 1 );
   if( p )
      ( p )->drawText( *hbqt_par_QPointF( 2 ), hbqt_par_QString( 3 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QPAINTER_DRAWTEXT FP=( p )->drawText( *hbqt_par_QPointF( 2 ), hbqt_par_QString( 3 ) ); p is NULL" ) );
   }
}

/*
 * void drawText ( const QPoint & position, const QString & text )
 */
HB_FUNC( QT_QPAINTER_DRAWTEXT_1 )
{
   QPainter * p = hbqt_par_QPainter( 1 );
   if( p )
      ( p )->drawText( *hbqt_par_QPoint( 2 ), hbqt_par_QString( 3 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QPAINTER_DRAWTEXT_1 FP=( p )->drawText( *hbqt_par_QPoint( 2 ), hbqt_par_QString( 3 ) ); p is NULL" ) );
   }
}

/*
 * void drawText ( const QRectF & rectangle, int flags, const QString & text, QRectF * boundingRect = 0 )
 */
HB_FUNC( QT_QPAINTER_DRAWTEXT_2 )
{
   QPainter * p = hbqt_par_QPainter( 1 );
   if( p )
      ( p )->drawText( *hbqt_par_QRectF( 2 ), hb_parni( 3 ), hbqt_par_QString( 4 ), hbqt_par_QRectF( 5 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QPAINTER_DRAWTEXT_2 FP=( p )->drawText( *hbqt_par_QRectF( 2 ), hb_parni( 3 ), hbqt_par_QString( 4 ), hbqt_par_QRectF( 5 ) ); p is NULL" ) );
   }
}

/*
 * void drawText ( const QRect & rectangle, int flags, const QString & text, QRect * boundingRect = 0 )
 */
HB_FUNC( QT_QPAINTER_DRAWTEXT_3 )
{
   QPainter * p = hbqt_par_QPainter( 1 );
   if( p )
      ( p )->drawText( *hbqt_par_QRect( 2 ), hb_parni( 3 ), hbqt_par_QString( 4 ), hbqt_par_QRect( 5 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QPAINTER_DRAWTEXT_3 FP=( p )->drawText( *hbqt_par_QRect( 2 ), hb_parni( 3 ), hbqt_par_QString( 4 ), hbqt_par_QRect( 5 ) ); p is NULL" ) );
   }
}

/*
 * void drawText ( int x, int y, const QString & text )
 */
HB_FUNC( QT_QPAINTER_DRAWTEXT_4 )
{
   QPainter * p = hbqt_par_QPainter( 1 );
   if( p )
      ( p )->drawText( hb_parni( 2 ), hb_parni( 3 ), hbqt_par_QString( 4 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QPAINTER_DRAWTEXT_4 FP=( p )->drawText( hb_parni( 2 ), hb_parni( 3 ), hbqt_par_QString( 4 ) ); p is NULL" ) );
   }
}

/*
 * void drawText ( int x, int y, int width, int height, int flags, const QString & text, QRect * boundingRect = 0 )
 */
HB_FUNC( QT_QPAINTER_DRAWTEXT_5 )
{
   QPainter * p = hbqt_par_QPainter( 1 );
   if( p )
      ( p )->drawText( hb_parni( 2 ), hb_parni( 3 ), hb_parni( 4 ), hb_parni( 5 ), hb_parni( 6 ), hbqt_par_QString( 7 ), hbqt_par_QRect( 8 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QPAINTER_DRAWTEXT_5 FP=( p )->drawText( hb_parni( 2 ), hb_parni( 3 ), hb_parni( 4 ), hb_parni( 5 ), hb_parni( 6 ), hbqt_par_QString( 7 ), hbqt_par_QRect( 8 ) ); p is NULL" ) );
   }
}

/*
 * void drawText ( const QRectF & rectangle, const QString & text, const QTextOption & option = QTextOption() )
 */
HB_FUNC( QT_QPAINTER_DRAWTEXT_6 )
{
   QPainter * p = hbqt_par_QPainter( 1 );
   if( p )
      ( p )->drawText( *hbqt_par_QRectF( 2 ), hbqt_par_QString( 3 ), ( HB_ISPOINTER( 4 ) ? *hbqt_par_QTextOption( 4 ) : QTextOption() ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QPAINTER_DRAWTEXT_6 FP=( p )->drawText( *hbqt_par_QRectF( 2 ), hbqt_par_QString( 3 ), ( HB_ISPOINTER( 4 ) ? *hbqt_par_QTextOption( 4 ) : QTextOption() ) ); p is NULL" ) );
   }
}

/*
 * void drawTiledPixmap ( const QRectF & rectangle, const QPixmap & pixmap, const QPointF & position = QPointF() )
 */
HB_FUNC( QT_QPAINTER_DRAWTILEDPIXMAP )
{
   QPainter * p = hbqt_par_QPainter( 1 );
   if( p )
      ( p )->drawTiledPixmap( *hbqt_par_QRectF( 2 ), *hbqt_par_QPixmap( 3 ), ( HB_ISPOINTER( 4 ) ? *hbqt_par_QPointF( 4 ) : QPointF() ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QPAINTER_DRAWTILEDPIXMAP FP=( p )->drawTiledPixmap( *hbqt_par_QRectF( 2 ), *hbqt_par_QPixmap( 3 ), ( HB_ISPOINTER( 4 ) ? *hbqt_par_QPointF( 4 ) : QPointF() ) ); p is NULL" ) );
   }
}

/*
 * void drawTiledPixmap ( const QRect & rectangle, const QPixmap & pixmap, const QPoint & position = QPoint() )
 */
HB_FUNC( QT_QPAINTER_DRAWTILEDPIXMAP_1 )
{
   QPainter * p = hbqt_par_QPainter( 1 );
   if( p )
      ( p )->drawTiledPixmap( *hbqt_par_QRect( 2 ), *hbqt_par_QPixmap( 3 ), ( HB_ISPOINTER( 4 ) ? *hbqt_par_QPoint( 4 ) : QPoint() ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QPAINTER_DRAWTILEDPIXMAP_1 FP=( p )->drawTiledPixmap( *hbqt_par_QRect( 2 ), *hbqt_par_QPixmap( 3 ), ( HB_ISPOINTER( 4 ) ? *hbqt_par_QPoint( 4 ) : QPoint() ) ); p is NULL" ) );
   }
}

/*
 * void drawTiledPixmap ( int x, int y, int width, int height, const QPixmap & pixmap, int sx = 0, int sy = 0 )
 */
HB_FUNC( QT_QPAINTER_DRAWTILEDPIXMAP_2 )
{
   QPainter * p = hbqt_par_QPainter( 1 );
   if( p )
      ( p )->drawTiledPixmap( hb_parni( 2 ), hb_parni( 3 ), hb_parni( 4 ), hb_parni( 5 ), *hbqt_par_QPixmap( 6 ), hb_parni( 7 ), hb_parni( 8 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QPAINTER_DRAWTILEDPIXMAP_2 FP=( p )->drawTiledPixmap( hb_parni( 2 ), hb_parni( 3 ), hb_parni( 4 ), hb_parni( 5 ), *hbqt_par_QPixmap( 6 ), hb_parni( 7 ), hb_parni( 8 ) ); p is NULL" ) );
   }
}

/*
 * bool end ()
 */
HB_FUNC( QT_QPAINTER_END )
{
   QPainter * p = hbqt_par_QPainter( 1 );
   if( p )
      hb_retl( ( p )->end() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QPAINTER_END FP=hb_retl( ( p )->end() ); p is NULL" ) );
   }
}

/*
 * void eraseRect ( const QRectF & rectangle )
 */
HB_FUNC( QT_QPAINTER_ERASERECT )
{
   QPainter * p = hbqt_par_QPainter( 1 );
   if( p )
      ( p )->eraseRect( *hbqt_par_QRectF( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QPAINTER_ERASERECT FP=( p )->eraseRect( *hbqt_par_QRectF( 2 ) ); p is NULL" ) );
   }
}

/*
 * void eraseRect ( const QRect & rectangle )
 */
HB_FUNC( QT_QPAINTER_ERASERECT_1 )
{
   QPainter * p = hbqt_par_QPainter( 1 );
   if( p )
      ( p )->eraseRect( *hbqt_par_QRect( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QPAINTER_ERASERECT_1 FP=( p )->eraseRect( *hbqt_par_QRect( 2 ) ); p is NULL" ) );
   }
}

/*
 * void eraseRect ( int x, int y, int width, int height )
 */
HB_FUNC( QT_QPAINTER_ERASERECT_2 )
{
   QPainter * p = hbqt_par_QPainter( 1 );
   if( p )
      ( p )->eraseRect( hb_parni( 2 ), hb_parni( 3 ), hb_parni( 4 ), hb_parni( 5 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QPAINTER_ERASERECT_2 FP=( p )->eraseRect( hb_parni( 2 ), hb_parni( 3 ), hb_parni( 4 ), hb_parni( 5 ) ); p is NULL" ) );
   }
}

/*
 * void fillPath ( const QPainterPath & path, const QBrush & brush )
 */
HB_FUNC( QT_QPAINTER_FILLPATH )
{
   QPainter * p = hbqt_par_QPainter( 1 );
   if( p )
      ( p )->fillPath( *hbqt_par_QPainterPath( 2 ), *hbqt_par_QBrush( 3 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QPAINTER_FILLPATH FP=( p )->fillPath( *hbqt_par_QPainterPath( 2 ), *hbqt_par_QBrush( 3 ) ); p is NULL" ) );
   }
}

/*
 * void fillRect ( const QRectF & rectangle, const QBrush & brush )
 */
HB_FUNC( QT_QPAINTER_FILLRECT )
{
   QPainter * p = hbqt_par_QPainter( 1 );
   if( p )
      ( p )->fillRect( *hbqt_par_QRectF( 2 ), *hbqt_par_QBrush( 3 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QPAINTER_FILLRECT FP=( p )->fillRect( *hbqt_par_QRectF( 2 ), *hbqt_par_QBrush( 3 ) ); p is NULL" ) );
   }
}

/*
 * void fillRect ( int x, int y, int width, int height, Qt::BrushStyle style )
 */
HB_FUNC( QT_QPAINTER_FILLRECT_1 )
{
   QPainter * p = hbqt_par_QPainter( 1 );
   if( p )
      ( p )->fillRect( hb_parni( 2 ), hb_parni( 3 ), hb_parni( 4 ), hb_parni( 5 ), ( Qt::BrushStyle ) hb_parni( 6 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QPAINTER_FILLRECT_1 FP=( p )->fillRect( hb_parni( 2 ), hb_parni( 3 ), hb_parni( 4 ), hb_parni( 5 ), ( Qt::BrushStyle ) hb_parni( 6 ) ); p is NULL" ) );
   }
}

/*
 * void fillRect ( const QRect & rectangle, Qt::BrushStyle style )
 */
HB_FUNC( QT_QPAINTER_FILLRECT_2 )
{
   QPainter * p = hbqt_par_QPainter( 1 );
   if( p )
      ( p )->fillRect( *hbqt_par_QRect( 2 ), ( Qt::BrushStyle ) hb_parni( 3 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QPAINTER_FILLRECT_2 FP=( p )->fillRect( *hbqt_par_QRect( 2 ), ( Qt::BrushStyle ) hb_parni( 3 ) ); p is NULL" ) );
   }
}

/*
 * void fillRect ( const QRectF & rectangle, Qt::BrushStyle style )
 */
HB_FUNC( QT_QPAINTER_FILLRECT_3 )
{
   QPainter * p = hbqt_par_QPainter( 1 );
   if( p )
      ( p )->fillRect( *hbqt_par_QRectF( 2 ), ( Qt::BrushStyle ) hb_parni( 3 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QPAINTER_FILLRECT_3 FP=( p )->fillRect( *hbqt_par_QRectF( 2 ), ( Qt::BrushStyle ) hb_parni( 3 ) ); p is NULL" ) );
   }
}

/*
 * void fillRect ( const QRect & rectangle, const QBrush & brush )
 */
HB_FUNC( QT_QPAINTER_FILLRECT_4 )
{
   QPainter * p = hbqt_par_QPainter( 1 );
   if( p )
      ( p )->fillRect( *hbqt_par_QRect( 2 ), *hbqt_par_QBrush( 3 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QPAINTER_FILLRECT_4 FP=( p )->fillRect( *hbqt_par_QRect( 2 ), *hbqt_par_QBrush( 3 ) ); p is NULL" ) );
   }
}

/*
 * void fillRect ( const QRect & rectangle, const QColor & color )
 */
HB_FUNC( QT_QPAINTER_FILLRECT_5 )
{
   QPainter * p = hbqt_par_QPainter( 1 );
   if( p )
      ( p )->fillRect( *hbqt_par_QRect( 2 ), *hbqt_par_QColor( 3 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QPAINTER_FILLRECT_5 FP=( p )->fillRect( *hbqt_par_QRect( 2 ), *hbqt_par_QColor( 3 ) ); p is NULL" ) );
   }
}

/*
 * void fillRect ( const QRectF & rectangle, const QColor & color )
 */
HB_FUNC( QT_QPAINTER_FILLRECT_6 )
{
   QPainter * p = hbqt_par_QPainter( 1 );
   if( p )
      ( p )->fillRect( *hbqt_par_QRectF( 2 ), *hbqt_par_QColor( 3 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QPAINTER_FILLRECT_6 FP=( p )->fillRect( *hbqt_par_QRectF( 2 ), *hbqt_par_QColor( 3 ) ); p is NULL" ) );
   }
}

/*
 * void fillRect ( int x, int y, int width, int height, const QBrush & brush )
 */
HB_FUNC( QT_QPAINTER_FILLRECT_7 )
{
   QPainter * p = hbqt_par_QPainter( 1 );
   if( p )
      ( p )->fillRect( hb_parni( 2 ), hb_parni( 3 ), hb_parni( 4 ), hb_parni( 5 ), *hbqt_par_QBrush( 6 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QPAINTER_FILLRECT_7 FP=( p )->fillRect( hb_parni( 2 ), hb_parni( 3 ), hb_parni( 4 ), hb_parni( 5 ), *hbqt_par_QBrush( 6 ) ); p is NULL" ) );
   }
}

/*
 * void fillRect ( int x, int y, int width, int height, const QColor & color )
 */
HB_FUNC( QT_QPAINTER_FILLRECT_8 )
{
   QPainter * p = hbqt_par_QPainter( 1 );
   if( p )
      ( p )->fillRect( hb_parni( 2 ), hb_parni( 3 ), hb_parni( 4 ), hb_parni( 5 ), *hbqt_par_QColor( 6 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QPAINTER_FILLRECT_8 FP=( p )->fillRect( hb_parni( 2 ), hb_parni( 3 ), hb_parni( 4 ), hb_parni( 5 ), *hbqt_par_QColor( 6 ) ); p is NULL" ) );
   }
}

/*
 * void fillRect ( int x, int y, int width, int height, Qt::GlobalColor color )
 */
HB_FUNC( QT_QPAINTER_FILLRECT_9 )
{
   QPainter * p = hbqt_par_QPainter( 1 );
   if( p )
      ( p )->fillRect( hb_parni( 2 ), hb_parni( 3 ), hb_parni( 4 ), hb_parni( 5 ), ( Qt::GlobalColor ) hb_parni( 6 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QPAINTER_FILLRECT_9 FP=( p )->fillRect( hb_parni( 2 ), hb_parni( 3 ), hb_parni( 4 ), hb_parni( 5 ), ( Qt::GlobalColor ) hb_parni( 6 ) ); p is NULL" ) );
   }
}

/*
 * void fillRect ( const QRect & rectangle, Qt::GlobalColor color )
 */
HB_FUNC( QT_QPAINTER_FILLRECT_10 )
{
   QPainter * p = hbqt_par_QPainter( 1 );
   if( p )
      ( p )->fillRect( *hbqt_par_QRect( 2 ), ( Qt::GlobalColor ) hb_parni( 3 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QPAINTER_FILLRECT_10 FP=( p )->fillRect( *hbqt_par_QRect( 2 ), ( Qt::GlobalColor ) hb_parni( 3 ) ); p is NULL" ) );
   }
}

/*
 * void fillRect ( const QRectF & rectangle, Qt::GlobalColor color )
 */
HB_FUNC( QT_QPAINTER_FILLRECT_11 )
{
   QPainter * p = hbqt_par_QPainter( 1 );
   if( p )
      ( p )->fillRect( *hbqt_par_QRectF( 2 ), ( Qt::GlobalColor ) hb_parni( 3 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QPAINTER_FILLRECT_11 FP=( p )->fillRect( *hbqt_par_QRectF( 2 ), ( Qt::GlobalColor ) hb_parni( 3 ) ); p is NULL" ) );
   }
}

/*
 * const QFont & font () const
 */
HB_FUNC( QT_QPAINTER_FONT )
{
   QPainter * p = hbqt_par_QPainter( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QFont( new QFont( ( p )->font() ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QPAINTER_FONT FP=hb_retptrGC( hbqt_gcAllocate_QFont( new QFont( ( p )->font() ), true ) ); p is NULL" ) );
   }
}

/*
 * QFontInfo fontInfo () const
 */
HB_FUNC( QT_QPAINTER_FONTINFO )
{
   QPainter * p = hbqt_par_QPainter( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QFontInfo( new QFontInfo( ( p )->fontInfo() ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QPAINTER_FONTINFO FP=hb_retptrGC( hbqt_gcAllocate_QFontInfo( new QFontInfo( ( p )->fontInfo() ), true ) ); p is NULL" ) );
   }
}

/*
 * QFontMetrics fontMetrics () const
 */
HB_FUNC( QT_QPAINTER_FONTMETRICS )
{
   QPainter * p = hbqt_par_QPainter( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QFontMetrics( new QFontMetrics( ( p )->fontMetrics() ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QPAINTER_FONTMETRICS FP=hb_retptrGC( hbqt_gcAllocate_QFontMetrics( new QFontMetrics( ( p )->fontMetrics() ), true ) ); p is NULL" ) );
   }
}

/*
 * bool hasClipping () const
 */
HB_FUNC( QT_QPAINTER_HASCLIPPING )
{
   QPainter * p = hbqt_par_QPainter( 1 );
   if( p )
      hb_retl( ( p )->hasClipping() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QPAINTER_HASCLIPPING FP=hb_retl( ( p )->hasClipping() ); p is NULL" ) );
   }
}

/*
 * void initFrom ( const QWidget * widget )
 */
HB_FUNC( QT_QPAINTER_INITFROM )
{
   QPainter * p = hbqt_par_QPainter( 1 );
   if( p )
      ( p )->initFrom( hbqt_par_QWidget( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QPAINTER_INITFROM FP=( p )->initFrom( hbqt_par_QWidget( 2 ) ); p is NULL" ) );
   }
}

/*
 * bool isActive () const
 */
HB_FUNC( QT_QPAINTER_ISACTIVE )
{
   QPainter * p = hbqt_par_QPainter( 1 );
   if( p )
      hb_retl( ( p )->isActive() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QPAINTER_ISACTIVE FP=hb_retl( ( p )->isActive() ); p is NULL" ) );
   }
}

/*
 * Qt::LayoutDirection layoutDirection () const
 */
HB_FUNC( QT_QPAINTER_LAYOUTDIRECTION )
{
   QPainter * p = hbqt_par_QPainter( 1 );
   if( p )
      hb_retni( ( Qt::LayoutDirection ) ( p )->layoutDirection() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QPAINTER_LAYOUTDIRECTION FP=hb_retni( ( Qt::LayoutDirection ) ( p )->layoutDirection() ); p is NULL" ) );
   }
}

/*
 * qreal opacity () const
 */
HB_FUNC( QT_QPAINTER_OPACITY )
{
   QPainter * p = hbqt_par_QPainter( 1 );
   if( p )
      hb_retnd( ( p )->opacity() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QPAINTER_OPACITY FP=hb_retnd( ( p )->opacity() ); p is NULL" ) );
   }
}

/*
 * QPaintEngine * paintEngine () const
 */
HB_FUNC( QT_QPAINTER_PAINTENGINE )
{
   QPainter * p = hbqt_par_QPainter( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QPaintEngine( ( p )->paintEngine(), false ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QPAINTER_PAINTENGINE FP=hb_retptrGC( hbqt_gcAllocate_QPaintEngine( ( p )->paintEngine(), false ) ); p is NULL" ) );
   }
}

/*
 * const QPen & pen () const
 */
HB_FUNC( QT_QPAINTER_PEN )
{
   QPainter * p = hbqt_par_QPainter( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QPen( new QPen( ( p )->pen() ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QPAINTER_PEN FP=hb_retptrGC( hbqt_gcAllocate_QPen( new QPen( ( p )->pen() ), true ) ); p is NULL" ) );
   }
}

/*
 * RenderHints renderHints () const
 */
HB_FUNC( QT_QPAINTER_RENDERHINTS )
{
   QPainter * p = hbqt_par_QPainter( 1 );
   if( p )
      hb_retni( ( QPainter::RenderHints ) ( p )->renderHints() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QPAINTER_RENDERHINTS FP=hb_retni( ( QPainter::RenderHints ) ( p )->renderHints() ); p is NULL" ) );
   }
}

/*
 * void resetMatrix ()
 */
HB_FUNC( QT_QPAINTER_RESETMATRIX )
{
   QPainter * p = hbqt_par_QPainter( 1 );
   if( p )
      ( p )->resetMatrix();
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QPAINTER_RESETMATRIX FP=( p )->resetMatrix(); p is NULL" ) );
   }
}

/*
 * void resetTransform ()
 */
HB_FUNC( QT_QPAINTER_RESETTRANSFORM )
{
   QPainter * p = hbqt_par_QPainter( 1 );
   if( p )
      ( p )->resetTransform();
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QPAINTER_RESETTRANSFORM FP=( p )->resetTransform(); p is NULL" ) );
   }
}

/*
 * void restore ()
 */
HB_FUNC( QT_QPAINTER_RESTORE )
{
   QPainter * p = hbqt_par_QPainter( 1 );
   if( p )
      ( p )->restore();
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QPAINTER_RESTORE FP=( p )->restore(); p is NULL" ) );
   }
}

/*
 * void rotate ( qreal angle )
 */
HB_FUNC( QT_QPAINTER_ROTATE )
{
   QPainter * p = hbqt_par_QPainter( 1 );
   if( p )
      ( p )->rotate( hb_parnd( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QPAINTER_ROTATE FP=( p )->rotate( hb_parnd( 2 ) ); p is NULL" ) );
   }
}

/*
 * void save ()
 */
HB_FUNC( QT_QPAINTER_SAVE )
{
   QPainter * p = hbqt_par_QPainter( 1 );
   if( p )
      ( p )->save();
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QPAINTER_SAVE FP=( p )->save(); p is NULL" ) );
   }
}

/*
 * void scale ( qreal sx, qreal sy )
 */
HB_FUNC( QT_QPAINTER_SCALE )
{
   QPainter * p = hbqt_par_QPainter( 1 );
   if( p )
      ( p )->scale( hb_parnd( 2 ), hb_parnd( 3 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QPAINTER_SCALE FP=( p )->scale( hb_parnd( 2 ), hb_parnd( 3 ) ); p is NULL" ) );
   }
}

/*
 * void setBackground ( const QBrush & brush )
 */
HB_FUNC( QT_QPAINTER_SETBACKGROUND )
{
   QPainter * p = hbqt_par_QPainter( 1 );
   if( p )
      ( p )->setBackground( *hbqt_par_QBrush( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QPAINTER_SETBACKGROUND FP=( p )->setBackground( *hbqt_par_QBrush( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setBackgroundMode ( Qt::BGMode mode )
 */
HB_FUNC( QT_QPAINTER_SETBACKGROUNDMODE )
{
   QPainter * p = hbqt_par_QPainter( 1 );
   if( p )
      ( p )->setBackgroundMode( ( Qt::BGMode ) hb_parni( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QPAINTER_SETBACKGROUNDMODE FP=( p )->setBackgroundMode( ( Qt::BGMode ) hb_parni( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setBrush ( const QBrush & brush )
 */
HB_FUNC( QT_QPAINTER_SETBRUSH )
{
   QPainter * p = hbqt_par_QPainter( 1 );
   if( p )
      ( p )->setBrush( *hbqt_par_QBrush( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QPAINTER_SETBRUSH FP=( p )->setBrush( *hbqt_par_QBrush( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setBrush ( Qt::BrushStyle style )
 */
HB_FUNC( QT_QPAINTER_SETBRUSH_1 )
{
   QPainter * p = hbqt_par_QPainter( 1 );
   if( p )
      ( p )->setBrush( ( Qt::BrushStyle ) hb_parni( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QPAINTER_SETBRUSH_1 FP=( p )->setBrush( ( Qt::BrushStyle ) hb_parni( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setBrushOrigin ( const QPointF & position )
 */
HB_FUNC( QT_QPAINTER_SETBRUSHORIGIN )
{
   QPainter * p = hbqt_par_QPainter( 1 );
   if( p )
      ( p )->setBrushOrigin( *hbqt_par_QPointF( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QPAINTER_SETBRUSHORIGIN FP=( p )->setBrushOrigin( *hbqt_par_QPointF( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setBrushOrigin ( const QPoint & position )
 */
HB_FUNC( QT_QPAINTER_SETBRUSHORIGIN_1 )
{
   QPainter * p = hbqt_par_QPainter( 1 );
   if( p )
      ( p )->setBrushOrigin( *hbqt_par_QPoint( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QPAINTER_SETBRUSHORIGIN_1 FP=( p )->setBrushOrigin( *hbqt_par_QPoint( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setBrushOrigin ( int x, int y )
 */
HB_FUNC( QT_QPAINTER_SETBRUSHORIGIN_2 )
{
   QPainter * p = hbqt_par_QPainter( 1 );
   if( p )
      ( p )->setBrushOrigin( hb_parni( 2 ), hb_parni( 3 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QPAINTER_SETBRUSHORIGIN_2 FP=( p )->setBrushOrigin( hb_parni( 2 ), hb_parni( 3 ) ); p is NULL" ) );
   }
}

/*
 * void setClipPath ( const QPainterPath & path, Qt::ClipOperation operation = Qt::ReplaceClip )
 */
HB_FUNC( QT_QPAINTER_SETCLIPPATH )
{
   QPainter * p = hbqt_par_QPainter( 1 );
   if( p )
      ( p )->setClipPath( *hbqt_par_QPainterPath( 2 ), ( HB_ISNUM( 3 ) ? ( Qt::ClipOperation ) hb_parni( 3 ) : ( Qt::ClipOperation ) Qt::ReplaceClip ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QPAINTER_SETCLIPPATH FP=( p )->setClipPath( *hbqt_par_QPainterPath( 2 ), ( HB_ISNUM( 3 ) ? ( Qt::ClipOperation ) hb_parni( 3 ) : ( Qt::ClipOperation ) Qt::ReplaceClip ) ); p is NULL" ) );
   }
}

/*
 * void setClipRect ( const QRectF & rectangle, Qt::ClipOperation operation = Qt::ReplaceClip )
 */
HB_FUNC( QT_QPAINTER_SETCLIPRECT )
{
   QPainter * p = hbqt_par_QPainter( 1 );
   if( p )
      ( p )->setClipRect( *hbqt_par_QRectF( 2 ), ( HB_ISNUM( 3 ) ? ( Qt::ClipOperation ) hb_parni( 3 ) : ( Qt::ClipOperation ) Qt::ReplaceClip ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QPAINTER_SETCLIPRECT FP=( p )->setClipRect( *hbqt_par_QRectF( 2 ), ( HB_ISNUM( 3 ) ? ( Qt::ClipOperation ) hb_parni( 3 ) : ( Qt::ClipOperation ) Qt::ReplaceClip ) ); p is NULL" ) );
   }
}

/*
 * void setClipRect ( int x, int y, int width, int height, Qt::ClipOperation operation = Qt::ReplaceClip )
 */
HB_FUNC( QT_QPAINTER_SETCLIPRECT_1 )
{
   QPainter * p = hbqt_par_QPainter( 1 );
   if( p )
      ( p )->setClipRect( hb_parni( 2 ), hb_parni( 3 ), hb_parni( 4 ), hb_parni( 5 ), ( HB_ISNUM( 6 ) ? ( Qt::ClipOperation ) hb_parni( 6 ) : ( Qt::ClipOperation ) Qt::ReplaceClip ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QPAINTER_SETCLIPRECT_1 FP=( p )->setClipRect( hb_parni( 2 ), hb_parni( 3 ), hb_parni( 4 ), hb_parni( 5 ), ( HB_ISNUM( 6 ) ? ( Qt::ClipOperation ) hb_parni( 6 ) : ( Qt::ClipOperation ) Qt::ReplaceClip ) ); p is NULL" ) );
   }
}

/*
 * void setClipRect ( const QRect & rectangle, Qt::ClipOperation operation = Qt::ReplaceClip )
 */
HB_FUNC( QT_QPAINTER_SETCLIPRECT_2 )
{
   QPainter * p = hbqt_par_QPainter( 1 );
   if( p )
      ( p )->setClipRect( *hbqt_par_QRect( 2 ), ( HB_ISNUM( 3 ) ? ( Qt::ClipOperation ) hb_parni( 3 ) : ( Qt::ClipOperation ) Qt::ReplaceClip ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QPAINTER_SETCLIPRECT_2 FP=( p )->setClipRect( *hbqt_par_QRect( 2 ), ( HB_ISNUM( 3 ) ? ( Qt::ClipOperation ) hb_parni( 3 ) : ( Qt::ClipOperation ) Qt::ReplaceClip ) ); p is NULL" ) );
   }
}

/*
 * void setClipRegion ( const QRegion & region, Qt::ClipOperation operation = Qt::ReplaceClip )
 */
HB_FUNC( QT_QPAINTER_SETCLIPREGION )
{
   QPainter * p = hbqt_par_QPainter( 1 );
   if( p )
      ( p )->setClipRegion( *hbqt_par_QRegion( 2 ), ( HB_ISNUM( 3 ) ? ( Qt::ClipOperation ) hb_parni( 3 ) : ( Qt::ClipOperation ) Qt::ReplaceClip ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QPAINTER_SETCLIPREGION FP=( p )->setClipRegion( *hbqt_par_QRegion( 2 ), ( HB_ISNUM( 3 ) ? ( Qt::ClipOperation ) hb_parni( 3 ) : ( Qt::ClipOperation ) Qt::ReplaceClip ) ); p is NULL" ) );
   }
}

/*
 * void setClipping ( bool enable )
 */
HB_FUNC( QT_QPAINTER_SETCLIPPING )
{
   QPainter * p = hbqt_par_QPainter( 1 );
   if( p )
      ( p )->setClipping( hb_parl( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QPAINTER_SETCLIPPING FP=( p )->setClipping( hb_parl( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setCompositionMode ( CompositionMode mode )
 */
HB_FUNC( QT_QPAINTER_SETCOMPOSITIONMODE )
{
   QPainter * p = hbqt_par_QPainter( 1 );
   if( p )
      ( p )->setCompositionMode( ( QPainter::CompositionMode ) hb_parni( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QPAINTER_SETCOMPOSITIONMODE FP=( p )->setCompositionMode( ( QPainter::CompositionMode ) hb_parni( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setFont ( const QFont & font )
 */
HB_FUNC( QT_QPAINTER_SETFONT )
{
   QPainter * p = hbqt_par_QPainter( 1 );
   if( p )
      ( p )->setFont( *hbqt_par_QFont( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QPAINTER_SETFONT FP=( p )->setFont( *hbqt_par_QFont( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setLayoutDirection ( Qt::LayoutDirection direction )
 */
HB_FUNC( QT_QPAINTER_SETLAYOUTDIRECTION )
{
   QPainter * p = hbqt_par_QPainter( 1 );
   if( p )
      ( p )->setLayoutDirection( ( Qt::LayoutDirection ) hb_parni( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QPAINTER_SETLAYOUTDIRECTION FP=( p )->setLayoutDirection( ( Qt::LayoutDirection ) hb_parni( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setOpacity ( qreal opacity )
 */
HB_FUNC( QT_QPAINTER_SETOPACITY )
{
   QPainter * p = hbqt_par_QPainter( 1 );
   if( p )
      ( p )->setOpacity( hb_parnd( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QPAINTER_SETOPACITY FP=( p )->setOpacity( hb_parnd( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setPen ( const QPen & pen )
 */
HB_FUNC( QT_QPAINTER_SETPEN )
{
   QPainter * p = hbqt_par_QPainter( 1 );
   if( p )
      ( p )->setPen( *hbqt_par_QPen( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QPAINTER_SETPEN FP=( p )->setPen( *hbqt_par_QPen( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setPen ( const QColor & color )
 */
HB_FUNC( QT_QPAINTER_SETPEN_1 )
{
   QPainter * p = hbqt_par_QPainter( 1 );
   if( p )
      ( p )->setPen( *hbqt_par_QColor( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QPAINTER_SETPEN_1 FP=( p )->setPen( *hbqt_par_QColor( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setPen ( Qt::PenStyle style )
 */
HB_FUNC( QT_QPAINTER_SETPEN_2 )
{
   QPainter * p = hbqt_par_QPainter( 1 );
   if( p )
      ( p )->setPen( ( Qt::PenStyle ) hb_parni( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QPAINTER_SETPEN_2 FP=( p )->setPen( ( Qt::PenStyle ) hb_parni( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setRenderHint ( RenderHint hint, bool on = true )
 */
HB_FUNC( QT_QPAINTER_SETRENDERHINT )
{
   QPainter * p = hbqt_par_QPainter( 1 );
   if( p )
      ( p )->setRenderHint( ( QPainter::RenderHint ) hb_parni( 2 ), hb_parl( 3 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QPAINTER_SETRENDERHINT FP=( p )->setRenderHint( ( QPainter::RenderHint ) hb_parni( 2 ), hb_parl( 3 ) ); p is NULL" ) );
   }
}

/*
 * void setRenderHints ( RenderHints hints, bool on = true )
 */
HB_FUNC( QT_QPAINTER_SETRENDERHINTS )
{
   QPainter * p = hbqt_par_QPainter( 1 );
   if( p )
      ( p )->setRenderHints( ( QPainter::RenderHints ) hb_parni( 2 ), hb_parl( 3 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QPAINTER_SETRENDERHINTS FP=( p )->setRenderHints( ( QPainter::RenderHints ) hb_parni( 2 ), hb_parl( 3 ) ); p is NULL" ) );
   }
}

/*
 * void setTransform ( const QTransform & transform, bool combine = false )
 */
HB_FUNC( QT_QPAINTER_SETTRANSFORM )
{
   QPainter * p = hbqt_par_QPainter( 1 );
   if( p )
      ( p )->setTransform( *hbqt_par_QTransform( 2 ), hb_parl( 3 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QPAINTER_SETTRANSFORM FP=( p )->setTransform( *hbqt_par_QTransform( 2 ), hb_parl( 3 ) ); p is NULL" ) );
   }
}

/*
 * void setViewTransformEnabled ( bool enable )
 */
HB_FUNC( QT_QPAINTER_SETVIEWTRANSFORMENABLED )
{
   QPainter * p = hbqt_par_QPainter( 1 );
   if( p )
      ( p )->setViewTransformEnabled( hb_parl( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QPAINTER_SETVIEWTRANSFORMENABLED FP=( p )->setViewTransformEnabled( hb_parl( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setViewport ( const QRect & rectangle )
 */
HB_FUNC( QT_QPAINTER_SETVIEWPORT )
{
   QPainter * p = hbqt_par_QPainter( 1 );
   if( p )
      ( p )->setViewport( *hbqt_par_QRect( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QPAINTER_SETVIEWPORT FP=( p )->setViewport( *hbqt_par_QRect( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setViewport ( int x, int y, int width, int height )
 */
HB_FUNC( QT_QPAINTER_SETVIEWPORT_1 )
{
   QPainter * p = hbqt_par_QPainter( 1 );
   if( p )
      ( p )->setViewport( hb_parni( 2 ), hb_parni( 3 ), hb_parni( 4 ), hb_parni( 5 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QPAINTER_SETVIEWPORT_1 FP=( p )->setViewport( hb_parni( 2 ), hb_parni( 3 ), hb_parni( 4 ), hb_parni( 5 ) ); p is NULL" ) );
   }
}

/*
 * void setWindow ( const QRect & rectangle )
 */
HB_FUNC( QT_QPAINTER_SETWINDOW )
{
   QPainter * p = hbqt_par_QPainter( 1 );
   if( p )
      ( p )->setWindow( *hbqt_par_QRect( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QPAINTER_SETWINDOW FP=( p )->setWindow( *hbqt_par_QRect( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setWindow ( int x, int y, int width, int height )
 */
HB_FUNC( QT_QPAINTER_SETWINDOW_1 )
{
   QPainter * p = hbqt_par_QPainter( 1 );
   if( p )
      ( p )->setWindow( hb_parni( 2 ), hb_parni( 3 ), hb_parni( 4 ), hb_parni( 5 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QPAINTER_SETWINDOW_1 FP=( p )->setWindow( hb_parni( 2 ), hb_parni( 3 ), hb_parni( 4 ), hb_parni( 5 ) ); p is NULL" ) );
   }
}

/*
 * void setWorldMatrix ( const QMatrix & matrix, bool combine = false )
 */
HB_FUNC( QT_QPAINTER_SETWORLDMATRIX )
{
   QPainter * p = hbqt_par_QPainter( 1 );
   if( p )
      ( p )->setWorldMatrix( *hbqt_par_QMatrix( 2 ), hb_parl( 3 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QPAINTER_SETWORLDMATRIX FP=( p )->setWorldMatrix( *hbqt_par_QMatrix( 2 ), hb_parl( 3 ) ); p is NULL" ) );
   }
}

/*
 * void setWorldMatrixEnabled ( bool enable )
 */
HB_FUNC( QT_QPAINTER_SETWORLDMATRIXENABLED )
{
   QPainter * p = hbqt_par_QPainter( 1 );
   if( p )
      ( p )->setWorldMatrixEnabled( hb_parl( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QPAINTER_SETWORLDMATRIXENABLED FP=( p )->setWorldMatrixEnabled( hb_parl( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setWorldTransform ( const QTransform & matrix, bool combine = false )
 */
HB_FUNC( QT_QPAINTER_SETWORLDTRANSFORM )
{
   QPainter * p = hbqt_par_QPainter( 1 );
   if( p )
      ( p )->setWorldTransform( *hbqt_par_QTransform( 2 ), hb_parl( 3 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QPAINTER_SETWORLDTRANSFORM FP=( p )->setWorldTransform( *hbqt_par_QTransform( 2 ), hb_parl( 3 ) ); p is NULL" ) );
   }
}

/*
 * void shear ( qreal sh, qreal sv )
 */
HB_FUNC( QT_QPAINTER_SHEAR )
{
   QPainter * p = hbqt_par_QPainter( 1 );
   if( p )
      ( p )->shear( hb_parnd( 2 ), hb_parnd( 3 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QPAINTER_SHEAR FP=( p )->shear( hb_parnd( 2 ), hb_parnd( 3 ) ); p is NULL" ) );
   }
}

/*
 * void strokePath ( const QPainterPath & path, const QPen & pen )
 */
HB_FUNC( QT_QPAINTER_STROKEPATH )
{
   QPainter * p = hbqt_par_QPainter( 1 );
   if( p )
      ( p )->strokePath( *hbqt_par_QPainterPath( 2 ), *hbqt_par_QPen( 3 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QPAINTER_STROKEPATH FP=( p )->strokePath( *hbqt_par_QPainterPath( 2 ), *hbqt_par_QPen( 3 ) ); p is NULL" ) );
   }
}

/*
 * bool testRenderHint ( RenderHint hint ) const
 */
HB_FUNC( QT_QPAINTER_TESTRENDERHINT )
{
   QPainter * p = hbqt_par_QPainter( 1 );
   if( p )
      hb_retl( ( p )->testRenderHint( ( QPainter::RenderHint ) hb_parni( 2 ) ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QPAINTER_TESTRENDERHINT FP=hb_retl( ( p )->testRenderHint( ( QPainter::RenderHint ) hb_parni( 2 ) ) ); p is NULL" ) );
   }
}

/*
 * const QTransform & transform () const
 */
HB_FUNC( QT_QPAINTER_TRANSFORM )
{
   QPainter * p = hbqt_par_QPainter( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QTransform( new QTransform( ( p )->transform() ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QPAINTER_TRANSFORM FP=hb_retptrGC( hbqt_gcAllocate_QTransform( new QTransform( ( p )->transform() ), true ) ); p is NULL" ) );
   }
}

/*
 * void translate ( const QPointF & offset )
 */
HB_FUNC( QT_QPAINTER_TRANSLATE )
{
   QPainter * p = hbqt_par_QPainter( 1 );
   if( p )
      ( p )->translate( *hbqt_par_QPointF( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QPAINTER_TRANSLATE FP=( p )->translate( *hbqt_par_QPointF( 2 ) ); p is NULL" ) );
   }
}

/*
 * void translate ( const QPoint & offset )
 */
HB_FUNC( QT_QPAINTER_TRANSLATE_1 )
{
   QPainter * p = hbqt_par_QPainter( 1 );
   if( p )
      ( p )->translate( *hbqt_par_QPoint( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QPAINTER_TRANSLATE_1 FP=( p )->translate( *hbqt_par_QPoint( 2 ) ); p is NULL" ) );
   }
}

/*
 * void translate ( qreal dx, qreal dy )
 */
HB_FUNC( QT_QPAINTER_TRANSLATE_2 )
{
   QPainter * p = hbqt_par_QPainter( 1 );
   if( p )
      ( p )->translate( hb_parnd( 2 ), hb_parnd( 3 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QPAINTER_TRANSLATE_2 FP=( p )->translate( hb_parnd( 2 ), hb_parnd( 3 ) ); p is NULL" ) );
   }
}

/*
 * bool viewTransformEnabled () const
 */
HB_FUNC( QT_QPAINTER_VIEWTRANSFORMENABLED )
{
   QPainter * p = hbqt_par_QPainter( 1 );
   if( p )
      hb_retl( ( p )->viewTransformEnabled() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QPAINTER_VIEWTRANSFORMENABLED FP=hb_retl( ( p )->viewTransformEnabled() ); p is NULL" ) );
   }
}

/*
 * QRect viewport () const
 */
HB_FUNC( QT_QPAINTER_VIEWPORT )
{
   QPainter * p = hbqt_par_QPainter( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QRect( new QRect( ( p )->viewport() ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QPAINTER_VIEWPORT FP=hb_retptrGC( hbqt_gcAllocate_QRect( new QRect( ( p )->viewport() ), true ) ); p is NULL" ) );
   }
}

/*
 * QRect window () const
 */
HB_FUNC( QT_QPAINTER_WINDOW )
{
   QPainter * p = hbqt_par_QPainter( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QRect( new QRect( ( p )->window() ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QPAINTER_WINDOW FP=hb_retptrGC( hbqt_gcAllocate_QRect( new QRect( ( p )->window() ), true ) ); p is NULL" ) );
   }
}

/*
 * const QMatrix & worldMatrix () const
 */
HB_FUNC( QT_QPAINTER_WORLDMATRIX )
{
   QPainter * p = hbqt_par_QPainter( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QMatrix( new QMatrix( ( p )->worldMatrix() ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QPAINTER_WORLDMATRIX FP=hb_retptrGC( hbqt_gcAllocate_QMatrix( new QMatrix( ( p )->worldMatrix() ), true ) ); p is NULL" ) );
   }
}

/*
 * bool worldMatrixEnabled () const
 */
HB_FUNC( QT_QPAINTER_WORLDMATRIXENABLED )
{
   QPainter * p = hbqt_par_QPainter( 1 );
   if( p )
      hb_retl( ( p )->worldMatrixEnabled() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QPAINTER_WORLDMATRIXENABLED FP=hb_retl( ( p )->worldMatrixEnabled() ); p is NULL" ) );
   }
}

/*
 * const QTransform & worldTransform () const
 */
HB_FUNC( QT_QPAINTER_WORLDTRANSFORM )
{
   QPainter * p = hbqt_par_QPainter( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QTransform( new QTransform( ( p )->worldTransform() ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QPAINTER_WORLDTRANSFORM FP=hb_retptrGC( hbqt_gcAllocate_QTransform( new QTransform( ( p )->worldTransform() ), true ) ); p is NULL" ) );
   }
}

/*
 * QPaintDevice * redirected ( const QPaintDevice * device, QPoint * offset = 0 )
 */
HB_FUNC( QT_QPAINTER_REDIRECTED )
{
   QPainter * p = hbqt_par_QPainter( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QPaintDevice( ( p )->redirected( hbqt_par_QPaintDevice( 2 ), hbqt_par_QPoint( 3 ) ), false ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QPAINTER_REDIRECTED FP=hb_retptrGC( hbqt_gcAllocate_QPaintDevice( ( p )->redirected( hbqt_par_QPaintDevice( 2 ), hbqt_par_QPoint( 3 ) ), false ) ); p is NULL" ) );
   }
}

/*
 * void restoreRedirected ( const QPaintDevice * device )
 */
HB_FUNC( QT_QPAINTER_RESTOREREDIRECTED )
{
   QPainter * p = hbqt_par_QPainter( 1 );
   if( p )
      ( p )->restoreRedirected( hbqt_par_QPaintDevice( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QPAINTER_RESTOREREDIRECTED FP=( p )->restoreRedirected( hbqt_par_QPaintDevice( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setRedirected ( const QPaintDevice * device, QPaintDevice * replacement, const QPoint & offset = QPoint() )
 */
HB_FUNC( QT_QPAINTER_SETREDIRECTED )
{
   QPainter * p = hbqt_par_QPainter( 1 );
   if( p )
      ( p )->setRedirected( hbqt_par_QPaintDevice( 2 ), hbqt_par_QPaintDevice( 3 ), ( HB_ISPOINTER( 4 ) ? *hbqt_par_QPoint( 4 ) : QPoint() ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QPAINTER_SETREDIRECTED FP=( p )->setRedirected( hbqt_par_QPaintDevice( 2 ), hbqt_par_QPaintDevice( 3 ), ( HB_ISPOINTER( 4 ) ? *hbqt_par_QPoint( 4 ) : QPoint() ) ); p is NULL" ) );
   }
}


/*----------------------------------------------------------------------*/
#endif             /* #if QT_VERSION >= 0x040500 */
/*----------------------------------------------------------------------*/
