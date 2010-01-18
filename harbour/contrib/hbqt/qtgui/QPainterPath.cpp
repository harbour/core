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

#include "hbapi.h"
#include "../hbqt.h"

/*----------------------------------------------------------------------*/
#if QT_VERSION >= 0x040500
/*----------------------------------------------------------------------*/

/*
 *  enum ElementType { MoveToElement, LineToElement, CurveToElement, CurveToDataElement }
 */

/*
 *  Constructed[ 51/56 [ 91.07% ] ]
 *
 *  *** Unconvered Prototypes ***
 *  -----------------------------
 *
 *  QList<QPolygonF> toFillPolygons ( const QTransform & matrix ) const
 *  QList<QPolygonF> toFillPolygons ( const QMatrix & matrix = QMatrix() ) const
 *  QList<QPolygonF> toSubpathPolygons ( const QTransform & matrix ) const
 *  QList<QPolygonF> toSubpathPolygons ( const QMatrix & matrix = QMatrix() ) const
 *
 *  *** Commented out protos which construct fine but do not compile ***
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
  void * ph;
  bool bNew;
  QT_G_FUNC_PTR func;
} QGC_POINTER_QPainterPath;

QT_G_FUNC( hbqt_gcRelease_QPainterPath )
{
   QGC_POINTER * p = ( QGC_POINTER * ) Cargo;

   if( p && p->bNew )
   {
      if( p->ph )
      {
         delete ( ( QPainterPath * ) p->ph );
         HB_TRACE( HB_TR_DEBUG, ( "YES_rel_QPainterPath               ph=%p %i B %i KB", p->ph, ( int ) hb_xquery( 1001 ), hbqt_getmemused() ) );
         p->ph = NULL;
      }
      else
      {
         HB_TRACE( HB_TR_DEBUG, ( "DEL_rel_QPainterPath                Object already deleted!" ) );
      }
   }
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "PTR_rel_QPainterPath                Object not created with - new" ) );
      p->ph = NULL;
   }
}

void * hbqt_gcAllocate_QPainterPath( void * pObj, bool bNew )
{
   QGC_POINTER * p = ( QGC_POINTER * ) hb_gcAllocate( sizeof( QGC_POINTER ), hbqt_gcFuncs() );

   p->ph = pObj;
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QPainterPath;

   if( bNew )
   {
      HB_TRACE( HB_TR_DEBUG, ( "   _new_QPainterPath               ph=%p %i B %i KB", pObj, ( int ) hb_xquery( 1001 ), hbqt_getmemused() ) );
   }
   return p;
}

HB_FUNC( QT_QPAINTERPATH )
{
   void * pObj = NULL;

   pObj = new QPainterPath() ;

   hb_retptrGC( hbqt_gcAllocate_QPainterPath( pObj, true ) );
}

/*
 * void addEllipse ( const QRectF & boundingRectangle )
 */
HB_FUNC( QT_QPAINTERPATH_ADDELLIPSE )
{
   hbqt_par_QPainterPath( 1 )->addEllipse( *hbqt_par_QRectF( 2 ) );
}

/*
 * void addEllipse ( qreal x, qreal y, qreal width, qreal height )
 */
HB_FUNC( QT_QPAINTERPATH_ADDELLIPSE_1 )
{
   hbqt_par_QPainterPath( 1 )->addEllipse( hb_parnd( 2 ), hb_parnd( 3 ), hb_parnd( 4 ), hb_parnd( 5 ) );
}

/*
 * void addEllipse ( const QPointF & center, qreal rx, qreal ry )
 */
HB_FUNC( QT_QPAINTERPATH_ADDELLIPSE_2 )
{
   hbqt_par_QPainterPath( 1 )->addEllipse( *hbqt_par_QPointF( 2 ), hb_parnd( 3 ), hb_parnd( 4 ) );
}

/*
 * void addPath ( const QPainterPath & path )
 */
HB_FUNC( QT_QPAINTERPATH_ADDPATH )
{
   hbqt_par_QPainterPath( 1 )->addPath( *hbqt_par_QPainterPath( 2 ) );
}

/*
 * void addPolygon ( const QPolygonF & polygon )
 */
HB_FUNC( QT_QPAINTERPATH_ADDPOLYGON )
{
   hbqt_par_QPainterPath( 1 )->addPolygon( *hbqt_par_QPolygonF( 2 ) );
}

/*
 * void addRect ( const QRectF & rectangle )
 */
HB_FUNC( QT_QPAINTERPATH_ADDRECT )
{
   hbqt_par_QPainterPath( 1 )->addRect( *hbqt_par_QRectF( 2 ) );
}

/*
 * void addRect ( qreal x, qreal y, qreal width, qreal height )
 */
HB_FUNC( QT_QPAINTERPATH_ADDRECT_1 )
{
   hbqt_par_QPainterPath( 1 )->addRect( hb_parnd( 2 ), hb_parnd( 3 ), hb_parnd( 4 ), hb_parnd( 5 ) );
}

/*
 * void addRegion ( const QRegion & region )
 */
HB_FUNC( QT_QPAINTERPATH_ADDREGION )
{
   hbqt_par_QPainterPath( 1 )->addRegion( *hbqt_par_QRegion( 2 ) );
}

/*
 * void addRoundedRect ( const QRectF & rect, qreal xRadius, qreal yRadius, Qt::SizeMode mode = Qt::AbsoluteSize )
 */
HB_FUNC( QT_QPAINTERPATH_ADDROUNDEDRECT )
{
   hbqt_par_QPainterPath( 1 )->addRoundedRect( *hbqt_par_QRectF( 2 ), hb_parnd( 3 ), hb_parnd( 4 ), ( HB_ISNUM( 5 ) ? ( Qt::SizeMode ) hb_parni( 5 ) : ( Qt::SizeMode ) Qt::AbsoluteSize ) );
}

/*
 * void addRoundedRect ( qreal x, qreal y, qreal w, qreal h, qreal xRadius, qreal yRadius, Qt::SizeMode mode = Qt::AbsoluteSize )
 */
HB_FUNC( QT_QPAINTERPATH_ADDROUNDEDRECT_1 )
{
   hbqt_par_QPainterPath( 1 )->addRoundedRect( hb_parnd( 2 ), hb_parnd( 3 ), hb_parnd( 4 ), hb_parnd( 5 ), hb_parnd( 6 ), hb_parnd( 7 ), ( HB_ISNUM( 8 ) ? ( Qt::SizeMode ) hb_parni( 8 ) : ( Qt::SizeMode ) Qt::AbsoluteSize ) );
}

/*
 * void addText ( const QPointF & point, const QFont & font, const QString & text )
 */
HB_FUNC( QT_QPAINTERPATH_ADDTEXT )
{
   hbqt_par_QPainterPath( 1 )->addText( *hbqt_par_QPointF( 2 ), *hbqt_par_QFont( 3 ), hbqt_par_QString( 4 ) );
}

/*
 * void addText ( qreal x, qreal y, const QFont & font, const QString & text )
 */
HB_FUNC( QT_QPAINTERPATH_ADDTEXT_1 )
{
   hbqt_par_QPainterPath( 1 )->addText( hb_parnd( 2 ), hb_parnd( 3 ), *hbqt_par_QFont( 4 ), hbqt_par_QString( 5 ) );
}

/*
 * qreal angleAtPercent ( qreal t ) const
 */
HB_FUNC( QT_QPAINTERPATH_ANGLEATPERCENT )
{
   hb_retnd( hbqt_par_QPainterPath( 1 )->angleAtPercent( hb_parnd( 2 ) ) );
}

/*
 * void arcMoveTo ( const QRectF & rectangle, qreal angle )
 */
HB_FUNC( QT_QPAINTERPATH_ARCMOVETO )
{
   hbqt_par_QPainterPath( 1 )->arcMoveTo( *hbqt_par_QRectF( 2 ), hb_parnd( 3 ) );
}

/*
 * void arcMoveTo ( qreal x, qreal y, qreal width, qreal height, qreal angle )
 */
HB_FUNC( QT_QPAINTERPATH_ARCMOVETO_1 )
{
   hbqt_par_QPainterPath( 1 )->arcMoveTo( hb_parnd( 2 ), hb_parnd( 3 ), hb_parnd( 4 ), hb_parnd( 5 ), hb_parnd( 6 ) );
}

/*
 * void arcTo ( const QRectF & rectangle, qreal startAngle, qreal sweepLength )
 */
HB_FUNC( QT_QPAINTERPATH_ARCTO )
{
   hbqt_par_QPainterPath( 1 )->arcTo( *hbqt_par_QRectF( 2 ), hb_parnd( 3 ), hb_parnd( 4 ) );
}

/*
 * void arcTo ( qreal x, qreal y, qreal width, qreal height, qreal startAngle, qreal sweepLength )
 */
HB_FUNC( QT_QPAINTERPATH_ARCTO_1 )
{
   hbqt_par_QPainterPath( 1 )->arcTo( hb_parnd( 2 ), hb_parnd( 3 ), hb_parnd( 4 ), hb_parnd( 5 ), hb_parnd( 6 ), hb_parnd( 7 ) );
}

/*
 * QRectF boundingRect () const
 */
HB_FUNC( QT_QPAINTERPATH_BOUNDINGRECT )
{
   hb_retptrGC( hbqt_gcAllocate_QRectF( new QRectF( hbqt_par_QPainterPath( 1 )->boundingRect() ), true ) );
}

/*
 * void closeSubpath ()
 */
HB_FUNC( QT_QPAINTERPATH_CLOSESUBPATH )
{
   hbqt_par_QPainterPath( 1 )->closeSubpath();
}

/*
 * void connectPath ( const QPainterPath & path )
 */
HB_FUNC( QT_QPAINTERPATH_CONNECTPATH )
{
   hbqt_par_QPainterPath( 1 )->connectPath( *hbqt_par_QPainterPath( 2 ) );
}

/*
 * bool contains ( const QPointF & point ) const
 */
HB_FUNC( QT_QPAINTERPATH_CONTAINS )
{
   hb_retl( hbqt_par_QPainterPath( 1 )->contains( *hbqt_par_QPointF( 2 ) ) );
}

/*
 * bool contains ( const QRectF & rectangle ) const
 */
HB_FUNC( QT_QPAINTERPATH_CONTAINS_1 )
{
   hb_retl( hbqt_par_QPainterPath( 1 )->contains( *hbqt_par_QRectF( 2 ) ) );
}

/*
 * bool contains ( const QPainterPath & p ) const
 */
HB_FUNC( QT_QPAINTERPATH_CONTAINS_2 )
{
   hb_retl( hbqt_par_QPainterPath( 1 )->contains( *hbqt_par_QPainterPath( 2 ) ) );
}

/*
 * QRectF controlPointRect () const
 */
HB_FUNC( QT_QPAINTERPATH_CONTROLPOINTRECT )
{
   hb_retptrGC( hbqt_gcAllocate_QRectF( new QRectF( hbqt_par_QPainterPath( 1 )->controlPointRect() ), true ) );
}

/*
 * void cubicTo ( const QPointF & c1, const QPointF & c2, const QPointF & endPoint )
 */
HB_FUNC( QT_QPAINTERPATH_CUBICTO )
{
   hbqt_par_QPainterPath( 1 )->cubicTo( *hbqt_par_QPointF( 2 ), *hbqt_par_QPointF( 3 ), *hbqt_par_QPointF( 4 ) );
}

/*
 * void cubicTo ( qreal c1X, qreal c1Y, qreal c2X, qreal c2Y, qreal endPointX, qreal endPointY )
 */
HB_FUNC( QT_QPAINTERPATH_CUBICTO_1 )
{
   hbqt_par_QPainterPath( 1 )->cubicTo( hb_parnd( 2 ), hb_parnd( 3 ), hb_parnd( 4 ), hb_parnd( 5 ), hb_parnd( 6 ), hb_parnd( 7 ) );
}

/*
 * QPointF currentPosition () const
 */
HB_FUNC( QT_QPAINTERPATH_CURRENTPOSITION )
{
   hb_retptrGC( hbqt_gcAllocate_QPointF( new QPointF( hbqt_par_QPainterPath( 1 )->currentPosition() ), true ) );
}

/*
 * int elementCount () const
 */
HB_FUNC( QT_QPAINTERPATH_ELEMENTCOUNT )
{
   hb_retni( hbqt_par_QPainterPath( 1 )->elementCount() );
}

/*
 * Qt::FillRule fillRule () const
 */
HB_FUNC( QT_QPAINTERPATH_FILLRULE )
{
   hb_retni( ( Qt::FillRule ) hbqt_par_QPainterPath( 1 )->fillRule() );
}

/*
 * QPainterPath intersected ( const QPainterPath & p ) const
 */
HB_FUNC( QT_QPAINTERPATH_INTERSECTED )
{
   hb_retptrGC( hbqt_gcAllocate_QPainterPath( new QPainterPath( hbqt_par_QPainterPath( 1 )->intersected( *hbqt_par_QPainterPath( 2 ) ) ), true ) );
}

/*
 * bool intersects ( const QRectF & rectangle ) const
 */
HB_FUNC( QT_QPAINTERPATH_INTERSECTS )
{
   hb_retl( hbqt_par_QPainterPath( 1 )->intersects( *hbqt_par_QRectF( 2 ) ) );
}

/*
 * bool intersects ( const QPainterPath & p ) const
 */
HB_FUNC( QT_QPAINTERPATH_INTERSECTS_1 )
{
   hb_retl( hbqt_par_QPainterPath( 1 )->intersects( *hbqt_par_QPainterPath( 2 ) ) );
}

/*
 * bool isEmpty () const
 */
HB_FUNC( QT_QPAINTERPATH_ISEMPTY )
{
   hb_retl( hbqt_par_QPainterPath( 1 )->isEmpty() );
}

/*
 * qreal length () const
 */
HB_FUNC( QT_QPAINTERPATH_LENGTH )
{
   hb_retnd( hbqt_par_QPainterPath( 1 )->length() );
}

/*
 * void lineTo ( const QPointF & endPoint )
 */
HB_FUNC( QT_QPAINTERPATH_LINETO )
{
   hbqt_par_QPainterPath( 1 )->lineTo( *hbqt_par_QPointF( 2 ) );
}

/*
 * void lineTo ( qreal x, qreal y )
 */
HB_FUNC( QT_QPAINTERPATH_LINETO_1 )
{
   hbqt_par_QPainterPath( 1 )->lineTo( hb_parnd( 2 ), hb_parnd( 3 ) );
}

/*
 * void moveTo ( const QPointF & point )
 */
HB_FUNC( QT_QPAINTERPATH_MOVETO )
{
   hbqt_par_QPainterPath( 1 )->moveTo( *hbqt_par_QPointF( 2 ) );
}

/*
 * void moveTo ( qreal x, qreal y )
 */
HB_FUNC( QT_QPAINTERPATH_MOVETO_1 )
{
   hbqt_par_QPainterPath( 1 )->moveTo( hb_parnd( 2 ), hb_parnd( 3 ) );
}

/*
 * qreal percentAtLength ( qreal len ) const
 */
HB_FUNC( QT_QPAINTERPATH_PERCENTATLENGTH )
{
   hb_retnd( hbqt_par_QPainterPath( 1 )->percentAtLength( hb_parnd( 2 ) ) );
}

/*
 * QPointF pointAtPercent ( qreal t ) const
 */
HB_FUNC( QT_QPAINTERPATH_POINTATPERCENT )
{
   hb_retptrGC( hbqt_gcAllocate_QPointF( new QPointF( hbqt_par_QPainterPath( 1 )->pointAtPercent( hb_parnd( 2 ) ) ), true ) );
}

/*
 * void quadTo ( const QPointF & c, const QPointF & endPoint )
 */
HB_FUNC( QT_QPAINTERPATH_QUADTO )
{
   hbqt_par_QPainterPath( 1 )->quadTo( *hbqt_par_QPointF( 2 ), *hbqt_par_QPointF( 3 ) );
}

/*
 * void quadTo ( qreal cx, qreal cy, qreal endPointX, qreal endPointY )
 */
HB_FUNC( QT_QPAINTERPATH_QUADTO_1 )
{
   hbqt_par_QPainterPath( 1 )->quadTo( hb_parnd( 2 ), hb_parnd( 3 ), hb_parnd( 4 ), hb_parnd( 5 ) );
}

/*
 * void setElementPositionAt ( int index, qreal x, qreal y )
 */
HB_FUNC( QT_QPAINTERPATH_SETELEMENTPOSITIONAT )
{
   hbqt_par_QPainterPath( 1 )->setElementPositionAt( hb_parni( 2 ), hb_parnd( 3 ), hb_parnd( 4 ) );
}

/*
 * void setFillRule ( Qt::FillRule fillRule )
 */
HB_FUNC( QT_QPAINTERPATH_SETFILLRULE )
{
   hbqt_par_QPainterPath( 1 )->setFillRule( ( Qt::FillRule ) hb_parni( 2 ) );
}

/*
 * QPainterPath simplified () const
 */
HB_FUNC( QT_QPAINTERPATH_SIMPLIFIED )
{
   hb_retptrGC( hbqt_gcAllocate_QPainterPath( new QPainterPath( hbqt_par_QPainterPath( 1 )->simplified() ), true ) );
}

/*
 * qreal slopeAtPercent ( qreal t ) const
 */
HB_FUNC( QT_QPAINTERPATH_SLOPEATPERCENT )
{
   hb_retnd( hbqt_par_QPainterPath( 1 )->slopeAtPercent( hb_parnd( 2 ) ) );
}

/*
 * QPainterPath subtracted ( const QPainterPath & p ) const
 */
HB_FUNC( QT_QPAINTERPATH_SUBTRACTED )
{
   hb_retptrGC( hbqt_gcAllocate_QPainterPath( new QPainterPath( hbqt_par_QPainterPath( 1 )->subtracted( *hbqt_par_QPainterPath( 2 ) ) ), true ) );
}

/*
 * QPolygonF toFillPolygon ( const QTransform & matrix ) const
 */
HB_FUNC( QT_QPAINTERPATH_TOFILLPOLYGON )
{
   hb_retptrGC( hbqt_gcAllocate_QPolygonF( new QPolygonF( hbqt_par_QPainterPath( 1 )->toFillPolygon( *hbqt_par_QTransform( 2 ) ) ), true ) );
}

/*
 * QPolygonF toFillPolygon ( const QMatrix & matrix = QMatrix() ) const
 */
HB_FUNC( QT_QPAINTERPATH_TOFILLPOLYGON_1 )
{
   hb_retptrGC( hbqt_gcAllocate_QPolygonF( new QPolygonF( hbqt_par_QPainterPath( 1 )->toFillPolygon( ( HB_ISPOINTER( 2 ) ? *hbqt_par_QMatrix( 2 ) : QMatrix() ) ) ), true ) );
}

/*
 * QPainterPath toReversed () const
 */
HB_FUNC( QT_QPAINTERPATH_TOREVERSED )
{
   hb_retptrGC( hbqt_gcAllocate_QPainterPath( new QPainterPath( hbqt_par_QPainterPath( 1 )->toReversed() ), true ) );
}

/*
 * QPainterPath united ( const QPainterPath & p ) const
 */
HB_FUNC( QT_QPAINTERPATH_UNITED )
{
   hb_retptrGC( hbqt_gcAllocate_QPainterPath( new QPainterPath( hbqt_par_QPainterPath( 1 )->united( *hbqt_par_QPainterPath( 2 ) ) ), true ) );
}


/*----------------------------------------------------------------------*/
#endif             /* #if QT_VERSION >= 0x040500 */
/*----------------------------------------------------------------------*/
