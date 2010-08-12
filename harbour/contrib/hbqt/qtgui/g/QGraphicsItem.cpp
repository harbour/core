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

#include "hbqtcore.h"
#include "hbqtgui.h"

/*----------------------------------------------------------------------*/
#if QT_VERSION >= 0x040500
/*----------------------------------------------------------------------*/

/*
 *  enum CacheMode { NoCache, ItemCoordinateCache, DeviceCoordinateCache }
 *  enum GraphicsItemChange { ItemEnabledChange, ItemEnabledHasChanged, ItemMatrixChange, ItemPositionChange, ..., ItemOpacityHasChanged }
 *  enum GraphicsItemFlag { ItemIsMovable, ItemIsSelectable, ItemIsFocusable, ItemClipsToShape, ..., ItemStacksBehindParent }
 *  flags GraphicsItemFlags
 */

#include <QtCore/QPointer>

#include <QtGui/QGraphicsItem>
#include <QtGui/QCursor>


/*
 * QGraphicsItem ( QGraphicsItem * parent = 0 )
 * virtual ~QGraphicsItem ()
 */

typedef struct
{
   QGraphicsItem * ph;
   bool bNew;
   PHBQT_GC_FUNC func;
   int type;
} HBQT_GC_T_QGraphicsItem;

HBQT_GC_FUNC( hbqt_gcRelease_QGraphicsItem )
{
   HB_SYMBOL_UNUSED( Cargo );
   HBQT_GC_T * p = ( HBQT_GC_T * ) Cargo;

   if( p && p->bNew )
   {
      p->ph = NULL;
   }
}

void * hbqt_gcAllocate_QGraphicsItem( void * pObj, bool bNew )
{
   HBQT_GC_T * p = ( HBQT_GC_T * ) hb_gcAllocate( sizeof( HBQT_GC_T ), hbqt_gcFuncs() );

   p->ph = ( QGraphicsItem * ) pObj;
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QGraphicsItem;
   p->type = HBQT_TYPE_QGraphicsItem;

   if( bNew )
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p    _new_QGraphicsItem", pObj ) );
   }
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p NOT_new_QGraphicsItem", pObj ) );
   }
   return p;
}

HB_FUNC( QT_QGRAPHICSITEM )
{

}

/*
 * bool acceptDrops () const
 */
HB_FUNC( QT_QGRAPHICSITEM_ACCEPTDROPS )
{
   QGraphicsItem * p = hbqt_par_QGraphicsItem( 1 );
   if( p )
      hb_retl( ( p )->acceptDrops() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QGRAPHICSITEM_ACCEPTDROPS FP=hb_retl( ( p )->acceptDrops() ); p is NULL" ) );
   }
}

/*
 * bool acceptHoverEvents () const
 */
HB_FUNC( QT_QGRAPHICSITEM_ACCEPTHOVEREVENTS )
{
   QGraphicsItem * p = hbqt_par_QGraphicsItem( 1 );
   if( p )
      hb_retl( ( p )->acceptHoverEvents() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QGRAPHICSITEM_ACCEPTHOVEREVENTS FP=hb_retl( ( p )->acceptHoverEvents() ); p is NULL" ) );
   }
}

/*
 * Qt::MouseButtons acceptedMouseButtons () const
 */
HB_FUNC( QT_QGRAPHICSITEM_ACCEPTEDMOUSEBUTTONS )
{
   QGraphicsItem * p = hbqt_par_QGraphicsItem( 1 );
   if( p )
      hb_retni( ( Qt::MouseButtons ) ( p )->acceptedMouseButtons() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QGRAPHICSITEM_ACCEPTEDMOUSEBUTTONS FP=hb_retni( ( Qt::MouseButtons ) ( p )->acceptedMouseButtons() ); p is NULL" ) );
   }
}

/*
 * virtual void advance ( int phase )
 */
HB_FUNC( QT_QGRAPHICSITEM_ADVANCE )
{
   QGraphicsItem * p = hbqt_par_QGraphicsItem( 1 );
   if( p )
      ( p )->advance( hb_parni( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QGRAPHICSITEM_ADVANCE FP=( p )->advance( hb_parni( 2 ) ); p is NULL" ) );
   }
}

/*
 * virtual QRectF boundingRect () const = 0
 */
HB_FUNC( QT_QGRAPHICSITEM_BOUNDINGRECT )
{
   QGraphicsItem * p = hbqt_par_QGraphicsItem( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QRectF( new QRectF( ( p )->boundingRect() ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QGRAPHICSITEM_BOUNDINGRECT FP=hb_retptrGC( hbqt_gcAllocate_QRectF( new QRectF( ( p )->boundingRect() ), true ) ); p is NULL" ) );
   }
}

/*
 * QRegion boundingRegion ( const QTransform & itemToDeviceTransform ) const
 */
HB_FUNC( QT_QGRAPHICSITEM_BOUNDINGREGION )
{
   QGraphicsItem * p = hbqt_par_QGraphicsItem( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QRegion( new QRegion( ( p )->boundingRegion( *hbqt_par_QTransform( 2 ) ) ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QGRAPHICSITEM_BOUNDINGREGION FP=hb_retptrGC( hbqt_gcAllocate_QRegion( new QRegion( ( p )->boundingRegion( *hbqt_par_QTransform( 2 ) ) ), true ) ); p is NULL" ) );
   }
}

/*
 * qreal boundingRegionGranularity () const
 */
HB_FUNC( QT_QGRAPHICSITEM_BOUNDINGREGIONGRANULARITY )
{
   QGraphicsItem * p = hbqt_par_QGraphicsItem( 1 );
   if( p )
      hb_retnd( ( p )->boundingRegionGranularity() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QGRAPHICSITEM_BOUNDINGREGIONGRANULARITY FP=hb_retnd( ( p )->boundingRegionGranularity() ); p is NULL" ) );
   }
}

/*
 * CacheMode cacheMode () const
 */
HB_FUNC( QT_QGRAPHICSITEM_CACHEMODE )
{
   QGraphicsItem * p = hbqt_par_QGraphicsItem( 1 );
   if( p )
      hb_retni( ( QGraphicsItem::CacheMode ) ( p )->cacheMode() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QGRAPHICSITEM_CACHEMODE FP=hb_retni( ( QGraphicsItem::CacheMode ) ( p )->cacheMode() ); p is NULL" ) );
   }
}

/*
 * QList<QGraphicsItem *> childItems () const
 */
HB_FUNC( QT_QGRAPHICSITEM_CHILDITEMS )
{
   QGraphicsItem * p = hbqt_par_QGraphicsItem( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QList( new QList<QGraphicsItem *>( ( p )->childItems() ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QGRAPHICSITEM_CHILDITEMS FP=hb_retptrGC( hbqt_gcAllocate_QList( new QList<QGraphicsItem *>( ( p )->childItems() ), true ) ); p is NULL" ) );
   }
}

/*
 * QRectF childrenBoundingRect () const
 */
HB_FUNC( QT_QGRAPHICSITEM_CHILDRENBOUNDINGRECT )
{
   QGraphicsItem * p = hbqt_par_QGraphicsItem( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QRectF( new QRectF( ( p )->childrenBoundingRect() ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QGRAPHICSITEM_CHILDRENBOUNDINGRECT FP=hb_retptrGC( hbqt_gcAllocate_QRectF( new QRectF( ( p )->childrenBoundingRect() ), true ) ); p is NULL" ) );
   }
}

/*
 * void clearFocus ()
 */
HB_FUNC( QT_QGRAPHICSITEM_CLEARFOCUS )
{
   QGraphicsItem * p = hbqt_par_QGraphicsItem( 1 );
   if( p )
      ( p )->clearFocus();
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QGRAPHICSITEM_CLEARFOCUS FP=( p )->clearFocus(); p is NULL" ) );
   }
}

/*
 * QPainterPath clipPath () const
 */
HB_FUNC( QT_QGRAPHICSITEM_CLIPPATH )
{
   QGraphicsItem * p = hbqt_par_QGraphicsItem( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QPainterPath( new QPainterPath( ( p )->clipPath() ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QGRAPHICSITEM_CLIPPATH FP=hb_retptrGC( hbqt_gcAllocate_QPainterPath( new QPainterPath( ( p )->clipPath() ), true ) ); p is NULL" ) );
   }
}

/*
 * virtual bool collidesWithItem ( const QGraphicsItem * other, Qt::ItemSelectionMode mode = Qt::IntersectsItemShape ) const
 */
HB_FUNC( QT_QGRAPHICSITEM_COLLIDESWITHITEM )
{
   QGraphicsItem * p = hbqt_par_QGraphicsItem( 1 );
   if( p )
      hb_retl( ( p )->collidesWithItem( hbqt_par_QGraphicsItem( 2 ), ( HB_ISNUM( 3 ) ? ( Qt::ItemSelectionMode ) hb_parni( 3 ) : ( Qt::ItemSelectionMode ) Qt::IntersectsItemShape ) ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QGRAPHICSITEM_COLLIDESWITHITEM FP=hb_retl( ( p )->collidesWithItem( hbqt_par_QGraphicsItem( 2 ), ( HB_ISNUM( 3 ) ? ( Qt::ItemSelectionMode ) hb_parni( 3 ) : ( Qt::ItemSelectionMode ) Qt::IntersectsItemShape ) ) ); p is NULL" ) );
   }
}

/*
 * virtual bool collidesWithPath ( const QPainterPath & path, Qt::ItemSelectionMode mode = Qt::IntersectsItemShape ) const
 */
HB_FUNC( QT_QGRAPHICSITEM_COLLIDESWITHPATH )
{
   QGraphicsItem * p = hbqt_par_QGraphicsItem( 1 );
   if( p )
      hb_retl( ( p )->collidesWithPath( *hbqt_par_QPainterPath( 2 ), ( HB_ISNUM( 3 ) ? ( Qt::ItemSelectionMode ) hb_parni( 3 ) : ( Qt::ItemSelectionMode ) Qt::IntersectsItemShape ) ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QGRAPHICSITEM_COLLIDESWITHPATH FP=hb_retl( ( p )->collidesWithPath( *hbqt_par_QPainterPath( 2 ), ( HB_ISNUM( 3 ) ? ( Qt::ItemSelectionMode ) hb_parni( 3 ) : ( Qt::ItemSelectionMode ) Qt::IntersectsItemShape ) ) ); p is NULL" ) );
   }
}

/*
 * QList<QGraphicsItem *> collidingItems ( Qt::ItemSelectionMode mode = Qt::IntersectsItemShape ) const
 */
HB_FUNC( QT_QGRAPHICSITEM_COLLIDINGITEMS )
{
   QGraphicsItem * p = hbqt_par_QGraphicsItem( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QList( new QList<QGraphicsItem *>( ( p )->collidingItems( ( HB_ISNUM( 2 ) ? ( Qt::ItemSelectionMode ) hb_parni( 2 ) : ( Qt::ItemSelectionMode ) Qt::IntersectsItemShape ) ) ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QGRAPHICSITEM_COLLIDINGITEMS FP=hb_retptrGC( hbqt_gcAllocate_QList( new QList<QGraphicsItem *>( ( p )->collidingItems( ( HB_ISNUM( 2 ) ? ( Qt::ItemSelectionMode ) hb_parni( 2 ) : ( Qt::ItemSelectionMode ) Qt::IntersectsItemShape ) ) ), true ) ); p is NULL" ) );
   }
}

/*
 * QGraphicsItem * commonAncestorItem ( const QGraphicsItem * other ) const
 */
HB_FUNC( QT_QGRAPHICSITEM_COMMONANCESTORITEM )
{
   QGraphicsItem * p = hbqt_par_QGraphicsItem( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QGraphicsItem( ( p )->commonAncestorItem( hbqt_par_QGraphicsItem( 2 ) ), false ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QGRAPHICSITEM_COMMONANCESTORITEM FP=hb_retptrGC( hbqt_gcAllocate_QGraphicsItem( ( p )->commonAncestorItem( hbqt_par_QGraphicsItem( 2 ) ), false ) ); p is NULL" ) );
   }
}

/*
 * virtual bool contains ( const QPointF & point ) const
 */
HB_FUNC( QT_QGRAPHICSITEM_CONTAINS )
{
   QGraphicsItem * p = hbqt_par_QGraphicsItem( 1 );
   if( p )
      hb_retl( ( p )->contains( *hbqt_par_QPointF( 2 ) ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QGRAPHICSITEM_CONTAINS FP=hb_retl( ( p )->contains( *hbqt_par_QPointF( 2 ) ) ); p is NULL" ) );
   }
}

/*
 * QCursor cursor () const
 */
HB_FUNC( QT_QGRAPHICSITEM_CURSOR )
{
   QGraphicsItem * p = hbqt_par_QGraphicsItem( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QCursor( new QCursor( ( p )->cursor() ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QGRAPHICSITEM_CURSOR FP=hb_retptrGC( hbqt_gcAllocate_QCursor( new QCursor( ( p )->cursor() ), true ) ); p is NULL" ) );
   }
}

/*
 * QVariant data ( int key ) const
 */
HB_FUNC( QT_QGRAPHICSITEM_DATA )
{
   QGraphicsItem * p = hbqt_par_QGraphicsItem( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QVariant( new QVariant( ( p )->data( hb_parni( 2 ) ) ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QGRAPHICSITEM_DATA FP=hb_retptrGC( hbqt_gcAllocate_QVariant( new QVariant( ( p )->data( hb_parni( 2 ) ) ), true ) ); p is NULL" ) );
   }
}

/*
 * QTransform deviceTransform ( const QTransform & viewportTransform ) const
 */
HB_FUNC( QT_QGRAPHICSITEM_DEVICETRANSFORM )
{
   QGraphicsItem * p = hbqt_par_QGraphicsItem( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QTransform( new QTransform( ( p )->deviceTransform( *hbqt_par_QTransform( 2 ) ) ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QGRAPHICSITEM_DEVICETRANSFORM FP=hb_retptrGC( hbqt_gcAllocate_QTransform( new QTransform( ( p )->deviceTransform( *hbqt_par_QTransform( 2 ) ) ), true ) ); p is NULL" ) );
   }
}

/*
 * qreal effectiveOpacity () const
 */
HB_FUNC( QT_QGRAPHICSITEM_EFFECTIVEOPACITY )
{
   QGraphicsItem * p = hbqt_par_QGraphicsItem( 1 );
   if( p )
      hb_retnd( ( p )->effectiveOpacity() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QGRAPHICSITEM_EFFECTIVEOPACITY FP=hb_retnd( ( p )->effectiveOpacity() ); p is NULL" ) );
   }
}

/*
 * void ensureVisible ( const QRectF & rect = QRectF(), int xmargin = 50, int ymargin = 50 )
 */
HB_FUNC( QT_QGRAPHICSITEM_ENSUREVISIBLE )
{
   QGraphicsItem * p = hbqt_par_QGraphicsItem( 1 );
   if( p )
      ( p )->ensureVisible( ( HB_ISPOINTER( 2 ) ? *hbqt_par_QRectF( 2 ) : QRectF() ), hb_parnidef( 3, 50 ), hb_parnidef( 4, 50 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QGRAPHICSITEM_ENSUREVISIBLE FP=( p )->ensureVisible( ( HB_ISPOINTER( 2 ) ? *hbqt_par_QRectF( 2 ) : QRectF() ), hb_parnidef( 3, 50 ), hb_parnidef( 4, 50 ) ); p is NULL" ) );
   }
}

/*
 * void ensureVisible ( qreal x, qreal y, qreal w, qreal h, int xmargin = 50, int ymargin = 50 )
 */
HB_FUNC( QT_QGRAPHICSITEM_ENSUREVISIBLE_1 )
{
   QGraphicsItem * p = hbqt_par_QGraphicsItem( 1 );
   if( p )
      ( p )->ensureVisible( hb_parnd( 2 ), hb_parnd( 3 ), hb_parnd( 4 ), hb_parnd( 5 ), hb_parnidef( 6, 50 ), hb_parnidef( 7, 50 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QGRAPHICSITEM_ENSUREVISIBLE_1 FP=( p )->ensureVisible( hb_parnd( 2 ), hb_parnd( 3 ), hb_parnd( 4 ), hb_parnd( 5 ), hb_parnidef( 6, 50 ), hb_parnidef( 7, 50 ) ); p is NULL" ) );
   }
}

/*
 * GraphicsItemFlags flags () const
 */
HB_FUNC( QT_QGRAPHICSITEM_FLAGS )
{
   QGraphicsItem * p = hbqt_par_QGraphicsItem( 1 );
   if( p )
      hb_retni( ( QGraphicsItem::GraphicsItemFlags ) ( p )->flags() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QGRAPHICSITEM_FLAGS FP=hb_retni( ( QGraphicsItem::GraphicsItemFlags ) ( p )->flags() ); p is NULL" ) );
   }
}

/*
 * void grabKeyboard ()
 */
HB_FUNC( QT_QGRAPHICSITEM_GRABKEYBOARD )
{
   QGraphicsItem * p = hbqt_par_QGraphicsItem( 1 );
   if( p )
      ( p )->grabKeyboard();
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QGRAPHICSITEM_GRABKEYBOARD FP=( p )->grabKeyboard(); p is NULL" ) );
   }
}

/*
 * void grabMouse ()
 */
HB_FUNC( QT_QGRAPHICSITEM_GRABMOUSE )
{
   QGraphicsItem * p = hbqt_par_QGraphicsItem( 1 );
   if( p )
      ( p )->grabMouse();
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QGRAPHICSITEM_GRABMOUSE FP=( p )->grabMouse(); p is NULL" ) );
   }
}

/*
 * QGraphicsItemGroup * group () const
 */
HB_FUNC( QT_QGRAPHICSITEM_GROUP )
{
   QGraphicsItem * p = hbqt_par_QGraphicsItem( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QGraphicsItemGroup( ( p )->group(), false ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QGRAPHICSITEM_GROUP FP=hb_retptrGC( hbqt_gcAllocate_QGraphicsItemGroup( ( p )->group(), false ) ); p is NULL" ) );
   }
}

/*
 * bool handlesChildEvents () const
 */
HB_FUNC( QT_QGRAPHICSITEM_HANDLESCHILDEVENTS )
{
   QGraphicsItem * p = hbqt_par_QGraphicsItem( 1 );
   if( p )
      hb_retl( ( p )->handlesChildEvents() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QGRAPHICSITEM_HANDLESCHILDEVENTS FP=hb_retl( ( p )->handlesChildEvents() ); p is NULL" ) );
   }
}

/*
 * bool hasCursor () const
 */
HB_FUNC( QT_QGRAPHICSITEM_HASCURSOR )
{
   QGraphicsItem * p = hbqt_par_QGraphicsItem( 1 );
   if( p )
      hb_retl( ( p )->hasCursor() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QGRAPHICSITEM_HASCURSOR FP=hb_retl( ( p )->hasCursor() ); p is NULL" ) );
   }
}

/*
 * bool hasFocus () const
 */
HB_FUNC( QT_QGRAPHICSITEM_HASFOCUS )
{
   QGraphicsItem * p = hbqt_par_QGraphicsItem( 1 );
   if( p )
      hb_retl( ( p )->hasFocus() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QGRAPHICSITEM_HASFOCUS FP=hb_retl( ( p )->hasFocus() ); p is NULL" ) );
   }
}

/*
 * void hide ()
 */
HB_FUNC( QT_QGRAPHICSITEM_HIDE )
{
   QGraphicsItem * p = hbqt_par_QGraphicsItem( 1 );
   if( p )
      ( p )->hide();
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QGRAPHICSITEM_HIDE FP=( p )->hide(); p is NULL" ) );
   }
}

/*
 * void installSceneEventFilter ( QGraphicsItem * filterItem )
 */
HB_FUNC( QT_QGRAPHICSITEM_INSTALLSCENEEVENTFILTER )
{
   QGraphicsItem * p = hbqt_par_QGraphicsItem( 1 );
   if( p )
      ( p )->installSceneEventFilter( hbqt_par_QGraphicsItem( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QGRAPHICSITEM_INSTALLSCENEEVENTFILTER FP=( p )->installSceneEventFilter( hbqt_par_QGraphicsItem( 2 ) ); p is NULL" ) );
   }
}

/*
 * bool isAncestorOf ( const QGraphicsItem * child ) const
 */
HB_FUNC( QT_QGRAPHICSITEM_ISANCESTOROF )
{
   QGraphicsItem * p = hbqt_par_QGraphicsItem( 1 );
   if( p )
      hb_retl( ( p )->isAncestorOf( hbqt_par_QGraphicsItem( 2 ) ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QGRAPHICSITEM_ISANCESTOROF FP=hb_retl( ( p )->isAncestorOf( hbqt_par_QGraphicsItem( 2 ) ) ); p is NULL" ) );
   }
}

/*
 * bool isClipped () const
 */
HB_FUNC( QT_QGRAPHICSITEM_ISCLIPPED )
{
   QGraphicsItem * p = hbqt_par_QGraphicsItem( 1 );
   if( p )
      hb_retl( ( p )->isClipped() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QGRAPHICSITEM_ISCLIPPED FP=hb_retl( ( p )->isClipped() ); p is NULL" ) );
   }
}

/*
 * bool isEnabled () const
 */
HB_FUNC( QT_QGRAPHICSITEM_ISENABLED )
{
   QGraphicsItem * p = hbqt_par_QGraphicsItem( 1 );
   if( p )
      hb_retl( ( p )->isEnabled() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QGRAPHICSITEM_ISENABLED FP=hb_retl( ( p )->isEnabled() ); p is NULL" ) );
   }
}

/*
 * bool isObscured () const
 */
HB_FUNC( QT_QGRAPHICSITEM_ISOBSCURED )
{
   QGraphicsItem * p = hbqt_par_QGraphicsItem( 1 );
   if( p )
      hb_retl( ( p )->isObscured() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QGRAPHICSITEM_ISOBSCURED FP=hb_retl( ( p )->isObscured() ); p is NULL" ) );
   }
}

/*
 * bool isObscured ( qreal x, qreal y, qreal w, qreal h ) const
 */
HB_FUNC( QT_QGRAPHICSITEM_ISOBSCURED_1 )
{
   QGraphicsItem * p = hbqt_par_QGraphicsItem( 1 );
   if( p )
      hb_retl( ( p )->isObscured( hb_parnd( 2 ), hb_parnd( 3 ), hb_parnd( 4 ), hb_parnd( 5 ) ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QGRAPHICSITEM_ISOBSCURED_1 FP=hb_retl( ( p )->isObscured( hb_parnd( 2 ), hb_parnd( 3 ), hb_parnd( 4 ), hb_parnd( 5 ) ) ); p is NULL" ) );
   }
}

/*
 * bool isObscured ( const QRectF & rect ) const
 */
HB_FUNC( QT_QGRAPHICSITEM_ISOBSCURED_2 )
{
   QGraphicsItem * p = hbqt_par_QGraphicsItem( 1 );
   if( p )
      hb_retl( ( p )->isObscured( *hbqt_par_QRectF( 2 ) ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QGRAPHICSITEM_ISOBSCURED_2 FP=hb_retl( ( p )->isObscured( *hbqt_par_QRectF( 2 ) ) ); p is NULL" ) );
   }
}

/*
 * virtual bool isObscuredBy ( const QGraphicsItem * item ) const
 */
HB_FUNC( QT_QGRAPHICSITEM_ISOBSCUREDBY )
{
   QGraphicsItem * p = hbqt_par_QGraphicsItem( 1 );
   if( p )
      hb_retl( ( p )->isObscuredBy( hbqt_par_QGraphicsItem( 2 ) ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QGRAPHICSITEM_ISOBSCUREDBY FP=hb_retl( ( p )->isObscuredBy( hbqt_par_QGraphicsItem( 2 ) ) ); p is NULL" ) );
   }
}

/*
 * bool isSelected () const
 */
HB_FUNC( QT_QGRAPHICSITEM_ISSELECTED )
{
   QGraphicsItem * p = hbqt_par_QGraphicsItem( 1 );
   if( p )
      hb_retl( ( p )->isSelected() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QGRAPHICSITEM_ISSELECTED FP=hb_retl( ( p )->isSelected() ); p is NULL" ) );
   }
}

/*
 * bool isUnderMouse () const
 */
HB_FUNC( QT_QGRAPHICSITEM_ISUNDERMOUSE )
{
   QGraphicsItem * p = hbqt_par_QGraphicsItem( 1 );
   if( p )
      hb_retl( ( p )->isUnderMouse() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QGRAPHICSITEM_ISUNDERMOUSE FP=hb_retl( ( p )->isUnderMouse() ); p is NULL" ) );
   }
}

/*
 * bool isVisible () const
 */
HB_FUNC( QT_QGRAPHICSITEM_ISVISIBLE )
{
   QGraphicsItem * p = hbqt_par_QGraphicsItem( 1 );
   if( p )
      hb_retl( ( p )->isVisible() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QGRAPHICSITEM_ISVISIBLE FP=hb_retl( ( p )->isVisible() ); p is NULL" ) );
   }
}

/*
 * bool isVisibleTo ( const QGraphicsItem * parent ) const
 */
HB_FUNC( QT_QGRAPHICSITEM_ISVISIBLETO )
{
   QGraphicsItem * p = hbqt_par_QGraphicsItem( 1 );
   if( p )
      hb_retl( ( p )->isVisibleTo( hbqt_par_QGraphicsItem( 2 ) ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QGRAPHICSITEM_ISVISIBLETO FP=hb_retl( ( p )->isVisibleTo( hbqt_par_QGraphicsItem( 2 ) ) ); p is NULL" ) );
   }
}

/*
 * bool isWidget () const
 */
HB_FUNC( QT_QGRAPHICSITEM_ISWIDGET )
{
   QGraphicsItem * p = hbqt_par_QGraphicsItem( 1 );
   if( p )
      hb_retl( ( p )->isWidget() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QGRAPHICSITEM_ISWIDGET FP=hb_retl( ( p )->isWidget() ); p is NULL" ) );
   }
}

/*
 * bool isWindow () const
 */
HB_FUNC( QT_QGRAPHICSITEM_ISWINDOW )
{
   QGraphicsItem * p = hbqt_par_QGraphicsItem( 1 );
   if( p )
      hb_retl( ( p )->isWindow() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QGRAPHICSITEM_ISWINDOW FP=hb_retl( ( p )->isWindow() ); p is NULL" ) );
   }
}

/*
 * QTransform itemTransform ( const QGraphicsItem * other, bool * ok = 0 ) const
 */
HB_FUNC( QT_QGRAPHICSITEM_ITEMTRANSFORM )
{
   QGraphicsItem * p = hbqt_par_QGraphicsItem( 1 );
   bool iOk = 0;

   if( p )
      hb_retptrGC( hbqt_gcAllocate_QTransform( new QTransform( ( p )->itemTransform( hbqt_par_QGraphicsItem( 2 ), &iOk ) ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QGRAPHICSITEM_ITEMTRANSFORM FP=hb_retptrGC( hbqt_gcAllocate_QTransform( new QTransform( ( p )->itemTransform( hbqt_par_QGraphicsItem( 2 ), &iOk ) ), true ) ); p is NULL" ) );
   }

   hb_stornl( iOk, 3 );
}

/*
 * QPointF mapFromItem ( const QGraphicsItem * item, const QPointF & point ) const
 */
HB_FUNC( QT_QGRAPHICSITEM_MAPFROMITEM )
{
   QGraphicsItem * p = hbqt_par_QGraphicsItem( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QPointF( new QPointF( ( p )->mapFromItem( hbqt_par_QGraphicsItem( 2 ), *hbqt_par_QPointF( 3 ) ) ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QGRAPHICSITEM_MAPFROMITEM FP=hb_retptrGC( hbqt_gcAllocate_QPointF( new QPointF( ( p )->mapFromItem( hbqt_par_QGraphicsItem( 2 ), *hbqt_par_QPointF( 3 ) ) ), true ) ); p is NULL" ) );
   }
}

/*
 * QPolygonF mapFromItem ( const QGraphicsItem * item, const QRectF & rect ) const
 */
HB_FUNC( QT_QGRAPHICSITEM_MAPFROMITEM_1 )
{
   QGraphicsItem * p = hbqt_par_QGraphicsItem( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QPolygonF( new QPolygonF( ( p )->mapFromItem( hbqt_par_QGraphicsItem( 2 ), *hbqt_par_QRectF( 3 ) ) ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QGRAPHICSITEM_MAPFROMITEM_1 FP=hb_retptrGC( hbqt_gcAllocate_QPolygonF( new QPolygonF( ( p )->mapFromItem( hbqt_par_QGraphicsItem( 2 ), *hbqt_par_QRectF( 3 ) ) ), true ) ); p is NULL" ) );
   }
}

/*
 * QPolygonF mapFromItem ( const QGraphicsItem * item, const QPolygonF & polygon ) const
 */
HB_FUNC( QT_QGRAPHICSITEM_MAPFROMITEM_2 )
{
   QGraphicsItem * p = hbqt_par_QGraphicsItem( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QPolygonF( new QPolygonF( ( p )->mapFromItem( hbqt_par_QGraphicsItem( 2 ), *hbqt_par_QPolygonF( 3 ) ) ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QGRAPHICSITEM_MAPFROMITEM_2 FP=hb_retptrGC( hbqt_gcAllocate_QPolygonF( new QPolygonF( ( p )->mapFromItem( hbqt_par_QGraphicsItem( 2 ), *hbqt_par_QPolygonF( 3 ) ) ), true ) ); p is NULL" ) );
   }
}

/*
 * QPainterPath mapFromItem ( const QGraphicsItem * item, const QPainterPath & path ) const
 */
HB_FUNC( QT_QGRAPHICSITEM_MAPFROMITEM_3 )
{
   QGraphicsItem * p = hbqt_par_QGraphicsItem( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QPainterPath( new QPainterPath( ( p )->mapFromItem( hbqt_par_QGraphicsItem( 2 ), *hbqt_par_QPainterPath( 3 ) ) ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QGRAPHICSITEM_MAPFROMITEM_3 FP=hb_retptrGC( hbqt_gcAllocate_QPainterPath( new QPainterPath( ( p )->mapFromItem( hbqt_par_QGraphicsItem( 2 ), *hbqt_par_QPainterPath( 3 ) ) ), true ) ); p is NULL" ) );
   }
}

/*
 * QPolygonF mapFromItem ( const QGraphicsItem * item, qreal x, qreal y, qreal w, qreal h ) const
 */
HB_FUNC( QT_QGRAPHICSITEM_MAPFROMITEM_4 )
{
   QGraphicsItem * p = hbqt_par_QGraphicsItem( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QPolygonF( new QPolygonF( ( p )->mapFromItem( hbqt_par_QGraphicsItem( 2 ), hb_parnd( 3 ), hb_parnd( 4 ), hb_parnd( 5 ), hb_parnd( 6 ) ) ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QGRAPHICSITEM_MAPFROMITEM_4 FP=hb_retptrGC( hbqt_gcAllocate_QPolygonF( new QPolygonF( ( p )->mapFromItem( hbqt_par_QGraphicsItem( 2 ), hb_parnd( 3 ), hb_parnd( 4 ), hb_parnd( 5 ), hb_parnd( 6 ) ) ), true ) ); p is NULL" ) );
   }
}

/*
 * QPointF mapFromItem ( const QGraphicsItem * item, qreal x, qreal y ) const
 */
HB_FUNC( QT_QGRAPHICSITEM_MAPFROMITEM_5 )
{
   QGraphicsItem * p = hbqt_par_QGraphicsItem( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QPointF( new QPointF( ( p )->mapFromItem( hbqt_par_QGraphicsItem( 2 ), hb_parnd( 3 ), hb_parnd( 4 ) ) ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QGRAPHICSITEM_MAPFROMITEM_5 FP=hb_retptrGC( hbqt_gcAllocate_QPointF( new QPointF( ( p )->mapFromItem( hbqt_par_QGraphicsItem( 2 ), hb_parnd( 3 ), hb_parnd( 4 ) ) ), true ) ); p is NULL" ) );
   }
}

/*
 * QPointF mapFromParent ( const QPointF & point ) const
 */
HB_FUNC( QT_QGRAPHICSITEM_MAPFROMPARENT )
{
   QGraphicsItem * p = hbqt_par_QGraphicsItem( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QPointF( new QPointF( ( p )->mapFromParent( *hbqt_par_QPointF( 2 ) ) ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QGRAPHICSITEM_MAPFROMPARENT FP=hb_retptrGC( hbqt_gcAllocate_QPointF( new QPointF( ( p )->mapFromParent( *hbqt_par_QPointF( 2 ) ) ), true ) ); p is NULL" ) );
   }
}

/*
 * QPolygonF mapFromParent ( const QRectF & rect ) const
 */
HB_FUNC( QT_QGRAPHICSITEM_MAPFROMPARENT_1 )
{
   QGraphicsItem * p = hbqt_par_QGraphicsItem( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QPolygonF( new QPolygonF( ( p )->mapFromParent( *hbqt_par_QRectF( 2 ) ) ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QGRAPHICSITEM_MAPFROMPARENT_1 FP=hb_retptrGC( hbqt_gcAllocate_QPolygonF( new QPolygonF( ( p )->mapFromParent( *hbqt_par_QRectF( 2 ) ) ), true ) ); p is NULL" ) );
   }
}

/*
 * QPolygonF mapFromParent ( const QPolygonF & polygon ) const
 */
HB_FUNC( QT_QGRAPHICSITEM_MAPFROMPARENT_2 )
{
   QGraphicsItem * p = hbqt_par_QGraphicsItem( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QPolygonF( new QPolygonF( ( p )->mapFromParent( *hbqt_par_QPolygonF( 2 ) ) ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QGRAPHICSITEM_MAPFROMPARENT_2 FP=hb_retptrGC( hbqt_gcAllocate_QPolygonF( new QPolygonF( ( p )->mapFromParent( *hbqt_par_QPolygonF( 2 ) ) ), true ) ); p is NULL" ) );
   }
}

/*
 * QPainterPath mapFromParent ( const QPainterPath & path ) const
 */
HB_FUNC( QT_QGRAPHICSITEM_MAPFROMPARENT_3 )
{
   QGraphicsItem * p = hbqt_par_QGraphicsItem( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QPainterPath( new QPainterPath( ( p )->mapFromParent( *hbqt_par_QPainterPath( 2 ) ) ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QGRAPHICSITEM_MAPFROMPARENT_3 FP=hb_retptrGC( hbqt_gcAllocate_QPainterPath( new QPainterPath( ( p )->mapFromParent( *hbqt_par_QPainterPath( 2 ) ) ), true ) ); p is NULL" ) );
   }
}

/*
 * QPolygonF mapFromParent ( qreal x, qreal y, qreal w, qreal h ) const
 */
HB_FUNC( QT_QGRAPHICSITEM_MAPFROMPARENT_4 )
{
   QGraphicsItem * p = hbqt_par_QGraphicsItem( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QPolygonF( new QPolygonF( ( p )->mapFromParent( hb_parnd( 2 ), hb_parnd( 3 ), hb_parnd( 4 ), hb_parnd( 5 ) ) ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QGRAPHICSITEM_MAPFROMPARENT_4 FP=hb_retptrGC( hbqt_gcAllocate_QPolygonF( new QPolygonF( ( p )->mapFromParent( hb_parnd( 2 ), hb_parnd( 3 ), hb_parnd( 4 ), hb_parnd( 5 ) ) ), true ) ); p is NULL" ) );
   }
}

/*
 * QPointF mapFromParent ( qreal x, qreal y ) const
 */
HB_FUNC( QT_QGRAPHICSITEM_MAPFROMPARENT_5 )
{
   QGraphicsItem * p = hbqt_par_QGraphicsItem( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QPointF( new QPointF( ( p )->mapFromParent( hb_parnd( 2 ), hb_parnd( 3 ) ) ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QGRAPHICSITEM_MAPFROMPARENT_5 FP=hb_retptrGC( hbqt_gcAllocate_QPointF( new QPointF( ( p )->mapFromParent( hb_parnd( 2 ), hb_parnd( 3 ) ) ), true ) ); p is NULL" ) );
   }
}

/*
 * QPointF mapFromScene ( const QPointF & point ) const
 */
HB_FUNC( QT_QGRAPHICSITEM_MAPFROMSCENE )
{
   QGraphicsItem * p = hbqt_par_QGraphicsItem( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QPointF( new QPointF( ( p )->mapFromScene( *hbqt_par_QPointF( 2 ) ) ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QGRAPHICSITEM_MAPFROMSCENE FP=hb_retptrGC( hbqt_gcAllocate_QPointF( new QPointF( ( p )->mapFromScene( *hbqt_par_QPointF( 2 ) ) ), true ) ); p is NULL" ) );
   }
}

/*
 * QPolygonF mapFromScene ( const QRectF & rect ) const
 */
HB_FUNC( QT_QGRAPHICSITEM_MAPFROMSCENE_1 )
{
   QGraphicsItem * p = hbqt_par_QGraphicsItem( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QPolygonF( new QPolygonF( ( p )->mapFromScene( *hbqt_par_QRectF( 2 ) ) ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QGRAPHICSITEM_MAPFROMSCENE_1 FP=hb_retptrGC( hbqt_gcAllocate_QPolygonF( new QPolygonF( ( p )->mapFromScene( *hbqt_par_QRectF( 2 ) ) ), true ) ); p is NULL" ) );
   }
}

/*
 * QPolygonF mapFromScene ( const QPolygonF & polygon ) const
 */
HB_FUNC( QT_QGRAPHICSITEM_MAPFROMSCENE_2 )
{
   QGraphicsItem * p = hbqt_par_QGraphicsItem( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QPolygonF( new QPolygonF( ( p )->mapFromScene( *hbqt_par_QPolygonF( 2 ) ) ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QGRAPHICSITEM_MAPFROMSCENE_2 FP=hb_retptrGC( hbqt_gcAllocate_QPolygonF( new QPolygonF( ( p )->mapFromScene( *hbqt_par_QPolygonF( 2 ) ) ), true ) ); p is NULL" ) );
   }
}

/*
 * QPainterPath mapFromScene ( const QPainterPath & path ) const
 */
HB_FUNC( QT_QGRAPHICSITEM_MAPFROMSCENE_3 )
{
   QGraphicsItem * p = hbqt_par_QGraphicsItem( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QPainterPath( new QPainterPath( ( p )->mapFromScene( *hbqt_par_QPainterPath( 2 ) ) ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QGRAPHICSITEM_MAPFROMSCENE_3 FP=hb_retptrGC( hbqt_gcAllocate_QPainterPath( new QPainterPath( ( p )->mapFromScene( *hbqt_par_QPainterPath( 2 ) ) ), true ) ); p is NULL" ) );
   }
}

/*
 * QPolygonF mapFromScene ( qreal x, qreal y, qreal w, qreal h ) const
 */
HB_FUNC( QT_QGRAPHICSITEM_MAPFROMSCENE_4 )
{
   QGraphicsItem * p = hbqt_par_QGraphicsItem( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QPolygonF( new QPolygonF( ( p )->mapFromScene( hb_parnd( 2 ), hb_parnd( 3 ), hb_parnd( 4 ), hb_parnd( 5 ) ) ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QGRAPHICSITEM_MAPFROMSCENE_4 FP=hb_retptrGC( hbqt_gcAllocate_QPolygonF( new QPolygonF( ( p )->mapFromScene( hb_parnd( 2 ), hb_parnd( 3 ), hb_parnd( 4 ), hb_parnd( 5 ) ) ), true ) ); p is NULL" ) );
   }
}

/*
 * QPointF mapFromScene ( qreal x, qreal y ) const
 */
HB_FUNC( QT_QGRAPHICSITEM_MAPFROMSCENE_5 )
{
   QGraphicsItem * p = hbqt_par_QGraphicsItem( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QPointF( new QPointF( ( p )->mapFromScene( hb_parnd( 2 ), hb_parnd( 3 ) ) ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QGRAPHICSITEM_MAPFROMSCENE_5 FP=hb_retptrGC( hbqt_gcAllocate_QPointF( new QPointF( ( p )->mapFromScene( hb_parnd( 2 ), hb_parnd( 3 ) ) ), true ) ); p is NULL" ) );
   }
}

/*
 * QRectF mapRectFromItem ( const QGraphicsItem * item, const QRectF & rect ) const
 */
HB_FUNC( QT_QGRAPHICSITEM_MAPRECTFROMITEM )
{
   QGraphicsItem * p = hbqt_par_QGraphicsItem( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QRectF( new QRectF( ( p )->mapRectFromItem( hbqt_par_QGraphicsItem( 2 ), *hbqt_par_QRectF( 3 ) ) ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QGRAPHICSITEM_MAPRECTFROMITEM FP=hb_retptrGC( hbqt_gcAllocate_QRectF( new QRectF( ( p )->mapRectFromItem( hbqt_par_QGraphicsItem( 2 ), *hbqt_par_QRectF( 3 ) ) ), true ) ); p is NULL" ) );
   }
}

/*
 * QRectF mapRectFromItem ( const QGraphicsItem * item, qreal x, qreal y, qreal w, qreal h ) const
 */
HB_FUNC( QT_QGRAPHICSITEM_MAPRECTFROMITEM_1 )
{
   QGraphicsItem * p = hbqt_par_QGraphicsItem( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QRectF( new QRectF( ( p )->mapRectFromItem( hbqt_par_QGraphicsItem( 2 ), hb_parnd( 3 ), hb_parnd( 4 ), hb_parnd( 5 ), hb_parnd( 6 ) ) ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QGRAPHICSITEM_MAPRECTFROMITEM_1 FP=hb_retptrGC( hbqt_gcAllocate_QRectF( new QRectF( ( p )->mapRectFromItem( hbqt_par_QGraphicsItem( 2 ), hb_parnd( 3 ), hb_parnd( 4 ), hb_parnd( 5 ), hb_parnd( 6 ) ) ), true ) ); p is NULL" ) );
   }
}

/*
 * QRectF mapRectFromParent ( const QRectF & rect ) const
 */
HB_FUNC( QT_QGRAPHICSITEM_MAPRECTFROMPARENT )
{
   QGraphicsItem * p = hbqt_par_QGraphicsItem( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QRectF( new QRectF( ( p )->mapRectFromParent( *hbqt_par_QRectF( 2 ) ) ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QGRAPHICSITEM_MAPRECTFROMPARENT FP=hb_retptrGC( hbqt_gcAllocate_QRectF( new QRectF( ( p )->mapRectFromParent( *hbqt_par_QRectF( 2 ) ) ), true ) ); p is NULL" ) );
   }
}

/*
 * QRectF mapRectFromParent ( qreal x, qreal y, qreal w, qreal h ) const
 */
HB_FUNC( QT_QGRAPHICSITEM_MAPRECTFROMPARENT_1 )
{
   QGraphicsItem * p = hbqt_par_QGraphicsItem( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QRectF( new QRectF( ( p )->mapRectFromParent( hb_parnd( 2 ), hb_parnd( 3 ), hb_parnd( 4 ), hb_parnd( 5 ) ) ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QGRAPHICSITEM_MAPRECTFROMPARENT_1 FP=hb_retptrGC( hbqt_gcAllocate_QRectF( new QRectF( ( p )->mapRectFromParent( hb_parnd( 2 ), hb_parnd( 3 ), hb_parnd( 4 ), hb_parnd( 5 ) ) ), true ) ); p is NULL" ) );
   }
}

/*
 * QRectF mapRectFromScene ( const QRectF & rect ) const
 */
HB_FUNC( QT_QGRAPHICSITEM_MAPRECTFROMSCENE )
{
   QGraphicsItem * p = hbqt_par_QGraphicsItem( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QRectF( new QRectF( ( p )->mapRectFromScene( *hbqt_par_QRectF( 2 ) ) ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QGRAPHICSITEM_MAPRECTFROMSCENE FP=hb_retptrGC( hbqt_gcAllocate_QRectF( new QRectF( ( p )->mapRectFromScene( *hbqt_par_QRectF( 2 ) ) ), true ) ); p is NULL" ) );
   }
}

/*
 * QRectF mapRectFromScene ( qreal x, qreal y, qreal w, qreal h ) const
 */
HB_FUNC( QT_QGRAPHICSITEM_MAPRECTFROMSCENE_1 )
{
   QGraphicsItem * p = hbqt_par_QGraphicsItem( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QRectF( new QRectF( ( p )->mapRectFromScene( hb_parnd( 2 ), hb_parnd( 3 ), hb_parnd( 4 ), hb_parnd( 5 ) ) ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QGRAPHICSITEM_MAPRECTFROMSCENE_1 FP=hb_retptrGC( hbqt_gcAllocate_QRectF( new QRectF( ( p )->mapRectFromScene( hb_parnd( 2 ), hb_parnd( 3 ), hb_parnd( 4 ), hb_parnd( 5 ) ) ), true ) ); p is NULL" ) );
   }
}

/*
 * QRectF mapRectToItem ( const QGraphicsItem * item, const QRectF & rect ) const
 */
HB_FUNC( QT_QGRAPHICSITEM_MAPRECTTOITEM )
{
   QGraphicsItem * p = hbqt_par_QGraphicsItem( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QRectF( new QRectF( ( p )->mapRectToItem( hbqt_par_QGraphicsItem( 2 ), *hbqt_par_QRectF( 3 ) ) ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QGRAPHICSITEM_MAPRECTTOITEM FP=hb_retptrGC( hbqt_gcAllocate_QRectF( new QRectF( ( p )->mapRectToItem( hbqt_par_QGraphicsItem( 2 ), *hbqt_par_QRectF( 3 ) ) ), true ) ); p is NULL" ) );
   }
}

/*
 * QRectF mapRectToItem ( const QGraphicsItem * item, qreal x, qreal y, qreal w, qreal h ) const
 */
HB_FUNC( QT_QGRAPHICSITEM_MAPRECTTOITEM_1 )
{
   QGraphicsItem * p = hbqt_par_QGraphicsItem( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QRectF( new QRectF( ( p )->mapRectToItem( hbqt_par_QGraphicsItem( 2 ), hb_parnd( 3 ), hb_parnd( 4 ), hb_parnd( 5 ), hb_parnd( 6 ) ) ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QGRAPHICSITEM_MAPRECTTOITEM_1 FP=hb_retptrGC( hbqt_gcAllocate_QRectF( new QRectF( ( p )->mapRectToItem( hbqt_par_QGraphicsItem( 2 ), hb_parnd( 3 ), hb_parnd( 4 ), hb_parnd( 5 ), hb_parnd( 6 ) ) ), true ) ); p is NULL" ) );
   }
}

/*
 * QRectF mapRectToParent ( const QRectF & rect ) const
 */
HB_FUNC( QT_QGRAPHICSITEM_MAPRECTTOPARENT )
{
   QGraphicsItem * p = hbqt_par_QGraphicsItem( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QRectF( new QRectF( ( p )->mapRectToParent( *hbqt_par_QRectF( 2 ) ) ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QGRAPHICSITEM_MAPRECTTOPARENT FP=hb_retptrGC( hbqt_gcAllocate_QRectF( new QRectF( ( p )->mapRectToParent( *hbqt_par_QRectF( 2 ) ) ), true ) ); p is NULL" ) );
   }
}

/*
 * QRectF mapRectToParent ( qreal x, qreal y, qreal w, qreal h ) const
 */
HB_FUNC( QT_QGRAPHICSITEM_MAPRECTTOPARENT_1 )
{
   QGraphicsItem * p = hbqt_par_QGraphicsItem( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QRectF( new QRectF( ( p )->mapRectToParent( hb_parnd( 2 ), hb_parnd( 3 ), hb_parnd( 4 ), hb_parnd( 5 ) ) ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QGRAPHICSITEM_MAPRECTTOPARENT_1 FP=hb_retptrGC( hbqt_gcAllocate_QRectF( new QRectF( ( p )->mapRectToParent( hb_parnd( 2 ), hb_parnd( 3 ), hb_parnd( 4 ), hb_parnd( 5 ) ) ), true ) ); p is NULL" ) );
   }
}

/*
 * QRectF mapRectToScene ( const QRectF & rect ) const
 */
HB_FUNC( QT_QGRAPHICSITEM_MAPRECTTOSCENE )
{
   QGraphicsItem * p = hbqt_par_QGraphicsItem( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QRectF( new QRectF( ( p )->mapRectToScene( *hbqt_par_QRectF( 2 ) ) ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QGRAPHICSITEM_MAPRECTTOSCENE FP=hb_retptrGC( hbqt_gcAllocate_QRectF( new QRectF( ( p )->mapRectToScene( *hbqt_par_QRectF( 2 ) ) ), true ) ); p is NULL" ) );
   }
}

/*
 * QRectF mapRectToScene ( qreal x, qreal y, qreal w, qreal h ) const
 */
HB_FUNC( QT_QGRAPHICSITEM_MAPRECTTOSCENE_1 )
{
   QGraphicsItem * p = hbqt_par_QGraphicsItem( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QRectF( new QRectF( ( p )->mapRectToScene( hb_parnd( 2 ), hb_parnd( 3 ), hb_parnd( 4 ), hb_parnd( 5 ) ) ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QGRAPHICSITEM_MAPRECTTOSCENE_1 FP=hb_retptrGC( hbqt_gcAllocate_QRectF( new QRectF( ( p )->mapRectToScene( hb_parnd( 2 ), hb_parnd( 3 ), hb_parnd( 4 ), hb_parnd( 5 ) ) ), true ) ); p is NULL" ) );
   }
}

/*
 * QPointF mapToItem ( const QGraphicsItem * item, const QPointF & point ) const
 */
HB_FUNC( QT_QGRAPHICSITEM_MAPTOITEM )
{
   QGraphicsItem * p = hbqt_par_QGraphicsItem( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QPointF( new QPointF( ( p )->mapToItem( hbqt_par_QGraphicsItem( 2 ), *hbqt_par_QPointF( 3 ) ) ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QGRAPHICSITEM_MAPTOITEM FP=hb_retptrGC( hbqt_gcAllocate_QPointF( new QPointF( ( p )->mapToItem( hbqt_par_QGraphicsItem( 2 ), *hbqt_par_QPointF( 3 ) ) ), true ) ); p is NULL" ) );
   }
}

/*
 * QPolygonF mapToItem ( const QGraphicsItem * item, const QRectF & rect ) const
 */
HB_FUNC( QT_QGRAPHICSITEM_MAPTOITEM_1 )
{
   QGraphicsItem * p = hbqt_par_QGraphicsItem( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QPolygonF( new QPolygonF( ( p )->mapToItem( hbqt_par_QGraphicsItem( 2 ), *hbqt_par_QRectF( 3 ) ) ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QGRAPHICSITEM_MAPTOITEM_1 FP=hb_retptrGC( hbqt_gcAllocate_QPolygonF( new QPolygonF( ( p )->mapToItem( hbqt_par_QGraphicsItem( 2 ), *hbqt_par_QRectF( 3 ) ) ), true ) ); p is NULL" ) );
   }
}

/*
 * QPolygonF mapToItem ( const QGraphicsItem * item, const QPolygonF & polygon ) const
 */
HB_FUNC( QT_QGRAPHICSITEM_MAPTOITEM_2 )
{
   QGraphicsItem * p = hbqt_par_QGraphicsItem( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QPolygonF( new QPolygonF( ( p )->mapToItem( hbqt_par_QGraphicsItem( 2 ), *hbqt_par_QPolygonF( 3 ) ) ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QGRAPHICSITEM_MAPTOITEM_2 FP=hb_retptrGC( hbqt_gcAllocate_QPolygonF( new QPolygonF( ( p )->mapToItem( hbqt_par_QGraphicsItem( 2 ), *hbqt_par_QPolygonF( 3 ) ) ), true ) ); p is NULL" ) );
   }
}

/*
 * QPainterPath mapToItem ( const QGraphicsItem * item, const QPainterPath & path ) const
 */
HB_FUNC( QT_QGRAPHICSITEM_MAPTOITEM_3 )
{
   QGraphicsItem * p = hbqt_par_QGraphicsItem( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QPainterPath( new QPainterPath( ( p )->mapToItem( hbqt_par_QGraphicsItem( 2 ), *hbqt_par_QPainterPath( 3 ) ) ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QGRAPHICSITEM_MAPTOITEM_3 FP=hb_retptrGC( hbqt_gcAllocate_QPainterPath( new QPainterPath( ( p )->mapToItem( hbqt_par_QGraphicsItem( 2 ), *hbqt_par_QPainterPath( 3 ) ) ), true ) ); p is NULL" ) );
   }
}

/*
 * QPolygonF mapToItem ( const QGraphicsItem * item, qreal x, qreal y, qreal w, qreal h ) const
 */
HB_FUNC( QT_QGRAPHICSITEM_MAPTOITEM_4 )
{
   QGraphicsItem * p = hbqt_par_QGraphicsItem( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QPolygonF( new QPolygonF( ( p )->mapToItem( hbqt_par_QGraphicsItem( 2 ), hb_parnd( 3 ), hb_parnd( 4 ), hb_parnd( 5 ), hb_parnd( 6 ) ) ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QGRAPHICSITEM_MAPTOITEM_4 FP=hb_retptrGC( hbqt_gcAllocate_QPolygonF( new QPolygonF( ( p )->mapToItem( hbqt_par_QGraphicsItem( 2 ), hb_parnd( 3 ), hb_parnd( 4 ), hb_parnd( 5 ), hb_parnd( 6 ) ) ), true ) ); p is NULL" ) );
   }
}

/*
 * QPointF mapToItem ( const QGraphicsItem * item, qreal x, qreal y ) const
 */
HB_FUNC( QT_QGRAPHICSITEM_MAPTOITEM_5 )
{
   QGraphicsItem * p = hbqt_par_QGraphicsItem( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QPointF( new QPointF( ( p )->mapToItem( hbqt_par_QGraphicsItem( 2 ), hb_parnd( 3 ), hb_parnd( 4 ) ) ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QGRAPHICSITEM_MAPTOITEM_5 FP=hb_retptrGC( hbqt_gcAllocate_QPointF( new QPointF( ( p )->mapToItem( hbqt_par_QGraphicsItem( 2 ), hb_parnd( 3 ), hb_parnd( 4 ) ) ), true ) ); p is NULL" ) );
   }
}

/*
 * QPointF mapToParent ( const QPointF & point ) const
 */
HB_FUNC( QT_QGRAPHICSITEM_MAPTOPARENT )
{
   QGraphicsItem * p = hbqt_par_QGraphicsItem( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QPointF( new QPointF( ( p )->mapToParent( *hbqt_par_QPointF( 2 ) ) ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QGRAPHICSITEM_MAPTOPARENT FP=hb_retptrGC( hbqt_gcAllocate_QPointF( new QPointF( ( p )->mapToParent( *hbqt_par_QPointF( 2 ) ) ), true ) ); p is NULL" ) );
   }
}

/*
 * QPolygonF mapToParent ( const QRectF & rect ) const
 */
HB_FUNC( QT_QGRAPHICSITEM_MAPTOPARENT_1 )
{
   QGraphicsItem * p = hbqt_par_QGraphicsItem( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QPolygonF( new QPolygonF( ( p )->mapToParent( *hbqt_par_QRectF( 2 ) ) ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QGRAPHICSITEM_MAPTOPARENT_1 FP=hb_retptrGC( hbqt_gcAllocate_QPolygonF( new QPolygonF( ( p )->mapToParent( *hbqt_par_QRectF( 2 ) ) ), true ) ); p is NULL" ) );
   }
}

/*
 * QPolygonF mapToParent ( const QPolygonF & polygon ) const
 */
HB_FUNC( QT_QGRAPHICSITEM_MAPTOPARENT_2 )
{
   QGraphicsItem * p = hbqt_par_QGraphicsItem( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QPolygonF( new QPolygonF( ( p )->mapToParent( *hbqt_par_QPolygonF( 2 ) ) ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QGRAPHICSITEM_MAPTOPARENT_2 FP=hb_retptrGC( hbqt_gcAllocate_QPolygonF( new QPolygonF( ( p )->mapToParent( *hbqt_par_QPolygonF( 2 ) ) ), true ) ); p is NULL" ) );
   }
}

/*
 * QPainterPath mapToParent ( const QPainterPath & path ) const
 */
HB_FUNC( QT_QGRAPHICSITEM_MAPTOPARENT_3 )
{
   QGraphicsItem * p = hbqt_par_QGraphicsItem( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QPainterPath( new QPainterPath( ( p )->mapToParent( *hbqt_par_QPainterPath( 2 ) ) ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QGRAPHICSITEM_MAPTOPARENT_3 FP=hb_retptrGC( hbqt_gcAllocate_QPainterPath( new QPainterPath( ( p )->mapToParent( *hbqt_par_QPainterPath( 2 ) ) ), true ) ); p is NULL" ) );
   }
}

/*
 * QPolygonF mapToParent ( qreal x, qreal y, qreal w, qreal h ) const
 */
HB_FUNC( QT_QGRAPHICSITEM_MAPTOPARENT_4 )
{
   QGraphicsItem * p = hbqt_par_QGraphicsItem( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QPolygonF( new QPolygonF( ( p )->mapToParent( hb_parnd( 2 ), hb_parnd( 3 ), hb_parnd( 4 ), hb_parnd( 5 ) ) ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QGRAPHICSITEM_MAPTOPARENT_4 FP=hb_retptrGC( hbqt_gcAllocate_QPolygonF( new QPolygonF( ( p )->mapToParent( hb_parnd( 2 ), hb_parnd( 3 ), hb_parnd( 4 ), hb_parnd( 5 ) ) ), true ) ); p is NULL" ) );
   }
}

/*
 * QPointF mapToParent ( qreal x, qreal y ) const
 */
HB_FUNC( QT_QGRAPHICSITEM_MAPTOPARENT_5 )
{
   QGraphicsItem * p = hbqt_par_QGraphicsItem( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QPointF( new QPointF( ( p )->mapToParent( hb_parnd( 2 ), hb_parnd( 3 ) ) ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QGRAPHICSITEM_MAPTOPARENT_5 FP=hb_retptrGC( hbqt_gcAllocate_QPointF( new QPointF( ( p )->mapToParent( hb_parnd( 2 ), hb_parnd( 3 ) ) ), true ) ); p is NULL" ) );
   }
}

/*
 * QPointF mapToScene ( const QPointF & point ) const
 */
HB_FUNC( QT_QGRAPHICSITEM_MAPTOSCENE )
{
   QGraphicsItem * p = hbqt_par_QGraphicsItem( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QPointF( new QPointF( ( p )->mapToScene( *hbqt_par_QPointF( 2 ) ) ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QGRAPHICSITEM_MAPTOSCENE FP=hb_retptrGC( hbqt_gcAllocate_QPointF( new QPointF( ( p )->mapToScene( *hbqt_par_QPointF( 2 ) ) ), true ) ); p is NULL" ) );
   }
}

/*
 * QPolygonF mapToScene ( const QRectF & rect ) const
 */
HB_FUNC( QT_QGRAPHICSITEM_MAPTOSCENE_1 )
{
   QGraphicsItem * p = hbqt_par_QGraphicsItem( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QPolygonF( new QPolygonF( ( p )->mapToScene( *hbqt_par_QRectF( 2 ) ) ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QGRAPHICSITEM_MAPTOSCENE_1 FP=hb_retptrGC( hbqt_gcAllocate_QPolygonF( new QPolygonF( ( p )->mapToScene( *hbqt_par_QRectF( 2 ) ) ), true ) ); p is NULL" ) );
   }
}

/*
 * QPolygonF mapToScene ( const QPolygonF & polygon ) const
 */
HB_FUNC( QT_QGRAPHICSITEM_MAPTOSCENE_2 )
{
   QGraphicsItem * p = hbqt_par_QGraphicsItem( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QPolygonF( new QPolygonF( ( p )->mapToScene( *hbqt_par_QPolygonF( 2 ) ) ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QGRAPHICSITEM_MAPTOSCENE_2 FP=hb_retptrGC( hbqt_gcAllocate_QPolygonF( new QPolygonF( ( p )->mapToScene( *hbqt_par_QPolygonF( 2 ) ) ), true ) ); p is NULL" ) );
   }
}

/*
 * QPainterPath mapToScene ( const QPainterPath & path ) const
 */
HB_FUNC( QT_QGRAPHICSITEM_MAPTOSCENE_3 )
{
   QGraphicsItem * p = hbqt_par_QGraphicsItem( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QPainterPath( new QPainterPath( ( p )->mapToScene( *hbqt_par_QPainterPath( 2 ) ) ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QGRAPHICSITEM_MAPTOSCENE_3 FP=hb_retptrGC( hbqt_gcAllocate_QPainterPath( new QPainterPath( ( p )->mapToScene( *hbqt_par_QPainterPath( 2 ) ) ), true ) ); p is NULL" ) );
   }
}

/*
 * QPolygonF mapToScene ( qreal x, qreal y, qreal w, qreal h ) const
 */
HB_FUNC( QT_QGRAPHICSITEM_MAPTOSCENE_4 )
{
   QGraphicsItem * p = hbqt_par_QGraphicsItem( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QPolygonF( new QPolygonF( ( p )->mapToScene( hb_parnd( 2 ), hb_parnd( 3 ), hb_parnd( 4 ), hb_parnd( 5 ) ) ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QGRAPHICSITEM_MAPTOSCENE_4 FP=hb_retptrGC( hbqt_gcAllocate_QPolygonF( new QPolygonF( ( p )->mapToScene( hb_parnd( 2 ), hb_parnd( 3 ), hb_parnd( 4 ), hb_parnd( 5 ) ) ), true ) ); p is NULL" ) );
   }
}

/*
 * QPointF mapToScene ( qreal x, qreal y ) const
 */
HB_FUNC( QT_QGRAPHICSITEM_MAPTOSCENE_5 )
{
   QGraphicsItem * p = hbqt_par_QGraphicsItem( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QPointF( new QPointF( ( p )->mapToScene( hb_parnd( 2 ), hb_parnd( 3 ) ) ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QGRAPHICSITEM_MAPTOSCENE_5 FP=hb_retptrGC( hbqt_gcAllocate_QPointF( new QPointF( ( p )->mapToScene( hb_parnd( 2 ), hb_parnd( 3 ) ) ), true ) ); p is NULL" ) );
   }
}

/*
 * void moveBy ( qreal dx, qreal dy )
 */
HB_FUNC( QT_QGRAPHICSITEM_MOVEBY )
{
   QGraphicsItem * p = hbqt_par_QGraphicsItem( 1 );
   if( p )
      ( p )->moveBy( hb_parnd( 2 ), hb_parnd( 3 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QGRAPHICSITEM_MOVEBY FP=( p )->moveBy( hb_parnd( 2 ), hb_parnd( 3 ) ); p is NULL" ) );
   }
}

/*
 * qreal opacity () const
 */
HB_FUNC( QT_QGRAPHICSITEM_OPACITY )
{
   QGraphicsItem * p = hbqt_par_QGraphicsItem( 1 );
   if( p )
      hb_retnd( ( p )->opacity() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QGRAPHICSITEM_OPACITY FP=hb_retnd( ( p )->opacity() ); p is NULL" ) );
   }
}

/*
 * virtual QPainterPath opaqueArea () const
 */
HB_FUNC( QT_QGRAPHICSITEM_OPAQUEAREA )
{
   QGraphicsItem * p = hbqt_par_QGraphicsItem( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QPainterPath( new QPainterPath( ( p )->opaqueArea() ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QGRAPHICSITEM_OPAQUEAREA FP=hb_retptrGC( hbqt_gcAllocate_QPainterPath( new QPainterPath( ( p )->opaqueArea() ), true ) ); p is NULL" ) );
   }
}

/*
 * virtual void paint ( QPainter * painter, const QStyleOptionGraphicsItem * option, QWidget * widget = 0 ) = 0
 */
HB_FUNC( QT_QGRAPHICSITEM_PAINT )
{
   QGraphicsItem * p = hbqt_par_QGraphicsItem( 1 );
   if( p )
      ( p )->paint( hbqt_par_QPainter( 2 ), hbqt_par_QStyleOptionGraphicsItem( 3 ), hbqt_par_QWidget( 4 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QGRAPHICSITEM_PAINT FP=( p )->paint( hbqt_par_QPainter( 2 ), hbqt_par_QStyleOptionGraphicsItem( 3 ), hbqt_par_QWidget( 4 ) ); p is NULL" ) );
   }
}

/*
 * QGraphicsItem * parentItem () const
 */
HB_FUNC( QT_QGRAPHICSITEM_PARENTITEM )
{
   QGraphicsItem * p = hbqt_par_QGraphicsItem( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QGraphicsItem( ( p )->parentItem(), false ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QGRAPHICSITEM_PARENTITEM FP=hb_retptrGC( hbqt_gcAllocate_QGraphicsItem( ( p )->parentItem(), false ) ); p is NULL" ) );
   }
}

/*
 * QGraphicsWidget * parentWidget () const
 */
HB_FUNC( QT_QGRAPHICSITEM_PARENTWIDGET )
{
   QGraphicsItem * p = hbqt_par_QGraphicsItem( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QGraphicsWidget( ( p )->parentWidget(), false ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QGRAPHICSITEM_PARENTWIDGET FP=hb_retptrGC( hbqt_gcAllocate_QGraphicsWidget( ( p )->parentWidget(), false ) ); p is NULL" ) );
   }
}

/*
 * QPointF pos () const
 */
HB_FUNC( QT_QGRAPHICSITEM_POS )
{
   QGraphicsItem * p = hbqt_par_QGraphicsItem( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QPointF( new QPointF( ( p )->pos() ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QGRAPHICSITEM_POS FP=hb_retptrGC( hbqt_gcAllocate_QPointF( new QPointF( ( p )->pos() ), true ) ); p is NULL" ) );
   }
}

/*
 * void removeSceneEventFilter ( QGraphicsItem * filterItem )
 */
HB_FUNC( QT_QGRAPHICSITEM_REMOVESCENEEVENTFILTER )
{
   QGraphicsItem * p = hbqt_par_QGraphicsItem( 1 );
   if( p )
      ( p )->removeSceneEventFilter( hbqt_par_QGraphicsItem( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QGRAPHICSITEM_REMOVESCENEEVENTFILTER FP=( p )->removeSceneEventFilter( hbqt_par_QGraphicsItem( 2 ) ); p is NULL" ) );
   }
}

/*
 * void resetTransform ()
 */
HB_FUNC( QT_QGRAPHICSITEM_RESETTRANSFORM )
{
   QGraphicsItem * p = hbqt_par_QGraphicsItem( 1 );
   if( p )
      ( p )->resetTransform();
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QGRAPHICSITEM_RESETTRANSFORM FP=( p )->resetTransform(); p is NULL" ) );
   }
}

/*
 * void rotate ( qreal angle )
 */
HB_FUNC( QT_QGRAPHICSITEM_ROTATE )
{
   QGraphicsItem * p = hbqt_par_QGraphicsItem( 1 );
   if( p )
      ( p )->rotate( hb_parnd( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QGRAPHICSITEM_ROTATE FP=( p )->rotate( hb_parnd( 2 ) ); p is NULL" ) );
   }
}

/*
 * void scale ( qreal sx, qreal sy )
 */
HB_FUNC( QT_QGRAPHICSITEM_SCALE )
{
   QGraphicsItem * p = hbqt_par_QGraphicsItem( 1 );
   if( p )
      ( p )->scale( hb_parnd( 2 ), hb_parnd( 3 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QGRAPHICSITEM_SCALE FP=( p )->scale( hb_parnd( 2 ), hb_parnd( 3 ) ); p is NULL" ) );
   }
}

/*
 * QGraphicsScene * scene () const
 */
HB_FUNC( QT_QGRAPHICSITEM_SCENE )
{
   QGraphicsItem * p = hbqt_par_QGraphicsItem( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QGraphicsScene( ( p )->scene(), false ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QGRAPHICSITEM_SCENE FP=hb_retptrGC( hbqt_gcAllocate_QGraphicsScene( ( p )->scene(), false ) ); p is NULL" ) );
   }
}

/*
 * QRectF sceneBoundingRect () const
 */
HB_FUNC( QT_QGRAPHICSITEM_SCENEBOUNDINGRECT )
{
   QGraphicsItem * p = hbqt_par_QGraphicsItem( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QRectF( new QRectF( ( p )->sceneBoundingRect() ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QGRAPHICSITEM_SCENEBOUNDINGRECT FP=hb_retptrGC( hbqt_gcAllocate_QRectF( new QRectF( ( p )->sceneBoundingRect() ), true ) ); p is NULL" ) );
   }
}

/*
 * QPointF scenePos () const
 */
HB_FUNC( QT_QGRAPHICSITEM_SCENEPOS )
{
   QGraphicsItem * p = hbqt_par_QGraphicsItem( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QPointF( new QPointF( ( p )->scenePos() ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QGRAPHICSITEM_SCENEPOS FP=hb_retptrGC( hbqt_gcAllocate_QPointF( new QPointF( ( p )->scenePos() ), true ) ); p is NULL" ) );
   }
}

/*
 * QTransform sceneTransform () const
 */
HB_FUNC( QT_QGRAPHICSITEM_SCENETRANSFORM )
{
   QGraphicsItem * p = hbqt_par_QGraphicsItem( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QTransform( new QTransform( ( p )->sceneTransform() ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QGRAPHICSITEM_SCENETRANSFORM FP=hb_retptrGC( hbqt_gcAllocate_QTransform( new QTransform( ( p )->sceneTransform() ), true ) ); p is NULL" ) );
   }
}

/*
 * void scroll ( qreal dx, qreal dy, const QRectF & rect = QRectF() )
 */
HB_FUNC( QT_QGRAPHICSITEM_SCROLL )
{
   QGraphicsItem * p = hbqt_par_QGraphicsItem( 1 );
   if( p )
      ( p )->scroll( hb_parnd( 2 ), hb_parnd( 3 ), ( HB_ISPOINTER( 4 ) ? *hbqt_par_QRectF( 4 ) : QRectF() ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QGRAPHICSITEM_SCROLL FP=( p )->scroll( hb_parnd( 2 ), hb_parnd( 3 ), ( HB_ISPOINTER( 4 ) ? *hbqt_par_QRectF( 4 ) : QRectF() ) ); p is NULL" ) );
   }
}

/*
 * void setAcceptDrops ( bool on )
 */
HB_FUNC( QT_QGRAPHICSITEM_SETACCEPTDROPS )
{
   QGraphicsItem * p = hbqt_par_QGraphicsItem( 1 );
   if( p )
      ( p )->setAcceptDrops( hb_parl( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QGRAPHICSITEM_SETACCEPTDROPS FP=( p )->setAcceptDrops( hb_parl( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setAcceptHoverEvents ( bool enabled )
 */
HB_FUNC( QT_QGRAPHICSITEM_SETACCEPTHOVEREVENTS )
{
   QGraphicsItem * p = hbqt_par_QGraphicsItem( 1 );
   if( p )
      ( p )->setAcceptHoverEvents( hb_parl( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QGRAPHICSITEM_SETACCEPTHOVEREVENTS FP=( p )->setAcceptHoverEvents( hb_parl( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setAcceptedMouseButtons ( Qt::MouseButtons buttons )
 */
HB_FUNC( QT_QGRAPHICSITEM_SETACCEPTEDMOUSEBUTTONS )
{
   QGraphicsItem * p = hbqt_par_QGraphicsItem( 1 );
   if( p )
      ( p )->setAcceptedMouseButtons( ( Qt::MouseButtons ) hb_parni( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QGRAPHICSITEM_SETACCEPTEDMOUSEBUTTONS FP=( p )->setAcceptedMouseButtons( ( Qt::MouseButtons ) hb_parni( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setBoundingRegionGranularity ( qreal granularity )
 */
HB_FUNC( QT_QGRAPHICSITEM_SETBOUNDINGREGIONGRANULARITY )
{
   QGraphicsItem * p = hbqt_par_QGraphicsItem( 1 );
   if( p )
      ( p )->setBoundingRegionGranularity( hb_parnd( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QGRAPHICSITEM_SETBOUNDINGREGIONGRANULARITY FP=( p )->setBoundingRegionGranularity( hb_parnd( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setCacheMode ( CacheMode mode, const QSize & logicalCacheSize = QSize() )
 */
HB_FUNC( QT_QGRAPHICSITEM_SETCACHEMODE )
{
   QGraphicsItem * p = hbqt_par_QGraphicsItem( 1 );
   if( p )
      ( p )->setCacheMode( ( QGraphicsItem::CacheMode ) hb_parni( 2 ), ( HB_ISPOINTER( 3 ) ? *hbqt_par_QSize( 3 ) : QSize() ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QGRAPHICSITEM_SETCACHEMODE FP=( p )->setCacheMode( ( QGraphicsItem::CacheMode ) hb_parni( 2 ), ( HB_ISPOINTER( 3 ) ? *hbqt_par_QSize( 3 ) : QSize() ) ); p is NULL" ) );
   }
}

/*
 * void setCursor ( const QCursor & cursor )
 */
HB_FUNC( QT_QGRAPHICSITEM_SETCURSOR )
{
   QGraphicsItem * p = hbqt_par_QGraphicsItem( 1 );
   if( p )
      ( p )->setCursor( *hbqt_par_QCursor( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QGRAPHICSITEM_SETCURSOR FP=( p )->setCursor( *hbqt_par_QCursor( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setData ( int key, const QVariant & value )
 */
HB_FUNC( QT_QGRAPHICSITEM_SETDATA )
{
   QGraphicsItem * p = hbqt_par_QGraphicsItem( 1 );
   if( p )
      ( p )->setData( hb_parni( 2 ), *hbqt_par_QVariant( 3 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QGRAPHICSITEM_SETDATA FP=( p )->setData( hb_parni( 2 ), *hbqt_par_QVariant( 3 ) ); p is NULL" ) );
   }
}

/*
 * void setEnabled ( bool enabled )
 */
HB_FUNC( QT_QGRAPHICSITEM_SETENABLED )
{
   QGraphicsItem * p = hbqt_par_QGraphicsItem( 1 );
   if( p )
      ( p )->setEnabled( hb_parl( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QGRAPHICSITEM_SETENABLED FP=( p )->setEnabled( hb_parl( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setFlag ( GraphicsItemFlag flag, bool enabled = true )
 */
HB_FUNC( QT_QGRAPHICSITEM_SETFLAG )
{
   QGraphicsItem * p = hbqt_par_QGraphicsItem( 1 );
   if( p )
      ( p )->setFlag( ( QGraphicsItem::GraphicsItemFlag ) hb_parni( 2 ), hb_parl( 3 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QGRAPHICSITEM_SETFLAG FP=( p )->setFlag( ( QGraphicsItem::GraphicsItemFlag ) hb_parni( 2 ), hb_parl( 3 ) ); p is NULL" ) );
   }
}

/*
 * void setFlags ( GraphicsItemFlags flags )
 */
HB_FUNC( QT_QGRAPHICSITEM_SETFLAGS )
{
   QGraphicsItem * p = hbqt_par_QGraphicsItem( 1 );
   if( p )
      ( p )->setFlags( ( QGraphicsItem::GraphicsItemFlags ) hb_parni( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QGRAPHICSITEM_SETFLAGS FP=( p )->setFlags( ( QGraphicsItem::GraphicsItemFlags ) hb_parni( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setFocus ( Qt::FocusReason focusReason = Qt::OtherFocusReason )
 */
HB_FUNC( QT_QGRAPHICSITEM_SETFOCUS )
{
   QGraphicsItem * p = hbqt_par_QGraphicsItem( 1 );
   if( p )
      ( p )->setFocus( ( HB_ISNUM( 2 ) ? ( Qt::FocusReason ) hb_parni( 2 ) : ( Qt::FocusReason ) Qt::OtherFocusReason ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QGRAPHICSITEM_SETFOCUS FP=( p )->setFocus( ( HB_ISNUM( 2 ) ? ( Qt::FocusReason ) hb_parni( 2 ) : ( Qt::FocusReason ) Qt::OtherFocusReason ) ); p is NULL" ) );
   }
}

/*
 * void setGroup ( QGraphicsItemGroup * group )
 */
HB_FUNC( QT_QGRAPHICSITEM_SETGROUP )
{
   QGraphicsItem * p = hbqt_par_QGraphicsItem( 1 );
   if( p )
      ( p )->setGroup( hbqt_par_QGraphicsItemGroup( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QGRAPHICSITEM_SETGROUP FP=( p )->setGroup( hbqt_par_QGraphicsItemGroup( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setHandlesChildEvents ( bool enabled )
 */
HB_FUNC( QT_QGRAPHICSITEM_SETHANDLESCHILDEVENTS )
{
   QGraphicsItem * p = hbqt_par_QGraphicsItem( 1 );
   if( p )
      ( p )->setHandlesChildEvents( hb_parl( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QGRAPHICSITEM_SETHANDLESCHILDEVENTS FP=( p )->setHandlesChildEvents( hb_parl( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setOpacity ( qreal opacity )
 */
HB_FUNC( QT_QGRAPHICSITEM_SETOPACITY )
{
   QGraphicsItem * p = hbqt_par_QGraphicsItem( 1 );
   if( p )
      ( p )->setOpacity( hb_parnd( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QGRAPHICSITEM_SETOPACITY FP=( p )->setOpacity( hb_parnd( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setParentItem ( QGraphicsItem * parent )
 */
HB_FUNC( QT_QGRAPHICSITEM_SETPARENTITEM )
{
   QGraphicsItem * p = hbqt_par_QGraphicsItem( 1 );
   if( p )
      ( p )->setParentItem( hbqt_par_QGraphicsItem( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QGRAPHICSITEM_SETPARENTITEM FP=( p )->setParentItem( hbqt_par_QGraphicsItem( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setPos ( const QPointF & pos )
 */
HB_FUNC( QT_QGRAPHICSITEM_SETPOS )
{
   QGraphicsItem * p = hbqt_par_QGraphicsItem( 1 );
   if( p )
      ( p )->setPos( *hbqt_par_QPointF( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QGRAPHICSITEM_SETPOS FP=( p )->setPos( *hbqt_par_QPointF( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setPos ( qreal x, qreal y )
 */
HB_FUNC( QT_QGRAPHICSITEM_SETPOS_1 )
{
   QGraphicsItem * p = hbqt_par_QGraphicsItem( 1 );
   if( p )
      ( p )->setPos( hb_parnd( 2 ), hb_parnd( 3 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QGRAPHICSITEM_SETPOS_1 FP=( p )->setPos( hb_parnd( 2 ), hb_parnd( 3 ) ); p is NULL" ) );
   }
}

/*
 * void setSelected ( bool selected )
 */
HB_FUNC( QT_QGRAPHICSITEM_SETSELECTED )
{
   QGraphicsItem * p = hbqt_par_QGraphicsItem( 1 );
   if( p )
      ( p )->setSelected( hb_parl( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QGRAPHICSITEM_SETSELECTED FP=( p )->setSelected( hb_parl( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setToolTip ( const QString & toolTip )
 */
HB_FUNC( QT_QGRAPHICSITEM_SETTOOLTIP )
{
   QGraphicsItem * p = hbqt_par_QGraphicsItem( 1 );
   if( p )
      ( p )->setToolTip( hbqt_par_QString( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QGRAPHICSITEM_SETTOOLTIP FP=( p )->setToolTip( hbqt_par_QString( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setTransform ( const QTransform & matrix, bool combine = false )
 */
HB_FUNC( QT_QGRAPHICSITEM_SETTRANSFORM )
{
   QGraphicsItem * p = hbqt_par_QGraphicsItem( 1 );
   if( p )
      ( p )->setTransform( *hbqt_par_QTransform( 2 ), hb_parl( 3 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QGRAPHICSITEM_SETTRANSFORM FP=( p )->setTransform( *hbqt_par_QTransform( 2 ), hb_parl( 3 ) ); p is NULL" ) );
   }
}

/*
 * void setVisible ( bool visible )
 */
HB_FUNC( QT_QGRAPHICSITEM_SETVISIBLE )
{
   QGraphicsItem * p = hbqt_par_QGraphicsItem( 1 );
   if( p )
      ( p )->setVisible( hb_parl( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QGRAPHICSITEM_SETVISIBLE FP=( p )->setVisible( hb_parl( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setZValue ( qreal z )
 */
HB_FUNC( QT_QGRAPHICSITEM_SETZVALUE )
{
   QGraphicsItem * p = hbqt_par_QGraphicsItem( 1 );
   if( p )
      ( p )->setZValue( hb_parnd( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QGRAPHICSITEM_SETZVALUE FP=( p )->setZValue( hb_parnd( 2 ) ); p is NULL" ) );
   }
}

/*
 * virtual QPainterPath shape () const
 */
HB_FUNC( QT_QGRAPHICSITEM_SHAPE )
{
   QGraphicsItem * p = hbqt_par_QGraphicsItem( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QPainterPath( new QPainterPath( ( p )->shape() ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QGRAPHICSITEM_SHAPE FP=hb_retptrGC( hbqt_gcAllocate_QPainterPath( new QPainterPath( ( p )->shape() ), true ) ); p is NULL" ) );
   }
}

/*
 * void shear ( qreal sh, qreal sv )
 */
HB_FUNC( QT_QGRAPHICSITEM_SHEAR )
{
   QGraphicsItem * p = hbqt_par_QGraphicsItem( 1 );
   if( p )
      ( p )->shear( hb_parnd( 2 ), hb_parnd( 3 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QGRAPHICSITEM_SHEAR FP=( p )->shear( hb_parnd( 2 ), hb_parnd( 3 ) ); p is NULL" ) );
   }
}

/*
 * void show ()
 */
HB_FUNC( QT_QGRAPHICSITEM_SHOW )
{
   QGraphicsItem * p = hbqt_par_QGraphicsItem( 1 );
   if( p )
      ( p )->show();
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QGRAPHICSITEM_SHOW FP=( p )->show(); p is NULL" ) );
   }
}

/*
 * QString toolTip () const
 */
HB_FUNC( QT_QGRAPHICSITEM_TOOLTIP )
{
   QGraphicsItem * p = hbqt_par_QGraphicsItem( 1 );
   if( p )
      hb_retc( ( p )->toolTip().toAscii().data() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QGRAPHICSITEM_TOOLTIP FP=hb_retc( ( p )->toolTip().toAscii().data() ); p is NULL" ) );
   }
}

/*
 * QGraphicsItem * topLevelItem () const
 */
HB_FUNC( QT_QGRAPHICSITEM_TOPLEVELITEM )
{
   QGraphicsItem * p = hbqt_par_QGraphicsItem( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QGraphicsItem( ( p )->topLevelItem(), false ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QGRAPHICSITEM_TOPLEVELITEM FP=hb_retptrGC( hbqt_gcAllocate_QGraphicsItem( ( p )->topLevelItem(), false ) ); p is NULL" ) );
   }
}

/*
 * QGraphicsWidget * topLevelWidget () const
 */
HB_FUNC( QT_QGRAPHICSITEM_TOPLEVELWIDGET )
{
   QGraphicsItem * p = hbqt_par_QGraphicsItem( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QGraphicsWidget( ( p )->topLevelWidget(), false ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QGRAPHICSITEM_TOPLEVELWIDGET FP=hb_retptrGC( hbqt_gcAllocate_QGraphicsWidget( ( p )->topLevelWidget(), false ) ); p is NULL" ) );
   }
}

/*
 * QTransform transform () const
 */
HB_FUNC( QT_QGRAPHICSITEM_TRANSFORM )
{
   QGraphicsItem * p = hbqt_par_QGraphicsItem( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QTransform( new QTransform( ( p )->transform() ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QGRAPHICSITEM_TRANSFORM FP=hb_retptrGC( hbqt_gcAllocate_QTransform( new QTransform( ( p )->transform() ), true ) ); p is NULL" ) );
   }
}

/*
 * void translate ( qreal dx, qreal dy )
 */
HB_FUNC( QT_QGRAPHICSITEM_TRANSLATE )
{
   QGraphicsItem * p = hbqt_par_QGraphicsItem( 1 );
   if( p )
      ( p )->translate( hb_parnd( 2 ), hb_parnd( 3 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QGRAPHICSITEM_TRANSLATE FP=( p )->translate( hb_parnd( 2 ), hb_parnd( 3 ) ); p is NULL" ) );
   }
}

/*
 * virtual int type () const
 */
HB_FUNC( QT_QGRAPHICSITEM_TYPE )
{
   QGraphicsItem * p = hbqt_par_QGraphicsItem( 1 );
   if( p )
      hb_retni( ( p )->type() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QGRAPHICSITEM_TYPE FP=hb_retni( ( p )->type() ); p is NULL" ) );
   }
}

/*
 * void ungrabKeyboard ()
 */
HB_FUNC( QT_QGRAPHICSITEM_UNGRABKEYBOARD )
{
   QGraphicsItem * p = hbqt_par_QGraphicsItem( 1 );
   if( p )
      ( p )->ungrabKeyboard();
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QGRAPHICSITEM_UNGRABKEYBOARD FP=( p )->ungrabKeyboard(); p is NULL" ) );
   }
}

/*
 * void ungrabMouse ()
 */
HB_FUNC( QT_QGRAPHICSITEM_UNGRABMOUSE )
{
   QGraphicsItem * p = hbqt_par_QGraphicsItem( 1 );
   if( p )
      ( p )->ungrabMouse();
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QGRAPHICSITEM_UNGRABMOUSE FP=( p )->ungrabMouse(); p is NULL" ) );
   }
}

/*
 * void unsetCursor ()
 */
HB_FUNC( QT_QGRAPHICSITEM_UNSETCURSOR )
{
   QGraphicsItem * p = hbqt_par_QGraphicsItem( 1 );
   if( p )
      ( p )->unsetCursor();
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QGRAPHICSITEM_UNSETCURSOR FP=( p )->unsetCursor(); p is NULL" ) );
   }
}

/*
 * void update ( const QRectF & rect = QRectF() )
 */
HB_FUNC( QT_QGRAPHICSITEM_UPDATE )
{
   QGraphicsItem * p = hbqt_par_QGraphicsItem( 1 );
   if( p )
      ( p )->update( ( HB_ISPOINTER( 2 ) ? *hbqt_par_QRectF( 2 ) : QRectF() ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QGRAPHICSITEM_UPDATE FP=( p )->update( ( HB_ISPOINTER( 2 ) ? *hbqt_par_QRectF( 2 ) : QRectF() ) ); p is NULL" ) );
   }
}

/*
 * void update ( qreal x, qreal y, qreal width, qreal height )
 */
HB_FUNC( QT_QGRAPHICSITEM_UPDATE_1 )
{
   QGraphicsItem * p = hbqt_par_QGraphicsItem( 1 );
   if( p )
      ( p )->update( hb_parnd( 2 ), hb_parnd( 3 ), hb_parnd( 4 ), hb_parnd( 5 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QGRAPHICSITEM_UPDATE_1 FP=( p )->update( hb_parnd( 2 ), hb_parnd( 3 ), hb_parnd( 4 ), hb_parnd( 5 ) ); p is NULL" ) );
   }
}

/*
 * QGraphicsWidget * window () const
 */
HB_FUNC( QT_QGRAPHICSITEM_WINDOW )
{
   QGraphicsItem * p = hbqt_par_QGraphicsItem( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QGraphicsWidget( ( p )->window(), false ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QGRAPHICSITEM_WINDOW FP=hb_retptrGC( hbqt_gcAllocate_QGraphicsWidget( ( p )->window(), false ) ); p is NULL" ) );
   }
}

/*
 * qreal x () const
 */
HB_FUNC( QT_QGRAPHICSITEM_X )
{
   QGraphicsItem * p = hbqt_par_QGraphicsItem( 1 );
   if( p )
      hb_retnd( ( p )->x() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QGRAPHICSITEM_X FP=hb_retnd( ( p )->x() ); p is NULL" ) );
   }
}

/*
 * qreal y () const
 */
HB_FUNC( QT_QGRAPHICSITEM_Y )
{
   QGraphicsItem * p = hbqt_par_QGraphicsItem( 1 );
   if( p )
      hb_retnd( ( p )->y() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QGRAPHICSITEM_Y FP=hb_retnd( ( p )->y() ); p is NULL" ) );
   }
}

/*
 * qreal zValue () const
 */
HB_FUNC( QT_QGRAPHICSITEM_ZVALUE )
{
   QGraphicsItem * p = hbqt_par_QGraphicsItem( 1 );
   if( p )
      hb_retnd( ( p )->zValue() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QGRAPHICSITEM_ZVALUE FP=hb_retnd( ( p )->zValue() ); p is NULL" ) );
   }
}


/*----------------------------------------------------------------------*/
#endif             /* #if QT_VERSION >= 0x040500 */
/*----------------------------------------------------------------------*/
