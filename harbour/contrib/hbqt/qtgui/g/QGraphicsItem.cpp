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
 *  enum CacheMode { NoCache, ItemCoordinateCache, DeviceCoordinateCache }
 *  enum GraphicsItemChange { ItemEnabledChange, ItemEnabledHasChanged, ItemMatrixChange, ItemPositionChange, ..., ItemOpacityHasChanged }
 *  enum GraphicsItemFlag { ItemIsMovable, ItemIsSelectable, ItemIsFocusable, ItemClipsToShape, ..., ItemStacksBehindParent }
 *  flags GraphicsItemFlags
 */

/*
 *  Constructed[ 150/150 [ 100.00% ] ]
 *
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
      p->ph = NULL;
}

void * hbqt_gcAllocate_QGraphicsItem( void * pObj, bool bNew )
{
   HBQT_GC_T * p = ( HBQT_GC_T * ) hb_gcAllocate( sizeof( HBQT_GC_T ), hbqt_gcFuncs() );

   p->ph = ( QGraphicsItem * ) pObj;
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QGraphicsItem;
   p->type = HBQT_TYPE_QGraphicsItem;

   return p;
}

HB_FUNC( QT_QGRAPHICSITEM )
{

}

/* bool acceptDrops () const */
HB_FUNC( QT_QGRAPHICSITEM_ACCEPTDROPS )
{
   QGraphicsItem * p = hbqt_par_QGraphicsItem( 1 );
   if( p )
      hb_retl( ( p )->acceptDrops() );
}

/* bool acceptHoverEvents () const */
HB_FUNC( QT_QGRAPHICSITEM_ACCEPTHOVEREVENTS )
{
   QGraphicsItem * p = hbqt_par_QGraphicsItem( 1 );
   if( p )
      hb_retl( ( p )->acceptHoverEvents() );
}

/* Qt::MouseButtons acceptedMouseButtons () const */
HB_FUNC( QT_QGRAPHICSITEM_ACCEPTEDMOUSEBUTTONS )
{
   QGraphicsItem * p = hbqt_par_QGraphicsItem( 1 );
   if( p )
      hb_retni( ( Qt::MouseButtons ) ( p )->acceptedMouseButtons() );
}

/* virtual void advance ( int phase ) */
HB_FUNC( QT_QGRAPHICSITEM_ADVANCE )
{
   QGraphicsItem * p = hbqt_par_QGraphicsItem( 1 );
   if( p )
      ( p )->advance( hb_parni( 2 ) );
}

/* virtual QRectF boundingRect () const = 0 */
HB_FUNC( QT_QGRAPHICSITEM_BOUNDINGRECT )
{
   QGraphicsItem * p = hbqt_par_QGraphicsItem( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QRectF( new QRectF( ( p )->boundingRect() ), true ) );
}

/* QRegion boundingRegion ( const QTransform & itemToDeviceTransform ) const */
HB_FUNC( QT_QGRAPHICSITEM_BOUNDINGREGION )
{
   QGraphicsItem * p = hbqt_par_QGraphicsItem( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QRegion( new QRegion( ( p )->boundingRegion( *hbqt_par_QTransform( 2 ) ) ), true ) );
}

/* qreal boundingRegionGranularity () const */
HB_FUNC( QT_QGRAPHICSITEM_BOUNDINGREGIONGRANULARITY )
{
   QGraphicsItem * p = hbqt_par_QGraphicsItem( 1 );
   if( p )
      hb_retnd( ( p )->boundingRegionGranularity() );
}

/* CacheMode cacheMode () const */
HB_FUNC( QT_QGRAPHICSITEM_CACHEMODE )
{
   QGraphicsItem * p = hbqt_par_QGraphicsItem( 1 );
   if( p )
      hb_retni( ( QGraphicsItem::CacheMode ) ( p )->cacheMode() );
}

/* QList<QGraphicsItem *> childItems () const */
HB_FUNC( QT_QGRAPHICSITEM_CHILDITEMS )
{
   QGraphicsItem * p = hbqt_par_QGraphicsItem( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QList( new QList<QGraphicsItem *>( ( p )->childItems() ), true ) );
}

/* QRectF childrenBoundingRect () const */
HB_FUNC( QT_QGRAPHICSITEM_CHILDRENBOUNDINGRECT )
{
   QGraphicsItem * p = hbqt_par_QGraphicsItem( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QRectF( new QRectF( ( p )->childrenBoundingRect() ), true ) );
}

/* void clearFocus () */
HB_FUNC( QT_QGRAPHICSITEM_CLEARFOCUS )
{
   QGraphicsItem * p = hbqt_par_QGraphicsItem( 1 );
   if( p )
      ( p )->clearFocus();
}

/* QPainterPath clipPath () const */
HB_FUNC( QT_QGRAPHICSITEM_CLIPPATH )
{
   QGraphicsItem * p = hbqt_par_QGraphicsItem( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QPainterPath( new QPainterPath( ( p )->clipPath() ), true ) );
}

/* virtual bool collidesWithItem ( const QGraphicsItem * other, Qt::ItemSelectionMode mode = Qt::IntersectsItemShape ) const */
HB_FUNC( QT_QGRAPHICSITEM_COLLIDESWITHITEM )
{
   QGraphicsItem * p = hbqt_par_QGraphicsItem( 1 );
   if( p )
      hb_retl( ( p )->collidesWithItem( hbqt_par_QGraphicsItem( 2 ), ( HB_ISNUM( 3 ) ? ( Qt::ItemSelectionMode ) hb_parni( 3 ) : ( Qt::ItemSelectionMode ) Qt::IntersectsItemShape ) ) );
}

/* virtual bool collidesWithPath ( const QPainterPath & path, Qt::ItemSelectionMode mode = Qt::IntersectsItemShape ) const */
HB_FUNC( QT_QGRAPHICSITEM_COLLIDESWITHPATH )
{
   QGraphicsItem * p = hbqt_par_QGraphicsItem( 1 );
   if( p )
      hb_retl( ( p )->collidesWithPath( *hbqt_par_QPainterPath( 2 ), ( HB_ISNUM( 3 ) ? ( Qt::ItemSelectionMode ) hb_parni( 3 ) : ( Qt::ItemSelectionMode ) Qt::IntersectsItemShape ) ) );
}

/* QList<QGraphicsItem *> collidingItems ( Qt::ItemSelectionMode mode = Qt::IntersectsItemShape ) const */
HB_FUNC( QT_QGRAPHICSITEM_COLLIDINGITEMS )
{
   QGraphicsItem * p = hbqt_par_QGraphicsItem( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QList( new QList<QGraphicsItem *>( ( p )->collidingItems( ( HB_ISNUM( 2 ) ? ( Qt::ItemSelectionMode ) hb_parni( 2 ) : ( Qt::ItemSelectionMode ) Qt::IntersectsItemShape ) ) ), true ) );
}

/* QGraphicsItem * commonAncestorItem ( const QGraphicsItem * other ) const */
HB_FUNC( QT_QGRAPHICSITEM_COMMONANCESTORITEM )
{
   QGraphicsItem * p = hbqt_par_QGraphicsItem( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QGraphicsItem( ( p )->commonAncestorItem( hbqt_par_QGraphicsItem( 2 ) ), false ) );
}

/* virtual bool contains ( const QPointF & point ) const */
HB_FUNC( QT_QGRAPHICSITEM_CONTAINS )
{
   QGraphicsItem * p = hbqt_par_QGraphicsItem( 1 );
   if( p )
      hb_retl( ( p )->contains( *hbqt_par_QPointF( 2 ) ) );
}

/* QCursor cursor () const */
HB_FUNC( QT_QGRAPHICSITEM_CURSOR )
{
   QGraphicsItem * p = hbqt_par_QGraphicsItem( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QCursor( new QCursor( ( p )->cursor() ), true ) );
}

/* QVariant data ( int key ) const */
HB_FUNC( QT_QGRAPHICSITEM_DATA )
{
   QGraphicsItem * p = hbqt_par_QGraphicsItem( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QVariant( new QVariant( ( p )->data( hb_parni( 2 ) ) ), true ) );
}

/* QTransform deviceTransform ( const QTransform & viewportTransform ) const */
HB_FUNC( QT_QGRAPHICSITEM_DEVICETRANSFORM )
{
   QGraphicsItem * p = hbqt_par_QGraphicsItem( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QTransform( new QTransform( ( p )->deviceTransform( *hbqt_par_QTransform( 2 ) ) ), true ) );
}

/* qreal effectiveOpacity () const */
HB_FUNC( QT_QGRAPHICSITEM_EFFECTIVEOPACITY )
{
   QGraphicsItem * p = hbqt_par_QGraphicsItem( 1 );
   if( p )
      hb_retnd( ( p )->effectiveOpacity() );
}

/* void ensureVisible ( const QRectF & rect = QRectF(), int xmargin = 50, int ymargin = 50 ) */
HB_FUNC( QT_QGRAPHICSITEM_ENSUREVISIBLE )
{
   QGraphicsItem * p = hbqt_par_QGraphicsItem( 1 );
   if( p )
      ( p )->ensureVisible( ( HB_ISOBJECT( 2 ) ? *hbqt_par_QRectF( 2 ) : QRectF() ), hb_parnidef( 3, 50 ), hb_parnidef( 4, 50 ) );
}

/* void ensureVisible ( qreal x, qreal y, qreal w, qreal h, int xmargin = 50, int ymargin = 50 ) */
HB_FUNC( QT_QGRAPHICSITEM_ENSUREVISIBLE_1 )
{
   QGraphicsItem * p = hbqt_par_QGraphicsItem( 1 );
   if( p )
      ( p )->ensureVisible( hb_parnd( 2 ), hb_parnd( 3 ), hb_parnd( 4 ), hb_parnd( 5 ), hb_parnidef( 6, 50 ), hb_parnidef( 7, 50 ) );
}

/* GraphicsItemFlags flags () const */
HB_FUNC( QT_QGRAPHICSITEM_FLAGS )
{
   QGraphicsItem * p = hbqt_par_QGraphicsItem( 1 );
   if( p )
      hb_retni( ( QGraphicsItem::GraphicsItemFlags ) ( p )->flags() );
}

/* void grabKeyboard () */
HB_FUNC( QT_QGRAPHICSITEM_GRABKEYBOARD )
{
   QGraphicsItem * p = hbqt_par_QGraphicsItem( 1 );
   if( p )
      ( p )->grabKeyboard();
}

/* void grabMouse () */
HB_FUNC( QT_QGRAPHICSITEM_GRABMOUSE )
{
   QGraphicsItem * p = hbqt_par_QGraphicsItem( 1 );
   if( p )
      ( p )->grabMouse();
}

/* QGraphicsItemGroup * group () const */
HB_FUNC( QT_QGRAPHICSITEM_GROUP )
{
   QGraphicsItem * p = hbqt_par_QGraphicsItem( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QGraphicsItemGroup( ( p )->group(), false ) );
}

/* bool handlesChildEvents () const */
HB_FUNC( QT_QGRAPHICSITEM_HANDLESCHILDEVENTS )
{
   QGraphicsItem * p = hbqt_par_QGraphicsItem( 1 );
   if( p )
      hb_retl( ( p )->handlesChildEvents() );
}

/* bool hasCursor () const */
HB_FUNC( QT_QGRAPHICSITEM_HASCURSOR )
{
   QGraphicsItem * p = hbqt_par_QGraphicsItem( 1 );
   if( p )
      hb_retl( ( p )->hasCursor() );
}

/* bool hasFocus () const */
HB_FUNC( QT_QGRAPHICSITEM_HASFOCUS )
{
   QGraphicsItem * p = hbqt_par_QGraphicsItem( 1 );
   if( p )
      hb_retl( ( p )->hasFocus() );
}

/* void hide () */
HB_FUNC( QT_QGRAPHICSITEM_HIDE )
{
   QGraphicsItem * p = hbqt_par_QGraphicsItem( 1 );
   if( p )
      ( p )->hide();
}

/* void installSceneEventFilter ( QGraphicsItem * filterItem ) */
HB_FUNC( QT_QGRAPHICSITEM_INSTALLSCENEEVENTFILTER )
{
   QGraphicsItem * p = hbqt_par_QGraphicsItem( 1 );
   if( p )
      ( p )->installSceneEventFilter( hbqt_par_QGraphicsItem( 2 ) );
}

/* bool isAncestorOf ( const QGraphicsItem * child ) const */
HB_FUNC( QT_QGRAPHICSITEM_ISANCESTOROF )
{
   QGraphicsItem * p = hbqt_par_QGraphicsItem( 1 );
   if( p )
      hb_retl( ( p )->isAncestorOf( hbqt_par_QGraphicsItem( 2 ) ) );
}

/* bool isClipped () const */
HB_FUNC( QT_QGRAPHICSITEM_ISCLIPPED )
{
   QGraphicsItem * p = hbqt_par_QGraphicsItem( 1 );
   if( p )
      hb_retl( ( p )->isClipped() );
}

/* bool isEnabled () const */
HB_FUNC( QT_QGRAPHICSITEM_ISENABLED )
{
   QGraphicsItem * p = hbqt_par_QGraphicsItem( 1 );
   if( p )
      hb_retl( ( p )->isEnabled() );
}

/* bool isObscured () const */
HB_FUNC( QT_QGRAPHICSITEM_ISOBSCURED )
{
   QGraphicsItem * p = hbqt_par_QGraphicsItem( 1 );
   if( p )
      hb_retl( ( p )->isObscured() );
}

/* bool isObscured ( qreal x, qreal y, qreal w, qreal h ) const */
HB_FUNC( QT_QGRAPHICSITEM_ISOBSCURED_1 )
{
   QGraphicsItem * p = hbqt_par_QGraphicsItem( 1 );
   if( p )
      hb_retl( ( p )->isObscured( hb_parnd( 2 ), hb_parnd( 3 ), hb_parnd( 4 ), hb_parnd( 5 ) ) );
}

/* bool isObscured ( const QRectF & rect ) const */
HB_FUNC( QT_QGRAPHICSITEM_ISOBSCURED_2 )
{
   QGraphicsItem * p = hbqt_par_QGraphicsItem( 1 );
   if( p )
      hb_retl( ( p )->isObscured( *hbqt_par_QRectF( 2 ) ) );
}

/* virtual bool isObscuredBy ( const QGraphicsItem * item ) const */
HB_FUNC( QT_QGRAPHICSITEM_ISOBSCUREDBY )
{
   QGraphicsItem * p = hbqt_par_QGraphicsItem( 1 );
   if( p )
      hb_retl( ( p )->isObscuredBy( hbqt_par_QGraphicsItem( 2 ) ) );
}

/* bool isSelected () const */
HB_FUNC( QT_QGRAPHICSITEM_ISSELECTED )
{
   QGraphicsItem * p = hbqt_par_QGraphicsItem( 1 );
   if( p )
      hb_retl( ( p )->isSelected() );
}

/* bool isUnderMouse () const */
HB_FUNC( QT_QGRAPHICSITEM_ISUNDERMOUSE )
{
   QGraphicsItem * p = hbqt_par_QGraphicsItem( 1 );
   if( p )
      hb_retl( ( p )->isUnderMouse() );
}

/* bool isVisible () const */
HB_FUNC( QT_QGRAPHICSITEM_ISVISIBLE )
{
   QGraphicsItem * p = hbqt_par_QGraphicsItem( 1 );
   if( p )
      hb_retl( ( p )->isVisible() );
}

/* bool isVisibleTo ( const QGraphicsItem * parent ) const */
HB_FUNC( QT_QGRAPHICSITEM_ISVISIBLETO )
{
   QGraphicsItem * p = hbqt_par_QGraphicsItem( 1 );
   if( p )
      hb_retl( ( p )->isVisibleTo( hbqt_par_QGraphicsItem( 2 ) ) );
}

/* bool isWidget () const */
HB_FUNC( QT_QGRAPHICSITEM_ISWIDGET )
{
   QGraphicsItem * p = hbqt_par_QGraphicsItem( 1 );
   if( p )
      hb_retl( ( p )->isWidget() );
}

/* bool isWindow () const */
HB_FUNC( QT_QGRAPHICSITEM_ISWINDOW )
{
   QGraphicsItem * p = hbqt_par_QGraphicsItem( 1 );
   if( p )
      hb_retl( ( p )->isWindow() );
}

/* QTransform itemTransform ( const QGraphicsItem * other, bool * ok = 0 ) const */
HB_FUNC( QT_QGRAPHICSITEM_ITEMTRANSFORM )
{
   QGraphicsItem * p = hbqt_par_QGraphicsItem( 1 );
   bool iOk = 0;

   if( p )
      hb_retptrGC( hbqt_gcAllocate_QTransform( new QTransform( ( p )->itemTransform( hbqt_par_QGraphicsItem( 2 ), &iOk ) ), true ) );

   hb_stornl( iOk, 3 );
}

/* QPointF mapFromItem ( const QGraphicsItem * item, const QPointF & point ) const */
HB_FUNC( QT_QGRAPHICSITEM_MAPFROMITEM )
{
   QGraphicsItem * p = hbqt_par_QGraphicsItem( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QPointF( new QPointF( ( p )->mapFromItem( hbqt_par_QGraphicsItem( 2 ), *hbqt_par_QPointF( 3 ) ) ), true ) );
}

/* QPolygonF mapFromItem ( const QGraphicsItem * item, const QRectF & rect ) const */
HB_FUNC( QT_QGRAPHICSITEM_MAPFROMITEM_1 )
{
   QGraphicsItem * p = hbqt_par_QGraphicsItem( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QPolygonF( new QPolygonF( ( p )->mapFromItem( hbqt_par_QGraphicsItem( 2 ), *hbqt_par_QRectF( 3 ) ) ), true ) );
}

/* QPolygonF mapFromItem ( const QGraphicsItem * item, const QPolygonF & polygon ) const */
HB_FUNC( QT_QGRAPHICSITEM_MAPFROMITEM_2 )
{
   QGraphicsItem * p = hbqt_par_QGraphicsItem( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QPolygonF( new QPolygonF( ( p )->mapFromItem( hbqt_par_QGraphicsItem( 2 ), *hbqt_par_QPolygonF( 3 ) ) ), true ) );
}

/* QPainterPath mapFromItem ( const QGraphicsItem * item, const QPainterPath & path ) const */
HB_FUNC( QT_QGRAPHICSITEM_MAPFROMITEM_3 )
{
   QGraphicsItem * p = hbqt_par_QGraphicsItem( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QPainterPath( new QPainterPath( ( p )->mapFromItem( hbqt_par_QGraphicsItem( 2 ), *hbqt_par_QPainterPath( 3 ) ) ), true ) );
}

/* QPolygonF mapFromItem ( const QGraphicsItem * item, qreal x, qreal y, qreal w, qreal h ) const */
HB_FUNC( QT_QGRAPHICSITEM_MAPFROMITEM_4 )
{
   QGraphicsItem * p = hbqt_par_QGraphicsItem( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QPolygonF( new QPolygonF( ( p )->mapFromItem( hbqt_par_QGraphicsItem( 2 ), hb_parnd( 3 ), hb_parnd( 4 ), hb_parnd( 5 ), hb_parnd( 6 ) ) ), true ) );
}

/* QPointF mapFromItem ( const QGraphicsItem * item, qreal x, qreal y ) const */
HB_FUNC( QT_QGRAPHICSITEM_MAPFROMITEM_5 )
{
   QGraphicsItem * p = hbqt_par_QGraphicsItem( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QPointF( new QPointF( ( p )->mapFromItem( hbqt_par_QGraphicsItem( 2 ), hb_parnd( 3 ), hb_parnd( 4 ) ) ), true ) );
}

/* QPointF mapFromParent ( const QPointF & point ) const */
HB_FUNC( QT_QGRAPHICSITEM_MAPFROMPARENT )
{
   QGraphicsItem * p = hbqt_par_QGraphicsItem( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QPointF( new QPointF( ( p )->mapFromParent( *hbqt_par_QPointF( 2 ) ) ), true ) );
}

/* QPolygonF mapFromParent ( const QRectF & rect ) const */
HB_FUNC( QT_QGRAPHICSITEM_MAPFROMPARENT_1 )
{
   QGraphicsItem * p = hbqt_par_QGraphicsItem( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QPolygonF( new QPolygonF( ( p )->mapFromParent( *hbqt_par_QRectF( 2 ) ) ), true ) );
}

/* QPolygonF mapFromParent ( const QPolygonF & polygon ) const */
HB_FUNC( QT_QGRAPHICSITEM_MAPFROMPARENT_2 )
{
   QGraphicsItem * p = hbqt_par_QGraphicsItem( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QPolygonF( new QPolygonF( ( p )->mapFromParent( *hbqt_par_QPolygonF( 2 ) ) ), true ) );
}

/* QPainterPath mapFromParent ( const QPainterPath & path ) const */
HB_FUNC( QT_QGRAPHICSITEM_MAPFROMPARENT_3 )
{
   QGraphicsItem * p = hbqt_par_QGraphicsItem( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QPainterPath( new QPainterPath( ( p )->mapFromParent( *hbqt_par_QPainterPath( 2 ) ) ), true ) );
}

/* QPolygonF mapFromParent ( qreal x, qreal y, qreal w, qreal h ) const */
HB_FUNC( QT_QGRAPHICSITEM_MAPFROMPARENT_4 )
{
   QGraphicsItem * p = hbqt_par_QGraphicsItem( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QPolygonF( new QPolygonF( ( p )->mapFromParent( hb_parnd( 2 ), hb_parnd( 3 ), hb_parnd( 4 ), hb_parnd( 5 ) ) ), true ) );
}

/* QPointF mapFromParent ( qreal x, qreal y ) const */
HB_FUNC( QT_QGRAPHICSITEM_MAPFROMPARENT_5 )
{
   QGraphicsItem * p = hbqt_par_QGraphicsItem( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QPointF( new QPointF( ( p )->mapFromParent( hb_parnd( 2 ), hb_parnd( 3 ) ) ), true ) );
}

/* QPointF mapFromScene ( const QPointF & point ) const */
HB_FUNC( QT_QGRAPHICSITEM_MAPFROMSCENE )
{
   QGraphicsItem * p = hbqt_par_QGraphicsItem( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QPointF( new QPointF( ( p )->mapFromScene( *hbqt_par_QPointF( 2 ) ) ), true ) );
}

/* QPolygonF mapFromScene ( const QRectF & rect ) const */
HB_FUNC( QT_QGRAPHICSITEM_MAPFROMSCENE_1 )
{
   QGraphicsItem * p = hbqt_par_QGraphicsItem( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QPolygonF( new QPolygonF( ( p )->mapFromScene( *hbqt_par_QRectF( 2 ) ) ), true ) );
}

/* QPolygonF mapFromScene ( const QPolygonF & polygon ) const */
HB_FUNC( QT_QGRAPHICSITEM_MAPFROMSCENE_2 )
{
   QGraphicsItem * p = hbqt_par_QGraphicsItem( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QPolygonF( new QPolygonF( ( p )->mapFromScene( *hbqt_par_QPolygonF( 2 ) ) ), true ) );
}

/* QPainterPath mapFromScene ( const QPainterPath & path ) const */
HB_FUNC( QT_QGRAPHICSITEM_MAPFROMSCENE_3 )
{
   QGraphicsItem * p = hbqt_par_QGraphicsItem( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QPainterPath( new QPainterPath( ( p )->mapFromScene( *hbqt_par_QPainterPath( 2 ) ) ), true ) );
}

/* QPolygonF mapFromScene ( qreal x, qreal y, qreal w, qreal h ) const */
HB_FUNC( QT_QGRAPHICSITEM_MAPFROMSCENE_4 )
{
   QGraphicsItem * p = hbqt_par_QGraphicsItem( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QPolygonF( new QPolygonF( ( p )->mapFromScene( hb_parnd( 2 ), hb_parnd( 3 ), hb_parnd( 4 ), hb_parnd( 5 ) ) ), true ) );
}

/* QPointF mapFromScene ( qreal x, qreal y ) const */
HB_FUNC( QT_QGRAPHICSITEM_MAPFROMSCENE_5 )
{
   QGraphicsItem * p = hbqt_par_QGraphicsItem( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QPointF( new QPointF( ( p )->mapFromScene( hb_parnd( 2 ), hb_parnd( 3 ) ) ), true ) );
}

/* QRectF mapRectFromItem ( const QGraphicsItem * item, const QRectF & rect ) const */
HB_FUNC( QT_QGRAPHICSITEM_MAPRECTFROMITEM )
{
   QGraphicsItem * p = hbqt_par_QGraphicsItem( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QRectF( new QRectF( ( p )->mapRectFromItem( hbqt_par_QGraphicsItem( 2 ), *hbqt_par_QRectF( 3 ) ) ), true ) );
}

/* QRectF mapRectFromItem ( const QGraphicsItem * item, qreal x, qreal y, qreal w, qreal h ) const */
HB_FUNC( QT_QGRAPHICSITEM_MAPRECTFROMITEM_1 )
{
   QGraphicsItem * p = hbqt_par_QGraphicsItem( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QRectF( new QRectF( ( p )->mapRectFromItem( hbqt_par_QGraphicsItem( 2 ), hb_parnd( 3 ), hb_parnd( 4 ), hb_parnd( 5 ), hb_parnd( 6 ) ) ), true ) );
}

/* QRectF mapRectFromParent ( const QRectF & rect ) const */
HB_FUNC( QT_QGRAPHICSITEM_MAPRECTFROMPARENT )
{
   QGraphicsItem * p = hbqt_par_QGraphicsItem( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QRectF( new QRectF( ( p )->mapRectFromParent( *hbqt_par_QRectF( 2 ) ) ), true ) );
}

/* QRectF mapRectFromParent ( qreal x, qreal y, qreal w, qreal h ) const */
HB_FUNC( QT_QGRAPHICSITEM_MAPRECTFROMPARENT_1 )
{
   QGraphicsItem * p = hbqt_par_QGraphicsItem( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QRectF( new QRectF( ( p )->mapRectFromParent( hb_parnd( 2 ), hb_parnd( 3 ), hb_parnd( 4 ), hb_parnd( 5 ) ) ), true ) );
}

/* QRectF mapRectFromScene ( const QRectF & rect ) const */
HB_FUNC( QT_QGRAPHICSITEM_MAPRECTFROMSCENE )
{
   QGraphicsItem * p = hbqt_par_QGraphicsItem( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QRectF( new QRectF( ( p )->mapRectFromScene( *hbqt_par_QRectF( 2 ) ) ), true ) );
}

/* QRectF mapRectFromScene ( qreal x, qreal y, qreal w, qreal h ) const */
HB_FUNC( QT_QGRAPHICSITEM_MAPRECTFROMSCENE_1 )
{
   QGraphicsItem * p = hbqt_par_QGraphicsItem( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QRectF( new QRectF( ( p )->mapRectFromScene( hb_parnd( 2 ), hb_parnd( 3 ), hb_parnd( 4 ), hb_parnd( 5 ) ) ), true ) );
}

/* QRectF mapRectToItem ( const QGraphicsItem * item, const QRectF & rect ) const */
HB_FUNC( QT_QGRAPHICSITEM_MAPRECTTOITEM )
{
   QGraphicsItem * p = hbqt_par_QGraphicsItem( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QRectF( new QRectF( ( p )->mapRectToItem( hbqt_par_QGraphicsItem( 2 ), *hbqt_par_QRectF( 3 ) ) ), true ) );
}

/* QRectF mapRectToItem ( const QGraphicsItem * item, qreal x, qreal y, qreal w, qreal h ) const */
HB_FUNC( QT_QGRAPHICSITEM_MAPRECTTOITEM_1 )
{
   QGraphicsItem * p = hbqt_par_QGraphicsItem( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QRectF( new QRectF( ( p )->mapRectToItem( hbqt_par_QGraphicsItem( 2 ), hb_parnd( 3 ), hb_parnd( 4 ), hb_parnd( 5 ), hb_parnd( 6 ) ) ), true ) );
}

/* QRectF mapRectToParent ( const QRectF & rect ) const */
HB_FUNC( QT_QGRAPHICSITEM_MAPRECTTOPARENT )
{
   QGraphicsItem * p = hbqt_par_QGraphicsItem( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QRectF( new QRectF( ( p )->mapRectToParent( *hbqt_par_QRectF( 2 ) ) ), true ) );
}

/* QRectF mapRectToParent ( qreal x, qreal y, qreal w, qreal h ) const */
HB_FUNC( QT_QGRAPHICSITEM_MAPRECTTOPARENT_1 )
{
   QGraphicsItem * p = hbqt_par_QGraphicsItem( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QRectF( new QRectF( ( p )->mapRectToParent( hb_parnd( 2 ), hb_parnd( 3 ), hb_parnd( 4 ), hb_parnd( 5 ) ) ), true ) );
}

/* QRectF mapRectToScene ( const QRectF & rect ) const */
HB_FUNC( QT_QGRAPHICSITEM_MAPRECTTOSCENE )
{
   QGraphicsItem * p = hbqt_par_QGraphicsItem( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QRectF( new QRectF( ( p )->mapRectToScene( *hbqt_par_QRectF( 2 ) ) ), true ) );
}

/* QRectF mapRectToScene ( qreal x, qreal y, qreal w, qreal h ) const */
HB_FUNC( QT_QGRAPHICSITEM_MAPRECTTOSCENE_1 )
{
   QGraphicsItem * p = hbqt_par_QGraphicsItem( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QRectF( new QRectF( ( p )->mapRectToScene( hb_parnd( 2 ), hb_parnd( 3 ), hb_parnd( 4 ), hb_parnd( 5 ) ) ), true ) );
}

/* QPointF mapToItem ( const QGraphicsItem * item, const QPointF & point ) const */
HB_FUNC( QT_QGRAPHICSITEM_MAPTOITEM )
{
   QGraphicsItem * p = hbqt_par_QGraphicsItem( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QPointF( new QPointF( ( p )->mapToItem( hbqt_par_QGraphicsItem( 2 ), *hbqt_par_QPointF( 3 ) ) ), true ) );
}

/* QPolygonF mapToItem ( const QGraphicsItem * item, const QRectF & rect ) const */
HB_FUNC( QT_QGRAPHICSITEM_MAPTOITEM_1 )
{
   QGraphicsItem * p = hbqt_par_QGraphicsItem( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QPolygonF( new QPolygonF( ( p )->mapToItem( hbqt_par_QGraphicsItem( 2 ), *hbqt_par_QRectF( 3 ) ) ), true ) );
}

/* QPolygonF mapToItem ( const QGraphicsItem * item, const QPolygonF & polygon ) const */
HB_FUNC( QT_QGRAPHICSITEM_MAPTOITEM_2 )
{
   QGraphicsItem * p = hbqt_par_QGraphicsItem( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QPolygonF( new QPolygonF( ( p )->mapToItem( hbqt_par_QGraphicsItem( 2 ), *hbqt_par_QPolygonF( 3 ) ) ), true ) );
}

/* QPainterPath mapToItem ( const QGraphicsItem * item, const QPainterPath & path ) const */
HB_FUNC( QT_QGRAPHICSITEM_MAPTOITEM_3 )
{
   QGraphicsItem * p = hbqt_par_QGraphicsItem( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QPainterPath( new QPainterPath( ( p )->mapToItem( hbqt_par_QGraphicsItem( 2 ), *hbqt_par_QPainterPath( 3 ) ) ), true ) );
}

/* QPolygonF mapToItem ( const QGraphicsItem * item, qreal x, qreal y, qreal w, qreal h ) const */
HB_FUNC( QT_QGRAPHICSITEM_MAPTOITEM_4 )
{
   QGraphicsItem * p = hbqt_par_QGraphicsItem( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QPolygonF( new QPolygonF( ( p )->mapToItem( hbqt_par_QGraphicsItem( 2 ), hb_parnd( 3 ), hb_parnd( 4 ), hb_parnd( 5 ), hb_parnd( 6 ) ) ), true ) );
}

/* QPointF mapToItem ( const QGraphicsItem * item, qreal x, qreal y ) const */
HB_FUNC( QT_QGRAPHICSITEM_MAPTOITEM_5 )
{
   QGraphicsItem * p = hbqt_par_QGraphicsItem( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QPointF( new QPointF( ( p )->mapToItem( hbqt_par_QGraphicsItem( 2 ), hb_parnd( 3 ), hb_parnd( 4 ) ) ), true ) );
}

/* QPointF mapToParent ( const QPointF & point ) const */
HB_FUNC( QT_QGRAPHICSITEM_MAPTOPARENT )
{
   QGraphicsItem * p = hbqt_par_QGraphicsItem( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QPointF( new QPointF( ( p )->mapToParent( *hbqt_par_QPointF( 2 ) ) ), true ) );
}

/* QPolygonF mapToParent ( const QRectF & rect ) const */
HB_FUNC( QT_QGRAPHICSITEM_MAPTOPARENT_1 )
{
   QGraphicsItem * p = hbqt_par_QGraphicsItem( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QPolygonF( new QPolygonF( ( p )->mapToParent( *hbqt_par_QRectF( 2 ) ) ), true ) );
}

/* QPolygonF mapToParent ( const QPolygonF & polygon ) const */
HB_FUNC( QT_QGRAPHICSITEM_MAPTOPARENT_2 )
{
   QGraphicsItem * p = hbqt_par_QGraphicsItem( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QPolygonF( new QPolygonF( ( p )->mapToParent( *hbqt_par_QPolygonF( 2 ) ) ), true ) );
}

/* QPainterPath mapToParent ( const QPainterPath & path ) const */
HB_FUNC( QT_QGRAPHICSITEM_MAPTOPARENT_3 )
{
   QGraphicsItem * p = hbqt_par_QGraphicsItem( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QPainterPath( new QPainterPath( ( p )->mapToParent( *hbqt_par_QPainterPath( 2 ) ) ), true ) );
}

/* QPolygonF mapToParent ( qreal x, qreal y, qreal w, qreal h ) const */
HB_FUNC( QT_QGRAPHICSITEM_MAPTOPARENT_4 )
{
   QGraphicsItem * p = hbqt_par_QGraphicsItem( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QPolygonF( new QPolygonF( ( p )->mapToParent( hb_parnd( 2 ), hb_parnd( 3 ), hb_parnd( 4 ), hb_parnd( 5 ) ) ), true ) );
}

/* QPointF mapToParent ( qreal x, qreal y ) const */
HB_FUNC( QT_QGRAPHICSITEM_MAPTOPARENT_5 )
{
   QGraphicsItem * p = hbqt_par_QGraphicsItem( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QPointF( new QPointF( ( p )->mapToParent( hb_parnd( 2 ), hb_parnd( 3 ) ) ), true ) );
}

/* QPointF mapToScene ( const QPointF & point ) const */
HB_FUNC( QT_QGRAPHICSITEM_MAPTOSCENE )
{
   QGraphicsItem * p = hbqt_par_QGraphicsItem( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QPointF( new QPointF( ( p )->mapToScene( *hbqt_par_QPointF( 2 ) ) ), true ) );
}

/* QPolygonF mapToScene ( const QRectF & rect ) const */
HB_FUNC( QT_QGRAPHICSITEM_MAPTOSCENE_1 )
{
   QGraphicsItem * p = hbqt_par_QGraphicsItem( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QPolygonF( new QPolygonF( ( p )->mapToScene( *hbqt_par_QRectF( 2 ) ) ), true ) );
}

/* QPolygonF mapToScene ( const QPolygonF & polygon ) const */
HB_FUNC( QT_QGRAPHICSITEM_MAPTOSCENE_2 )
{
   QGraphicsItem * p = hbqt_par_QGraphicsItem( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QPolygonF( new QPolygonF( ( p )->mapToScene( *hbqt_par_QPolygonF( 2 ) ) ), true ) );
}

/* QPainterPath mapToScene ( const QPainterPath & path ) const */
HB_FUNC( QT_QGRAPHICSITEM_MAPTOSCENE_3 )
{
   QGraphicsItem * p = hbqt_par_QGraphicsItem( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QPainterPath( new QPainterPath( ( p )->mapToScene( *hbqt_par_QPainterPath( 2 ) ) ), true ) );
}

/* QPolygonF mapToScene ( qreal x, qreal y, qreal w, qreal h ) const */
HB_FUNC( QT_QGRAPHICSITEM_MAPTOSCENE_4 )
{
   QGraphicsItem * p = hbqt_par_QGraphicsItem( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QPolygonF( new QPolygonF( ( p )->mapToScene( hb_parnd( 2 ), hb_parnd( 3 ), hb_parnd( 4 ), hb_parnd( 5 ) ) ), true ) );
}

/* QPointF mapToScene ( qreal x, qreal y ) const */
HB_FUNC( QT_QGRAPHICSITEM_MAPTOSCENE_5 )
{
   QGraphicsItem * p = hbqt_par_QGraphicsItem( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QPointF( new QPointF( ( p )->mapToScene( hb_parnd( 2 ), hb_parnd( 3 ) ) ), true ) );
}

/* void moveBy ( qreal dx, qreal dy ) */
HB_FUNC( QT_QGRAPHICSITEM_MOVEBY )
{
   QGraphicsItem * p = hbqt_par_QGraphicsItem( 1 );
   if( p )
      ( p )->moveBy( hb_parnd( 2 ), hb_parnd( 3 ) );
}

/* qreal opacity () const */
HB_FUNC( QT_QGRAPHICSITEM_OPACITY )
{
   QGraphicsItem * p = hbqt_par_QGraphicsItem( 1 );
   if( p )
      hb_retnd( ( p )->opacity() );
}

/* virtual QPainterPath opaqueArea () const */
HB_FUNC( QT_QGRAPHICSITEM_OPAQUEAREA )
{
   QGraphicsItem * p = hbqt_par_QGraphicsItem( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QPainterPath( new QPainterPath( ( p )->opaqueArea() ), true ) );
}

/* virtual void paint ( QPainter * painter, const QStyleOptionGraphicsItem * option, QWidget * widget = 0 ) = 0 */
HB_FUNC( QT_QGRAPHICSITEM_PAINT )
{
   QGraphicsItem * p = hbqt_par_QGraphicsItem( 1 );
   if( p )
      ( p )->paint( hbqt_par_QPainter( 2 ), hbqt_par_QStyleOptionGraphicsItem( 3 ), hbqt_par_QWidget( 4 ) );
}

/* QGraphicsItem * parentItem () const */
HB_FUNC( QT_QGRAPHICSITEM_PARENTITEM )
{
   QGraphicsItem * p = hbqt_par_QGraphicsItem( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QGraphicsItem( ( p )->parentItem(), false ) );
}

/* QGraphicsWidget * parentWidget () const */
HB_FUNC( QT_QGRAPHICSITEM_PARENTWIDGET )
{
   QGraphicsItem * p = hbqt_par_QGraphicsItem( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QGraphicsWidget( ( p )->parentWidget(), false ) );
}

/* QPointF pos () const */
HB_FUNC( QT_QGRAPHICSITEM_POS )
{
   QGraphicsItem * p = hbqt_par_QGraphicsItem( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QPointF( new QPointF( ( p )->pos() ), true ) );
}

/* void removeSceneEventFilter ( QGraphicsItem * filterItem ) */
HB_FUNC( QT_QGRAPHICSITEM_REMOVESCENEEVENTFILTER )
{
   QGraphicsItem * p = hbqt_par_QGraphicsItem( 1 );
   if( p )
      ( p )->removeSceneEventFilter( hbqt_par_QGraphicsItem( 2 ) );
}

/* void resetTransform () */
HB_FUNC( QT_QGRAPHICSITEM_RESETTRANSFORM )
{
   QGraphicsItem * p = hbqt_par_QGraphicsItem( 1 );
   if( p )
      ( p )->resetTransform();
}

/* void rotate ( qreal angle ) */
HB_FUNC( QT_QGRAPHICSITEM_ROTATE )
{
   QGraphicsItem * p = hbqt_par_QGraphicsItem( 1 );
   if( p )
      ( p )->rotate( hb_parnd( 2 ) );
}

/* void scale ( qreal sx, qreal sy ) */
HB_FUNC( QT_QGRAPHICSITEM_SCALE )
{
   QGraphicsItem * p = hbqt_par_QGraphicsItem( 1 );
   if( p )
      ( p )->scale( hb_parnd( 2 ), hb_parnd( 3 ) );
}

/* QGraphicsScene * scene () const */
HB_FUNC( QT_QGRAPHICSITEM_SCENE )
{
   QGraphicsItem * p = hbqt_par_QGraphicsItem( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QGraphicsScene( ( p )->scene(), false ) );
}

/* QRectF sceneBoundingRect () const */
HB_FUNC( QT_QGRAPHICSITEM_SCENEBOUNDINGRECT )
{
   QGraphicsItem * p = hbqt_par_QGraphicsItem( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QRectF( new QRectF( ( p )->sceneBoundingRect() ), true ) );
}

/* QPointF scenePos () const */
HB_FUNC( QT_QGRAPHICSITEM_SCENEPOS )
{
   QGraphicsItem * p = hbqt_par_QGraphicsItem( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QPointF( new QPointF( ( p )->scenePos() ), true ) );
}

/* QTransform sceneTransform () const */
HB_FUNC( QT_QGRAPHICSITEM_SCENETRANSFORM )
{
   QGraphicsItem * p = hbqt_par_QGraphicsItem( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QTransform( new QTransform( ( p )->sceneTransform() ), true ) );
}

/* void scroll ( qreal dx, qreal dy, const QRectF & rect = QRectF() ) */
HB_FUNC( QT_QGRAPHICSITEM_SCROLL )
{
   QGraphicsItem * p = hbqt_par_QGraphicsItem( 1 );
   if( p )
      ( p )->scroll( hb_parnd( 2 ), hb_parnd( 3 ), ( HB_ISOBJECT( 4 ) ? *hbqt_par_QRectF( 4 ) : QRectF() ) );
}

/* void setAcceptDrops ( bool on ) */
HB_FUNC( QT_QGRAPHICSITEM_SETACCEPTDROPS )
{
   QGraphicsItem * p = hbqt_par_QGraphicsItem( 1 );
   if( p )
      ( p )->setAcceptDrops( hb_parl( 2 ) );
}

/* void setAcceptHoverEvents ( bool enabled ) */
HB_FUNC( QT_QGRAPHICSITEM_SETACCEPTHOVEREVENTS )
{
   QGraphicsItem * p = hbqt_par_QGraphicsItem( 1 );
   if( p )
      ( p )->setAcceptHoverEvents( hb_parl( 2 ) );
}

/* void setAcceptedMouseButtons ( Qt::MouseButtons buttons ) */
HB_FUNC( QT_QGRAPHICSITEM_SETACCEPTEDMOUSEBUTTONS )
{
   QGraphicsItem * p = hbqt_par_QGraphicsItem( 1 );
   if( p )
      ( p )->setAcceptedMouseButtons( ( Qt::MouseButtons ) hb_parni( 2 ) );
}

/* void setBoundingRegionGranularity ( qreal granularity ) */
HB_FUNC( QT_QGRAPHICSITEM_SETBOUNDINGREGIONGRANULARITY )
{
   QGraphicsItem * p = hbqt_par_QGraphicsItem( 1 );
   if( p )
      ( p )->setBoundingRegionGranularity( hb_parnd( 2 ) );
}

/* void setCacheMode ( CacheMode mode, const QSize & logicalCacheSize = QSize() ) */
HB_FUNC( QT_QGRAPHICSITEM_SETCACHEMODE )
{
   QGraphicsItem * p = hbqt_par_QGraphicsItem( 1 );
   if( p )
      ( p )->setCacheMode( ( QGraphicsItem::CacheMode ) hb_parni( 2 ), ( HB_ISOBJECT( 3 ) ? *hbqt_par_QSize( 3 ) : QSize() ) );
}

/* void setCursor ( const QCursor & cursor ) */
HB_FUNC( QT_QGRAPHICSITEM_SETCURSOR )
{
   QGraphicsItem * p = hbqt_par_QGraphicsItem( 1 );
   if( p )
      ( p )->setCursor( *hbqt_par_QCursor( 2 ) );
}

/* void setData ( int key, const QVariant & value ) */
HB_FUNC( QT_QGRAPHICSITEM_SETDATA )
{
   QGraphicsItem * p = hbqt_par_QGraphicsItem( 1 );
   if( p )
      ( p )->setData( hb_parni( 2 ), *hbqt_par_QVariant( 3 ) );
}

/* void setEnabled ( bool enabled ) */
HB_FUNC( QT_QGRAPHICSITEM_SETENABLED )
{
   QGraphicsItem * p = hbqt_par_QGraphicsItem( 1 );
   if( p )
      ( p )->setEnabled( hb_parl( 2 ) );
}

/* void setFlag ( GraphicsItemFlag flag, bool enabled = true ) */
HB_FUNC( QT_QGRAPHICSITEM_SETFLAG )
{
   QGraphicsItem * p = hbqt_par_QGraphicsItem( 1 );
   if( p )
      ( p )->setFlag( ( QGraphicsItem::GraphicsItemFlag ) hb_parni( 2 ), hb_parl( 3 ) );
}

/* void setFlags ( GraphicsItemFlags flags ) */
HB_FUNC( QT_QGRAPHICSITEM_SETFLAGS )
{
   QGraphicsItem * p = hbqt_par_QGraphicsItem( 1 );
   if( p )
      ( p )->setFlags( ( QGraphicsItem::GraphicsItemFlags ) hb_parni( 2 ) );
}

/* void setFocus ( Qt::FocusReason focusReason = Qt::OtherFocusReason ) */
HB_FUNC( QT_QGRAPHICSITEM_SETFOCUS )
{
   QGraphicsItem * p = hbqt_par_QGraphicsItem( 1 );
   if( p )
      ( p )->setFocus( ( HB_ISNUM( 2 ) ? ( Qt::FocusReason ) hb_parni( 2 ) : ( Qt::FocusReason ) Qt::OtherFocusReason ) );
}

/* void setGroup ( QGraphicsItemGroup * group ) */
HB_FUNC( QT_QGRAPHICSITEM_SETGROUP )
{
   QGraphicsItem * p = hbqt_par_QGraphicsItem( 1 );
   if( p )
      ( p )->setGroup( hbqt_par_QGraphicsItemGroup( 2 ) );
}

/* void setHandlesChildEvents ( bool enabled ) */
HB_FUNC( QT_QGRAPHICSITEM_SETHANDLESCHILDEVENTS )
{
   QGraphicsItem * p = hbqt_par_QGraphicsItem( 1 );
   if( p )
      ( p )->setHandlesChildEvents( hb_parl( 2 ) );
}

/* void setOpacity ( qreal opacity ) */
HB_FUNC( QT_QGRAPHICSITEM_SETOPACITY )
{
   QGraphicsItem * p = hbqt_par_QGraphicsItem( 1 );
   if( p )
      ( p )->setOpacity( hb_parnd( 2 ) );
}

/* void setParentItem ( QGraphicsItem * parent ) */
HB_FUNC( QT_QGRAPHICSITEM_SETPARENTITEM )
{
   QGraphicsItem * p = hbqt_par_QGraphicsItem( 1 );
   if( p )
      ( p )->setParentItem( hbqt_par_QGraphicsItem( 2 ) );
}

/* void setPos ( const QPointF & pos ) */
HB_FUNC( QT_QGRAPHICSITEM_SETPOS )
{
   QGraphicsItem * p = hbqt_par_QGraphicsItem( 1 );
   if( p )
      ( p )->setPos( *hbqt_par_QPointF( 2 ) );
}

/* void setPos ( qreal x, qreal y ) */
HB_FUNC( QT_QGRAPHICSITEM_SETPOS_1 )
{
   QGraphicsItem * p = hbqt_par_QGraphicsItem( 1 );
   if( p )
      ( p )->setPos( hb_parnd( 2 ), hb_parnd( 3 ) );
}

/* void setSelected ( bool selected ) */
HB_FUNC( QT_QGRAPHICSITEM_SETSELECTED )
{
   QGraphicsItem * p = hbqt_par_QGraphicsItem( 1 );
   if( p )
      ( p )->setSelected( hb_parl( 2 ) );
}

/* void setToolTip ( const QString & toolTip ) */
HB_FUNC( QT_QGRAPHICSITEM_SETTOOLTIP )
{
   QGraphicsItem * p = hbqt_par_QGraphicsItem( 1 );
   if( p )
   {
      void * pText;
      ( p )->setToolTip( hb_parstr_utf8( 2, &pText, NULL ) );
      hb_strfree( pText );
   }
}

/* void setTransform ( const QTransform & matrix, bool combine = false ) */
HB_FUNC( QT_QGRAPHICSITEM_SETTRANSFORM )
{
   QGraphicsItem * p = hbqt_par_QGraphicsItem( 1 );
   if( p )
      ( p )->setTransform( *hbqt_par_QTransform( 2 ), hb_parl( 3 ) );
}

/* void setVisible ( bool visible ) */
HB_FUNC( QT_QGRAPHICSITEM_SETVISIBLE )
{
   QGraphicsItem * p = hbqt_par_QGraphicsItem( 1 );
   if( p )
      ( p )->setVisible( hb_parl( 2 ) );
}

/* void setZValue ( qreal z ) */
HB_FUNC( QT_QGRAPHICSITEM_SETZVALUE )
{
   QGraphicsItem * p = hbqt_par_QGraphicsItem( 1 );
   if( p )
      ( p )->setZValue( hb_parnd( 2 ) );
}

/* virtual QPainterPath shape () const */
HB_FUNC( QT_QGRAPHICSITEM_SHAPE )
{
   QGraphicsItem * p = hbqt_par_QGraphicsItem( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QPainterPath( new QPainterPath( ( p )->shape() ), true ) );
}

/* void shear ( qreal sh, qreal sv ) */
HB_FUNC( QT_QGRAPHICSITEM_SHEAR )
{
   QGraphicsItem * p = hbqt_par_QGraphicsItem( 1 );
   if( p )
      ( p )->shear( hb_parnd( 2 ), hb_parnd( 3 ) );
}

/* void show () */
HB_FUNC( QT_QGRAPHICSITEM_SHOW )
{
   QGraphicsItem * p = hbqt_par_QGraphicsItem( 1 );
   if( p )
      ( p )->show();
}

/* QString toolTip () const */
HB_FUNC( QT_QGRAPHICSITEM_TOOLTIP )
{
   QGraphicsItem * p = hbqt_par_QGraphicsItem( 1 );
   if( p )
      hb_retstr_utf8( ( p )->toolTip().toUtf8().data() );
}

/* QGraphicsItem * topLevelItem () const */
HB_FUNC( QT_QGRAPHICSITEM_TOPLEVELITEM )
{
   QGraphicsItem * p = hbqt_par_QGraphicsItem( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QGraphicsItem( ( p )->topLevelItem(), false ) );
}

/* QGraphicsWidget * topLevelWidget () const */
HB_FUNC( QT_QGRAPHICSITEM_TOPLEVELWIDGET )
{
   QGraphicsItem * p = hbqt_par_QGraphicsItem( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QGraphicsWidget( ( p )->topLevelWidget(), false ) );
}

/* QTransform transform () const */
HB_FUNC( QT_QGRAPHICSITEM_TRANSFORM )
{
   QGraphicsItem * p = hbqt_par_QGraphicsItem( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QTransform( new QTransform( ( p )->transform() ), true ) );
}

/* void translate ( qreal dx, qreal dy ) */
HB_FUNC( QT_QGRAPHICSITEM_TRANSLATE )
{
   QGraphicsItem * p = hbqt_par_QGraphicsItem( 1 );
   if( p )
      ( p )->translate( hb_parnd( 2 ), hb_parnd( 3 ) );
}

/* virtual int type () const */
HB_FUNC( QT_QGRAPHICSITEM_TYPE )
{
   QGraphicsItem * p = hbqt_par_QGraphicsItem( 1 );
   if( p )
      hb_retni( ( p )->type() );
}

/* void ungrabKeyboard () */
HB_FUNC( QT_QGRAPHICSITEM_UNGRABKEYBOARD )
{
   QGraphicsItem * p = hbqt_par_QGraphicsItem( 1 );
   if( p )
      ( p )->ungrabKeyboard();
}

/* void ungrabMouse () */
HB_FUNC( QT_QGRAPHICSITEM_UNGRABMOUSE )
{
   QGraphicsItem * p = hbqt_par_QGraphicsItem( 1 );
   if( p )
      ( p )->ungrabMouse();
}

/* void unsetCursor () */
HB_FUNC( QT_QGRAPHICSITEM_UNSETCURSOR )
{
   QGraphicsItem * p = hbqt_par_QGraphicsItem( 1 );
   if( p )
      ( p )->unsetCursor();
}

/* void update ( const QRectF & rect = QRectF() ) */
HB_FUNC( QT_QGRAPHICSITEM_UPDATE )
{
   QGraphicsItem * p = hbqt_par_QGraphicsItem( 1 );
   if( p )
      ( p )->update( ( HB_ISOBJECT( 2 ) ? *hbqt_par_QRectF( 2 ) : QRectF() ) );
}

/* void update ( qreal x, qreal y, qreal width, qreal height ) */
HB_FUNC( QT_QGRAPHICSITEM_UPDATE_1 )
{
   QGraphicsItem * p = hbqt_par_QGraphicsItem( 1 );
   if( p )
      ( p )->update( hb_parnd( 2 ), hb_parnd( 3 ), hb_parnd( 4 ), hb_parnd( 5 ) );
}

/* QGraphicsWidget * window () const */
HB_FUNC( QT_QGRAPHICSITEM_WINDOW )
{
   QGraphicsItem * p = hbqt_par_QGraphicsItem( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QGraphicsWidget( ( p )->window(), false ) );
}

/* qreal x () const */
HB_FUNC( QT_QGRAPHICSITEM_X )
{
   QGraphicsItem * p = hbqt_par_QGraphicsItem( 1 );
   if( p )
      hb_retnd( ( p )->x() );
}

/* qreal y () const */
HB_FUNC( QT_QGRAPHICSITEM_Y )
{
   QGraphicsItem * p = hbqt_par_QGraphicsItem( 1 );
   if( p )
      hb_retnd( ( p )->y() );
}

/* qreal zValue () const */
HB_FUNC( QT_QGRAPHICSITEM_ZVALUE )
{
   QGraphicsItem * p = hbqt_par_QGraphicsItem( 1 );
   if( p )
      hb_retnd( ( p )->zValue() );
}


#endif /* #if QT_VERSION >= 0x040500 */
