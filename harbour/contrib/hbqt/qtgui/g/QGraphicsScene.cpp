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
 *  enum ItemIndexMethod { BspTreeIndex, NoIndex }
 *  enum SceneLayer { ItemLayer, BackgroundLayer, ForegroundLayer, AllLayers }
 *  flags SceneLayers
 */

/*
 *  Constructed[ 69/70 [ 98.57% ] ]
 *
 *  *** Unconvered Prototypes ***
 *
 *  QGraphicsItemGroup * createItemGroup ( const QList<QGraphicsItem *> & items )
 *
 *  *** Commented out protostypes ***
 *
 *  // virtual QVariant inputMethodQuery ( Qt::InputMethodQuery query ) const
 */

#include <QtCore/QPointer>

#include <QtGui/QGraphicsScene>
#include <QtGui/QPalette>

/*
 * QGraphicsScene ( QObject * parent = 0 )
 * QGraphicsScene ( const QRectF & sceneRect, QObject * parent = 0 )
 * QGraphicsScene ( qreal x, qreal y, qreal width, qreal height, QObject * parent = 0 )
 * virtual ~QGraphicsScene ()
 */

typedef struct
{
   QPointer< QGraphicsScene > ph;
   bool bNew;
   PHBQT_GC_FUNC func;
   int type;
} HBQT_GC_T_QGraphicsScene;

HBQT_GC_FUNC( hbqt_gcRelease_QGraphicsScene )
{
   QGraphicsScene  * ph = NULL;
   HBQT_GC_T_QGraphicsScene * p = ( HBQT_GC_T_QGraphicsScene * ) Cargo;

   if( p && p->bNew && p->ph )
   {
      ph = p->ph;
      if( ph )
      {
         const QMetaObject * m = ( ph )->metaObject();
         if( ( QString ) m->className() != ( QString ) "QObject" )
         {
            delete ( p->ph );
            p->ph = NULL;
         }
         else
            p->ph = NULL;
      }
      else
         p->ph = NULL;
   }
   else
      p->ph = NULL;
}

void * hbqt_gcAllocate_QGraphicsScene( void * pObj, bool bNew )
{
   HBQT_GC_T_QGraphicsScene * p = ( HBQT_GC_T_QGraphicsScene * ) hb_gcAllocate( sizeof( HBQT_GC_T_QGraphicsScene ), hbqt_gcFuncs() );

   new( & p->ph ) QPointer< QGraphicsScene >( ( QGraphicsScene * ) pObj );
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QGraphicsScene;
   p->type = HBQT_TYPE_QGraphicsScene;

   return p;
}

HB_FUNC( QT_QGRAPHICSSCENE )
{
   QGraphicsScene * pObj = NULL;

   if( hb_pcount() >= 4 && HB_ISNUM( 1 ) && HB_ISNUM( 2 ) && HB_ISNUM( 3 ) && HB_ISNUM( 4 ) )
   {
      pObj = new QGraphicsScene( hb_parnd( 1 ), hb_parnd( 2 ), hb_parnd( 3 ), hb_parnd( 4 ), ( HB_ISPOINTER( 5 ) ? hbqt_par_QObject( 5 ) : 0 ) ) ;
   }
   else if( hb_pcount() >= 1 && HB_ISPOINTER( 1 ) )
   {
      HBQT_GC_T * p = ( HBQT_GC_T * ) hb_parptrGC( hbqt_gcFuncs(), 1 );
      if( p->type == HBQT_TYPE_QRectF )
      {
         pObj = new QGraphicsScene( *hbqt_par_QRectF( 1 ), ( HB_ISPOINTER( 2 ) ? hbqt_par_QObject( 2 ) : 0 ) ) ;
      }
      else
      {
         pObj = new QGraphicsScene( hbqt_par_QObject( 1 ) ) ;
      }
   }
   else
   {
      pObj = new QGraphicsScene() ;
   }

   hb_retptrGC( hbqt_gcAllocate_QGraphicsScene( ( void * ) pObj, true ) );
}

/* QGraphicsWidget * activeWindow () const */
HB_FUNC( QT_QGRAPHICSSCENE_ACTIVEWINDOW )
{
   QGraphicsScene * p = hbqt_par_QGraphicsScene( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QGraphicsWidget( ( p )->activeWindow(), false ) );
}

/* QGraphicsEllipseItem * addEllipse ( const QRectF & rect, const QPen & pen = QPen(), const QBrush & brush = QBrush() ) */
HB_FUNC( QT_QGRAPHICSSCENE_ADDELLIPSE )
{
   QGraphicsScene * p = hbqt_par_QGraphicsScene( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QGraphicsEllipseItem( ( p )->addEllipse( *hbqt_par_QRectF( 2 ), ( HB_ISOBJECT( 3 ) ? *hbqt_par_QPen( 3 ) : QPen() ), ( HB_ISOBJECT( 4 ) ? *hbqt_par_QBrush( 4 ) : QBrush() ) ), false ) );
}

/* QGraphicsEllipseItem * addEllipse ( qreal x, qreal y, qreal w, qreal h, const QPen & pen = QPen(), const QBrush & brush = QBrush() ) */
HB_FUNC( QT_QGRAPHICSSCENE_ADDELLIPSE_1 )
{
   QGraphicsScene * p = hbqt_par_QGraphicsScene( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QGraphicsEllipseItem( ( p )->addEllipse( hb_parnd( 2 ), hb_parnd( 3 ), hb_parnd( 4 ), hb_parnd( 5 ), ( HB_ISOBJECT( 6 ) ? *hbqt_par_QPen( 6 ) : QPen() ), ( HB_ISOBJECT( 7 ) ? *hbqt_par_QBrush( 7 ) : QBrush() ) ), false ) );
}

/* void addItem ( QGraphicsItem * item )   [*D=1*] */
HB_FUNC( QT_QGRAPHICSSCENE_ADDITEM )
{
   QGraphicsScene * p = hbqt_par_QGraphicsScene( 1 );
   if( p )
   {
      hbqt_detachgcpointer( 2 );
      ( p )->addItem( hbqt_par_QGraphicsItem( 2 ) );
   }
}

/* QGraphicsLineItem * addLine ( const QLineF & line, const QPen & pen = QPen() ) */
HB_FUNC( QT_QGRAPHICSSCENE_ADDLINE )
{
   QGraphicsScene * p = hbqt_par_QGraphicsScene( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QGraphicsLineItem( ( p )->addLine( *hbqt_par_QLineF( 2 ), ( HB_ISOBJECT( 3 ) ? *hbqt_par_QPen( 3 ) : QPen() ) ), false ) );
}

/* QGraphicsLineItem * addLine ( qreal x1, qreal y1, qreal x2, qreal y2, const QPen & pen = QPen() ) */
HB_FUNC( QT_QGRAPHICSSCENE_ADDLINE_1 )
{
   QGraphicsScene * p = hbqt_par_QGraphicsScene( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QGraphicsLineItem( ( p )->addLine( hb_parnd( 2 ), hb_parnd( 3 ), hb_parnd( 4 ), hb_parnd( 5 ), ( HB_ISOBJECT( 6 ) ? *hbqt_par_QPen( 6 ) : QPen() ) ), false ) );
}

/* QGraphicsPathItem * addPath ( const QPainterPath & path, const QPen & pen = QPen(), const QBrush & brush = QBrush() ) */
HB_FUNC( QT_QGRAPHICSSCENE_ADDPATH )
{
   QGraphicsScene * p = hbqt_par_QGraphicsScene( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QGraphicsPathItem( ( p )->addPath( *hbqt_par_QPainterPath( 2 ), ( HB_ISOBJECT( 3 ) ? *hbqt_par_QPen( 3 ) : QPen() ), ( HB_ISOBJECT( 4 ) ? *hbqt_par_QBrush( 4 ) : QBrush() ) ), false ) );
}

/* QGraphicsPixmapItem * addPixmap ( const QPixmap & pixmap ) */
HB_FUNC( QT_QGRAPHICSSCENE_ADDPIXMAP )
{
   QGraphicsScene * p = hbqt_par_QGraphicsScene( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QGraphicsPixmapItem( ( p )->addPixmap( *hbqt_par_QPixmap( 2 ) ), false ) );
}

/* QGraphicsPolygonItem * addPolygon ( const QPolygonF & polygon, const QPen & pen = QPen(), const QBrush & brush = QBrush() ) */
HB_FUNC( QT_QGRAPHICSSCENE_ADDPOLYGON )
{
   QGraphicsScene * p = hbqt_par_QGraphicsScene( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QGraphicsPolygonItem( ( p )->addPolygon( *hbqt_par_QPolygonF( 2 ), ( HB_ISOBJECT( 3 ) ? *hbqt_par_QPen( 3 ) : QPen() ), ( HB_ISOBJECT( 4 ) ? *hbqt_par_QBrush( 4 ) : QBrush() ) ), false ) );
}

/* QGraphicsRectItem * addRect ( const QRectF & rect, const QPen & pen = QPen(), const QBrush & brush = QBrush() ) */
HB_FUNC( QT_QGRAPHICSSCENE_ADDRECT )
{
   QGraphicsScene * p = hbqt_par_QGraphicsScene( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QGraphicsRectItem( ( p )->addRect( *hbqt_par_QRectF( 2 ), ( HB_ISOBJECT( 3 ) ? *hbqt_par_QPen( 3 ) : QPen() ), ( HB_ISOBJECT( 4 ) ? *hbqt_par_QBrush( 4 ) : QBrush() ) ), false ) );
}

/* QGraphicsRectItem * addRect ( qreal x, qreal y, qreal w, qreal h, const QPen & pen = QPen(), const QBrush & brush = QBrush() ) */
HB_FUNC( QT_QGRAPHICSSCENE_ADDRECT_1 )
{
   QGraphicsScene * p = hbqt_par_QGraphicsScene( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QGraphicsRectItem( ( p )->addRect( hb_parnd( 2 ), hb_parnd( 3 ), hb_parnd( 4 ), hb_parnd( 5 ), ( HB_ISOBJECT( 6 ) ? *hbqt_par_QPen( 6 ) : QPen() ), ( HB_ISOBJECT( 7 ) ? *hbqt_par_QBrush( 7 ) : QBrush() ) ), false ) );
}

/* QGraphicsSimpleTextItem * addSimpleText ( const QString & text, const QFont & font = QFont() ) */
HB_FUNC( QT_QGRAPHICSSCENE_ADDSIMPLETEXT )
{
   QGraphicsScene * p = hbqt_par_QGraphicsScene( 1 );
   if( p )
   {
      void * pText;
      hb_retptrGC( hbqt_gcAllocate_QGraphicsSimpleTextItem( ( p )->addSimpleText( hb_parstr_utf8( 2, &pText, NULL ), ( HB_ISOBJECT( 3 ) ? *hbqt_par_QFont( 3 ) : QFont() ) ), false ) );
      hb_strfree( pText );
   }
}

/* QGraphicsTextItem * addText ( const QString & text, const QFont & font = QFont() ) */
HB_FUNC( QT_QGRAPHICSSCENE_ADDTEXT )
{
   QGraphicsScene * p = hbqt_par_QGraphicsScene( 1 );
   if( p )
   {
      void * pText;
      hb_retptrGC( hbqt_gcAllocate_QGraphicsTextItem( ( p )->addText( hb_parstr_utf8( 2, &pText, NULL ), ( HB_ISOBJECT( 3 ) ? *hbqt_par_QFont( 3 ) : QFont() ) ), false ) );
      hb_strfree( pText );
   }
}

/* QGraphicsProxyWidget * addWidget ( QWidget * widget, Qt::WindowFlags wFlags = 0 ) */
HB_FUNC( QT_QGRAPHICSSCENE_ADDWIDGET )
{
   QGraphicsScene * p = hbqt_par_QGraphicsScene( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QGraphicsProxyWidget( ( p )->addWidget( hbqt_par_QWidget( 2 ), ( Qt::WindowFlags ) hb_parni( 3 ) ), false ) );
}

/* QBrush backgroundBrush () const */
HB_FUNC( QT_QGRAPHICSSCENE_BACKGROUNDBRUSH )
{
   QGraphicsScene * p = hbqt_par_QGraphicsScene( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QBrush( new QBrush( ( p )->backgroundBrush() ), true ) );
}

/* int bspTreeDepth () const */
HB_FUNC( QT_QGRAPHICSSCENE_BSPTREEDEPTH )
{
   QGraphicsScene * p = hbqt_par_QGraphicsScene( 1 );
   if( p )
      hb_retni( ( p )->bspTreeDepth() );
}

/* void clearFocus () */
HB_FUNC( QT_QGRAPHICSSCENE_CLEARFOCUS )
{
   QGraphicsScene * p = hbqt_par_QGraphicsScene( 1 );
   if( p )
      ( p )->clearFocus();
}

/* QList<QGraphicsItem *> collidingItems ( const QGraphicsItem * item, Qt::ItemSelectionMode mode = Qt::IntersectsItemShape ) const */
HB_FUNC( QT_QGRAPHICSSCENE_COLLIDINGITEMS )
{
   QGraphicsScene * p = hbqt_par_QGraphicsScene( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QList( new QList<QGraphicsItem *>( ( p )->collidingItems( hbqt_par_QGraphicsItem( 2 ), ( HB_ISNUM( 3 ) ? ( Qt::ItemSelectionMode ) hb_parni( 3 ) : ( Qt::ItemSelectionMode ) Qt::IntersectsItemShape ) ) ), true ) );
}

/* void destroyItemGroup ( QGraphicsItemGroup * group ) */
HB_FUNC( QT_QGRAPHICSSCENE_DESTROYITEMGROUP )
{
   QGraphicsScene * p = hbqt_par_QGraphicsScene( 1 );
   if( p )
      ( p )->destroyItemGroup( hbqt_par_QGraphicsItemGroup( 2 ) );
}

/* QGraphicsItem * focusItem () const */
HB_FUNC( QT_QGRAPHICSSCENE_FOCUSITEM )
{
   QGraphicsScene * p = hbqt_par_QGraphicsScene( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QGraphicsItem( ( p )->focusItem(), false ) );
}

/* QFont font () const */
HB_FUNC( QT_QGRAPHICSSCENE_FONT )
{
   QGraphicsScene * p = hbqt_par_QGraphicsScene( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QFont( new QFont( ( p )->font() ), true ) );
}

/* QBrush foregroundBrush () const */
HB_FUNC( QT_QGRAPHICSSCENE_FOREGROUNDBRUSH )
{
   QGraphicsScene * p = hbqt_par_QGraphicsScene( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QBrush( new QBrush( ( p )->foregroundBrush() ), true ) );
}

/* bool hasFocus () const */
HB_FUNC( QT_QGRAPHICSSCENE_HASFOCUS )
{
   QGraphicsScene * p = hbqt_par_QGraphicsScene( 1 );
   if( p )
      hb_retl( ( p )->hasFocus() );
}

/* qreal height () const */
HB_FUNC( QT_QGRAPHICSSCENE_HEIGHT )
{
   QGraphicsScene * p = hbqt_par_QGraphicsScene( 1 );
   if( p )
      hb_retnd( ( p )->height() );
}

/* void invalidate ( qreal x, qreal y, qreal w, qreal h, SceneLayers layers = AllLayers ) */
HB_FUNC( QT_QGRAPHICSSCENE_INVALIDATE )
{
   QGraphicsScene * p = hbqt_par_QGraphicsScene( 1 );
   if( p )
      ( p )->invalidate( hb_parnd( 2 ), hb_parnd( 3 ), hb_parnd( 4 ), hb_parnd( 5 ), ( HB_ISNUM( 6 ) ? ( QGraphicsScene::SceneLayers ) hb_parni( 6 ) : ( QGraphicsScene::SceneLayers ) QGraphicsScene::AllLayers ) );
}

/* bool isSortCacheEnabled () const */
HB_FUNC( QT_QGRAPHICSSCENE_ISSORTCACHEENABLED )
{
   QGraphicsScene * p = hbqt_par_QGraphicsScene( 1 );
   if( p )
      hb_retl( ( p )->isSortCacheEnabled() );
}

/* QGraphicsItem * itemAt ( const QPointF & position ) const */
HB_FUNC( QT_QGRAPHICSSCENE_ITEMAT )
{
   QGraphicsScene * p = hbqt_par_QGraphicsScene( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QGraphicsItem( ( p )->itemAt( *hbqt_par_QPointF( 2 ) ), false ) );
}

/* QGraphicsItem * itemAt ( qreal x, qreal y ) const */
HB_FUNC( QT_QGRAPHICSSCENE_ITEMAT_1 )
{
   QGraphicsScene * p = hbqt_par_QGraphicsScene( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QGraphicsItem( ( p )->itemAt( hb_parnd( 2 ), hb_parnd( 3 ) ), false ) );
}

/* ItemIndexMethod itemIndexMethod () const */
HB_FUNC( QT_QGRAPHICSSCENE_ITEMINDEXMETHOD )
{
   QGraphicsScene * p = hbqt_par_QGraphicsScene( 1 );
   if( p )
      hb_retni( ( QGraphicsScene::ItemIndexMethod ) ( p )->itemIndexMethod() );
}

/* QList<QGraphicsItem *> items () const */
HB_FUNC( QT_QGRAPHICSSCENE_ITEMS )
{
   QGraphicsScene * p = hbqt_par_QGraphicsScene( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QList( new QList<QGraphicsItem *>( ( p )->items() ), true ) );
}

/* QList<QGraphicsItem *> items ( const QPointF & pos ) const */
HB_FUNC( QT_QGRAPHICSSCENE_ITEMS_1 )
{
   QGraphicsScene * p = hbqt_par_QGraphicsScene( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QList( new QList<QGraphicsItem *>( ( p )->items( *hbqt_par_QPointF( 2 ) ) ), true ) );
}

/* QList<QGraphicsItem *> items ( qreal x, qreal y, qreal w, qreal h, Qt::ItemSelectionMode mode = Qt::IntersectsItemShape ) const */
HB_FUNC( QT_QGRAPHICSSCENE_ITEMS_2 )
{
   QGraphicsScene * p = hbqt_par_QGraphicsScene( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QList( new QList<QGraphicsItem *>( ( p )->items( hb_parnd( 2 ), hb_parnd( 3 ), hb_parnd( 4 ), hb_parnd( 5 ), ( HB_ISNUM( 6 ) ? ( Qt::ItemSelectionMode ) hb_parni( 6 ) : ( Qt::ItemSelectionMode ) Qt::IntersectsItemShape ) ) ), true ) );
}

/* QList<QGraphicsItem *> items ( const QRectF & rectangle, Qt::ItemSelectionMode mode = Qt::IntersectsItemShape ) const */
HB_FUNC( QT_QGRAPHICSSCENE_ITEMS_3 )
{
   QGraphicsScene * p = hbqt_par_QGraphicsScene( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QList( new QList<QGraphicsItem *>( ( p )->items( *hbqt_par_QRectF( 2 ), ( HB_ISNUM( 3 ) ? ( Qt::ItemSelectionMode ) hb_parni( 3 ) : ( Qt::ItemSelectionMode ) Qt::IntersectsItemShape ) ) ), true ) );
}

/* QList<QGraphicsItem *> items ( const QPolygonF & polygon, Qt::ItemSelectionMode mode = Qt::IntersectsItemShape ) const */
HB_FUNC( QT_QGRAPHICSSCENE_ITEMS_4 )
{
   QGraphicsScene * p = hbqt_par_QGraphicsScene( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QList( new QList<QGraphicsItem *>( ( p )->items( *hbqt_par_QPolygonF( 2 ), ( HB_ISNUM( 3 ) ? ( Qt::ItemSelectionMode ) hb_parni( 3 ) : ( Qt::ItemSelectionMode ) Qt::IntersectsItemShape ) ) ), true ) );
}

/* QList<QGraphicsItem *> items ( const QPainterPath & path, Qt::ItemSelectionMode mode = Qt::IntersectsItemShape ) const */
HB_FUNC( QT_QGRAPHICSSCENE_ITEMS_5 )
{
   QGraphicsScene * p = hbqt_par_QGraphicsScene( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QList( new QList<QGraphicsItem *>( ( p )->items( *hbqt_par_QPainterPath( 2 ), ( HB_ISNUM( 3 ) ? ( Qt::ItemSelectionMode ) hb_parni( 3 ) : ( Qt::ItemSelectionMode ) Qt::IntersectsItemShape ) ) ), true ) );
}

/* QRectF itemsBoundingRect () const */
HB_FUNC( QT_QGRAPHICSSCENE_ITEMSBOUNDINGRECT )
{
   QGraphicsScene * p = hbqt_par_QGraphicsScene( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QRectF( new QRectF( ( p )->itemsBoundingRect() ), true ) );
}

/* QGraphicsItem * mouseGrabberItem () const */
HB_FUNC( QT_QGRAPHICSSCENE_MOUSEGRABBERITEM )
{
   QGraphicsScene * p = hbqt_par_QGraphicsScene( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QGraphicsItem( ( p )->mouseGrabberItem(), false ) );
}

/* QPalette palette () const */
HB_FUNC( QT_QGRAPHICSSCENE_PALETTE )
{
   QGraphicsScene * p = hbqt_par_QGraphicsScene( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QPalette( new QPalette( ( p )->palette() ), true ) );
}

/* void removeItem ( QGraphicsItem * item ) */
HB_FUNC( QT_QGRAPHICSSCENE_REMOVEITEM )
{
   QGraphicsScene * p = hbqt_par_QGraphicsScene( 1 );
   if( p )
      ( p )->removeItem( hbqt_par_QGraphicsItem( 2 ) );
}

/* void render ( QPainter * painter, const QRectF & target = QRectF(), const QRectF & source = QRectF(), Qt::AspectRatioMode aspectRatioMode = Qt::KeepAspectRatio ) */
HB_FUNC( QT_QGRAPHICSSCENE_RENDER )
{
   QGraphicsScene * p = hbqt_par_QGraphicsScene( 1 );
   if( p )
      ( p )->render( hbqt_par_QPainter( 2 ), ( HB_ISOBJECT( 3 ) ? *hbqt_par_QRectF( 3 ) : QRectF() ), ( HB_ISOBJECT( 4 ) ? *hbqt_par_QRectF( 4 ) : QRectF() ), ( HB_ISNUM( 5 ) ? ( Qt::AspectRatioMode ) hb_parni( 5 ) : ( Qt::AspectRatioMode ) Qt::KeepAspectRatio ) );
}

/* QRectF sceneRect () const */
HB_FUNC( QT_QGRAPHICSSCENE_SCENERECT )
{
   QGraphicsScene * p = hbqt_par_QGraphicsScene( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QRectF( new QRectF( ( p )->sceneRect() ), true ) );
}

/* QList<QGraphicsItem *> selectedItems () const */
HB_FUNC( QT_QGRAPHICSSCENE_SELECTEDITEMS )
{
   QGraphicsScene * p = hbqt_par_QGraphicsScene( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QList( new QList<QGraphicsItem *>( ( p )->selectedItems() ), true ) );
}

/* QPainterPath selectionArea () const */
HB_FUNC( QT_QGRAPHICSSCENE_SELECTIONAREA )
{
   QGraphicsScene * p = hbqt_par_QGraphicsScene( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QPainterPath( new QPainterPath( ( p )->selectionArea() ), true ) );
}

/* void setActiveWindow ( QGraphicsWidget * widget ) */
HB_FUNC( QT_QGRAPHICSSCENE_SETACTIVEWINDOW )
{
   QGraphicsScene * p = hbqt_par_QGraphicsScene( 1 );
   if( p )
      ( p )->setActiveWindow( hbqt_par_QGraphicsWidget( 2 ) );
}

/* void setBackgroundBrush ( const QBrush & brush ) */
HB_FUNC( QT_QGRAPHICSSCENE_SETBACKGROUNDBRUSH )
{
   QGraphicsScene * p = hbqt_par_QGraphicsScene( 1 );
   if( p )
      ( p )->setBackgroundBrush( *hbqt_par_QBrush( 2 ) );
}

/* void setBspTreeDepth ( int depth ) */
HB_FUNC( QT_QGRAPHICSSCENE_SETBSPTREEDEPTH )
{
   QGraphicsScene * p = hbqt_par_QGraphicsScene( 1 );
   if( p )
      ( p )->setBspTreeDepth( hb_parni( 2 ) );
}

/* void setFocus ( Qt::FocusReason focusReason = Qt::OtherFocusReason ) */
HB_FUNC( QT_QGRAPHICSSCENE_SETFOCUS )
{
   QGraphicsScene * p = hbqt_par_QGraphicsScene( 1 );
   if( p )
      ( p )->setFocus( ( HB_ISNUM( 2 ) ? ( Qt::FocusReason ) hb_parni( 2 ) : ( Qt::FocusReason ) Qt::OtherFocusReason ) );
}

/* void setFocusItem ( QGraphicsItem * item, Qt::FocusReason focusReason = Qt::OtherFocusReason ) */
HB_FUNC( QT_QGRAPHICSSCENE_SETFOCUSITEM )
{
   QGraphicsScene * p = hbqt_par_QGraphicsScene( 1 );
   if( p )
      ( p )->setFocusItem( hbqt_par_QGraphicsItem( 2 ), ( HB_ISNUM( 3 ) ? ( Qt::FocusReason ) hb_parni( 3 ) : ( Qt::FocusReason ) Qt::OtherFocusReason ) );
}

/* void setFont ( const QFont & font ) */
HB_FUNC( QT_QGRAPHICSSCENE_SETFONT )
{
   QGraphicsScene * p = hbqt_par_QGraphicsScene( 1 );
   if( p )
      ( p )->setFont( *hbqt_par_QFont( 2 ) );
}

/* void setForegroundBrush ( const QBrush & brush ) */
HB_FUNC( QT_QGRAPHICSSCENE_SETFOREGROUNDBRUSH )
{
   QGraphicsScene * p = hbqt_par_QGraphicsScene( 1 );
   if( p )
      ( p )->setForegroundBrush( *hbqt_par_QBrush( 2 ) );
}

/* void setItemIndexMethod ( ItemIndexMethod method ) */
HB_FUNC( QT_QGRAPHICSSCENE_SETITEMINDEXMETHOD )
{
   QGraphicsScene * p = hbqt_par_QGraphicsScene( 1 );
   if( p )
      ( p )->setItemIndexMethod( ( QGraphicsScene::ItemIndexMethod ) hb_parni( 2 ) );
}

/* void setPalette ( const QPalette & palette ) */
HB_FUNC( QT_QGRAPHICSSCENE_SETPALETTE )
{
   QGraphicsScene * p = hbqt_par_QGraphicsScene( 1 );
   if( p )
      ( p )->setPalette( *hbqt_par_QPalette( 2 ) );
}

/* void setSceneRect ( const QRectF & rect ) */
HB_FUNC( QT_QGRAPHICSSCENE_SETSCENERECT )
{
   QGraphicsScene * p = hbqt_par_QGraphicsScene( 1 );
   if( p )
      ( p )->setSceneRect( *hbqt_par_QRectF( 2 ) );
}

/* void setSceneRect ( qreal x, qreal y, qreal w, qreal h ) */
HB_FUNC( QT_QGRAPHICSSCENE_SETSCENERECT_1 )
{
   QGraphicsScene * p = hbqt_par_QGraphicsScene( 1 );
   if( p )
      ( p )->setSceneRect( hb_parnd( 2 ), hb_parnd( 3 ), hb_parnd( 4 ), hb_parnd( 5 ) );
}

/* void setSelectionArea ( const QPainterPath & path ) */
HB_FUNC( QT_QGRAPHICSSCENE_SETSELECTIONAREA )
{
   QGraphicsScene * p = hbqt_par_QGraphicsScene( 1 );
   if( p )
      ( p )->setSelectionArea( *hbqt_par_QPainterPath( 2 ) );
}

/* void setSelectionArea ( const QPainterPath & path, Qt::ItemSelectionMode mode ) */
HB_FUNC( QT_QGRAPHICSSCENE_SETSELECTIONAREA_1 )
{
   QGraphicsScene * p = hbqt_par_QGraphicsScene( 1 );
   if( p )
      ( p )->setSelectionArea( *hbqt_par_QPainterPath( 2 ), ( Qt::ItemSelectionMode ) hb_parni( 3 ) );
}

/* void setSortCacheEnabled ( bool enabled ) */
HB_FUNC( QT_QGRAPHICSSCENE_SETSORTCACHEENABLED )
{
   QGraphicsScene * p = hbqt_par_QGraphicsScene( 1 );
   if( p )
      ( p )->setSortCacheEnabled( hb_parl( 2 ) );
}

/* void setStickyFocus ( bool enabled ) */
HB_FUNC( QT_QGRAPHICSSCENE_SETSTICKYFOCUS )
{
   QGraphicsScene * p = hbqt_par_QGraphicsScene( 1 );
   if( p )
      ( p )->setStickyFocus( hb_parl( 2 ) );
}

/* void setStyle ( QStyle * style ) */
HB_FUNC( QT_QGRAPHICSSCENE_SETSTYLE )
{
   QGraphicsScene * p = hbqt_par_QGraphicsScene( 1 );
   if( p )
      ( p )->setStyle( hbqt_par_QStyle( 2 ) );
}

/* bool stickyFocus () const */
HB_FUNC( QT_QGRAPHICSSCENE_STICKYFOCUS )
{
   QGraphicsScene * p = hbqt_par_QGraphicsScene( 1 );
   if( p )
      hb_retl( ( p )->stickyFocus() );
}

/* QStyle * style () const */
HB_FUNC( QT_QGRAPHICSSCENE_STYLE )
{
   QGraphicsScene * p = hbqt_par_QGraphicsScene( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QStyle( ( p )->style(), false ) );
}

/* void update ( qreal x, qreal y, qreal w, qreal h ) */
HB_FUNC( QT_QGRAPHICSSCENE_UPDATE )
{
   QGraphicsScene * p = hbqt_par_QGraphicsScene( 1 );
   if( p )
      ( p )->update( hb_parnd( 2 ), hb_parnd( 3 ), hb_parnd( 4 ), hb_parnd( 5 ) );
}

/* QList<QGraphicsView *> views () const */
HB_FUNC( QT_QGRAPHICSSCENE_VIEWS )
{
   QGraphicsScene * p = hbqt_par_QGraphicsScene( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QList( new QList<QGraphicsView *>( ( p )->views() ), true ) );
}

/* qreal width () const */
HB_FUNC( QT_QGRAPHICSSCENE_WIDTH )
{
   QGraphicsScene * p = hbqt_par_QGraphicsScene( 1 );
   if( p )
      hb_retnd( ( p )->width() );
}

/* void advance () */
HB_FUNC( QT_QGRAPHICSSCENE_ADVANCE )
{
   QGraphicsScene * p = hbqt_par_QGraphicsScene( 1 );
   if( p )
      ( p )->advance();
}

/* void clear () */
HB_FUNC( QT_QGRAPHICSSCENE_CLEAR )
{
   QGraphicsScene * p = hbqt_par_QGraphicsScene( 1 );
   if( p )
      ( p )->clear();
}

/* void clearSelection () */
HB_FUNC( QT_QGRAPHICSSCENE_CLEARSELECTION )
{
   QGraphicsScene * p = hbqt_par_QGraphicsScene( 1 );
   if( p )
      ( p )->clearSelection();
}

/* void invalidate ( const QRectF & rect = QRectF(), SceneLayers layers = AllLayers ) */
HB_FUNC( QT_QGRAPHICSSCENE_INVALIDATE_1 )
{
   QGraphicsScene * p = hbqt_par_QGraphicsScene( 1 );
   if( p )
      ( p )->invalidate( ( HB_ISOBJECT( 2 ) ? *hbqt_par_QRectF( 2 ) : QRectF() ), ( HB_ISNUM( 3 ) ? ( QGraphicsScene::SceneLayers ) hb_parni( 3 ) : ( QGraphicsScene::SceneLayers ) QGraphicsScene::AllLayers ) );
}

/* void update ( const QRectF & rect = QRectF() ) */
HB_FUNC( QT_QGRAPHICSSCENE_UPDATE_1 )
{
   QGraphicsScene * p = hbqt_par_QGraphicsScene( 1 );
   if( p )
      ( p )->update( ( HB_ISOBJECT( 2 ) ? *hbqt_par_QRectF( 2 ) : QRectF() ) );
}


#endif /* #if QT_VERSION >= 0x040500 */
