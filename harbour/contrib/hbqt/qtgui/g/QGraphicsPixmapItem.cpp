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
 *  enum ShapeMode { MaskShape, BoundingRectShape, HeuristicMaskShape }
 */

/*
 *  Constructed[ 9/9 [ 100.00% ] ]
 *
 */

#include <QtCore/QPointer>

#include <QtGui/QGraphicsPixmapItem>


/*
 * QGraphicsPixmapItem ( QGraphicsItem * parent = 0 )
 * QGraphicsPixmapItem ( const QPixmap & pixmap, QGraphicsItem * parent = 0 )
 * ~QGraphicsPixmapItem ()
 */

typedef struct
{
   QGraphicsPixmapItem * ph;
   bool bNew;
   PHBQT_GC_FUNC func;
   int type;
} HBQT_GC_T_QGraphicsPixmapItem;

HBQT_GC_FUNC( hbqt_gcRelease_QGraphicsPixmapItem )
{
   HBQT_GC_T * p = ( HBQT_GC_T * ) Cargo;

   if( p && p->bNew )
   {
      if( p->ph )
      {
         delete ( ( QGraphicsPixmapItem * ) p->ph );
         p->ph = NULL;
      }
      else
         p->ph = NULL;
   }
   else
      p->ph = NULL;
}

void * hbqt_gcAllocate_QGraphicsPixmapItem( void * pObj, bool bNew )
{
   HBQT_GC_T * p = ( HBQT_GC_T * ) hb_gcAllocate( sizeof( HBQT_GC_T ), hbqt_gcFuncs() );

   p->ph = ( QGraphicsPixmapItem * ) pObj;
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QGraphicsPixmapItem;
   p->type = HBQT_TYPE_QGraphicsPixmapItem;

   return p;
}

HB_FUNC( QT_QGRAPHICSPIXMAPITEM )
{
   QGraphicsPixmapItem * pObj = NULL;

   if( hb_pcount() >= 1 && HB_ISPOINTER( 1 ) )
   {
      HBQT_GC_T * p = ( HBQT_GC_T * ) hb_parptrGC( hbqt_gcFuncs(), 1 );
      if( p->type == HBQT_TYPE_QPixmap )
      {
         pObj = new QGraphicsPixmapItem( *hbqt_par_QPixmap( 1 ), ( HB_ISPOINTER( 2 ) ? hbqt_par_QGraphicsItem( 2 ) : 0 ) ) ;
      }
      else
      {
         pObj = new QGraphicsPixmapItem( hbqt_par_QGraphicsItem( 1 ) ) ;
      }
   }
   else
   {
      pObj = new QGraphicsPixmapItem() ;
   }

   hb_retptrGC( hbqt_gcAllocate_QGraphicsPixmapItem( ( void * ) pObj, true ) );
}

/* QPointF offset () const */
HB_FUNC( QT_QGRAPHICSPIXMAPITEM_OFFSET )
{
   QGraphicsPixmapItem * p = hbqt_par_QGraphicsPixmapItem( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QPointF( new QPointF( ( p )->offset() ), true ) );
}

/* QPixmap pixmap () const */
HB_FUNC( QT_QGRAPHICSPIXMAPITEM_PIXMAP )
{
   QGraphicsPixmapItem * p = hbqt_par_QGraphicsPixmapItem( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QPixmap( new QPixmap( ( p )->pixmap() ), true ) );
}

/* void setOffset ( const QPointF & offset ) */
HB_FUNC( QT_QGRAPHICSPIXMAPITEM_SETOFFSET )
{
   QGraphicsPixmapItem * p = hbqt_par_QGraphicsPixmapItem( 1 );
   if( p )
      ( p )->setOffset( *hbqt_par_QPointF( 2 ) );
}

/* void setOffset ( qreal x, qreal y ) */
HB_FUNC( QT_QGRAPHICSPIXMAPITEM_SETOFFSET_1 )
{
   QGraphicsPixmapItem * p = hbqt_par_QGraphicsPixmapItem( 1 );
   if( p )
      ( p )->setOffset( hb_parnd( 2 ), hb_parnd( 3 ) );
}

/* void setPixmap ( const QPixmap & pixmap ) */
HB_FUNC( QT_QGRAPHICSPIXMAPITEM_SETPIXMAP )
{
   QGraphicsPixmapItem * p = hbqt_par_QGraphicsPixmapItem( 1 );
   if( p )
      ( p )->setPixmap( *hbqt_par_QPixmap( 2 ) );
}

/* void setShapeMode ( ShapeMode mode ) */
HB_FUNC( QT_QGRAPHICSPIXMAPITEM_SETSHAPEMODE )
{
   QGraphicsPixmapItem * p = hbqt_par_QGraphicsPixmapItem( 1 );
   if( p )
      ( p )->setShapeMode( ( QGraphicsPixmapItem::ShapeMode ) hb_parni( 2 ) );
}

/* void setTransformationMode ( Qt::TransformationMode mode ) */
HB_FUNC( QT_QGRAPHICSPIXMAPITEM_SETTRANSFORMATIONMODE )
{
   QGraphicsPixmapItem * p = hbqt_par_QGraphicsPixmapItem( 1 );
   if( p )
      ( p )->setTransformationMode( ( Qt::TransformationMode ) hb_parni( 2 ) );
}

/* ShapeMode shapeMode () const */
HB_FUNC( QT_QGRAPHICSPIXMAPITEM_SHAPEMODE )
{
   QGraphicsPixmapItem * p = hbqt_par_QGraphicsPixmapItem( 1 );
   if( p )
      hb_retni( ( QGraphicsPixmapItem::ShapeMode ) ( p )->shapeMode() );
}

/* Qt::TransformationMode transformationMode () const */
HB_FUNC( QT_QGRAPHICSPIXMAPITEM_TRANSFORMATIONMODE )
{
   QGraphicsPixmapItem * p = hbqt_par_QGraphicsPixmapItem( 1 );
   if( p )
      hb_retni( ( Qt::TransformationMode ) ( p )->transformationMode() );
}


#endif /* #if QT_VERSION >= 0x040500 */
