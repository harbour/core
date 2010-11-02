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
 *  Constructed[ 3/3 [ 100.00% ] ]
 *
 */

#include <QtCore/QPointer>

#include <QtGui/QGraphicsRectItem>


/*
 * QGraphicsRectItem ( QGraphicsItem * parent = 0 )
 * QGraphicsRectItem ( const QRectF & rect, QGraphicsItem * parent = 0 )
 * QGraphicsRectItem ( qreal x, qreal y, qreal width, qreal height, QGraphicsItem * parent = 0 )
 * ~QGraphicsRectItem ()
 */

typedef struct
{
   QGraphicsRectItem * ph;
   bool bNew;
   PHBQT_GC_FUNC func;
   int type;
} HBQT_GC_T_QGraphicsRectItem;

HBQT_GC_FUNC( hbqt_gcRelease_QGraphicsRectItem )
{
   HBQT_GC_T * p = ( HBQT_GC_T * ) Cargo;

   if( p && p->bNew )
   {
      if( p->ph )
      {
         delete ( ( QGraphicsRectItem * ) p->ph );
         p->ph = NULL;
      }
      else
         p->ph = NULL;
   }
   else
      p->ph = NULL;
}

void * hbqt_gcAllocate_QGraphicsRectItem( void * pObj, bool bNew )
{
   HBQT_GC_T * p = ( HBQT_GC_T * ) hb_gcAllocate( sizeof( HBQT_GC_T ), hbqt_gcFuncs() );

   p->ph = ( QGraphicsRectItem * ) pObj;
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QGraphicsRectItem;
   p->type = HBQT_TYPE_QGraphicsRectItem;

   return p;
}

HB_FUNC( QT_QGRAPHICSRECTITEM )
{
   QGraphicsRectItem * pObj = NULL;

   if( hb_pcount() >= 4 && HB_ISNUM( 1 ) && HB_ISNUM( 2 ) && HB_ISNUM( 3 ) && HB_ISNUM( 4 ) )
   {
      pObj = new QGraphicsRectItem( hb_parnd( 1 ), hb_parnd( 2 ), hb_parnd( 3 ), hb_parnd( 4 ), ( HB_ISPOINTER( 5 ) ? hbqt_par_QGraphicsItem( 5 ) : 0 ) ) ;
   }
   else if( hb_pcount() >= 1 && HB_ISPOINTER( 1 ) )
   {
      HBQT_GC_T * p = ( HBQT_GC_T * ) hb_parptrGC( hbqt_gcFuncs(), 1 );
      if( p->type == HBQT_TYPE_QRectF )
      {
         pObj = new QGraphicsRectItem( *hbqt_par_QRectF( 1 ), ( HB_ISPOINTER( 2 ) ? hbqt_par_QGraphicsItem( 2 ) : 0 ) ) ;
      }
      else
      {
         pObj = new QGraphicsRectItem( hbqt_par_QGraphicsItem( 1 ) ) ;
      }
   }
   else
   {
      pObj = new QGraphicsRectItem() ;
   }

   hb_retptrGC( hbqt_gcAllocate_QGraphicsRectItem( ( void * ) pObj, true ) );
}

/* QRectF rect () const */
HB_FUNC( QT_QGRAPHICSRECTITEM_RECT )
{
   QGraphicsRectItem * p = hbqt_par_QGraphicsRectItem( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QRectF( new QRectF( ( p )->rect() ), true ) );
}

/* void setRect ( const QRectF & rectangle ) */
HB_FUNC( QT_QGRAPHICSRECTITEM_SETRECT )
{
   QGraphicsRectItem * p = hbqt_par_QGraphicsRectItem( 1 );
   if( p )
      ( p )->setRect( *hbqt_par_QRectF( 2 ) );
}

/* void setRect ( qreal x, qreal y, qreal width, qreal height ) */
HB_FUNC( QT_QGRAPHICSRECTITEM_SETRECT_1 )
{
   QGraphicsRectItem * p = hbqt_par_QGraphicsRectItem( 1 );
   if( p )
      ( p )->setRect( hb_parnd( 2 ), hb_parnd( 3 ), hb_parnd( 4 ), hb_parnd( 5 ) );
}


#endif /* #if QT_VERSION >= 0x040500 */
