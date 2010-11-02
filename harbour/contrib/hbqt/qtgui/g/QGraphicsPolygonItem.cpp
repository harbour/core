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
 *  Constructed[ 4/4 [ 100.00% ] ]
 *
 */

#include <QtCore/QPointer>

#include <QtGui/QGraphicsPolygonItem>


/*
 * QGraphicsPolygonItem ( QGraphicsItem * parent = 0 )
 * QGraphicsPolygonItem ( const QPolygonF & polygon, QGraphicsItem * parent = 0 )
 * ~QGraphicsPolygonItem ()
 */

typedef struct
{
   QGraphicsPolygonItem * ph;
   bool bNew;
   PHBQT_GC_FUNC func;
   int type;
} HBQT_GC_T_QGraphicsPolygonItem;

HBQT_GC_FUNC( hbqt_gcRelease_QGraphicsPolygonItem )
{
   HBQT_GC_T * p = ( HBQT_GC_T * ) Cargo;

   if( p && p->bNew )
   {
      if( p->ph )
      {
         delete ( ( QGraphicsPolygonItem * ) p->ph );
         p->ph = NULL;
      }
      else
         p->ph = NULL;
   }
   else
      p->ph = NULL;
}

void * hbqt_gcAllocate_QGraphicsPolygonItem( void * pObj, bool bNew )
{
   HBQT_GC_T * p = ( HBQT_GC_T * ) hb_gcAllocate( sizeof( HBQT_GC_T ), hbqt_gcFuncs() );

   p->ph = ( QGraphicsPolygonItem * ) pObj;
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QGraphicsPolygonItem;
   p->type = HBQT_TYPE_QGraphicsPolygonItem;

   return p;
}

HB_FUNC( QT_QGRAPHICSPOLYGONITEM )
{
   QGraphicsPolygonItem * pObj = NULL;

   if( hb_pcount() >= 1 && HB_ISPOINTER( 1 ) )
   {
      HBQT_GC_T * p = ( HBQT_GC_T * ) hb_parptrGC( hbqt_gcFuncs(), 1 );
      if( p->type == HBQT_TYPE_QPolygonF )
      {
         pObj = new QGraphicsPolygonItem( *hbqt_par_QPolygonF( 1 ), ( HB_ISPOINTER( 2 ) ? hbqt_par_QGraphicsItem( 2 ) : 0 ) ) ;
      }
      else
      {
         pObj = new QGraphicsPolygonItem( hbqt_par_QGraphicsItem( 1 ) ) ;
      }
   }
   else
   {
      pObj = new QGraphicsPolygonItem() ;
   }

   hb_retptrGC( hbqt_gcAllocate_QGraphicsPolygonItem( ( void * ) pObj, true ) );
}

/* Qt::FillRule fillRule () const */
HB_FUNC( QT_QGRAPHICSPOLYGONITEM_FILLRULE )
{
   QGraphicsPolygonItem * p = hbqt_par_QGraphicsPolygonItem( 1 );
   if( p )
      hb_retni( ( Qt::FillRule ) ( p )->fillRule() );
}

/* QPolygonF polygon () const */
HB_FUNC( QT_QGRAPHICSPOLYGONITEM_POLYGON )
{
   QGraphicsPolygonItem * p = hbqt_par_QGraphicsPolygonItem( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QPolygonF( new QPolygonF( ( p )->polygon() ), true ) );
}

/* void setFillRule ( Qt::FillRule rule ) */
HB_FUNC( QT_QGRAPHICSPOLYGONITEM_SETFILLRULE )
{
   QGraphicsPolygonItem * p = hbqt_par_QGraphicsPolygonItem( 1 );
   if( p )
      ( p )->setFillRule( ( Qt::FillRule ) hb_parni( 2 ) );
}

/* void setPolygon ( const QPolygonF & polygon ) */
HB_FUNC( QT_QGRAPHICSPOLYGONITEM_SETPOLYGON )
{
   QGraphicsPolygonItem * p = hbqt_par_QGraphicsPolygonItem( 1 );
   if( p )
      ( p )->setPolygon( *hbqt_par_QPolygonF( 2 ) );
}


#endif /* #if QT_VERSION >= 0x040500 */
