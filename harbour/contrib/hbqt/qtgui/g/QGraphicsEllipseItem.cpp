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
 *  Constructed[ 7/7 [ 100.00% ] ]
 *
 */

#include <QtCore/QPointer>

#include <QtGui/QGraphicsEllipseItem>


/*
 * QGraphicsEllipseItem ( QGraphicsItem * parent = 0 )
 * QGraphicsEllipseItem ( const QRectF & rect, QGraphicsItem * parent = 0 )
 * QGraphicsEllipseItem ( qreal x, qreal y, qreal width, qreal height, QGraphicsItem * parent = 0 )
 * ~QGraphicsEllipseItem ()
 */

typedef struct
{
   QGraphicsEllipseItem * ph;
   bool bNew;
   PHBQT_GC_FUNC func;
   int type;
} HBQT_GC_T_QGraphicsEllipseItem;

HBQT_GC_FUNC( hbqt_gcRelease_QGraphicsEllipseItem )
{
   HBQT_GC_T * p = ( HBQT_GC_T * ) Cargo;

   if( p )
   {
      if( p->bNew && p->ph )
         delete ( ( QGraphicsEllipseItem * ) p->ph );
      p->ph = NULL;
   }
}

void * hbqt_gcAllocate_QGraphicsEllipseItem( void * pObj, bool bNew )
{
   HBQT_GC_T * p = ( HBQT_GC_T * ) hb_gcAllocate( sizeof( HBQT_GC_T ), hbqt_gcFuncs() );

   p->ph = ( QGraphicsEllipseItem * ) pObj;
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QGraphicsEllipseItem;
   p->type = HBQT_TYPE_QGraphicsEllipseItem;

   return p;
}

HB_FUNC( QT_QGRAPHICSELLIPSEITEM )
{
   QGraphicsEllipseItem * pObj = NULL;

   if( hb_pcount() >= 4 && HB_ISNUM( 1 ) && HB_ISNUM( 2 ) && HB_ISNUM( 3 ) && HB_ISNUM( 4 ) )
   {
      pObj = new QGraphicsEllipseItem( hb_parnd( 1 ), hb_parnd( 2 ), hb_parnd( 3 ), hb_parnd( 4 ), ( HB_ISPOINTER( 5 ) ? hbqt_par_QGraphicsItem( 5 ) : 0 ) ) ;
   }
   else if( hb_pcount() >= 1 && HB_ISPOINTER( 1 ) )
   {
      HBQT_GC_T * p = ( HBQT_GC_T * ) hb_parptrGC( hbqt_gcFuncs(), 1 );
      if( p->type == HBQT_TYPE_QRectF )
      {
         pObj = new QGraphicsEllipseItem( *hbqt_par_QRectF( 1 ), ( HB_ISPOINTER( 2 ) ? hbqt_par_QGraphicsItem( 2 ) : 0 ) ) ;
      }
      else
      {
         pObj = new QGraphicsEllipseItem( hbqt_par_QGraphicsItem( 1 ) ) ;
      }
   }
   else
   {
      pObj = new QGraphicsEllipseItem() ;
   }

   hb_retptrGC( hbqt_gcAllocate_QGraphicsEllipseItem( ( void * ) pObj, true ) );
}

/* QRectF rect () const */
HB_FUNC( QT_QGRAPHICSELLIPSEITEM_RECT )
{
   QGraphicsEllipseItem * p = hbqt_par_QGraphicsEllipseItem( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QRectF( new QRectF( ( p )->rect() ), true ) );
}

/* void setRect ( const QRectF & rect ) */
HB_FUNC( QT_QGRAPHICSELLIPSEITEM_SETRECT )
{
   QGraphicsEllipseItem * p = hbqt_par_QGraphicsEllipseItem( 1 );
   if( p )
      ( p )->setRect( *hbqt_par_QRectF( 2 ) );
}

/* void setRect ( qreal x, qreal y, qreal width, qreal height ) */
HB_FUNC( QT_QGRAPHICSELLIPSEITEM_SETRECT_1 )
{
   QGraphicsEllipseItem * p = hbqt_par_QGraphicsEllipseItem( 1 );
   if( p )
      ( p )->setRect( hb_parnd( 2 ), hb_parnd( 3 ), hb_parnd( 4 ), hb_parnd( 5 ) );
}

/* void setSpanAngle ( int angle ) */
HB_FUNC( QT_QGRAPHICSELLIPSEITEM_SETSPANANGLE )
{
   QGraphicsEllipseItem * p = hbqt_par_QGraphicsEllipseItem( 1 );
   if( p )
      ( p )->setSpanAngle( hb_parni( 2 ) );
}

/* void setStartAngle ( int angle ) */
HB_FUNC( QT_QGRAPHICSELLIPSEITEM_SETSTARTANGLE )
{
   QGraphicsEllipseItem * p = hbqt_par_QGraphicsEllipseItem( 1 );
   if( p )
      ( p )->setStartAngle( hb_parni( 2 ) );
}

/* int spanAngle () const */
HB_FUNC( QT_QGRAPHICSELLIPSEITEM_SPANANGLE )
{
   QGraphicsEllipseItem * p = hbqt_par_QGraphicsEllipseItem( 1 );
   if( p )
      hb_retni( ( p )->spanAngle() );
}

/* int startAngle () const */
HB_FUNC( QT_QGRAPHICSELLIPSEITEM_STARTANGLE )
{
   QGraphicsEllipseItem * p = hbqt_par_QGraphicsEllipseItem( 1 );
   if( p )
      hb_retni( ( p )->startAngle() );
}


#endif /* #if QT_VERSION >= 0x040500 */
