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
 *  Constructed[ 5/5 [ 100.00% ] ]
 *
 */

#include <QtCore/QPointer>

#include <QtGui/QGraphicsLineItem>
#include <QtGui/QPen>


/*
 * QGraphicsLineItem ( QGraphicsItem * parent = 0 )
 * QGraphicsLineItem ( const QLineF & line, QGraphicsItem * parent = 0 )
 * QGraphicsLineItem ( qreal x1, qreal y1, qreal x2, qreal y2, QGraphicsItem * parent = 0 )
 * ~QGraphicsLineItem ()
 */

typedef struct
{
   QGraphicsLineItem * ph;
   bool bNew;
   PHBQT_GC_FUNC func;
   int type;
} HBQT_GC_T_QGraphicsLineItem;

HBQT_GC_FUNC( hbqt_gcRelease_QGraphicsLineItem )
{
   HBQT_GC_T * p = ( HBQT_GC_T * ) Cargo;

   if( p )
   {
      if( p->bNew && p->ph )
         delete ( ( QGraphicsLineItem * ) p->ph );
      p->ph = NULL;
   }
}

void * hbqt_gcAllocate_QGraphicsLineItem( void * pObj, bool bNew )
{
   HBQT_GC_T * p = ( HBQT_GC_T * ) hb_gcAllocate( sizeof( HBQT_GC_T ), hbqt_gcFuncs() );

   p->ph = ( QGraphicsLineItem * ) pObj;
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QGraphicsLineItem;
   p->type = HBQT_TYPE_QGraphicsLineItem;

   return p;
}

HB_FUNC( QT_QGRAPHICSLINEITEM )
{
   QGraphicsLineItem * pObj = NULL;

   if( hb_pcount() >= 4 && HB_ISNUM( 1 ) && HB_ISNUM( 2 ) && HB_ISNUM( 3 ) && HB_ISNUM( 4 ) )
   {
      pObj = new QGraphicsLineItem( hb_parnd( 1 ), hb_parnd( 2 ), hb_parnd( 3 ), hb_parnd( 4 ), ( HB_ISPOINTER( 5 ) ? hbqt_par_QGraphicsItem( 5 ) : 0 ) ) ;
   }
   else if( hb_pcount() >= 1 && HB_ISPOINTER( 1 ) )
   {
      HBQT_GC_T * p = ( HBQT_GC_T * ) hb_parptrGC( hbqt_gcFuncs(), 1 );
      if( p->type == HBQT_TYPE_QLineF )
      {
         pObj = new QGraphicsLineItem( *hbqt_par_QLineF( 1 ), ( HB_ISPOINTER( 2 ) ? hbqt_par_QGraphicsItem( 2 ) : 0 ) ) ;
      }
      else
      {
         pObj = new QGraphicsLineItem( hbqt_par_QGraphicsItem( 1 ) ) ;
      }
   }
   else
   {
      pObj = new QGraphicsLineItem() ;
   }

   hb_retptrGC( hbqt_gcAllocate_QGraphicsLineItem( ( void * ) pObj, true ) );
}

/* QLineF line () const */
HB_FUNC( QT_QGRAPHICSLINEITEM_LINE )
{
   QGraphicsLineItem * p = hbqt_par_QGraphicsLineItem( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QLineF( new QLineF( ( p )->line() ), true ) );
}

/* QPen pen () const */
HB_FUNC( QT_QGRAPHICSLINEITEM_PEN )
{
   QGraphicsLineItem * p = hbqt_par_QGraphicsLineItem( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QPen( new QPen( ( p )->pen() ), true ) );
}

/* void setLine ( const QLineF & line ) */
HB_FUNC( QT_QGRAPHICSLINEITEM_SETLINE )
{
   QGraphicsLineItem * p = hbqt_par_QGraphicsLineItem( 1 );
   if( p )
      ( p )->setLine( *hbqt_par_QLineF( 2 ) );
}

/* void setLine ( qreal x1, qreal y1, qreal x2, qreal y2 ) */
HB_FUNC( QT_QGRAPHICSLINEITEM_SETLINE_1 )
{
   QGraphicsLineItem * p = hbqt_par_QGraphicsLineItem( 1 );
   if( p )
      ( p )->setLine( hb_parnd( 2 ), hb_parnd( 3 ), hb_parnd( 4 ), hb_parnd( 5 ) );
}

/* void setPen ( const QPen & pen ) */
HB_FUNC( QT_QGRAPHICSLINEITEM_SETPEN )
{
   QGraphicsLineItem * p = hbqt_par_QGraphicsLineItem( 1 );
   if( p )
      ( p )->setPen( *hbqt_par_QPen( 2 ) );
}


#endif /* #if QT_VERSION >= 0x040500 */
