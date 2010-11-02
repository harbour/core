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
 *  Constructed[ 2/2 [ 100.00% ] ]
 *
 */

#include <QtCore/QPointer>

#include <QtGui/QGraphicsPathItem>


/*
 * QGraphicsPathItem ( QGraphicsItem * parent = 0 )
 * QGraphicsPathItem ( const QPainterPath & path, QGraphicsItem * parent = 0 )
 * ~QGraphicsPathItem ()
 */

typedef struct
{
   QGraphicsPathItem * ph;
   bool bNew;
   PHBQT_GC_FUNC func;
   int type;
} HBQT_GC_T_QGraphicsPathItem;

HBQT_GC_FUNC( hbqt_gcRelease_QGraphicsPathItem )
{
   HBQT_GC_T * p = ( HBQT_GC_T * ) Cargo;

   if( p && p->bNew )
   {
      if( p->ph )
      {
         delete ( ( QGraphicsPathItem * ) p->ph );
         p->ph = NULL;
      }
      else
         p->ph = NULL;
   }
   else
      p->ph = NULL;
}

void * hbqt_gcAllocate_QGraphicsPathItem( void * pObj, bool bNew )
{
   HBQT_GC_T * p = ( HBQT_GC_T * ) hb_gcAllocate( sizeof( HBQT_GC_T ), hbqt_gcFuncs() );

   p->ph = ( QGraphicsPathItem * ) pObj;
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QGraphicsPathItem;
   p->type = HBQT_TYPE_QGraphicsPathItem;

   return p;
}

HB_FUNC( QT_QGRAPHICSPATHITEM )
{
   QGraphicsPathItem * pObj = NULL;

   if( hb_pcount() >= 1 && HB_ISPOINTER( 1 ) )
   {
      HBQT_GC_T * p = ( HBQT_GC_T * ) hb_parptrGC( hbqt_gcFuncs(), 1 );
      if( p->type == HBQT_TYPE_QPainterPath )
      {
         pObj = new QGraphicsPathItem( *hbqt_par_QPainterPath( 1 ), ( HB_ISPOINTER( 2 ) ? hbqt_par_QGraphicsItem( 2 ) : 0 ) ) ;
      }
      else
      {
         pObj = new QGraphicsPathItem( hbqt_par_QGraphicsItem( 1 ) ) ;
      }
   }
   else
   {
      pObj = new QGraphicsPathItem() ;
   }

   hb_retptrGC( hbqt_gcAllocate_QGraphicsPathItem( ( void * ) pObj, true ) );
}

/* QPainterPath path () const */
HB_FUNC( QT_QGRAPHICSPATHITEM_PATH )
{
   QGraphicsPathItem * p = hbqt_par_QGraphicsPathItem( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QPainterPath( new QPainterPath( ( p )->path() ), true ) );
}

/* void setPath ( const QPainterPath & path ) */
HB_FUNC( QT_QGRAPHICSPATHITEM_SETPATH )
{
   QGraphicsPathItem * p = hbqt_par_QGraphicsPathItem( 1 );
   if( p )
      ( p )->setPath( *hbqt_par_QPainterPath( 2 ) );
}


#endif /* #if QT_VERSION >= 0x040500 */
