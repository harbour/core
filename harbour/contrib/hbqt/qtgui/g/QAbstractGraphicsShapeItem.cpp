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

#include <QtGui/QAbstractGraphicsShapeItem>
#include <QtGui/QBrush>
#include <QtGui/QPen>


/*
 * QAbstractGraphicsShapeItem ( QGraphicsItem * parent = 0 )
 * ~QAbstractGraphicsShapeItem ()
 */

typedef struct
{
   QAbstractGraphicsShapeItem * ph;
   bool bNew;
   PHBQT_GC_FUNC func;
   int type;
} HBQT_GC_T_QAbstractGraphicsShapeItem;

HBQT_GC_FUNC( hbqt_gcRelease_QAbstractGraphicsShapeItem )
{
   HBQT_GC_T * p = ( HBQT_GC_T * ) Cargo;

   if( p && p->bNew )
      p->ph = NULL;
}

void * hbqt_gcAllocate_QAbstractGraphicsShapeItem( void * pObj, bool bNew )
{
   HBQT_GC_T * p = ( HBQT_GC_T * ) hb_gcAllocate( sizeof( HBQT_GC_T ), hbqt_gcFuncs() );

   p->ph = ( QAbstractGraphicsShapeItem * ) pObj;
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QAbstractGraphicsShapeItem;
   p->type = HBQT_TYPE_QAbstractGraphicsShapeItem;

   return p;
}

HB_FUNC( QT_QABSTRACTGRAPHICSSHAPEITEM )
{
   // __HB_RETPTRGC__( new QAbstractGraphicsShapeItem() );
}

/* QBrush brush () const */
HB_FUNC( QT_QABSTRACTGRAPHICSSHAPEITEM_BRUSH )
{
   QAbstractGraphicsShapeItem * p = hbqt_par_QAbstractGraphicsShapeItem( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QBrush( new QBrush( ( p )->brush() ), true ) );
}

/* QPen pen () const */
HB_FUNC( QT_QABSTRACTGRAPHICSSHAPEITEM_PEN )
{
   QAbstractGraphicsShapeItem * p = hbqt_par_QAbstractGraphicsShapeItem( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QPen( new QPen( ( p )->pen() ), true ) );
}

/* void setBrush ( const QBrush & brush ) */
HB_FUNC( QT_QABSTRACTGRAPHICSSHAPEITEM_SETBRUSH )
{
   QAbstractGraphicsShapeItem * p = hbqt_par_QAbstractGraphicsShapeItem( 1 );
   if( p )
      ( p )->setBrush( *hbqt_par_QBrush( 2 ) );
}

/* void setPen ( const QPen & pen ) */
HB_FUNC( QT_QABSTRACTGRAPHICSSHAPEITEM_SETPEN )
{
   QAbstractGraphicsShapeItem * p = hbqt_par_QAbstractGraphicsShapeItem( 1 );
   if( p )
      ( p )->setPen( *hbqt_par_QPen( 2 ) );
}


#endif /* #if QT_VERSION >= 0x040500 */
