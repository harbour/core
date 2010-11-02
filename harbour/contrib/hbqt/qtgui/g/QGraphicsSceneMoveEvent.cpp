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

#include <QtGui/QGraphicsSceneMoveEvent>
#include <QtCore/QPointF>
#include <QtCore/QPoint>


/*
 * QGraphicsSceneMoveEvent ()
 * ~QGraphicsSceneMoveEvent ()
 */

typedef struct
{
   QGraphicsSceneMoveEvent * ph;
   bool bNew;
   PHBQT_GC_FUNC func;
   int type;
} HBQT_GC_T_QGraphicsSceneMoveEvent;

HBQT_GC_FUNC( hbqt_gcRelease_QGraphicsSceneMoveEvent )
{
   HB_SYMBOL_UNUSED( Cargo );
   HBQT_GC_T * p = ( HBQT_GC_T * ) Cargo;

   if( p && p->bNew )
      p->ph = NULL;
}

void * hbqt_gcAllocate_QGraphicsSceneMoveEvent( void * pObj, bool bNew )
{
   HBQT_GC_T * p = ( HBQT_GC_T * ) hb_gcAllocate( sizeof( HBQT_GC_T ), hbqt_gcFuncs() );

   p->ph = ( QGraphicsSceneMoveEvent * ) pObj;
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QGraphicsSceneMoveEvent;
   p->type = HBQT_TYPE_QGraphicsSceneMoveEvent;

   return p;
}

HB_FUNC( QT_QGRAPHICSSCENEMOVEEVENT )
{
   //__HB_RETPTRGC__( new QGraphicsSceneMoveEvent() );
}

/* QPointF newPos () const */
HB_FUNC( QT_QGRAPHICSSCENEMOVEEVENT_NEWPOS )
{
   QGraphicsSceneMoveEvent * p = hbqt_par_QGraphicsSceneMoveEvent( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QPointF( new QPointF( ( p )->newPos() ), true ) );
}

/* QPointF oldPos () const */
HB_FUNC( QT_QGRAPHICSSCENEMOVEEVENT_OLDPOS )
{
   QGraphicsSceneMoveEvent * p = hbqt_par_QGraphicsSceneMoveEvent( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QPointF( new QPointF( ( p )->oldPos() ), true ) );
}


#endif /* #if QT_VERSION >= 0x040500 */
