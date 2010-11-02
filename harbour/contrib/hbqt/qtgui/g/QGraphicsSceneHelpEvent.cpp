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

#include <QtGui/QGraphicsSceneHelpEvent>
#include <QtCore/QPointF>
#include <QtCore/QPoint>


/*
 * QGraphicsSceneHelpEvent ()
 * ~QGraphicsSceneHelpEvent ()
 */

typedef struct
{
   QGraphicsSceneHelpEvent * ph;
   bool bNew;
   PHBQT_GC_FUNC func;
   int type;
} HBQT_GC_T_QGraphicsSceneHelpEvent;

HBQT_GC_FUNC( hbqt_gcRelease_QGraphicsSceneHelpEvent )
{
   HB_SYMBOL_UNUSED( Cargo );
   HBQT_GC_T * p = ( HBQT_GC_T * ) Cargo;

   if( p && p->bNew )
      p->ph = NULL;
}

void * hbqt_gcAllocate_QGraphicsSceneHelpEvent( void * pObj, bool bNew )
{
   HBQT_GC_T * p = ( HBQT_GC_T * ) hb_gcAllocate( sizeof( HBQT_GC_T ), hbqt_gcFuncs() );

   p->ph = ( QGraphicsSceneHelpEvent * ) pObj;
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QGraphicsSceneHelpEvent;
   p->type = HBQT_TYPE_QGraphicsSceneHelpEvent;

   return p;
}

HB_FUNC( QT_QGRAPHICSSCENEHELPEVENT )
{
   //__HB_RETPTRGC__( new QGraphicsSceneHelpEvent() );
}

/* QPointF scenePos () const */
HB_FUNC( QT_QGRAPHICSSCENEHELPEVENT_SCENEPOS )
{
   QGraphicsSceneHelpEvent * p = hbqt_par_QGraphicsSceneHelpEvent( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QPointF( new QPointF( ( p )->scenePos() ), true ) );
}

/* QPoint screenPos () const */
HB_FUNC( QT_QGRAPHICSSCENEHELPEVENT_SCREENPOS )
{
   QGraphicsSceneHelpEvent * p = hbqt_par_QGraphicsSceneHelpEvent( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QPoint( new QPoint( ( p )->screenPos() ), true ) );
}


#endif /* #if QT_VERSION >= 0x040500 */
