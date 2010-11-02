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

#include <QtGui/QGraphicsSceneHoverEvent>
#include <QtCore/QPointF>
#include <QtCore/QPoint>


/*
 * QGraphicsSceneHoverEvent ()
 * ~QGraphicsSceneHoverEvent ()
 */

typedef struct
{
   QGraphicsSceneHoverEvent * ph;
   bool bNew;
   PHBQT_GC_FUNC func;
   int type;
} HBQT_GC_T_QGraphicsSceneHoverEvent;

HBQT_GC_FUNC( hbqt_gcRelease_QGraphicsSceneHoverEvent )
{
   HBQT_GC_T * p = ( HBQT_GC_T * ) Cargo;

   if( p && p->bNew )
      p->ph = NULL;
}

void * hbqt_gcAllocate_QGraphicsSceneHoverEvent( void * pObj, bool bNew )
{
   HBQT_GC_T * p = ( HBQT_GC_T * ) hb_gcAllocate( sizeof( HBQT_GC_T ), hbqt_gcFuncs() );

   p->ph = ( QGraphicsSceneHoverEvent * ) pObj;
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QGraphicsSceneHoverEvent;
   p->type = HBQT_TYPE_QGraphicsSceneHoverEvent;

   return p;
}

HB_FUNC( QT_QGRAPHICSSCENEHOVEREVENT )
{
   //__HB_RETPTRGC__( new QGraphicsSceneHoverEvent() );
}

/* QPointF lastPos () const */
HB_FUNC( QT_QGRAPHICSSCENEHOVEREVENT_LASTPOS )
{
   QGraphicsSceneHoverEvent * p = hbqt_par_QGraphicsSceneHoverEvent( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QPointF( new QPointF( ( p )->lastPos() ), true ) );
}

/* QPointF lastScenePos () const */
HB_FUNC( QT_QGRAPHICSSCENEHOVEREVENT_LASTSCENEPOS )
{
   QGraphicsSceneHoverEvent * p = hbqt_par_QGraphicsSceneHoverEvent( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QPointF( new QPointF( ( p )->lastScenePos() ), true ) );
}

/* QPoint lastScreenPos () const */
HB_FUNC( QT_QGRAPHICSSCENEHOVEREVENT_LASTSCREENPOS )
{
   QGraphicsSceneHoverEvent * p = hbqt_par_QGraphicsSceneHoverEvent( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QPoint( new QPoint( ( p )->lastScreenPos() ), true ) );
}

/* Qt::KeyboardModifiers modifiers () const */
HB_FUNC( QT_QGRAPHICSSCENEHOVEREVENT_MODIFIERS )
{
   QGraphicsSceneHoverEvent * p = hbqt_par_QGraphicsSceneHoverEvent( 1 );
   if( p )
      hb_retni( ( Qt::KeyboardModifiers ) ( p )->modifiers() );
}

/* QPointF pos () const */
HB_FUNC( QT_QGRAPHICSSCENEHOVEREVENT_POS )
{
   QGraphicsSceneHoverEvent * p = hbqt_par_QGraphicsSceneHoverEvent( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QPointF( new QPointF( ( p )->pos() ), true ) );
}

/* QPointF scenePos () const */
HB_FUNC( QT_QGRAPHICSSCENEHOVEREVENT_SCENEPOS )
{
   QGraphicsSceneHoverEvent * p = hbqt_par_QGraphicsSceneHoverEvent( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QPointF( new QPointF( ( p )->scenePos() ), true ) );
}

/* QPoint screenPos () const */
HB_FUNC( QT_QGRAPHICSSCENEHOVEREVENT_SCREENPOS )
{
   QGraphicsSceneHoverEvent * p = hbqt_par_QGraphicsSceneHoverEvent( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QPoint( new QPoint( ( p )->screenPos() ), true ) );
}


#endif /* #if QT_VERSION >= 0x040500 */
