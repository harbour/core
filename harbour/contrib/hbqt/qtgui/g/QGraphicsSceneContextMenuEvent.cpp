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
 *  enum Reason { Mouse, Keyboard, Other }
 */

/*
 *  Constructed[ 5/5 [ 100.00% ] ]
 *
 */

#include <QtCore/QPointer>

#include <QtGui/QGraphicsSceneContextMenuEvent>
#include <QtCore/QPointF>
#include <QtCore/QPoint>


/*
 * QGraphicsSceneContextMenuEvent ()
 * ~QGraphicsSceneContextMenuEvent ()
 */

typedef struct
{
   QGraphicsSceneContextMenuEvent * ph;
   bool bNew;
   PHBQT_GC_FUNC func;
   int type;
} HBQT_GC_T_QGraphicsSceneContextMenuEvent;

HBQT_GC_FUNC( hbqt_gcRelease_QGraphicsSceneContextMenuEvent )
{
   HBQT_GC_T * p = ( HBQT_GC_T * ) Cargo;

   if( p && p->bNew )
      p->ph = NULL;
}

void * hbqt_gcAllocate_QGraphicsSceneContextMenuEvent( void * pObj, bool bNew )
{
   HBQT_GC_T * p = ( HBQT_GC_T * ) hb_gcAllocate( sizeof( HBQT_GC_T ), hbqt_gcFuncs() );

   p->ph = ( QGraphicsSceneContextMenuEvent * ) pObj;
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QGraphicsSceneContextMenuEvent;
   p->type = HBQT_TYPE_QGraphicsSceneContextMenuEvent;

   return p;
}

HB_FUNC( QT_QGRAPHICSSCENECONTEXTMENUEVENT )
{
   //__HB_RETPTRGC__( new QGraphicsSceneContextMenuEvent() );
}

/* Qt::KeyboardModifiers modifiers () const */
HB_FUNC( QT_QGRAPHICSSCENECONTEXTMENUEVENT_MODIFIERS )
{
   QGraphicsSceneContextMenuEvent * p = hbqt_par_QGraphicsSceneContextMenuEvent( 1 );
   if( p )
      hb_retni( ( Qt::KeyboardModifiers ) ( p )->modifiers() );
}

/* QPointF pos () const */
HB_FUNC( QT_QGRAPHICSSCENECONTEXTMENUEVENT_POS )
{
   QGraphicsSceneContextMenuEvent * p = hbqt_par_QGraphicsSceneContextMenuEvent( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QPointF( new QPointF( ( p )->pos() ), true ) );
}

/* Reason reason () const */
HB_FUNC( QT_QGRAPHICSSCENECONTEXTMENUEVENT_REASON )
{
   QGraphicsSceneContextMenuEvent * p = hbqt_par_QGraphicsSceneContextMenuEvent( 1 );
   if( p )
      hb_retni( ( QGraphicsSceneContextMenuEvent::Reason ) ( p )->reason() );
}

/* QPointF scenePos () const */
HB_FUNC( QT_QGRAPHICSSCENECONTEXTMENUEVENT_SCENEPOS )
{
   QGraphicsSceneContextMenuEvent * p = hbqt_par_QGraphicsSceneContextMenuEvent( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QPointF( new QPointF( ( p )->scenePos() ), true ) );
}

/* QPoint screenPos () const */
HB_FUNC( QT_QGRAPHICSSCENECONTEXTMENUEVENT_SCREENPOS )
{
   QGraphicsSceneContextMenuEvent * p = hbqt_par_QGraphicsSceneContextMenuEvent( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QPoint( new QPoint( ( p )->screenPos() ), true ) );
}


#endif /* #if QT_VERSION >= 0x040500 */
