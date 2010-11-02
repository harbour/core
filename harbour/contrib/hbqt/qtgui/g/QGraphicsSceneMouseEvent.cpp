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
 *  Constructed[ 12/12 [ 100.00% ] ]
 *
 */

#include <QtCore/QPointer>

#include <QtGui/QGraphicsSceneMouseEvent>
#include <QtCore/QPointF>
#include <QtCore/QPoint>


/*
 * QGraphicsSceneMouseEvent ()
 * ~QGraphicsSceneMouseEvent ()
 */

typedef struct
{
   QGraphicsSceneMouseEvent * ph;
   bool bNew;
   PHBQT_GC_FUNC func;
   int type;
} HBQT_GC_T_QGraphicsSceneMouseEvent;

HBQT_GC_FUNC( hbqt_gcRelease_QGraphicsSceneMouseEvent )
{
   HBQT_GC_T * p = ( HBQT_GC_T * ) Cargo;

   if( p && p->bNew )
      p->ph = NULL;
}

void * hbqt_gcAllocate_QGraphicsSceneMouseEvent( void * pObj, bool bNew )
{
   HBQT_GC_T * p = ( HBQT_GC_T * ) hb_gcAllocate( sizeof( HBQT_GC_T ), hbqt_gcFuncs() );

   p->ph = ( QGraphicsSceneMouseEvent * ) pObj;
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QGraphicsSceneMouseEvent;
   p->type = HBQT_TYPE_QGraphicsSceneMouseEvent;

   return p;
}

HB_FUNC( QT_QGRAPHICSSCENEMOUSEEVENT )
{
   //__HB_RETPTRGC__( new QGraphicsSceneMouseEvent() );
}

/* Qt::MouseButton button () const */
HB_FUNC( QT_QGRAPHICSSCENEMOUSEEVENT_BUTTON )
{
   QGraphicsSceneMouseEvent * p = hbqt_par_QGraphicsSceneMouseEvent( 1 );
   if( p )
      hb_retni( ( Qt::MouseButton ) ( p )->button() );
}

/* QPointF buttonDownPos ( Qt::MouseButton button ) const */
HB_FUNC( QT_QGRAPHICSSCENEMOUSEEVENT_BUTTONDOWNPOS )
{
   QGraphicsSceneMouseEvent * p = hbqt_par_QGraphicsSceneMouseEvent( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QPointF( new QPointF( ( p )->buttonDownPos( ( Qt::MouseButton ) hb_parni( 2 ) ) ), true ) );
}

/* QPointF buttonDownScenePos ( Qt::MouseButton button ) const */
HB_FUNC( QT_QGRAPHICSSCENEMOUSEEVENT_BUTTONDOWNSCENEPOS )
{
   QGraphicsSceneMouseEvent * p = hbqt_par_QGraphicsSceneMouseEvent( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QPointF( new QPointF( ( p )->buttonDownScenePos( ( Qt::MouseButton ) hb_parni( 2 ) ) ), true ) );
}

/* QPoint buttonDownScreenPos ( Qt::MouseButton button ) const */
HB_FUNC( QT_QGRAPHICSSCENEMOUSEEVENT_BUTTONDOWNSCREENPOS )
{
   QGraphicsSceneMouseEvent * p = hbqt_par_QGraphicsSceneMouseEvent( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QPoint( new QPoint( ( p )->buttonDownScreenPos( ( Qt::MouseButton ) hb_parni( 2 ) ) ), true ) );
}

/* Qt::MouseButtons buttons () const */
HB_FUNC( QT_QGRAPHICSSCENEMOUSEEVENT_BUTTONS )
{
   QGraphicsSceneMouseEvent * p = hbqt_par_QGraphicsSceneMouseEvent( 1 );
   if( p )
      hb_retni( ( Qt::MouseButtons ) ( p )->buttons() );
}

/* QPointF lastPos () const */
HB_FUNC( QT_QGRAPHICSSCENEMOUSEEVENT_LASTPOS )
{
   QGraphicsSceneMouseEvent * p = hbqt_par_QGraphicsSceneMouseEvent( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QPointF( new QPointF( ( p )->lastPos() ), true ) );
}

/* QPointF lastScenePos () const */
HB_FUNC( QT_QGRAPHICSSCENEMOUSEEVENT_LASTSCENEPOS )
{
   QGraphicsSceneMouseEvent * p = hbqt_par_QGraphicsSceneMouseEvent( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QPointF( new QPointF( ( p )->lastScenePos() ), true ) );
}

/* QPoint lastScreenPos () const */
HB_FUNC( QT_QGRAPHICSSCENEMOUSEEVENT_LASTSCREENPOS )
{
   QGraphicsSceneMouseEvent * p = hbqt_par_QGraphicsSceneMouseEvent( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QPoint( new QPoint( ( p )->lastScreenPos() ), true ) );
}

/* Qt::KeyboardModifiers modifiers () const */
HB_FUNC( QT_QGRAPHICSSCENEMOUSEEVENT_MODIFIERS )
{
   QGraphicsSceneMouseEvent * p = hbqt_par_QGraphicsSceneMouseEvent( 1 );
   if( p )
      hb_retni( ( Qt::KeyboardModifiers ) ( p )->modifiers() );
}

/* QPointF pos () const */
HB_FUNC( QT_QGRAPHICSSCENEMOUSEEVENT_POS )
{
   QGraphicsSceneMouseEvent * p = hbqt_par_QGraphicsSceneMouseEvent( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QPointF( new QPointF( ( p )->pos() ), true ) );
}

/* QPointF scenePos () const */
HB_FUNC( QT_QGRAPHICSSCENEMOUSEEVENT_SCENEPOS )
{
   QGraphicsSceneMouseEvent * p = hbqt_par_QGraphicsSceneMouseEvent( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QPointF( new QPointF( ( p )->scenePos() ), true ) );
}

/* QPoint screenPos () const */
HB_FUNC( QT_QGRAPHICSSCENEMOUSEEVENT_SCREENPOS )
{
   QGraphicsSceneMouseEvent * p = hbqt_par_QGraphicsSceneMouseEvent( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QPoint( new QPoint( ( p )->screenPos() ), true ) );
}


#endif /* #if QT_VERSION >= 0x040500 */
