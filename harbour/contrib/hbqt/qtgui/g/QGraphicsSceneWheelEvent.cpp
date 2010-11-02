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

#include <QtGui/QGraphicsSceneWheelEvent>
#include <QtCore/QPointF>
#include <QtCore/QPoint>

/*
 * QGraphicsScreenWheelEvent ()
 * ~QGraphicsSceneWheelEvent ()
 */

typedef struct
{
   QGraphicsSceneWheelEvent * ph;
   bool bNew;
   PHBQT_GC_FUNC func;
   int type;
} HBQT_GC_T_QGraphicsSceneWheelEvent;

HBQT_GC_FUNC( hbqt_gcRelease_QGraphicsSceneWheelEvent )
{
   HBQT_GC_T * p = ( HBQT_GC_T * ) Cargo;

   if( p && p->bNew )
      p->ph = NULL;
}

void * hbqt_gcAllocate_QGraphicsSceneWheelEvent( void * pObj, bool bNew )
{
   HBQT_GC_T * p = ( HBQT_GC_T * ) hb_gcAllocate( sizeof( HBQT_GC_T ), hbqt_gcFuncs() );

   p->ph = ( QGraphicsSceneWheelEvent * ) pObj;
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QGraphicsSceneWheelEvent;
   p->type = HBQT_TYPE_QGraphicsSceneWheelEvent;

   return p;
}

HB_FUNC( QT_QGRAPHICSSCENEWHEELEVENT )
{
   //__HB_RETPTRGC__( new QGraphicsSceneWheelEvent() );
}

/* Qt::MouseButtons buttons () const */
HB_FUNC( QT_QGRAPHICSSCENEWHEELEVENT_BUTTONS )
{
   QGraphicsSceneWheelEvent * p = hbqt_par_QGraphicsSceneWheelEvent( 1 );
   if( p )
      hb_retni( ( Qt::MouseButtons ) ( p )->buttons() );
}

/* int delta () const */
HB_FUNC( QT_QGRAPHICSSCENEWHEELEVENT_DELTA )
{
   QGraphicsSceneWheelEvent * p = hbqt_par_QGraphicsSceneWheelEvent( 1 );
   if( p )
      hb_retni( ( p )->delta() );
}

/* Qt::KeyboardModifiers modifiers () const */
HB_FUNC( QT_QGRAPHICSSCENEWHEELEVENT_MODIFIERS )
{
   QGraphicsSceneWheelEvent * p = hbqt_par_QGraphicsSceneWheelEvent( 1 );
   if( p )
      hb_retni( ( Qt::KeyboardModifiers ) ( p )->modifiers() );
}

/* Qt::Orientation orientation () const */
HB_FUNC( QT_QGRAPHICSSCENEWHEELEVENT_ORIENTATION )
{
   QGraphicsSceneWheelEvent * p = hbqt_par_QGraphicsSceneWheelEvent( 1 );
   if( p )
      hb_retni( ( Qt::Orientation ) ( p )->orientation() );
}

/* QPointF pos () const */
HB_FUNC( QT_QGRAPHICSSCENEWHEELEVENT_POS )
{
   QGraphicsSceneWheelEvent * p = hbqt_par_QGraphicsSceneWheelEvent( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QPointF( new QPointF( ( p )->pos() ), true ) );
}

/* QPointF scenePos () const */
HB_FUNC( QT_QGRAPHICSSCENEWHEELEVENT_SCENEPOS )
{
   QGraphicsSceneWheelEvent * p = hbqt_par_QGraphicsSceneWheelEvent( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QPointF( new QPointF( ( p )->scenePos() ), true ) );
}

/* QPoint screenPos () const */
HB_FUNC( QT_QGRAPHICSSCENEWHEELEVENT_SCREENPOS )
{
   QGraphicsSceneWheelEvent * p = hbqt_par_QGraphicsSceneWheelEvent( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QPoint( new QPoint( ( p )->screenPos() ), true ) );
}


#endif /* #if QT_VERSION >= 0x040500 */
