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

#include <QtGui/QGraphicsSceneDragDropEvent>
#include <QtGui/QWidget>
#include <QtCore/QMimeData>
#include <QtCore/QPointF>
#include <QtCore/QPoint>


/*
 * QGraphicsSceneDragDropEvent ()
 * ~QGraphicsSceneDragDropEvent ()
 */

typedef struct
{
   QGraphicsSceneDragDropEvent * ph;
   bool bNew;
   PHBQT_GC_FUNC func;
   int type;
} HBQT_GC_T_QGraphicsSceneDragDropEvent;

HBQT_GC_FUNC( hbqt_gcRelease_QGraphicsSceneDragDropEvent )
{
   HBQT_GC_T * p = ( HBQT_GC_T * ) Cargo;

   if( p && p->bNew )
      p->ph = NULL;
}

void * hbqt_gcAllocate_QGraphicsSceneDragDropEvent( void * pObj, bool bNew )
{
   HBQT_GC_T * p = ( HBQT_GC_T * ) hb_gcAllocate( sizeof( HBQT_GC_T ), hbqt_gcFuncs() );

   p->ph = ( QGraphicsSceneDragDropEvent * ) pObj;
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QGraphicsSceneDragDropEvent;
   p->type = HBQT_TYPE_QGraphicsSceneDragDropEvent;

   return p;
}

HB_FUNC( QT_QGRAPHICSSCENEDRAGDROPEVENT )
{
   //__HB_RETPTRGC__( new QGraphicsSceneDragDropEvent() );
}

/* void acceptProposedAction () */
HB_FUNC( QT_QGRAPHICSSCENEDRAGDROPEVENT_ACCEPTPROPOSEDACTION )
{
   QGraphicsSceneDragDropEvent * p = hbqt_par_QGraphicsSceneDragDropEvent( 1 );
   if( p )
      ( p )->acceptProposedAction();
}

/* Qt::MouseButtons buttons () const */
HB_FUNC( QT_QGRAPHICSSCENEDRAGDROPEVENT_BUTTONS )
{
   QGraphicsSceneDragDropEvent * p = hbqt_par_QGraphicsSceneDragDropEvent( 1 );
   if( p )
      hb_retni( ( Qt::MouseButtons ) ( p )->buttons() );
}

/* Qt::DropAction dropAction () const */
HB_FUNC( QT_QGRAPHICSSCENEDRAGDROPEVENT_DROPACTION )
{
   QGraphicsSceneDragDropEvent * p = hbqt_par_QGraphicsSceneDragDropEvent( 1 );
   if( p )
      hb_retni( ( Qt::DropAction ) ( p )->dropAction() );
}

/* const QMimeData * mimeData () const */
HB_FUNC( QT_QGRAPHICSSCENEDRAGDROPEVENT_MIMEDATA )
{
   hb_retptrGC( hbqt_gcAllocate_QMimeData( ( void* ) hbqt_par_QGraphicsSceneDragDropEvent( 1 )->mimeData(), false ) );
}

/* Qt::KeyboardModifiers modifiers () const */
HB_FUNC( QT_QGRAPHICSSCENEDRAGDROPEVENT_MODIFIERS )
{
   QGraphicsSceneDragDropEvent * p = hbqt_par_QGraphicsSceneDragDropEvent( 1 );
   if( p )
      hb_retni( ( Qt::KeyboardModifiers ) ( p )->modifiers() );
}

/* QPointF pos () const */
HB_FUNC( QT_QGRAPHICSSCENEDRAGDROPEVENT_POS )
{
   QGraphicsSceneDragDropEvent * p = hbqt_par_QGraphicsSceneDragDropEvent( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QPointF( new QPointF( ( p )->pos() ), true ) );
}

/* Qt::DropActions possibleActions () const */
HB_FUNC( QT_QGRAPHICSSCENEDRAGDROPEVENT_POSSIBLEACTIONS )
{
   QGraphicsSceneDragDropEvent * p = hbqt_par_QGraphicsSceneDragDropEvent( 1 );
   if( p )
      hb_retni( ( Qt::DropActions ) ( p )->possibleActions() );
}

/* Qt::DropAction proposedAction () const */
HB_FUNC( QT_QGRAPHICSSCENEDRAGDROPEVENT_PROPOSEDACTION )
{
   QGraphicsSceneDragDropEvent * p = hbqt_par_QGraphicsSceneDragDropEvent( 1 );
   if( p )
      hb_retni( ( Qt::DropAction ) ( p )->proposedAction() );
}

/* QPointF scenePos () const */
HB_FUNC( QT_QGRAPHICSSCENEDRAGDROPEVENT_SCENEPOS )
{
   QGraphicsSceneDragDropEvent * p = hbqt_par_QGraphicsSceneDragDropEvent( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QPointF( new QPointF( ( p )->scenePos() ), true ) );
}

/* QPoint screenPos () const */
HB_FUNC( QT_QGRAPHICSSCENEDRAGDROPEVENT_SCREENPOS )
{
   QGraphicsSceneDragDropEvent * p = hbqt_par_QGraphicsSceneDragDropEvent( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QPoint( new QPoint( ( p )->screenPos() ), true ) );
}

/* void setDropAction ( Qt::DropAction action ) */
HB_FUNC( QT_QGRAPHICSSCENEDRAGDROPEVENT_SETDROPACTION )
{
   QGraphicsSceneDragDropEvent * p = hbqt_par_QGraphicsSceneDragDropEvent( 1 );
   if( p )
      ( p )->setDropAction( ( Qt::DropAction ) hb_parni( 2 ) );
}

/* QWidget * source () const */
HB_FUNC( QT_QGRAPHICSSCENEDRAGDROPEVENT_SOURCE )
{
   QGraphicsSceneDragDropEvent * p = hbqt_par_QGraphicsSceneDragDropEvent( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QWidget( ( p )->source(), false ) );
}


#endif /* #if QT_VERSION >= 0x040500 */
