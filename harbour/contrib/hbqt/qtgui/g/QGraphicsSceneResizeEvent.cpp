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

#include <QtGui/QGraphicsSceneResizeEvent>
#include <QtCore/QSizeF>


/*
 * QGraphicsSceneResizeEvent ()
 * ~QGraphicsSceneResizeEvent ()
 */

typedef struct
{
   QGraphicsSceneResizeEvent * ph;
   bool bNew;
   PHBQT_GC_FUNC func;
   int type;
} HBQT_GC_T_QGraphicsSceneResizeEvent;

HBQT_GC_FUNC( hbqt_gcRelease_QGraphicsSceneResizeEvent )
{
   HBQT_GC_T * p = ( HBQT_GC_T * ) Cargo;

   if( p && p->bNew )
      p->ph = NULL;
}

void * hbqt_gcAllocate_QGraphicsSceneResizeEvent( void * pObj, bool bNew )
{
   HBQT_GC_T * p = ( HBQT_GC_T * ) hb_gcAllocate( sizeof( HBQT_GC_T ), hbqt_gcFuncs() );

   p->ph = ( QGraphicsSceneResizeEvent * ) pObj;
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QGraphicsSceneResizeEvent;
   p->type = HBQT_TYPE_QGraphicsSceneResizeEvent;

   return p;
}

HB_FUNC( QT_QGRAPHICSSCENERESIZEEVENT )
{
   //__HB_RETPTRGC__( new QGraphicsSceneResizeEvent() );
}

/* QSizeF newSize () const */
HB_FUNC( QT_QGRAPHICSSCENERESIZEEVENT_NEWSIZE )
{
   QGraphicsSceneResizeEvent * p = hbqt_par_QGraphicsSceneResizeEvent( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QSizeF( new QSizeF( ( p )->newSize() ), true ) );
}

/* QSizeF oldSize () const */
HB_FUNC( QT_QGRAPHICSSCENERESIZEEVENT_OLDSIZE )
{
   QGraphicsSceneResizeEvent * p = hbqt_par_QGraphicsSceneResizeEvent( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QSizeF( new QSizeF( ( p )->oldSize() ), true ) );
}


#endif /* #if QT_VERSION >= 0x040500 */
