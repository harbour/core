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

#include <QtGui/QMoveEvent>


/*
 * QMoveEvent ( const QPoint & pos, const QPoint & oldPos )
 * ~QMoveEvent ()
 */

typedef struct
{
   QMoveEvent * ph;
   bool bNew;
   PHBQT_GC_FUNC func;
   int type;
} HBQT_GC_T_QMoveEvent;

HBQT_GC_FUNC( hbqt_gcRelease_QMoveEvent )
{
   HB_SYMBOL_UNUSED( Cargo );
   HBQT_GC_T * p = ( HBQT_GC_T * ) Cargo;

   if( p && p->bNew )
      p->ph = NULL;
}

void * hbqt_gcAllocate_QMoveEvent( void * pObj, bool bNew )
{
   HBQT_GC_T * p = ( HBQT_GC_T * ) hb_gcAllocate( sizeof( HBQT_GC_T ), hbqt_gcFuncs() );

   p->ph = ( QMoveEvent * ) pObj;
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QMoveEvent;
   p->type = HBQT_TYPE_QMoveEvent;

   return p;
}

HB_FUNC( QT_QMOVEEVENT )
{
   // __HB_RETPTRGC__( new QMoveEvent() );
}

/* const QPoint & oldPos () const */
HB_FUNC( QT_QMOVEEVENT_OLDPOS )
{
   QMoveEvent * p = hbqt_par_QMoveEvent( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QPoint( new QPoint( ( p )->oldPos() ), true ) );
}

/* const QPoint & pos () const */
HB_FUNC( QT_QMOVEEVENT_POS )
{
   QMoveEvent * p = hbqt_par_QMoveEvent( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QPoint( new QPoint( ( p )->pos() ), true ) );
}


#endif /* #if QT_VERSION >= 0x040500 */
