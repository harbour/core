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
 *  Constructed[ 6/6 [ 100.00% ] ]
 *
 */

#include <QtCore/QPointer>

#include <QtGui/QHelpEvent>


/* QHelpEvent ( Type type, const QPoint & pos, const QPoint & globalPos )
 *
 */

typedef struct
{
   QHelpEvent * ph;
   bool bNew;
   PHBQT_GC_FUNC func;
   int type;
} HBQT_GC_T_QHelpEvent;

HBQT_GC_FUNC( hbqt_gcRelease_QHelpEvent )
{
   HBQT_GC_T * p = ( HBQT_GC_T * ) Cargo;

   if( p && p->bNew )
      p->ph = NULL;
}

void * hbqt_gcAllocate_QHelpEvent( void * pObj, bool bNew )
{
   HBQT_GC_T * p = ( HBQT_GC_T * ) hb_gcAllocate( sizeof( HBQT_GC_T ), hbqt_gcFuncs() );

   p->ph = ( QHelpEvent * ) pObj;
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QHelpEvent;
   p->type = HBQT_TYPE_QHelpEvent;

   return p;
}

HB_FUNC( QT_QHELPEVENT )
{
   // __HB_RETPTRGC__( new QHelpEvent() );
}

/* const QPoint & globalPos () const */
HB_FUNC( QT_QHELPEVENT_GLOBALPOS )
{
   QHelpEvent * p = hbqt_par_QHelpEvent( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QPoint( new QPoint( ( p )->globalPos() ), true ) );
}

/* int globalX () const */
HB_FUNC( QT_QHELPEVENT_GLOBALX )
{
   QHelpEvent * p = hbqt_par_QHelpEvent( 1 );
   if( p )
      hb_retni( ( p )->globalX() );
}

/* int globalY () const */
HB_FUNC( QT_QHELPEVENT_GLOBALY )
{
   QHelpEvent * p = hbqt_par_QHelpEvent( 1 );
   if( p )
      hb_retni( ( p )->globalY() );
}

/* const QPoint & pos () const */
HB_FUNC( QT_QHELPEVENT_POS )
{
   QHelpEvent * p = hbqt_par_QHelpEvent( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QPoint( new QPoint( ( p )->pos() ), true ) );
}

/* int x () const */
HB_FUNC( QT_QHELPEVENT_X )
{
   QHelpEvent * p = hbqt_par_QHelpEvent( 1 );
   if( p )
      hb_retni( ( p )->x() );
}

/* int y () const */
HB_FUNC( QT_QHELPEVENT_Y )
{
   QHelpEvent * p = hbqt_par_QHelpEvent( 1 );
   if( p )
      hb_retni( ( p )->y() );
}


#endif /* #if QT_VERSION >= 0x040500 */
