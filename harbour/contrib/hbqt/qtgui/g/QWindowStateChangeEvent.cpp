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
 *  Constructed[ 1/1 [ 100.00% ] ]
 *
 */

#include <QtCore/QPointer>

#include <QtGui/QWindowStateChangeEvent>


/*
 * QWindowStateChangeEvent ()
 */

typedef struct
{
   QWindowStateChangeEvent * ph;
   bool bNew;
   PHBQT_GC_FUNC func;
   int type;
} HBQT_GC_T_QWindowStateChangeEvent;

HBQT_GC_FUNC( hbqt_gcRelease_QWindowStateChangeEvent )
{
   HB_SYMBOL_UNUSED( Cargo );
   HBQT_GC_T * p = ( HBQT_GC_T * ) Cargo;

   if( p && p->bNew )
      p->ph = NULL;
}

void * hbqt_gcAllocate_QWindowStateChangeEvent( void * pObj, bool bNew )
{
   HBQT_GC_T * p = ( HBQT_GC_T * ) hb_gcAllocate( sizeof( HBQT_GC_T ), hbqt_gcFuncs() );

   p->ph = ( QWindowStateChangeEvent * ) pObj;
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QWindowStateChangeEvent;
   p->type = HBQT_TYPE_QWindowStateChangeEvent;

   return p;
}

HB_FUNC( QT_QWINDOWSTATECHANGEEVENT )
{
   // __HB_RETPTRGC__( new QWindowStateChangeEvent() );
}

/* Qt::WindowStates oldState () const */
HB_FUNC( QT_QWINDOWSTATECHANGEEVENT_OLDSTATE )
{
   QWindowStateChangeEvent * p = hbqt_par_QWindowStateChangeEvent( 1 );
   if( p )
      hb_retni( ( Qt::WindowStates ) ( p )->oldState() );
}


#endif /* #if QT_VERSION >= 0x040500 */
