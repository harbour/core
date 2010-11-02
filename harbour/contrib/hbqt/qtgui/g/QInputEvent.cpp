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

#include <QtGui/QInputEvent>
#include <QtCore/QEvent>

/*
 *
 *
 */

typedef struct
{
   QInputEvent * ph;
   bool bNew;
   PHBQT_GC_FUNC func;
   int type;
} HBQT_GC_T_QInputEvent;

HBQT_GC_FUNC( hbqt_gcRelease_QInputEvent )
{
   HBQT_GC_T * p = ( HBQT_GC_T * ) Cargo;

   if( p )
   {
      if( p->bNew && p->ph )
         delete ( ( QInputEvent * ) p->ph );
      p->ph = NULL;
   }
}

void * hbqt_gcAllocate_QInputEvent( void * pObj, bool bNew )
{
   HBQT_GC_T * p = ( HBQT_GC_T * ) hb_gcAllocate( sizeof( HBQT_GC_T ), hbqt_gcFuncs() );

   p->ph = ( QInputEvent * ) pObj;
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QInputEvent;
   p->type = HBQT_TYPE_QInputEvent;

   return p;
}

HB_FUNC( QT_QINPUTEVENT )
{
   QInputEvent * pObj = NULL;

   pObj = new QInputEvent( ( QEvent::Type ) hb_parni( 1 ), ( Qt::KeyboardModifiers ) hb_parni( 2 ) ) ;

   hb_retptrGC( hbqt_gcAllocate_QInputEvent( ( void * ) pObj, true ) );
}

/* Qt::KeyboardModifiers modifiers () const */
HB_FUNC( QT_QINPUTEVENT_MODIFIERS )
{
   QInputEvent * p = hbqt_par_QInputEvent( 1 );
   if( p )
      hb_retni( ( Qt::KeyboardModifiers ) ( p )->modifiers() );
}


#endif /* #if QT_VERSION >= 0x040500 */
