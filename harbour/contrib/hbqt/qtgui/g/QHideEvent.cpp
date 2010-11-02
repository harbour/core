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
 *  Constructed[ 0/0 [ 0% ] ]
 *
 */

#include <QtCore/QPointer>

#include <QtGui/QHideEvent>


/*
 * QHideEvent ()
 */

typedef struct
{
   QHideEvent * ph;
   bool bNew;
   PHBQT_GC_FUNC func;
   int type;
} HBQT_GC_T_QHideEvent;

HBQT_GC_FUNC( hbqt_gcRelease_QHideEvent )
{
   HB_SYMBOL_UNUSED( Cargo );
   HBQT_GC_T * p = ( HBQT_GC_T * ) Cargo;

   if( p && p->bNew )
      p->ph = NULL;
}

void * hbqt_gcAllocate_QHideEvent( void * pObj, bool bNew )
{
   HBQT_GC_T * p = ( HBQT_GC_T * ) hb_gcAllocate( sizeof( HBQT_GC_T ), hbqt_gcFuncs() );

   p->ph = ( QHideEvent * ) pObj;
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QHideEvent;
   p->type = HBQT_TYPE_QHideEvent;

   return p;
}

HB_FUNC( QT_QHIDEEVENT )
{
   // __HB_RETPTRGC__( new QHideEvent() );
}


#endif /* #if QT_VERSION >= 0x040500 */
