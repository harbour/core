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

#include <QtGui/QDragLeaveEvent>


/*
 * QDragLeaveEvent ()
 */

typedef struct
{
   QDragLeaveEvent * ph;
   bool bNew;
   PHBQT_GC_FUNC func;
   int type;
} HBQT_GC_T_QDragLeaveEvent;

HBQT_GC_FUNC( hbqt_gcRelease_QDragLeaveEvent )
{
   HB_SYMBOL_UNUSED( Cargo );
   HBQT_GC_T * p = ( HBQT_GC_T * ) Cargo;

   if( p && p->bNew )
      p->ph = NULL;
}

void * hbqt_gcAllocate_QDragLeaveEvent( void * pObj, bool bNew )
{
   HBQT_GC_T * p = ( HBQT_GC_T * ) hb_gcAllocate( sizeof( HBQT_GC_T ), hbqt_gcFuncs() );

   p->ph = ( QDragLeaveEvent * ) pObj;
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QDragLeaveEvent;
   p->type = HBQT_TYPE_QDragLeaveEvent;

   return p;
}

HB_FUNC( QT_QDRAGLEAVEEVENT )
{
   // __HB_RETPTRGC__( new QDragLeaveEvent() );
}


#endif /* #if QT_VERSION >= 0x040500 */
