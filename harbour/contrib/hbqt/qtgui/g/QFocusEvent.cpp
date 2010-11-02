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
 *  Constructed[ 3/3 [ 100.00% ] ]
 *
 */

#include <QtCore/QPointer>

#include <QtGui/QFocusEvent>


/*
 * QFocusEvent ( Type type, Qt::FocusReason reason = Qt::OtherFocusReason )
 */

typedef struct
{
   QFocusEvent * ph;
   bool bNew;
   PHBQT_GC_FUNC func;
   int type;
} HBQT_GC_T_QFocusEvent;

HBQT_GC_FUNC( hbqt_gcRelease_QFocusEvent )
{
   HBQT_GC_T * p = ( HBQT_GC_T * ) Cargo;

   if( p && p->bNew )
      p->ph = NULL;
}

void * hbqt_gcAllocate_QFocusEvent( void * pObj, bool bNew )
{
   HBQT_GC_T * p = ( HBQT_GC_T * ) hb_gcAllocate( sizeof( HBQT_GC_T ), hbqt_gcFuncs() );

   p->ph = ( QFocusEvent * ) pObj;
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QFocusEvent;
   p->type = HBQT_TYPE_QFocusEvent;

   return p;
}

HB_FUNC( QT_QFOCUSEVENT )
{
   // __HB_RETPTRGC__( new QFocusEvent() );
}

/* bool gotFocus () const */
HB_FUNC( QT_QFOCUSEVENT_GOTFOCUS )
{
   QFocusEvent * p = hbqt_par_QFocusEvent( 1 );
   if( p )
      hb_retl( ( p )->gotFocus() );
}

/* bool lostFocus () const */
HB_FUNC( QT_QFOCUSEVENT_LOSTFOCUS )
{
   QFocusEvent * p = hbqt_par_QFocusEvent( 1 );
   if( p )
      hb_retl( ( p )->lostFocus() );
}

/* Qt::FocusReason reason () const */
HB_FUNC( QT_QFOCUSEVENT_REASON )
{
   QFocusEvent * p = hbqt_par_QFocusEvent( 1 );
   if( p )
      hb_retni( ( Qt::FocusReason ) ( p )->reason() );
}


#endif /* #if QT_VERSION >= 0x040500 */
