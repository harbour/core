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
 *  enum StyleOptionType { Type }
 *  enum StyleOptionVersion { Version }
 *  Public Functions
 */

/*
 *  Constructed[ 0/1 [ 0.00% ] ]
 *
 *  *** Unconvered Prototypes ***
 *
 *  QVariant variant
 */

#include <QtCore/QPointer>

#include <QtGui/QStyleHintReturnVariant>


/*
 * QStyleHintReturnVariant ()
 */

typedef struct
{
   QStyleHintReturnVariant * ph;
   bool bNew;
   PHBQT_GC_FUNC func;
   int type;
} HBQT_GC_T_QStyleHintReturnVariant;

HBQT_GC_FUNC( hbqt_gcRelease_QStyleHintReturnVariant )
{
   HBQT_GC_T * p = ( HBQT_GC_T * ) Cargo;

   if( p && p->bNew )
      p->ph = NULL;
}

void * hbqt_gcAllocate_QStyleHintReturnVariant( void * pObj, bool bNew )
{
   HBQT_GC_T * p = ( HBQT_GC_T * ) hb_gcAllocate( sizeof( HBQT_GC_T ), hbqt_gcFuncs() );

   p->ph = ( QStyleHintReturnVariant * ) pObj;
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QStyleHintReturnVariant;
   p->type = HBQT_TYPE_QStyleHintReturnVariant;

   return p;
}

HB_FUNC( QT_QSTYLEHINTRETURNVARIANT )
{

}


#endif /* #if QT_VERSION >= 0x040500 */
