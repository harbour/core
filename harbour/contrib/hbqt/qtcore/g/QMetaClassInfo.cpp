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

#if QT_VERSION >= 0x040500

/*
 *  Constructed[ 2/2 [ 100.00% ] ]
 *
 */

#include <QtCore/QPointer>

#include <QtCore/QMetaClassInfo>


/*
 * QMetaClassInfo ()
 *
 */

typedef struct
{
   QMetaClassInfo * ph;
   bool bNew;
   PHBQT_GC_FUNC func;
   int type;
} HBQT_GC_T_QMetaClassInfo;

HBQT_GC_FUNC( hbqt_gcRelease_QMetaClassInfo )
{
   HBQT_GC_T * p = ( HBQT_GC_T * ) Cargo;

   if( p && p->bNew )
      p->ph = NULL;
}

void * hbqt_gcAllocate_QMetaClassInfo( void * pObj, bool bNew )
{
   HBQT_GC_T * p = ( HBQT_GC_T * ) hb_gcAllocate( sizeof( HBQT_GC_T ), hbqt_gcFuncs() );

   p->ph = ( QMetaClassInfo * ) pObj;
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QMetaClassInfo;
   p->type = HBQT_TYPE_QMetaClassInfo;

   return p;
}

HB_FUNC( QT_QMETACLASSINFO )
{
   // __HB_RETPTRGC__( new QMetaClassInfo() );
}

/* const char * name () const */
HB_FUNC( QT_QMETACLASSINFO_NAME )
{
   QMetaClassInfo * p = hbqt_par_QMetaClassInfo( 1 );
   if( p )
      hb_retc( ( p )->name() );
}

/* const char * value () const */
HB_FUNC( QT_QMETACLASSINFO_VALUE )
{
   QMetaClassInfo * p = hbqt_par_QMetaClassInfo( 1 );
   if( p )
      hb_retc( ( p )->value() );
}


#endif /* #if QT_VERSION >= 0x040500 */
