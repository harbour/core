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
 *  enum Type { Void, Bool, Int, UInt, ..., User }
 */

/*
 *  Constructed[ 4/4 [ 100.00% ] ]
 *
 *
 *  *** Commented out protostypes ***
 *
 *  //void * construct ( int type, const void * copy = 0 )
 *  //void destroy ( int type, void * data )
 *  //bool load ( QDataStream & stream, int type, void * data )
 *  //bool save ( QDataStream & stream, int type, const void * data )
 */

#include <QtCore/QPointer>

#include <QtCore/QMetaType>


/*
 * QMetaType ()
 *
 */

typedef struct
{
   QMetaType * ph;
   bool bNew;
   PHBQT_GC_FUNC func;
   int type;
} HBQT_GC_T_QMetaType;

HBQT_GC_FUNC( hbqt_gcRelease_QMetaType )
{
   HBQT_GC_T * p = ( HBQT_GC_T * ) Cargo;

   if( p && p->bNew )
      p->ph = NULL;
}

void * hbqt_gcAllocate_QMetaType( void * pObj, bool bNew )
{
   HBQT_GC_T * p = ( HBQT_GC_T * ) hb_gcAllocate( sizeof( HBQT_GC_T ), hbqt_gcFuncs() );

   p->ph = ( QMetaType * ) pObj;
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QMetaType;
   p->type = HBQT_TYPE_QMetaType;

   return p;
}

HB_FUNC( QT_QMETATYPE )
{
   // __HB_RETPTRGC__( new QMetaType() );
}

/* bool isRegistered ( int type ) */
HB_FUNC( QT_QMETATYPE_ISREGISTERED )
{
   QMetaType * p = hbqt_par_QMetaType( 1 );
   if( p )
      hb_retl( ( p )->isRegistered( hb_parni( 2 ) ) );
}

/* int type ( const char * typeName ) */
HB_FUNC( QT_QMETATYPE_TYPE )
{
   QMetaType * p = hbqt_par_QMetaType( 1 );
   if( p )
      hb_retni( ( p )->type( ( const char * ) hb_parc( 2 ) ) );
}

/* const char * typeName ( int type ) */
HB_FUNC( QT_QMETATYPE_TYPENAME )
{
   QMetaType * p = hbqt_par_QMetaType( 1 );
   if( p )
      hb_retc( ( p )->typeName( hb_parni( 2 ) ) );
}

/* void unregisterType ( const char * typeName ) */
HB_FUNC( QT_QMETATYPE_UNREGISTERTYPE )
{
   QMetaType * p = hbqt_par_QMetaType( 1 );
   if( p )
      ( p )->unregisterType( ( const char * ) hb_parc( 2 ) );
}


#endif /* #if QT_VERSION >= 0x040500 */
