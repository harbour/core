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
 *  Constructed[ 11/11 [ 100.00% ] ]
 *
 */

#include <QtCore/QPointer>

#include <QtCore/QMetaEnum>


/*
 * QMetaEnum ()
 *
 */

typedef struct
{
   QMetaEnum * ph;
   bool bNew;
   PHBQT_GC_FUNC func;
   int type;
} HBQT_GC_T_QMetaEnum;

HBQT_GC_FUNC( hbqt_gcRelease_QMetaEnum )
{
   HB_SYMBOL_UNUSED( Cargo );
   HBQT_GC_T * p = ( HBQT_GC_T * ) Cargo;

   if( p && p->bNew )
      p->ph = NULL;
}

void * hbqt_gcAllocate_QMetaEnum( void * pObj, bool bNew )
{
   HBQT_GC_T * p = ( HBQT_GC_T * ) hb_gcAllocate( sizeof( HBQT_GC_T ), hbqt_gcFuncs() );

   p->ph = ( QMetaEnum * ) pObj;
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QMetaEnum;
   p->type = HBQT_TYPE_QMetaEnum;

   return p;
}

HB_FUNC( QT_QMETAENUM )
{
   // __HB_RETPTRGC__( new QMetaEnum() );
}

/* bool isFlag () const */
HB_FUNC( QT_QMETAENUM_ISFLAG )
{
   QMetaEnum * p = hbqt_par_QMetaEnum( 1 );
   if( p )
      hb_retl( ( p )->isFlag() );
}

/* bool isValid () const */
HB_FUNC( QT_QMETAENUM_ISVALID )
{
   QMetaEnum * p = hbqt_par_QMetaEnum( 1 );
   if( p )
      hb_retl( ( p )->isValid() );
}

/* const char * key ( int index ) const */
HB_FUNC( QT_QMETAENUM_KEY )
{
   QMetaEnum * p = hbqt_par_QMetaEnum( 1 );
   if( p )
      hb_retc( ( p )->key( hb_parni( 2 ) ) );
}

/* int keyCount () const */
HB_FUNC( QT_QMETAENUM_KEYCOUNT )
{
   QMetaEnum * p = hbqt_par_QMetaEnum( 1 );
   if( p )
      hb_retni( ( p )->keyCount() );
}

/* int keyToValue ( const char * key ) const */
HB_FUNC( QT_QMETAENUM_KEYTOVALUE )
{
   QMetaEnum * p = hbqt_par_QMetaEnum( 1 );
   if( p )
      hb_retni( ( p )->keyToValue( ( const char * ) hb_parc( 2 ) ) );
}

/* int keysToValue ( const char * keys ) const */
HB_FUNC( QT_QMETAENUM_KEYSTOVALUE )
{
   QMetaEnum * p = hbqt_par_QMetaEnum( 1 );
   if( p )
      hb_retni( ( p )->keysToValue( ( const char * ) hb_parc( 2 ) ) );
}

/* const char * name () const */
HB_FUNC( QT_QMETAENUM_NAME )
{
   QMetaEnum * p = hbqt_par_QMetaEnum( 1 );
   if( p )
      hb_retc( ( p )->name() );
}

/* const char * scope () const */
HB_FUNC( QT_QMETAENUM_SCOPE )
{
   QMetaEnum * p = hbqt_par_QMetaEnum( 1 );
   if( p )
      hb_retc( ( p )->scope() );
}

/* int value ( int index ) const */
HB_FUNC( QT_QMETAENUM_VALUE )
{
   QMetaEnum * p = hbqt_par_QMetaEnum( 1 );
   if( p )
      hb_retni( ( p )->value( hb_parni( 2 ) ) );
}

/* const char * valueToKey ( int value ) const */
HB_FUNC( QT_QMETAENUM_VALUETOKEY )
{
   QMetaEnum * p = hbqt_par_QMetaEnum( 1 );
   if( p )
      hb_retc( ( p )->valueToKey( hb_parni( 2 ) ) );
}

/* QByteArray valueToKeys ( int value ) const */
HB_FUNC( QT_QMETAENUM_VALUETOKEYS )
{
   QMetaEnum * p = hbqt_par_QMetaEnum( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QByteArray( new QByteArray( ( p )->valueToKeys( hb_parni( 2 ) ) ), true ) );
}


#endif /* #if QT_VERSION >= 0x040500 */
