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
 *  Constructed[ 16/16 [ 100.00% ] ]
 *
 */

#include <QtCore/QPointer>

#include <QtCore/QBitArray>


/* QBitArray ()
 * QBitArray ( int size, bool value = false )
 * QBitArray ( const QBitArray & other )
 */

typedef struct
{
   QBitArray * ph;
   bool bNew;
   PHBQT_GC_FUNC func;
   int type;
} HBQT_GC_T_QBitArray;

HBQT_GC_FUNC( hbqt_gcRelease_QBitArray )
{
   HBQT_GC_T * p = ( HBQT_GC_T * ) Cargo;

   if( p )
   {
      if( p->bNew && p->ph )
         delete ( ( QBitArray * ) p->ph );
      p->ph = NULL;
   }
}

void * hbqt_gcAllocate_QBitArray( void * pObj, bool bNew )
{
   HBQT_GC_T * p = ( HBQT_GC_T * ) hb_gcAllocate( sizeof( HBQT_GC_T ), hbqt_gcFuncs() );

   p->ph = ( QBitArray * ) pObj;
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QBitArray;
   p->type = HBQT_TYPE_QBitArray;

   return p;
}

HB_FUNC( QT_QBITARRAY )
{
   QBitArray * pObj = NULL;

   pObj = new QBitArray() ;

   hb_retptrGC( hbqt_gcAllocate_QBitArray( ( void * ) pObj, true ) );
}

/* bool at ( int i ) const */
HB_FUNC( QT_QBITARRAY_AT )
{
   QBitArray * p = hbqt_par_QBitArray( 1 );
   if( p )
      hb_retl( ( p )->at( hb_parni( 2 ) ) );
}

/* void clear () */
HB_FUNC( QT_QBITARRAY_CLEAR )
{
   QBitArray * p = hbqt_par_QBitArray( 1 );
   if( p )
      ( p )->clear();
}

/* void clearBit ( int i ) */
HB_FUNC( QT_QBITARRAY_CLEARBIT )
{
   QBitArray * p = hbqt_par_QBitArray( 1 );
   if( p )
      ( p )->clearBit( hb_parni( 2 ) );
}

/* int count () const */
HB_FUNC( QT_QBITARRAY_COUNT )
{
   QBitArray * p = hbqt_par_QBitArray( 1 );
   if( p )
      hb_retni( ( p )->count() );
}

/* int count ( bool on ) const */
HB_FUNC( QT_QBITARRAY_COUNT_1 )
{
   QBitArray * p = hbqt_par_QBitArray( 1 );
   if( p )
      hb_retni( ( p )->count( hb_parl( 2 ) ) );
}

/* bool fill ( bool value, int size = -1 ) */
HB_FUNC( QT_QBITARRAY_FILL )
{
   QBitArray * p = hbqt_par_QBitArray( 1 );
   if( p )
      hb_retl( ( p )->fill( hb_parl( 2 ), hb_parnidef( 3, -1 ) ) );
}

/* void fill ( bool value, int begin, int end ) */
HB_FUNC( QT_QBITARRAY_FILL_1 )
{
   QBitArray * p = hbqt_par_QBitArray( 1 );
   if( p )
      ( p )->fill( hb_parl( 2 ), hb_parni( 3 ), hb_parni( 4 ) );
}

/* bool isEmpty () const */
HB_FUNC( QT_QBITARRAY_ISEMPTY )
{
   QBitArray * p = hbqt_par_QBitArray( 1 );
   if( p )
      hb_retl( ( p )->isEmpty() );
}

/* bool isNull () const */
HB_FUNC( QT_QBITARRAY_ISNULL )
{
   QBitArray * p = hbqt_par_QBitArray( 1 );
   if( p )
      hb_retl( ( p )->isNull() );
}

/* void resize ( int size ) */
HB_FUNC( QT_QBITARRAY_RESIZE )
{
   QBitArray * p = hbqt_par_QBitArray( 1 );
   if( p )
      ( p )->resize( hb_parni( 2 ) );
}

/* void setBit ( int i ) */
HB_FUNC( QT_QBITARRAY_SETBIT )
{
   QBitArray * p = hbqt_par_QBitArray( 1 );
   if( p )
      ( p )->setBit( hb_parni( 2 ) );
}

/* void setBit ( int i, bool value ) */
HB_FUNC( QT_QBITARRAY_SETBIT_1 )
{
   QBitArray * p = hbqt_par_QBitArray( 1 );
   if( p )
      ( p )->setBit( hb_parni( 2 ), hb_parl( 3 ) );
}

/* int size () const */
HB_FUNC( QT_QBITARRAY_SIZE )
{
   QBitArray * p = hbqt_par_QBitArray( 1 );
   if( p )
      hb_retni( ( p )->size() );
}

/* bool testBit ( int i ) const */
HB_FUNC( QT_QBITARRAY_TESTBIT )
{
   QBitArray * p = hbqt_par_QBitArray( 1 );
   if( p )
      hb_retl( ( p )->testBit( hb_parni( 2 ) ) );
}

/* bool toggleBit ( int i ) */
HB_FUNC( QT_QBITARRAY_TOGGLEBIT )
{
   QBitArray * p = hbqt_par_QBitArray( 1 );
   if( p )
      hb_retl( ( p )->toggleBit( hb_parni( 2 ) ) );
}

/* void truncate ( int pos ) */
HB_FUNC( QT_QBITARRAY_TRUNCATE )
{
   QBitArray * p = hbqt_par_QBitArray( 1 );
   if( p )
      ( p )->truncate( hb_parni( 2 ) );
}


#endif /* #if QT_VERSION >= 0x040500 */
