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

#include <QtCore/QLatin1Char>


/*
 * QLatin1Char ( char c )
 */

typedef struct
{
   QLatin1Char * ph;
   bool bNew;
   PHBQT_GC_FUNC func;
   int type;
} HBQT_GC_T_QLatin1Char;

HBQT_GC_FUNC( hbqt_gcRelease_QLatin1Char )
{
   HBQT_GC_T * p = ( HBQT_GC_T * ) Cargo;

   if( p )
   {
      if( p->bNew && p->ph )
         delete ( ( QLatin1Char * ) p->ph );
      p->ph = NULL;
   }
}

void * hbqt_gcAllocate_QLatin1Char( void * pObj, bool bNew )
{
   HBQT_GC_T * p = ( HBQT_GC_T * ) hb_gcAllocate( sizeof( HBQT_GC_T ), hbqt_gcFuncs() );

   p->ph = ( QLatin1Char * ) pObj;
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QLatin1Char;
   p->type = HBQT_TYPE_QLatin1Char;

   return p;
}

HB_FUNC( QT_QLATIN1CHAR )
{
   QLatin1Char * pObj = NULL;

   pObj = new QLatin1Char( *hb_parcx( 1 ) ) ;

   hb_retptrGC( hbqt_gcAllocate_QLatin1Char( ( void * ) pObj, true ) );
}

/* char toLatin1 () const */
HB_FUNC( QT_QLATIN1CHAR_TOLATIN1 )
{
   QLatin1Char * p = hbqt_par_QLatin1Char( 1 );
   if( p )
      hb_retni( ( p )->toLatin1() );
}

/* ushort unicode () const */
HB_FUNC( QT_QLATIN1CHAR_UNICODE )
{
   QLatin1Char * p = hbqt_par_QLatin1Char( 1 );
   if( p )
      hb_retni( ( p )->unicode() );
}


#endif /* #if QT_VERSION >= 0x040500 */
