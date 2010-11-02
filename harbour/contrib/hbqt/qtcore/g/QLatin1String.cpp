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
 *  Constructed[ 1/1 [ 100.00% ] ]
 *
 */

#include <QtCore/QPointer>

#include <QtCore/QLatin1String>


/*
 * QLatin1String ( const char * str )
 */

typedef struct
{
   QLatin1String * ph;
   bool bNew;
   PHBQT_GC_FUNC func;
   int type;
} HBQT_GC_T_QLatin1String;

HBQT_GC_FUNC( hbqt_gcRelease_QLatin1String )
{
   HBQT_GC_T * p = ( HBQT_GC_T * ) Cargo;

   if( p && p->bNew )
   {
      if( p->ph )
      {
         delete ( ( QLatin1String * ) p->ph );
         p->ph = NULL;
      }
      else
         p->ph = NULL;
   }
   else
      p->ph = NULL;
}

void * hbqt_gcAllocate_QLatin1String( void * pObj, bool bNew )
{
   HBQT_GC_T * p = ( HBQT_GC_T * ) hb_gcAllocate( sizeof( HBQT_GC_T ), hbqt_gcFuncs() );

   p->ph = ( QLatin1String * ) pObj;
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QLatin1String;
   p->type = HBQT_TYPE_QLatin1String;

   return p;
}

HB_FUNC( QT_QLATIN1STRING )
{
   QLatin1String * pObj = NULL;

   pObj = new QLatin1String( hb_parcx( 1 ) ) ;

   hb_retptrGC( hbqt_gcAllocate_QLatin1String( ( void * ) pObj, true ) );
}

/* const char * latin1 () const */
HB_FUNC( QT_QLATIN1STRING_LATIN1 )
{
   QLatin1String * p = hbqt_par_QLatin1String( 1 );
   if( p )
      hb_retc( ( p )->latin1() );
}


#endif /* #if QT_VERSION >= 0x040500 */
