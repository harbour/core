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
 *  Constructed[ 0/0 [ 0% ] ]
 *
 */

#include <QtCore/QPointer>

#include <QtCore/QGenericReturnArgument>


/*
 * QGenericArgument ( const char * name = 0, const void * data = 0 )
 *
 */

typedef struct
{
   QGenericReturnArgument * ph;
   bool bNew;
   PHBQT_GC_FUNC func;
   int type;
} HBQT_GC_T_QGenericReturnArgument;

HBQT_GC_FUNC( hbqt_gcRelease_QGenericReturnArgument )
{
   HBQT_GC_T * p = ( HBQT_GC_T * ) Cargo;

   if( p )
   {
      if( p->bNew && p->ph )
         delete ( ( QGenericReturnArgument * ) p->ph );
      p->ph = NULL;
   }
}

void * hbqt_gcAllocate_QGenericReturnArgument( void * pObj, bool bNew )
{
   HBQT_GC_T * p = ( HBQT_GC_T * ) hb_gcAllocate( sizeof( HBQT_GC_T ), hbqt_gcFuncs() );

   p->ph = ( QGenericReturnArgument * ) pObj;
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QGenericReturnArgument;
   p->type = HBQT_TYPE_QGenericReturnArgument;

   return p;
}

HB_FUNC( QT_QGENERICRETURNARGUMENT )
{
   QGenericReturnArgument * pObj = NULL;

   if( hb_pcount() == 1 && HB_ISCHAR( 1 ) )
   {
      pObj = new QGenericReturnArgument( hb_parc( 1 ), 0 ) ;
   }
   else if( hb_pcount() == 2 && HB_ISCHAR( 1 ) && HB_ISPOINTER( 2 ) )
   {
      pObj = new QGenericReturnArgument( hb_parc( 1 ), hb_parptr( 2 ) ) ;
   }
   else
   {
      pObj = new QGenericReturnArgument() ;
   }

   hb_retptrGC( hbqt_gcAllocate_QGenericReturnArgument( ( void * ) pObj, true ) );
}


#endif /* #if QT_VERSION >= 0x040500 */
